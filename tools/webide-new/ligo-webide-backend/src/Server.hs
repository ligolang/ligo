module Server
  ( startApp
  , mkApp
  , Config (..)
  )
where

import Control.Arrow ((>>>))
import Control.Monad (forM_, (>=>))
import Control.Monad.Except (ExceptT, runExcept, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson (decodeStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Katip (Environment(..), KatipT, initLogEnv, runKatipT)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Numeric (showFFloat)
import Servant
  (Application, Handler(..), JSON, Post, ReqBody, Server, ServerError, err400, err500, errBody,
  hoistServer, serve, (:<|>)((:<|>)), (:>))
import Servant.Client (BaseUrl(..), Scheme(Https))
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withTempDirectory)
import System.Process (proc, readCreateProcessWithExitCode)
import Text.Megaparsec (errorBundlePretty)

import Morley.Client
  (MorleyClientConfig(..), MorleyClientEnv, MorleyClientM, OperationInfo(OpOriginate),
  OriginationData(..), dryRunOperationsNonEmpty, getProtocolParameters, importKey,
  mkMorleyClientEnv, revealKeyUnlessRevealed, runMorleyClientM)
import Morley.Client.Action.Common (computeStorageLimit)
import Morley.Client.RPC (AppliedResult, ProtocolParameters(ppCostPerByte))
import Morley.Micheline (StringEncode(unStringEncode), TezosMutez(..), unStringEncode)
import Morley.Michelson.Macro (expandContract)
import Morley.Michelson.Parser
  (MichelsonSource(MSUnspecified), ParserException(..), parseExpandValue, parseNoEnv, program)
import Morley.Michelson.Printer (renderDoc)
import Morley.Michelson.Printer.Util (doesntNeedParens)
import Morley.Michelson.TypeCheck (TypeCheckOptions(..), typeCheckContractAndStorage)
import Morley.Michelson.Typed (SomeContractAndStorage(SomeContractAndStorage))
import Morley.Michelson.Untyped (Contract, Value)
import Morley.Tezos.Address (KindedAddress(ImplicitAddress))
import Morley.Tezos.Address.Alias
  (AddressOrAlias(AddressAlias), Alias(ContractAlias, ImplicitAlias))
import Morley.Tezos.Core (Mutez(UnsafeMutez), unMutez)
import Morley.Tezos.Crypto (KeyHash, PublicKey, SecretKey, detSecretKey, hashKey, toPublic)

import Schema.CompileExpressionRequest (CompileExpressionRequest(..))
import Schema.CompileRequest (CompileRequest(..))
import Schema.CompilerResponse (CompilerResponse(..))
import Schema.DeployScript (DeployScript(..))
import Schema.DryRunRequest (DryRunRequest(..))
import Schema.GenerateDeployScriptRequest (GenerateDeployScriptRequest(..))
import Schema.ListDeclarationsRequest (ListDeclarationsRequest(..))
import Schema.ListDeclarationsResponse (ListDeclarationsResponse)
import Types (DisplayFormat(..), Source(..))

type API =
       "compile" :> ReqBody '[JSON] CompileRequest :> Post '[JSON] CompilerResponse
  :<|> "generate-deploy-script" :> ReqBody '[JSON] GenerateDeployScriptRequest :> Post '[JSON] DeployScript
  :<|> "compile-expression" :> ReqBody '[JSON] CompileExpressionRequest :> Post '[JSON] CompilerResponse
  :<|> "dry-run" :> ReqBody '[JSON] DryRunRequest :> Post '[JSON] CompilerResponse
  :<|> "list-declarations" :> ReqBody '[JSON] ListDeclarationsRequest :> Post '[JSON] ListDeclarationsResponse

type SwaggeredAPI =
  SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> API

data Config = Config
  { cLigoPath :: Maybe FilePath
  , cTezosClientPath :: Maybe FilePath
  , cPort :: Int
  , cVerbose :: Bool
  , cDockerizedLigoVersion :: Maybe String
  }

type WebIDEM = KatipT (ReaderT Config (ExceptT ServerError IO))

startApp :: Config -> IO ()
startApp config = run (cPort config) (mkApp config)

mkApp :: Config -> Application
mkApp config =
  maybeLogRequests . corsWithContentType $ serve (Proxy @SwaggeredAPI) server
  where
    maybeLogRequests :: Middleware
    maybeLogRequests =
      if cVerbose config
      then logStdoutDev
      else id

    -- Allow Content-Type header with values other then allowed by simpleCors.
    corsWithContentType :: Middleware
    corsWithContentType = cors (const $ Just policy)
      where
        policy = simpleCorsResourcePolicy
          {corsRequestHeaders = ["Content-Type"]}

    server :: Server SwaggeredAPI
    server =
      swaggerSchemaUIServer (toSwagger (Proxy @API))
        :<|> hoistServer (Proxy @API) hoist (compile :<|> generateDeployScript :<|> compileExpression :<|> dryRun :<|> listDeclarations)

    hoist :: WebIDEM a -> Handler a
    hoist x = Handler $ do
      logEnv <- liftIO $ initLogEnv "ligo-webide" (Environment "devel")
      runReaderT (runKatipT logEnv x) config


removeExcessWhitespace :: Text -> Text
removeExcessWhitespace =
  Text.lines >>> map Text.strip >>> Text.intercalate " " >>> Text.strip

generateDeployScript :: GenerateDeployScriptRequest -> WebIDEM DeployScript
generateDeployScript request = do
  let build :: CompileRequest -> WebIDEM Text
      build = fmap unCompilerResponse . compile

  let buildJSON :: CompileRequest -> WebIDEM Text
      buildJSON = build >=> decodeTextCode

  michelsonCode <- build CompileRequest
    { rSources = gdsrSources request
    , rMain = gdsrMain request
    , rStorage = Nothing
    , rEntrypoint = gdsrEntrypoint request
    , rProtocol = Nothing
    , rDisplayFormat = Just DFHumanReadable
    }
  michelsonStorage <- build CompileRequest
    { rSources = gdsrSources request
    , rMain = gdsrMain request
    , rStorage = Just (gdsrStorage request)
    , rEntrypoint = gdsrEntrypoint request
    , rProtocol = Nothing
    , rDisplayFormat = Just DFHumanReadable
    }
  michelsonCodeJson <- buildJSON CompileRequest
    { rSources = gdsrSources request
    , rMain = gdsrMain request
    , rStorage = Nothing
    , rEntrypoint = gdsrEntrypoint request
    , rProtocol = Nothing
    , rDisplayFormat = Just DFJson
    }
  michelsonStorageJson <- buildJSON CompileRequest
    { rSources = gdsrSources request
    , rMain = gdsrMain request
    , rStorage = Just (gdsrStorage request)
    , rEntrypoint = gdsrEntrypoint request
    , rProtocol = Nothing
    , rDisplayFormat = Just DFJson
    }

  contract :: Contract <-
    case parseNoEnv program MSUnspecified michelsonCodeJson of
      Left bundle -> lift . throwError $
        err400 {errBody = LBS.pack $ errorBundlePretty bundle}
      Right y -> pure (expandContract y)

  storage :: Value <-
    case parseExpandValue MSUnspecified michelsonStorageJson of
      Left (ParserException bundle) -> lift . throwError $
        err400 {errBody = LBS.pack $ errorBundlePretty bundle}
      Right y -> pure y

  typeCheckResult :: SomeContractAndStorage <- do
      let options :: TypeCheckOptions
          options = TypeCheckOptions
            { tcVerbose = False
            , tcStrict = False
            }
          typeCheck =
               runExcept
             $ runReaderT (typeCheckContractAndStorage contract storage) options
       in case typeCheck of
            Left tcError -> lift . throwError $
              err400 {errBody = LBS.pack (show (renderDoc doesntNeedParens tcError))}
            Right good -> pure good

  let originationData :: OriginationData
      originationData = mkOriginationData typeCheckResult

  tezosClientPath <- lift (asks cTezosClientPath) >>= \case
    Nothing -> lift . throwError $ err500
      {errBody = "server doesn't have access to LIGO binary."}
    Just p -> pure p

  let morleyConfig :: MorleyClientConfig
      morleyConfig = MorleyClientConfig
        { mccEndpointUrl = Just (BaseUrl Https "jakarta.testnet.tezos.serokell.team" 443 "")
        , mccTezosClientPath = tezosClientPath
        , mccMbTezosClientDataDir = Nothing
        , mccVerbosity = 0
        , mccSecretKey = Nothing
        }

  env :: MorleyClientEnv <- liftIO $ mkMorleyClientEnv morleyConfig
  ops <- liftIO $ runMorleyClientM env (dryRunOperations originationData)
  let appliedResults :: [AppliedResult]
      appliedResults = map fst . NonEmpty.toList $ ops
  protocolParams <- liftIO $ runMorleyClientM env getProtocolParameters

  let storageLimit = unStringEncode $ computeStorageLimit appliedResults protocolParams
  let costPerByte = unMutez $ unTezosMutez $ ppCostPerByte protocolParams
  let burnFee = fromIntegral costPerByte * storageLimit

  let script = Text.pack $
          "tezos-client \\\
        \ originate \\\
        \ contract \\\
        \ " ++ Text.unpack (gdsrName request) ++ " \\\
        \ transferring 0 \\\
        \ from $YOUR_SOURCE_ACCOUNT \\\
        \ running '" ++ Text.unpack (removeExcessWhitespace michelsonCode) ++ "' \\\
        \ --init '" ++ Text.unpack (removeExcessWhitespace michelsonStorage) ++ "' \\\
        \ --burn-cap " ++ showFFloat (Just 5) (fromIntegral burnFee / (1e6 :: Double)) "" ++ "\n"

  pure $ DeployScript
    { dsScript = script
    , dsBuild = CompilerResponse michelsonCode
    }

decodeTextCode :: Text -> WebIDEM Text
decodeTextCode text =
  let mvalue = do
        mp <- decodeStrict @(Map Text Text) . Text.encodeUtf8 $ text
        Map.lookup "text_code" mp
   in case mvalue of
        Nothing -> lift . throwError $ err500
          {errBody = "could not decode compiler call"}
        Just value -> pure value

mkOriginationData :: SomeContractAndStorage -> OriginationData
mkOriginationData (SomeContractAndStorage con val) =
  OriginationData
    { odReplaceExisting = True
    , odName = ContractAlias "contract"
    , odBalance = UnsafeMutez 0
    , odContract = con
    , odStorage = val
    , odMbFee = Nothing
    }

randomBytes :: BS.ByteString
randomBytes = BS.pack
  [ 94, 31, 110, 237, 170,
    152, 124, 126, 134, 113,
    23, 165, 114, 255, 236,
    28, 207, 30, 40, 11
  ]

secretKey :: SecretKey
secretKey = detSecretKey randomBytes

publicKey :: PublicKey
publicKey = toPublic secretKey

keyHash :: KeyHash
keyHash = hashKey publicKey

dryRunOperations
  :: OriginationData
  -> MorleyClientM (NonEmpty (AppliedResult, TezosMutez))
dryRunOperations originationData = do
  alias <- importKey True (ImplicitAlias "sender") secretKey
  revealKeyUnlessRevealed (ImplicitAddress keyHash) Nothing
  dryRunOperationsNonEmpty
    (AddressAlias alias)
    (NonEmpty.singleton (OpOriginate originationData))

compile :: CompileRequest -> WebIDEM CompilerResponse
compile request = do
  pwd <- liftIO $ getCurrentDirectory
  let (filepaths, sources) = unzip (rSources request)
   in withTempDirectory pwd "" $ \dirPath -> do
        let fullFilepaths = map (dirPath </>) filepaths
        let fullMainPath = dirPath </> rMain request

        liftIO . forM_ (zip fullFilepaths sources) $ \(fp, src) -> do
          createDirectoryIfMissing True (takeDirectory fp)
          Text.writeFile fp (unSource src)

        (ec, out, err) <- do
          let commands = case rStorage request of
                Nothing -> ["compile", "contract", fullMainPath]
                Just storage -> ["compile", "storage", fullMainPath, Text.unpack storage]
          let commands1 = (commands ++) $ case rDisplayFormat request of
                Nothing -> []
                Just df -> ("--display-format":) $ case df of
                  DFDev -> ["dev"]
                  DFHumanReadable -> ["human-readable"]
                  DFJson -> ["json"]
          let commands2 = (commands1 ++) $ case rProtocol request of
                Nothing -> []
                Just pr -> ["-p", Text.unpack pr]
           in runLigo dirPath commands2

        case ec of
          ExitSuccess -> pure (CompilerResponse $ Text.pack out)
          ExitFailure _ -> pure (CompilerResponse $ Text.pack err)

compileExpression :: CompileExpressionRequest -> WebIDEM CompilerResponse
compileExpression request = do
  pwd <- liftIO $ getCurrentDirectory
  let (filepaths, sources) = unzip (cerSources request)
   in withTempDirectory pwd "" $ \dirPath -> do
        let fullFilepaths = map (dirPath </>) filepaths
        let fullMainPath = dirPath </> cerMain request

        liftIO . forM_ (zip fullFilepaths sources) $ \(fp, src) -> do
          createDirectoryIfMissing True (takeDirectory fp)
          Text.writeFile fp (unSource src)

        dialect <- case inferDialect fullMainPath of
          Nothing -> lift . throwError $ err400
            {errBody = "couldn't infer dialect from filetype"}
          Just d -> pure d

        (ec, out, err) <- do
          let commands = ["compile", "expression", prettyDialect dialect, Text.unpack (cerFunction request)]
          let commands1 = (commands ++) $ case cerDisplayFormat request of
                Nothing -> []
                Just df -> ("--display-format":) $ case df of
                  DFDev -> ["dev"]
                  DFHumanReadable -> ["human-readable"]
                  DFJson -> ["json"]
          let commands2 = (commands1 ++) $ case cerProtocol request of
                Nothing -> []
                Just pr -> ["-p", Text.unpack pr]
          let commands3 = commands2 ++ ["--init-file", fullMainPath]

           in runLigo dirPath commands3

        case ec of
          ExitSuccess -> pure (CompilerResponse $ Text.pack out)
          ExitFailure _ -> pure (CompilerResponse $ Text.pack err)

dryRun :: DryRunRequest -> WebIDEM CompilerResponse
dryRun request = do
  pwd <- liftIO $ getCurrentDirectory
  let (filepaths, sources) = unzip (drrSources request)
   in withTempDirectory pwd "" $ \dirPath -> do
        let fullFilepaths = map (dirPath </>) filepaths
        let fullMainPath = dirPath </> drrMain request

        liftIO . forM_ (zip fullFilepaths sources) $ \(fp, src) -> do
          createDirectoryIfMissing True (takeDirectory fp)
          Text.writeFile fp (unSource src)

        (ec, out, err) <- do
          let commands = ["run", "dry-run", fullMainPath, Text.unpack (drrParameters request), Text.unpack (drrStorage request)]
          let commands1 = (commands ++) $ case drrDisplayFormat request of
                Nothing -> []
                Just df -> ("--display-format":) $ case df of
                  DFDev -> ["dev"]
                  DFHumanReadable -> ["human-readable"]
                  DFJson -> ["json"]
          let commands2 = (commands1 ++) $ case drrProtocol request of
                Nothing -> []
                Just pr -> ["-p", Text.unpack pr]
          let commands3 = (commands2 ++) $ case drrEntrypoint request of
                Nothing -> []
                Just e -> ["-e", Text.unpack e]
           in runLigo dirPath commands3

        case ec of
          ExitSuccess -> pure (CompilerResponse $ Text.pack out)
          ExitFailure _ -> pure (CompilerResponse $ Text.pack err)

listDeclarations :: ListDeclarationsRequest -> WebIDEM ListDeclarationsResponse
listDeclarations request = do
  pwd <- liftIO $ getCurrentDirectory
  let (filepaths, sources) = unzip (ldrSources request)
   in withTempDirectory pwd "" $ \dirPath -> do
        let fullFilepaths = map (dirPath </>) filepaths
        let fullMainPath = dirPath </> ldrMain request

        liftIO . forM_ (zip fullFilepaths sources) $ \(fp, src) -> do
          createDirectoryIfMissing True (takeDirectory fp)
          Text.writeFile fp (unSource src)

        (ec, out, err) <- do
          let commands =
                [ "info"
                , "list-declarations"
                , fullMainPath
                , "--display-format"
                , "dev"]
                 ++["--only-ep" | ldrOnlyEndpoint request]
           in runLigo dirPath commands

        case ec of
          ExitSuccess -> do
            -- TODO: make this more robust
            pure . tail . Text.lines . Text.strip . Text.pack $ out
          ExitFailure _ -> lift . throwError $ err400
           {errBody = LBS.pack err}

data Dialect = CameLIGO | PascaLIGO | JsLIGO
  deriving stock (Eq, Show, Ord, Enum)

prettyDialect :: Dialect -> String
prettyDialect = \case
  CameLIGO -> "cameligo"
  PascaLIGO -> "pascaligo"
  JsLIGO -> "jsligo"

inferDialect :: FilePath -> Maybe Dialect
inferDialect filepath =
  case Text.takeWhileEnd (/= '.') (Text.pack filepath) of
    "mligo" -> Just CameLIGO
    "ligo" -> Just PascaLIGO
    "jsligo" -> Just JsLIGO
    _ -> Nothing

runLigo :: FilePath -> [String] -> WebIDEM (ExitCode, String, String)
runLigo dirPath commands = do
  dockerizedLigo <- lift (asks cDockerizedLigoVersion)
  case dockerizedLigo of
    Just version ->
      liftIO
      $ flip readCreateProcessWithExitCode ""
      $ proc "docker"
      $ [ "run"
        , "--rm"
        , "-v"
        , dirPath ++ ":" ++ dirPath
        , "-w"
        , dirPath
        , "ligolang/ligo:" ++ version
        ]
        ++ commands
    Nothing -> do
      mLigoPath <- lift (asks cLigoPath)
      case mLigoPath of
        Nothing -> lift $ throwError err500
          {errBody = "server doesn't have access to LIGO binary."}
        Just ligoPath ->
          liftIO $ readCreateProcessWithExitCode (proc ligoPath commands) ""
