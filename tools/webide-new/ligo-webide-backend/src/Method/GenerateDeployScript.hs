module Method.GenerateDeployScript (generateDeployScript) where

import Control.Arrow ((>>>))
import Control.Monad.Except (runExcept)
import Data.Aeson (decodeStrict)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Numeric (showFFloat)
import Servant.Client (BaseUrl(..), Scheme(Https))
import Text.Megaparsec (errorBundlePretty)

import Morley.Client
  (AliasBehavior(..), MorleyClientConfig(..), MorleyClientEnv, MorleyClientM,
  OperationInfo(OpOriginate), OriginationData(..), dryRunOperationsNonEmpty, getProtocolParameters,
  mkMorleyClientEnv, revealKeyUnlessRevealed, runMorleyClientM)
import Morley.Client.Action.Common (computeStorageLimit)
import Morley.Client.RPC (AppliedResult, ProtocolParameters(ppCostPerByte))
import Morley.Client.TezosClient.Impl as TezosClient (importKey)
import Morley.Micheline (StringEncode(..), TezosMutez(..))
import Morley.Michelson.Macro (expandContract)
import Morley.Michelson.Parser
  (MichelsonSource(MSUnspecified), ParserException(..), parseExpandValue, parseNoEnv, program)
import Morley.Michelson.Printer (renderDoc)
import Morley.Michelson.Printer.Util (doesntNeedParens, printDocS)
import Morley.Michelson.TypeCheck (TypeCheckOptions(..), typeCheckContractAndStorage)
import Morley.Michelson.Typed (SomeContractAndStorage(..))
import Morley.Michelson.Untyped (Contract, Value)
import Morley.Tezos.Address (KindedAddress(ImplicitAddress))
import Morley.Tezos.Address.Alias
  (AddressOrAlias(AddressAlias), Alias(ContractAlias, ImplicitAlias))
import Morley.Tezos.Core (Mutez(UnsafeMutez, unMutez))
import Morley.Tezos.Crypto (KeyHash, PublicKey, SecretKey, detSecretKey, hashKey, toPublic)

import Common (WebIDEM)
import Config (ServerConfig(..))
import Error (LigoCompilerError(..), MorleyError(..))
import Method.Compile (compile)
import Schema.CompileRequest (CompileRequest(..))
import Schema.CompilerResponse (CompilerResponse(..))
import Schema.DeployScript (DeployScript(..))
import Schema.GenerateDeployScriptRequest (GenerateDeployScriptRequest(..))
import Types (DisplayFormat(..))

generateDeployScript :: GenerateDeployScriptRequest -> WebIDEM DeployScript
generateDeployScript request = do
  let build :: CompileRequest -> WebIDEM Text
      build = fmap unCompilerResponse . compile

  let buildJSON :: CompileRequest -> WebIDEM Text
      buildJSON = build >=> decodeTextCode

  michelsonCode <- build CompileRequest
    { rProject = gdsrProject request
    , rStorage = Nothing
    , rEntrypoint = gdsrEntrypoint request
    , rProtocol = Nothing
    , rDisplayFormat = Just DFHumanReadable
    }
  michelsonStorage <- build CompileRequest
    { rProject = gdsrProject request
    , rStorage = Just (gdsrStorage request)
    , rEntrypoint = gdsrEntrypoint request
    , rProtocol = Nothing
    , rDisplayFormat = Just DFHumanReadable
    }
  michelsonCodeJson <- buildJSON CompileRequest
    { rProject = gdsrProject request
    , rStorage = Nothing
    , rEntrypoint = gdsrEntrypoint request
    , rProtocol = Nothing
    , rDisplayFormat = Just DFJson
    }
  michelsonStorageJson <- buildJSON CompileRequest
    { rProject = gdsrProject request
    , rStorage = Just (gdsrStorage request)
    , rEntrypoint = gdsrEntrypoint request
    , rProtocol = Nothing
    , rDisplayFormat = Just DFJson
    }

  contract :: Contract <-
    case parseNoEnv program MSUnspecified michelsonCodeJson of
      Left bundle -> throwM $ MorleyError $ Text.pack $ errorBundlePretty bundle
      Right y -> pure (expandContract y)

  storage :: Value <-
    case parseExpandValue MSUnspecified michelsonStorageJson of
      Left (ParserException bundle) -> throwM $ MorleyStorageParsingError $ Text.pack $ errorBundlePretty bundle
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
            Left tcError -> throwM $ MorleyError $ Text.pack $ printDocS True $ renderDoc doesntNeedParens tcError
            Right good -> pure good

  let originationData :: OriginationData
      originationData = mkOriginationData typeCheckResult

  octezClientPath <- lift (asks scOctezClientPath) >>= \case
    Nothing -> throwM NoLigoBinary
    Just p -> pure p

  let morleyConfig :: MorleyClientConfig
      morleyConfig = MorleyClientConfig
        { mccEndpointUrl = Just (BaseUrl Https "ghostnet.tezos.marigold.dev" 443 "")
        , mccTezosClientPath = octezClientPath
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
          "octez-client \\\
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
        Nothing -> throwM $ LigoCompilerError "Could not decode compiler call"
        Just value -> pure value

mkOriginationData :: SomeContractAndStorage -> OriginationData
mkOriginationData (SomeContractAndStorage con val) =
  OriginationData
    { odAliasBehavior = DontSaveAlias
    , odName = ContractAlias "contract"
    , odBalance = UnsafeMutez 0
    , odContract = con
    , odStorage = val
    , odDelegate = Nothing
    , odMbFee = Nothing
    }

removeExcessWhitespace :: Text -> Text
removeExcessWhitespace =
  Text.lines >>> map Text.strip >>> Text.intercalate " " >>> Text.strip

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
