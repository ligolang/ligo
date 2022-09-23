module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Network.HTTP.Types.Method (methodPost)
import Network.Wai.Test (SResponse, simpleBody)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Wai (WaiSession, request, with)

import Server
  (CompileExpressionRequest(..), CompileRequest(..), CompilerResponse(..), Config(..),
  DeployScript(..), DryRunRequest(..), GenerateDeployScriptRequest(..), ListDeclarationsRequest(..),
  Source(..), mkApp)

main :: IO ()
main = do
  ligoPath <- fromMaybe (error "need to set LIGO_PATH")
          <$> lookupEnv "LIGO_PATH"
  tezosClientPath <- fromMaybe (error "need to set TEZOS_CLIENT_PATH")
          <$> lookupEnv "TEZOS_CLIENT_PATH"
  let config = Config
        { cLigoPath = Just ligoPath
        , cTezosClientPath = Just tezosClientPath
        , cPort = 8080
        , cVerbose = False
        , cDockerizedLigoVersion = Nothing
        }
  hspec $ do
    describe "standard LIGO" (spec config)
    describe "dockerized LIGO"
      (spec config {cDockerizedLigoVersion = Just "0.50.0"})

post :: BS.ByteString -> LBS.ByteString -> WaiSession () SResponse
post path =
  request
    methodPost
    path
    [ ("Accept", "application/json"),
      ("Content-type", "application/json")
    ]

contractsDir :: FilePath
contractsDir = "test/contracts"

spec :: Config -> Spec
spec config = with (return (mkApp config)) $ do
  describe "POST /compile" $ do
    it "compiles basic single-file input correctly" $ do
      source <- liftIO $ Text.readFile $ contractsDir </> "basic/main.mligo"
      let input =
            CompileRequest
              { rSources = [("main.mligo", Source source)],
                rMain = "main.mligo",
                rEntrypoint = Nothing,
                rProtocol = Nothing,
                rStorage = Nothing,
                rDisplayFormat = Nothing
              }
      response <- post "/compile" (Aeson.encode input)
      expected <- liftIO . fmap CompilerResponse $ Text.readFile (contractsDir </> "basic/output.tz")
      liftIO $
        case Aeson.decode (simpleBody response) of
          Nothing -> expectationFailure ("could not decode response: " ++ LBS.unpack (simpleBody response))
          Just actual -> actual `shouldBe` expected

    it "compiles multi-file input correctly" $ do
      let loadSource :: FilePath -> IO Source
          loadSource = fmap Source . Text.readFile . (contractsDir </>)
      typesSource <- liftIO (loadSource "multifile/dir/types.mligo")
      mainSource <- liftIO (loadSource "multifile/main.mligo")

      let input =
            CompileRequest
              { rSources =
                  [ ("dir/types.mligo", typesSource),
                    ("main.mligo", mainSource)
                  ],
                rMain = "main.mligo",
                rEntrypoint = Just "main",
                rProtocol = Just "jakarta",
                rStorage = Nothing,
                rDisplayFormat = Nothing
              }
      response <- post "/compile" (Aeson.encode input)
      expected <- liftIO . fmap CompilerResponse $ Text.readFile (contractsDir </> "multifile/output.tz")
      liftIO $
        case Aeson.decode (simpleBody response) of
          Nothing -> expectationFailure ("could not decode response: " ++ LBS.unpack (simpleBody response))
          Just actual -> actual `shouldBe` expected

  describe "POST /generate-deploy-script" $ do
    it "generates deploy script for single-file contract correctly" $ do
      source <- liftIO $ Text.readFile $ contractsDir </> "basic/main.mligo"
      let input =
            GenerateDeployScriptRequest
              { gdsrStorage = "0"
              , gdsrSources = [("main.mligo", Source source)]
              , gdsrMain = "main.mligo"
              , gdsrName = "increment-cameligo"
              , gdsrEntrypoint = Just "main"
              , gdsrProtocol = Just "jakarta"
              }
      response <- post "/generate-deploy-script" (Aeson.encode input)
      let expectationPath = contractsDir </> "basic/deploy_script.json"
      liftIO (Aeson.decodeFileStrict expectationPath) >>= \case
        Nothing -> liftIO (expectationFailure "oops")
        Just expected -> liftIO $
          case Aeson.decode @DeployScript (simpleBody response) of
            Nothing -> expectationFailure ("could not decode response: " ++ show response)
            Just actual -> actual `shouldBe` expected

  describe "POST /compile-expression" $ do
    it "compiles basic single-file input correctly" $ do
      source <- liftIO $ Text.readFile $ contractsDir </> "basic/main.mligo"
      let input =
            CompileExpressionRequest
              { cerSources = [("main.mligo", Source source)],
                cerMain = "main.mligo",
                cerFunction = "main",
                cerProtocol = Nothing,
                cerDisplayFormat = Nothing
              }
      response <- post "/compile-expression" (Aeson.encode input)
      expected <- liftIO . fmap CompilerResponse $ Text.readFile (contractsDir </> "basic/compile_expression_output.tz")
      liftIO $
        case Aeson.decode (simpleBody response) of
          Nothing -> expectationFailure ("could not decode response: " ++ LBS.unpack (simpleBody response))
          Just actual -> actual `shouldBe` expected

  describe "POST /dry-run" $ do
    it "compiles basic single-file input correctly" $ do
      source <- liftIO $ Text.readFile $ contractsDir </> "basic/main.mligo"
      let input =
            DryRunRequest
              { drrSources = [("main.mligo", Source source)],
                drrMain = "main.mligo",
                drrParameters = "Increment (1)",
                drrStorage = "0",
                drrEntrypoint = Nothing,
                drrProtocol = Nothing,
                drrDisplayFormat = Nothing
              }
      response <- post "/dry-run" (Aeson.encode input)
      expected <- liftIO . fmap CompilerResponse $ Text.readFile (contractsDir </> "basic/dry_run.txt")
      liftIO $
        case Aeson.decode (simpleBody response) of
          Nothing -> expectationFailure ("could not decode response: " ++ show response)
          Just actual -> actual `shouldBe` expected

  describe "POST /list-declarations" $ do
    it "compiles basic single-file input correctly" $ do
      source <- liftIO $ Text.readFile $ contractsDir </> "basic/main.mligo"
      let input =
            ListDeclarationsRequest
              { ldrSources = [("main.mligo", Source source)],
                ldrMain = "main.mligo",
                ldrOnlyEndpoint = False
              }
      response <- post "/list-declarations" (Aeson.encode input)
      expected <- liftIO . fmap Text.lines
        $ Text.readFile (contractsDir </> "basic/list_declarations.txt")
      liftIO $
        case Aeson.decode (simpleBody response) of
          Nothing -> expectationFailure ("could not decode response: " ++ show response)
          Just actual -> actual `shouldBe` expected
