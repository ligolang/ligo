module Test.Method.GenerateDeployScript (test_generateDeployScript) where

import Data.Aeson qualified as Aeson
import Data.Text.IO qualified as Text
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, (@?=))

import Schema.DeployScript (DeployScript(..))
import Schema.GenerateDeployScriptRequest (GenerateDeployScriptRequest(..))
import Source (Project(..), Source(..), SourceFile(..))

import Test.Common (TestM, contractsDir, mkTest, post)

test_generateDeployScript :: TestM TestTree
test_generateDeployScript =
  testGroup "POST /generate-deploy-script"
  <$> sequence [test_singleFile]

test_singleFile :: TestM TestTree
test_singleFile = mkTest "generates deploy script for single-file contract correctly" $ do
  source <- liftIO $ Text.readFile $ contractsDir </> "basic/main.mligo"
  let body =
        GenerateDeployScriptRequest
          { gdsrStorage = "0"
          , gdsrProject = Project
            { pMain = "main.mligo",
              pSourceFiles =
                [SourceFile "main.mligo" (Source source)]
              }
          , gdsrName = "increment-cameligo"
          , gdsrEntrypoint = Just "main"
          , gdsrProtocol = Just "nairobi"
          }
  actual :: DeployScript <- post "generate-deploy-script" body
  let expectationPath = contractsDir </> "basic/deploy_script.json"
  liftIO (Aeson.decodeFileStrict expectationPath) >>= \case
    Nothing -> liftIO (assertFailure "couldn't decode expected file")
    Just expected -> liftIO (actual @?= expected)
