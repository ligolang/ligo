module Main (main) where

import Network.Wai.Handler.Warp (testWithApplication)
import Data.Text qualified as Text
import System.Environment qualified (lookupEnv)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)

import Config (ServerConfig(..))
import Server (mkApp)

import Test.Common (TestM)
import Test.Method.Compile (test_compile)
import Test.Method.CompileExpression (test_compileExpression)
import Test.Method.DryRun (test_dryRun)
import Test.Method.GenerateDeployScript (test_generateDeployScript)
import Test.Method.ListDeclarations (test_listDeclarations)

test_backend :: TestName -> TestM TestTree
test_backend name = testGroup name <$> sequence
  [ test_compile
  , test_compileExpression
  , test_dryRun
  , test_generateDeployScript
  , test_listDeclarations
  ]

lookupEnv :: String -> IO String
lookupEnv var = fromMaybe (error . Text.pack $ "need to set " ++ var)
  <$> System.Environment.lookupEnv var

main :: IO ()
main = do
  ligoPath <- lookupEnv "LIGO_PATH"
  octezClientPath <- lookupEnv "OCTEZ_CLIENT_PATH"
  dockerLigoVersion <- lookupEnv "DOCKER_LIGO_VERSION"
  clientCounter <- newIORef 0

  let standardConfig = ServerConfig
        { scLigoPath = ligoPath
        , scOctezClientPath = Just octezClientPath
        , scPort = 0 -- not used
        , scVerbosity = 0
        , scDockerizedLigoVersion = Nothing
        , scGistToken = ""
        , scLSPWorkspacePrefix = "/tmp" -- not used
        , scLSPClientCounter = clientCounter -- not used
        }
  let dockerizedConfig = standardConfig
        {scDockerizedLigoVersion = Just dockerLigoVersion}

  testWithApplication (pure (mkApp standardConfig)) $ \standardPort ->
    testWithApplication (pure (mkApp dockerizedConfig)) $ \dockerizedPort ->
      defaultMain $ testGroup "LIGO Web IDE backend tests"
        [ runReader (test_backend "Standard LIGO") standardPort
        , runReader (test_backend "Dockerized LIGO") dockerizedPort
        ]
