module Test.Method.DryRun (test_dryRun) where

import Data.Text.IO qualified as Text
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=))

import Schema.CompilerResponse (CompilerResponse(..))
import Schema.DryRunRequest (DryRunRequest(..))
import Source (Project(..), Source(..), SourceFile(..))

import Test.Common (TestM, contractsDir, mkTest, post)

test_dryRun :: TestM TestTree
test_dryRun = testGroup "POST /dry-run" <$> sequence [test_singleFile]

test_singleFile :: TestM TestTree
test_singleFile = mkTest "compiles basic single-file input correctly" $ do
  source <- liftIO $ Text.readFile $ contractsDir </> "basic/main.mligo"
  let body =
        DryRunRequest
          { drrProject = Project
              { pMain = "main.mligo",
                pSourceFiles =
                  [SourceFile "main.mligo" (Source source)]
                , pModule = Nothing
              },
            drrParameters = "Increment (1)",
            drrStorage = "0",
            drrEntrypoint = Nothing,
            drrProtocol = Nothing,
            drrDisplayFormat = Nothing
          }
  actual <- post "dry-run" body
  expected <- liftIO . fmap CompilerResponse
    $ Text.readFile (contractsDir </> "basic/dry_run.txt")
  liftIO (actual @?= expected)
