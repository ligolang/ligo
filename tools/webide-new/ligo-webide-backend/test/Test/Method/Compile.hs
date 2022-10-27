module Test.Method.Compile (test_compile) where

import Data.Text.IO qualified as Text
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=))

import Schema.CompileRequest (CompileRequest(..))
import Schema.CompilerResponse (CompilerResponse(..))
import Source (Project(..), Source(..), SourceFile(..))

import Test.Common (TestM, contractsDir, mkTest, post)

test_compile :: TestM TestTree
test_compile =
  testGroup "POST /compile"
  <$> sequence [test_singleFile, test_multiFile]

test_singleFile :: TestM TestTree
test_singleFile = mkTest "compiles basic single-file input correctly" $ do
  source <- liftIO $ Text.readFile $ contractsDir </> "basic/main.mligo"
  let body = CompileRequest
        { rProject = Project
            { pMain = "main.mligo",
              pSourceFiles =
                [SourceFile "main.mligo" (Source source)]
            },
          rEntrypoint = Nothing,
          rProtocol = Nothing,
          rStorage = Nothing,
          rDisplayFormat = Nothing
        }
  actual <- post "compile" body
  expected <- liftIO . fmap CompilerResponse
    $ Text.readFile (contractsDir </> "basic/output.tz")
  liftIO (actual @?= expected)

test_multiFile :: TestM TestTree
test_multiFile = mkTest "compiles multi-file input correctly" $ do
    let loadSource :: FilePath -> IO Source
        loadSource = fmap Source . Text.readFile . (contractsDir </>)
    typesSource <- liftIO (loadSource "multifile/dir/types.mligo")
    mainSource <- liftIO (loadSource "multifile/main.mligo")
    let body =
          CompileRequest
            { rProject = Project
                { pMain = "main.mligo",
                  pSourceFiles =
                    [SourceFile "main.mligo" mainSource,
                     SourceFile "dir/types.mligo" typesSource
                    ]
                },
              rEntrypoint = Just "main",
              rProtocol = Just "jakarta",
              rStorage = Nothing,
              rDisplayFormat = Nothing
            }
    actual <- post "compile" body
    expected <- liftIO . fmap CompilerResponse
      $ Text.readFile (contractsDir </> "multifile/output.tz")
    liftIO (actual @?= expected)
