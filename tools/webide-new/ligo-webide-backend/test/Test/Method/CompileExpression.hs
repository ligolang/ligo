module Test.Method.CompileExpression (test_compileExpression) where

import Data.Text.IO qualified as Text
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=))

import Schema.CompileExpressionRequest (CompileExpressionRequest(..))
import Schema.CompilerResponse (CompilerResponse(..))
import Source (Project(..), Source(..), SourceFile(..))

import Test.Common (TestM, contractsDir, mkTest, post)

test_compileExpression :: TestM TestTree
test_compileExpression =
  testGroup "POST /compileExpression"
  <$> sequence [test_singleFile]

test_singleFile :: TestM TestTree
test_singleFile = mkTest "compiles basic single-file input correctly" $ do
  source <- liftIO $ Text.readFile $ contractsDir </> "basic/main.mligo"
  let body = CompileExpressionRequest
        { cerProject = Project
            { pMain = "main.mligo",
              pSourceFiles =
                [SourceFile "main.mligo" (Source source)]
              , pModule = Nothing
            },
          cerFunction = "main",
          cerProtocol = Nothing,
          cerDisplayFormat = Nothing
        }
  actual <- post "compile-expression" body
  expected <- liftIO . fmap CompilerResponse
    $ Text.readFile (contractsDir </> "basic/compile_expression_output.tz")
  liftIO (actual @?= expected)
