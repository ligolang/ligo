module Test.Method.ListDeclarations (test_listDeclarations) where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=))

import Schema.ListDeclarationsRequest (ListDeclarationsRequest(..))
import Source (Project(..), Source(..), SourceFile(..))

import Test.Common (TestM, contractsDir, mkTest, post)

test_listDeclarations :: TestM TestTree
test_listDeclarations = testGroup "POST /list-declarations" <$> sequence [test_singleFile]

test_singleFile :: TestM TestTree
test_singleFile = mkTest "compiles basic single-file input correctly" $ do
  source <- liftIO $ Text.readFile $ contractsDir </> "basic/main.mligo"
  let body =
        ListDeclarationsRequest
          { ldrProject = Project
              { pMain = "main.mligo"
              , pSourceFiles =
                 [SourceFile "main.mligo" (Source source)]
              }
          , ldrOnlyEndpoint = Just False
          }
  actual <- post "list-declarations" body
  expected <- liftIO . fmap Text.lines
    $ Text.readFile (contractsDir </> "basic/list_declarations.txt")
  liftIO (actual @?= expected)
