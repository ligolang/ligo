module Test.Capabilities.Completion
  ( test_completions
  ) where

import Data.Default (def)
import Language.LSP.Test
import Language.LSP.Types (Position (..))
import Language.LSP.Types.Lens (label)
import System.FilePath ((</>))

import AST (completionName, getNameCompletion)
import AST.Scope
import Config (_cScopingSystem)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Test.Common.Capabilities.Completion (caseInfos)
import Test.Common.Capabilities.Completion qualified as Common
import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldMatchList)
import Test.Common.LSP (openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "completion"

mkTest :: Common.TestInfo -> TestTree
mkTest Common.TestInfo{..} = testCase tiContract do

  completions <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc tiContract
    getCompletions doc $ toPosition tiPosition

  fmap (^. label) completions `shouldMatchList`
    map (getNameCompletion . completionName) tiExpected

  where
    toPosition (ln, col) = Position (ln - 1) (col - 1)

test_completions :: TestTree
test_completions = testGroup "Completion" $ map mkTest
  case _cScopingSystem def of -- `runHandlersTest` use default config
    FallbackScopes -> caseInfos @Fallback
    CompilerScopes -> caseInfos @FromCompiler
    StandardScopes -> caseInfos @Standard
