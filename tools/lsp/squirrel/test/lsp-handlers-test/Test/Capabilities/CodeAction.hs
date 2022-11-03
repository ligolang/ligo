module Test.Capabilities.CodeAction (unit_code_action) where

import Data.List (find)
import Data.Maybe (fromJust)
import Language.LSP.Test
import Language.LSP.Types (CodeAction, Command, Position (..), Range (..), type (|?) (..))
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import Test.Common.Capabilities.CodeAction.ExtractTypeAlias
  (TestInfo (..), constructExpectedWorkspaceEdit, extractTextEdits, testInfos)
import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe, shouldMatchList)
import Test.Common.LSP (openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "code-action" </> "extract-type-definition"

toCodeAction :: Command |? CodeAction -> CodeAction
toCodeAction (InL _) = error "expected a CodeAction, not a Command"
toCodeAction (InR action) = action

unit_code_action :: Assertion
unit_code_action = do
  let filename = "simple.ligo"
  let testInfo = fromJust $ find ((== filename) . tiContract) testInfos

  codeActions <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    getCodeActions doc (Range (Position 1 23) (Position 1 23))

  length codeActions `shouldBe` 1
  let textEdits = extractTextEdits . toCodeAction =<< codeActions
  textEdits `shouldMatchList` constructExpectedWorkspaceEdit (tiExpectedEdits testInfo)
