module Test.Common.Capabilities.Rename
  ( renameFail
  , renameId
  , renameParam
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.LSP.Types as J
import System.FilePath ((</>))
import Test.HUnit (Assertion)

import AST.Capabilities.Rename (RenameDeclarationResult (NotFound, Ok), prepareRenameDeclarationAt, renameDeclarationAt)
import AST.Scope (HasScopeForest)
import Range (toLspRange, point)

import qualified Test.Common.Capabilities.Util as Common (contractsDir)
import Test.Common.FixedExpectations (expectationFailure, shouldBe)
import Test.Common.Util (readContractWithScopes)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "rename"

testRenameOk
  :: forall impl. HasScopeForest impl IO
  => FilePath  -- ^ Contract path
  -> (Int, Int)  -- ^ Rename location
  -> Text  -- ^ Expected old name
  -> (Int, Int)  -- ^ Expected declaration position
  -> Text  -- ^ New name
  -> [(Int, Int)]  -- ^ Expected starts of rename edits
  -> Assertion
testRenameOk fp pos name (declLine, declCol) newName expected = do
    tree <- readContractWithScopes @impl fp

    case prepareRenameDeclarationAt (uncurry point pos) tree of
      Nothing -> expectationFailure "Should be able to rename"
      Just decl -> toLspRange decl `shouldBe`
        J.Range (J.Position declLine declCol) (J.Position declLine (declCol + len))

    case renameDeclarationAt (uncurry point pos) tree newName of
      NotFound -> expectationFailure "Should return edits"
      Ok results -> results `shouldBe`
        fmap
          (\(line, col)-> J.TextEdit
              (J.Range (J.Position line col) (J.Position line (col + len)))
              newName
          )
          expected
  where
    len = T.length name

testRenameFail
  :: forall impl. HasScopeForest impl IO
  => FilePath  -- ^ Contract path
  -> (Int, Int)  -- ^ Rename location
  -> Assertion
testRenameFail fp pos = do
    tree <- readContractWithScopes @impl fp

    case prepareRenameDeclarationAt (uncurry point pos) tree of
      Nothing -> pure ()
      Just _ -> expectationFailure "Should not be able to rename"

    case renameDeclarationAt (uncurry point pos) tree "<newName>" of
      NotFound -> pure ()
      Ok _ -> expectationFailure "Should not return edits"

renameFail :: forall impl. HasScopeForest impl IO => Assertion
renameFail =
  testRenameFail @impl (contractsDir </> "id.ligo") (1, 16)

renameId :: forall impl. HasScopeForest impl IO => Assertion
renameId =
  testRenameOk @impl (contractsDir </> "id.ligo") (1, 11) "id" (0, 9) "very_id"
    [(0, 9)]

renameParam :: forall impl. HasScopeForest impl IO => Assertion
renameParam =
  testRenameOk @impl (contractsDir </> "params.mligo") (3, 11) "a" (2, 10) "aa"
    [(2, 35), (2, 10)]
