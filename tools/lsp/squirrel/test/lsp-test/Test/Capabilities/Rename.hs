module Test.Capabilities.Rename
  ( unit_rename_id
  , unit_rename_param

  , unit_rename_fail
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.LSP.Types as J
import System.FilePath ((</>))
import Test.HUnit (Assertion)

import AST.Capabilities.Rename (RenameDeclarationResult (NotFound, Ok), prepareRenameDeclarationAt, renameDeclarationAt)
import AST.Scope (Fallback)
import Range (toLspRange, point)

import qualified Test.Capabilities.Util as Common (contractsDir)
import Test.FixedExpectations (expectationFailure, shouldBe)
import Test.Util (readContractWithScopes)
import Test.Util.LigoEnv ()
--  Test.Util.LigoEnv for "instance HasLigoClient IO"

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "rename"

testRenameOk
  :: FilePath  -- ^ Contract path
  -> (Int, Int)  -- ^ Rename location
  -> Text  -- ^ Expected old name
  -> (Int, Int)  -- ^ Expected declaration position
  -> Text  -- ^ New name
  -> [(Int, Int)]  -- ^ Expected starts of rename edits
  -> Assertion
testRenameOk fp pos name (declLine, declCol) newName expected = do
    tree <- readContractWithScopes @Fallback fp

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
  :: FilePath  -- ^ Contract path
  -> (Int, Int)  -- ^ Rename location
  -> Assertion
testRenameFail fp pos = do
    tree <- readContractWithScopes @Fallback fp

    case prepareRenameDeclarationAt (uncurry point pos) tree of
      Nothing -> pure ()
      Just _ -> expectationFailure "Should not be able to rename"

    case renameDeclarationAt (uncurry point pos) tree "<newName>" of
      NotFound -> pure ()
      Ok _ -> expectationFailure "Should not return edits"


unit_rename_id :: Assertion
unit_rename_id =
  testRenameOk (contractsDir </> "id.ligo") (1, 11) "id" (0, 9) "very_id"
    [(0, 9)]

unit_rename_param :: Assertion
unit_rename_param =
  testRenameOk (contractsDir </> "params.mligo") (3, 11) "a" (2, 10) "aa"
    [(2, 35), (2, 10)]

unit_rename_fail :: Assertion
unit_rename_fail =
  testRenameFail (contractsDir </> "id.ligo") (1, 16)
