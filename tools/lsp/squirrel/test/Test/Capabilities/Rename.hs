module Test.Capabilities.Rename
  ( unit_rename_id
  , unit_rename_param
  ) where

import qualified Language.LSP.Types as J
import System.FilePath ((</>))
import Test.HUnit (Assertion)

import AST.Capabilities.Rename (RenameDeclarationResult (Ok), renameDeclarationAt)
import AST.Scope (Fallback)
import Range (point)

import qualified Test.Capabilities.Util as Common (contractsDir)
import Test.FixedExpectations (shouldBe)
import Test.Util (readContractWithScopes)
import Test.Util.LigoEnv ()
--  Test.Util.LigoEnv for "instance HasLigoClient IO"

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "rename"

unit_rename_id :: Assertion
unit_rename_id = do
  tree <- readContractWithScopes @Fallback (contractsDir </> "id.ligo")
  let Ok results = renameDeclarationAt (point 1 11) tree "very_id"
  results `shouldBe`
    [ J.TextEdit (J.Range (J.Position 0 9) (J.Position 0 11)) "very_id"
    ]

unit_rename_param :: Assertion
unit_rename_param = do
  tree <- readContractWithScopes @Fallback (contractsDir </> "params.mligo")
  let Ok results = renameDeclarationAt (point 3 11) tree "aa"
  results `shouldBe`
    [ J.TextEdit (J.Range (J.Position 2 35) (J.Position 2 36)) "aa"
    , J.TextEdit (J.Range (J.Position 2 10) (J.Position 2 11)) "aa"
    ]
