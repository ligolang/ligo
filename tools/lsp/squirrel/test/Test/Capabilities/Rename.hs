module Test.Capabilities.Rename
  ( unit_rename_id
  , unit_rename_param
  ) where

import qualified Language.Haskell.LSP.Types as J
import System.FilePath ((</>))
import Test.HUnit (Assertion)

import AST.Capabilities.Rename (renameDeclarationAt)
import AST.Scope (Fallback)
import Range (point)

import Test.Capabilities.Util (contractsDir)
import Test.FixedExpectations (shouldBe)
import Test.Util (readContractWithScopes)


unit_rename_id :: Assertion
unit_rename_id = do
  tree <- readContractWithScopes @Fallback (contractsDir </> "id.ligo")
  let Just results = renameDeclarationAt (point 1 11) tree "very_id"
  results `shouldBe`
    [ J.TextEdit (J.Range (J.Position 0 9) (J.Position 0 11)) "very_id"
    ]

unit_rename_param :: Assertion
unit_rename_param = do
  tree <- readContractWithScopes @Fallback (contractsDir </> "params.mligo")
  let Just results = renameDeclarationAt (point 3 11) tree "aa"
  results `shouldBe`
    [ J.TextEdit (J.Range (J.Position 2 35) (J.Position 2 36)) "aa"
    , J.TextEdit (J.Range (J.Position 2 10) (J.Position 2 11)) "aa"
    ]
