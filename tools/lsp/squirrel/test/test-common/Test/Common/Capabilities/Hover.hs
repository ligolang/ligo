module Test.Common.Capabilities.Hover
  ( contractsDir
  , HoverTest (..)
  , hover'
  , checkHover
  , unit_hover_apply_type
  , unit_hover_inferred_simple
  , unit_hover_inferred_recursion
  , unit_hover_arrow_type
  , unit_hover_arrow_type_mligo
  , unit_hover_arrow_type_jsligo
  , unit_hover_sum_type_jsligo
  , unit_hover_sum_type_mligo
  ) where

import Prelude hiding (lines)

import Data.List (find)
import Data.Text (Text, lines)
import Control.Monad (unless)
import Language.LSP.Types (Hover (..), HoverContents (..), MarkupContent (..))
import System.Directory (makeAbsolute)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import AST.Capabilities.Hover (hoverDecl)
import AST.Pretty (docToText, ppToText)
import AST.Scope.ScopedDecl (Type (..), lppLigoLike)
import AST.Skeleton (Lang (..))

import Range (Range (..), interval, point)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (expectationFailure, shouldBe)
import Test.Common.Util (ScopeTester, readContractWithScopes)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "hover"

data HoverTest = HoverTest
  { htDefinition :: Range
  , htName :: Text
  , htType :: Type
  , htDoc :: [Text]
  , htDialect :: Lang
  }

hover' :: Range -> Text -> Type -> Lang -> HoverTest
hover' definition name type' = HoverTest definition name type' []

checkHover :: forall parser. ScopeTester parser => FilePath -> Range -> HoverTest -> Assertion
checkHover fp reference HoverTest{..} = do
  contract <- readContractWithScopes @parser fp
  case hoverDecl reference contract of
    Nothing -> expectationFailure "Expected a hover definition, but got Nothing"
    Just (Hover (HoverContents (MarkupContent _ (lines -> (ty : _ : def : doc)))) _) -> do
      ty `shouldBe` (htName <> " : " <> docToText (lppLigoLike htDialect htType))
      def `shouldBe` ("*defined at* " <> ppToText htDefinition)
      case doc of
        [] -> unless (null htDoc) $ expectationFailure "Expected no documentation, but got some"
        _  -> Just (ppToText htDoc) `shouldBe` find (/= "") doc
    _ -> expectationFailure "Hover definition is not of the expected type"

unit_hover_arrow_type :: forall parser. ScopeTester parser => Assertion
unit_hover_arrow_type = do
  fp <- makeAbsolute $ contractsDir </> "arrow-type.ligo"
  let type' = ArrowType (ApplyType (AliasType "list") [AliasType "int"]) (AliasType "int")
  checkHover @parser fp (interval 7 3 10){_rFile = fp} (hover' (interval 1 10 17){_rFile = fp} "fold_op" type' Pascal)

unit_hover_arrow_type_mligo :: forall parser. ScopeTester parser => Assertion
unit_hover_arrow_type_mligo = do
  fp <- makeAbsolute $ contractsDir </> "arrow-type.mligo"
  let type' = ArrowType (ApplyType (AliasType "list") [AliasType "int"]) (AliasType "unit")
  checkHover @parser fp (interval 5 34 41){_rFile = fp} (hover' (interval 1 5 12){_rFile = fp} "iter_op" type' Caml)

unit_hover_arrow_type_jsligo :: forall parser. ScopeTester parser => Assertion
unit_hover_arrow_type_jsligo = do
  fp <- makeAbsolute $ contractsDir </> "arrow-type.jsligo"
  let type' = ArrowType (ApplyType (AliasType "list") [AliasType "int"]) (AliasType "unit")
  checkHover @parser fp (interval 6 12 19){_rFile = fp} (hover' (interval 1 5 12){_rFile = fp} "iter_op" type' Js)

unit_hover_apply_type :: forall parser. ScopeTester parser => Assertion
unit_hover_apply_type = do
  fp <- makeAbsolute $ contractsDir </> "apply-type.ligo"
  let type' = ApplyType (AliasType "contract") [AliasType "unit"]
  checkHover @parser fp (point 3 23){_rFile = fp} (hover' (interval 2 9 10){_rFile = fp} "c" type' Pascal)

unit_hover_inferred_simple :: forall parser. ScopeTester parser => Assertion
unit_hover_inferred_simple = do
  fp <- makeAbsolute $ contractsDir </> "simple.mligo"
  let type' = AliasType "int"
  checkHover @parser fp (point 2 9){_rFile = fp} (hover' (interval 1 5 6){_rFile = fp} "x" type' Caml)

unit_hover_inferred_recursion :: forall parser. ScopeTester parser => Assertion
unit_hover_inferred_recursion = do
  fp <- makeAbsolute $ contractsDir </> "recursion.mligo"
  let type' = AliasType "int"
  checkHover @parser fp (point 2 21){_rFile = fp} (hover' (interval 1 18 21){_rFile = fp} "acc" type' Caml)

unit_hover_sum_type_jsligo :: forall parser. ScopeTester parser => Assertion
unit_hover_sum_type_jsligo = do
  fp <- makeAbsolute $ contractsDir </> "sum.jsligo"
  let type' = AliasType "parameter"
  checkHover @parser fp (point 13 12){_rFile = fp} (hover' (interval 10 16 22){_rFile = fp} "action" type' Js)

unit_hover_sum_type_mligo :: forall parser. ScopeTester parser => Assertion
unit_hover_sum_type_mligo = do
  fp <- makeAbsolute $ contractsDir </> "sum.mligo"
  let type' = AliasType "parameter"
  checkHover @parser fp (point 13 12){_rFile = fp} (hover' (interval 10 11 17){_rFile = fp} "action" type' Caml)
