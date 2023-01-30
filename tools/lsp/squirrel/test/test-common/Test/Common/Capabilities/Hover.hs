module Test.Common.Capabilities.Hover
  ( contractsDir
  , HoverKind (..)
  , HoverTest (..)
  , hover'
  , checkHover
  , unit_hover_apply_type_mligo
  , unit_hover_inferred_simple
  , unit_hover_inferred_recursion
  , unit_hover_arrow_type_mligo
  , unit_hover_arrow_type_jsligo
  , unit_hover_sum_type_jsligo
  , unit_hover_sum_type_mligo
  , unit_hover_parametric_type_mligo
  , unit_hover_parameter_type_jsligo
  , unit_hover_parameter_type_mligo
  , unit_hover_module_type_access_mligo
  ) where

import Prelude hiding (Type)

import Language.LSP.Types (Hover (..), HoverContents (..), MarkupContent (..))
import System.Directory (makeAbsolute)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import AST.Capabilities.Hover (hoverDecl)
import AST.Pretty (ppToText)
import AST.Scope.ScopedDecl (Type (..), TypeVariable (..), lppLigoLike)
import AST.Skeleton (Lang (..))

import Range (Range (..), interval, point)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (expectationFailure, shouldBe)
import Test.Common.Util (ScopeTester, readContractWithScopes)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "hover"

data HoverKind = Type | Module | Value

data HoverTest = HoverTest
  { htDefinition :: Range
  , htName :: Text
  , htType :: Type
  , htDoc :: [Text]
  , htDialect :: Lang
  , htKind :: HoverKind
  }

hover' :: Range -> Text -> Type -> Lang -> HoverKind -> HoverTest
hover' definition name type' = HoverTest definition name type' []

checkHover :: forall parser. ScopeTester parser => FilePath -> Range -> HoverTest -> Assertion
checkHover fp reference HoverTest{..} = do
  contract <- readContractWithScopes @parser fp
  case hoverDecl reference contract of
    Nothing -> expectationFailure "Expected a hover definition, but got Nothing"
    Just (Hover (HoverContents (MarkupContent _ (lines -> (_dialect : ty : "```" : _ : def : doc)))) _) -> do
      ty `shouldBe` case htKind of
        Type   -> "type " <> htName <> " = " <> show (lppLigoLike htDialect htType)
        Module -> "module " <> htName
        Value  -> htName <> " : " <> show (lppLigoLike htDialect htType)
      def `shouldBe` ("*defined at* " <> ppToText htDefinition)
      case doc of
        [] -> unless (null htDoc) $ expectationFailure "Expected no documentation, but got some"
        _  -> Just (ppToText htDoc) `shouldBe` find (/= "") doc
    _ -> expectationFailure "Hover definition is not of the expected type"

unit_hover_arrow_type_mligo :: forall parser. ScopeTester parser => Assertion
unit_hover_arrow_type_mligo = do
  fp <- makeAbsolute $ contractsDir </> "arrow-type.mligo"
  let type' = ArrowType (ApplyType (AliasType "list") [AliasType "int"]) (AliasType "unit")
  checkHover @parser fp (interval 5 34 41){_rFile = fp} (hover' (interval 1 5 12){_rFile = fp} "iter_op" type' Caml Value)

unit_hover_arrow_type_jsligo :: forall parser. ScopeTester parser => Assertion
unit_hover_arrow_type_jsligo = do
  fp <- makeAbsolute $ contractsDir </> "arrow-type.jsligo"
  let type' = ArrowType (ApplyType (AliasType "list") [AliasType "int"]) (AliasType "unit")
  checkHover @parser fp (interval 6 12 19){_rFile = fp} (hover' (interval 1 5 12){_rFile = fp} "iter_op" type' Js Value)

unit_hover_apply_type_mligo :: forall parser. ScopeTester parser => Assertion
unit_hover_apply_type_mligo = do
  fp <- makeAbsolute $ contractsDir </> "apply-type.mligo"
  let type' = ApplyType (AliasType "contract") [AliasType "unit"]
  checkHover @parser fp (point 3 17){_rFile = fp} (hover' (interval 2 7 8){_rFile = fp} "c" type' Caml Value)

unit_hover_inferred_simple :: forall parser. ScopeTester parser => Assertion
unit_hover_inferred_simple = do
  fp <- makeAbsolute $ contractsDir </> "simple.mligo"
  let type' = AliasType "int"
  checkHover @parser fp (point 2 9){_rFile = fp} (hover' (interval 1 5 6){_rFile = fp} "x" type' Caml Value)
  checkHover @parser fp (point 2 5){_rFile = fp} (hover' (interval 2 5 6){_rFile = fp} "y" type' Caml Value)

unit_hover_inferred_recursion :: forall parser. ScopeTester parser => Assertion
unit_hover_inferred_recursion = do
  fp <- makeAbsolute $ contractsDir </> "recursion.mligo"
  let type' = AliasType "int"
  checkHover @parser fp (point 2 21){_rFile = fp} (hover' (interval 1 18 21){_rFile = fp} "acc" type' Caml Value)

unit_hover_sum_type_jsligo :: forall parser. ScopeTester parser => Assertion
unit_hover_sum_type_jsligo = do
  fp <- makeAbsolute $ contractsDir </> "sum.jsligo"
  let type' = AliasType "parameter"
  checkHover @parser fp (point 13 12){_rFile = fp} (hover' (interval 10 16 22){_rFile = fp} "action" type' Js Value)

unit_hover_sum_type_mligo :: forall parser. ScopeTester parser => Assertion
unit_hover_sum_type_mligo = do
  fp <- makeAbsolute $ contractsDir </> "sum.mligo"
  let type' = AliasType "parameter"
  checkHover @parser fp (point 13 12){_rFile = fp} (hover' (interval 10 11 17){_rFile = fp} "action" type' Caml Value)

unit_hover_parametric_type_mligo :: forall parser. ScopeTester parser => Assertion
unit_hover_parametric_type_mligo = do
  fp <- makeAbsolute $ contractsDir </> "parametric.mligo"
  let type' = VariableType $ TypeVariable "a"
  checkHover @parser fp (point 1 45){_rFile = fp} (hover' (interval 1 13 14){_rFile = fp} "a" type' Caml Type)

unit_hover_parameter_type_jsligo :: forall parser. ScopeTester parser => Assertion
unit_hover_parameter_type_jsligo = do
  fp <- makeAbsolute $ contractsDir </> "types.jsligo"
  checkHover @parser fp
    (interval 7 35 36){_rFile = fp}
    (hover' (interval 6 14 15){_rFile = fp} "p" (AliasType "unit") Js Value)

unit_hover_parameter_type_mligo :: forall parser. ScopeTester parser => Assertion
unit_hover_parameter_type_mligo = do
  fp <- makeAbsolute $ contractsDir </> "types.mligo"
  checkHover @parser fp
    (interval 7 26 27){_rFile = fp}
    (hover' (interval 6 11 12){_rFile = fp} "p" (AliasType "unit") Caml Value)

unit_hover_module_type_access_mligo :: forall parser. ScopeTester parser => Assertion
unit_hover_module_type_access_mligo = do
  fp <- makeAbsolute $ contractsDir </> "module.mligo"
  checkHover @parser fp
    (interval 5 5 9){_rFile = fp}
    (hover' (interval 5 5 9){_rFile = fp} "test" (AliasType "Test.issue") Caml Value)
