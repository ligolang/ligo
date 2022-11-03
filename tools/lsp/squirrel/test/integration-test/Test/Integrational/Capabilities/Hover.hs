module Test.Integrational.Capabilities.Hover
  ( unit_hover_apply_type
  , unit_hover_inferred_simple_from_compiler
  , unit_hover_inferred_recursion_from_compiler
  , unit_hover_inferred_simple_fallback
  , unit_hover_inferred_recursion_fallback
  , unit_hover_arrow_type
  , unit_hover_arrow_type_jsligo
  , unit_hover_arrow_type_mligo
  , unit_hover_sum_type_jsligo
  , unit_hover_sum_type_mligo
  ) where

import Test.HUnit (Assertion)

import AST.Scope (Fallback, FromCompiler, Standard)

import Test.Common.Capabilities.Hover qualified as Hover
import Test.Common.FixedExpectations (anyException, shouldThrow)

unit_hover_arrow_type :: Assertion
unit_hover_arrow_type = do
  Hover.unit_hover_arrow_type @Standard
  Hover.unit_hover_arrow_type @FromCompiler

unit_hover_arrow_type_mligo :: Assertion
unit_hover_arrow_type_mligo = do
  Hover.unit_hover_arrow_type_mligo @Standard
  Hover.unit_hover_arrow_type_mligo @FromCompiler

unit_hover_arrow_type_jsligo :: Assertion
unit_hover_arrow_type_jsligo = do
  -- FIXME: LIGO-797
  --Hover.unit_hover_arrow_type_jsligo @Standard
  -- FIXME: LIGO doesn't yield all references in this test
  --Hover.unit_hover_arrow_type_jsligo @FromCompiler
  pure ()

unit_hover_apply_type :: Assertion
unit_hover_apply_type = do
  Hover.unit_hover_apply_type @Standard
  Hover.unit_hover_apply_type @FromCompiler

unit_hover_inferred_simple_from_compiler :: Assertion
unit_hover_inferred_simple_from_compiler = do
  Hover.unit_hover_inferred_simple @Standard
  Hover.unit_hover_inferred_simple @FromCompiler

unit_hover_inferred_recursion_from_compiler :: Assertion
unit_hover_inferred_recursion_from_compiler = do
  Hover.unit_hover_inferred_recursion @Standard
  Hover.unit_hover_inferred_recursion @FromCompiler

unit_hover_inferred_recursion_fallback :: Assertion
unit_hover_inferred_recursion_fallback =
  Hover.unit_hover_inferred_recursion @Fallback

-- (LIGO-243) Fallback scopes can't infer types and we rely on LIGO for this.
unit_hover_inferred_simple_fallback :: Assertion
unit_hover_inferred_simple_fallback =
  Hover.unit_hover_inferred_simple @Fallback `shouldThrow` anyException

unit_hover_sum_type_jsligo :: Assertion
unit_hover_sum_type_jsligo = do
  Hover.unit_hover_sum_type_jsligo @Standard
  Hover.unit_hover_sum_type_jsligo @FromCompiler

unit_hover_sum_type_mligo :: Assertion
unit_hover_sum_type_mligo = do
  Hover.unit_hover_sum_type_mligo @Standard
  Hover.unit_hover_sum_type_mligo @FromCompiler
