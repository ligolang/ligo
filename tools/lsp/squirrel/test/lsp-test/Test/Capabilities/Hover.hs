module Test.Capabilities.Hover
  ( unit_hover_apply_type
  , unit_hover_parametric_type_ligo
  ) where

import Test.HUnit (Assertion)

import AST.Scope (Fallback)

import Test.Common.Capabilities.Hover qualified as Hover

unit_hover_apply_type :: Assertion
unit_hover_apply_type = Hover.unit_hover_apply_type @Fallback

-- FIXME: LIGO-827
--unit_hover_sum_type_jsligo :: Assertion
--unit_hover_sum_type_jsligo = Hover.unit_hover_sum_type_jsligo @Fallback

--unit_hover_sum_type_mligo :: Assertion
--unit_hover_sum_type_mligo = Hover.unit_hover_sum_type_mligo @Fallback

--unit_hover_sum_type_pascaligo :: Assertion
--unit_hover_sum_type_pascaligo = Hover.unit_hover_sum_type_pascaligo @Fallback

unit_hover_parametric_type_ligo :: Assertion
unit_hover_parametric_type_ligo = Hover.unit_hover_parametric_type_ligo @Fallback
