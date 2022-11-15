module Test.Capabilities.Hover
  ( unit_hover_apply_type
  ) where

import Test.HUnit (Assertion)

import AST.Scope (Fallback)

import qualified Test.Common.Capabilities.Hover as Hover

unit_hover_apply_type :: Assertion
unit_hover_apply_type = Hover.unit_hover_apply_type @Fallback

-- FIXME: LIGO-827
--unit_hover_sum_type_jsligo :: Assertion
--unit_hover_sum_type_jsligo = Hover.unit_hover_sum_type_jsligo @Fallback

--unit_hover_sum_type_mligo :: Assertion
--unit_hover_sum_type_mligo = Hover.unit_hover_sum_type_mligo @Fallback
