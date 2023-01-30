module Test.Capabilities.Hover
  ( unit_hover_apply_type_mligo
  , unit_hover_sum_type_jsligo
  , unit_hover_sum_type_mligo
  , unit_hover_parametric_type_mligo
  , unit_hover_parameter_type_jsligo
  , unit_hover_parameter_type_mligo
  , unit_hover_module_type_access_mligo
  ) where

import Test.HUnit (Assertion)

import AST.Scope (Fallback)

import Test.Common.Capabilities.Hover qualified as Hover

unit_hover_apply_type_mligo :: Assertion
unit_hover_apply_type_mligo = Hover.unit_hover_apply_type_mligo @Fallback

unit_hover_sum_type_jsligo :: Assertion
unit_hover_sum_type_jsligo = Hover.unit_hover_sum_type_jsligo @Fallback

unit_hover_sum_type_mligo :: Assertion
unit_hover_sum_type_mligo = Hover.unit_hover_sum_type_mligo @Fallback

unit_hover_parametric_type_mligo :: Assertion
unit_hover_parametric_type_mligo = Hover.unit_hover_parametric_type_mligo @Fallback

unit_hover_parameter_type_jsligo :: Assertion
unit_hover_parameter_type_jsligo = Hover.unit_hover_parameter_type_jsligo @Fallback

unit_hover_parameter_type_mligo :: Assertion
unit_hover_parameter_type_mligo = Hover.unit_hover_parameter_type_mligo @Fallback

unit_hover_module_type_access_mligo :: Assertion
unit_hover_module_type_access_mligo = Hover.unit_hover_module_type_access_mligo @Fallback
