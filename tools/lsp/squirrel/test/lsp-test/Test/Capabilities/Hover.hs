module Test.Capabilities.Hover
  ( unit_hover_apply_type
  ) where

import Test.HUnit (Assertion)

import AST.Scope (Fallback)

import qualified Test.Common.Capabilities.Hover as Hover (unit_hover_apply_type)

unit_hover_apply_type :: Assertion
unit_hover_apply_type = Hover.unit_hover_apply_type @Fallback
