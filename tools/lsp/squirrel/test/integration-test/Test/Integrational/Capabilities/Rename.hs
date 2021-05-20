module Test.Integrational.Capabilities.Rename
  ( unit_rename_id
  , unit_rename_param
  , unit_rename_fail
  ) where

import AST.Scope (Standard)

import Test.Common.Capabilities.Rename
import Test.HUnit (Assertion)

unit_rename_fail :: Assertion
unit_rename_fail = renameFail @Standard

unit_rename_id :: Assertion
unit_rename_id = renameId @Standard

unit_rename_param :: Assertion
unit_rename_param = renameParam @Standard
