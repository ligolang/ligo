module Test.Capabilities.Rename
  ( unit_rename_id
  , unit_rename_param
  , unit_rename_fail
--  , unit_rename_type_variable
  , unit_rename_conflicting_module_name
  ) where

import AST.Scope (Fallback)

import Test.Common.Capabilities.Rename
import Test.HUnit (Assertion)

unit_rename_fail :: Assertion
unit_rename_fail = renameFail @Fallback

unit_rename_id :: Assertion
unit_rename_id = renameId @Fallback

unit_rename_param :: Assertion
unit_rename_param = renameParam @Fallback

-- FIXME (LIGO-331)
--unit_rename_type_variable :: Assertion
--unit_rename_type_variable = renameTypeVariable @Fallback

unit_rename_conflicting_module_name :: Assertion
unit_rename_conflicting_module_name = renameConflictingModuleName @Fallback
