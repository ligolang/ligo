module Test.Integrational.Capabilities.Rename
  ( unit_rename_id
  , unit_rename_param
  , unit_rename_fail
  , unit_rename_in_included_file
  , unit_rename_in_included_file_fallback
  ) where

import AST.Scope (Fallback, Standard)

import Test.Common.Capabilities.Rename
import Test.HUnit (Assertion)

unit_rename_fail :: Assertion
unit_rename_fail = renameFail @Standard

unit_rename_id :: Assertion
unit_rename_id = renameId @Standard

unit_rename_param :: Assertion
unit_rename_param = renameParam @Standard

unit_rename_in_included_file :: Assertion
unit_rename_in_included_file = renameInIncludedFile @Standard

-- Since we require `ligo preprocess` for includes, we run `Fallback` tests for
-- includes in integration tests.
unit_rename_in_included_file_fallback :: Assertion
unit_rename_in_included_file_fallback = renameInIncludedFile @Fallback
