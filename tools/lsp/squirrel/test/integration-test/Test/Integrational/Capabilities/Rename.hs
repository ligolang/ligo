module Test.Integrational.Capabilities.Rename
  ( unit_rename_id
  , unit_rename_param
  , unit_rename_fail
  , unit_rename_in_included_file
  , unit_rename_in_included_file_fallback
  , unit_rename_nested_include
  , unit_rename_nested_include_fallback
--  , unit_rename_type_variable
  ) where

import AST.Scope (Fallback, FromCompiler, Standard)

import Test.Common.Capabilities.Rename
import Test.HUnit (Assertion)

unit_rename_fail :: Assertion
unit_rename_fail = do
  renameFail @Standard
  renameFail @FromCompiler

unit_rename_id :: Assertion
unit_rename_id = do
  renameId @Standard
  --renameId @FromCompiler -- FIXME (LIGO-93)

unit_rename_param :: Assertion
unit_rename_param = do
  renameParam @Standard
  --renameParam @FromCompiler -- FIXME (LIGO-93)

unit_rename_in_included_file :: Assertion
unit_rename_in_included_file = do
  renameInIncludedFile @Standard
  --renameInIncludedFile @FromCompiler -- FIXME (LIGO-93)

-- Since we require `ligo preprocess` for includes, we run `Fallback` tests for
-- includes in integration tests.
unit_rename_in_included_file_fallback :: Assertion
unit_rename_in_included_file_fallback = renameInIncludedFile @Fallback

unit_rename_nested_include :: Assertion
unit_rename_nested_include = do
  renameNestedInclude @Standard
  --renameNestedInclude @FromCompiler -- FIXME (LIGO-93)

unit_rename_nested_include_fallback :: Assertion
unit_rename_nested_include_fallback = renameNestedInclude @Fallback

-- FIXME (LIGO-331)
--unit_rename_type_variable :: Assertion
--unit_rename_type_variable = do
--  renameTypeVariable @Standard
--  --renameTypeVariable @FromCompiler -- FIXME (LIGO-93)
