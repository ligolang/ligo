module Test.Integrational.Diagnostics
  ( unit_bad_parse
  , unit_name_not_found
  ) where

import AST.Scope (FromCompiler, Standard)

import Test.Common.Diagnostics (parseDiagnosticsDriver, simpleTest, treeDoesNotContainNameTest)

import Test.Tasty.HUnit (Assertion)

-- Try to parse a file, and check that the proper error messages are generated
unit_bad_parse :: HasCallStack => Assertion
unit_bad_parse = do
  simpleTest' <- simpleTest
  parseDiagnosticsDriver @Standard simpleTest'
  parseDiagnosticsDriver @FromCompiler simpleTest'

unit_name_not_found :: HasCallStack => Assertion
unit_name_not_found = do
  treeDoesNotContainNameTest' <- treeDoesNotContainNameTest
  parseDiagnosticsDriver @Standard treeDoesNotContainNameTest'
  parseDiagnosticsDriver @FromCompiler treeDoesNotContainNameTest'
