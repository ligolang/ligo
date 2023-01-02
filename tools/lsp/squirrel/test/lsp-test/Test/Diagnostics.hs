module Test.Diagnostics
  ( unit_bad_parse
  , unit_name_not_found
  ) where

import AST.Scope (ScopingSystem (FallbackScopes))

import Test.Common.Diagnostics (parseDiagnosticsDriver, simpleTest, treeDoesNotContainNameTest)

import Test.Tasty.HUnit (Assertion)

-- Try to parse a file, and check that the proper error messages are generated
unit_bad_parse :: HasCallStack => Assertion
unit_bad_parse = parseDiagnosticsDriver FallbackScopes =<< simpleTest

unit_name_not_found :: HasCallStack => Assertion
unit_name_not_found = parseDiagnosticsDriver FallbackScopes =<< treeDoesNotContainNameTest
