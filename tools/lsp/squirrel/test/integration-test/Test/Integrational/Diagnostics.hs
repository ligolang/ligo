module Test.Integrational.Diagnostics
  ( unit_bad_parse
  , unit_name_not_found
  ) where

import Test.Common.Diagnostics
  (DiagnosticSource (..), parseDiagnosticsDriver, simpleTest, treeDoesNotContainNameTest)
import Test.Common.FixedExpectations (HasCallStack)
import Test.Tasty.HUnit (Assertion)

-- Try to parse a file, and check that the proper error messages are generated
unit_bad_parse :: HasCallStack => Assertion
unit_bad_parse = do
  simpleTest' <- simpleTest
  parseDiagnosticsDriver StandardSource simpleTest'
  parseDiagnosticsDriver CompilerSource simpleTest'

unit_name_not_found :: HasCallStack => Assertion
unit_name_not_found = do
  treeDoesNotContainNameTest' <- treeDoesNotContainNameTest
  parseDiagnosticsDriver StandardSource treeDoesNotContainNameTest'
  parseDiagnosticsDriver CompilerSource treeDoesNotContainNameTest'
