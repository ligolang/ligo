module Test.Integrational.Diagnostics
  ( unit_bad_parse
  , unit_name_not_found
  , unit_external_libs
  ) where

import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import UnliftIO.Exception qualified as UnliftIO (catch, throwIO)

import AST.Scope (FromCompiler, Standard)
import Cli (LigoClientFailureException (..), callLigo)
import Diagnostic (Message (..), MessageDetail (..), Severity (..))

import Test.Common.Capabilities.Util (contractsDir)
import Test.Common.Diagnostics
  (DiagnosticTest (..), MessageGroup (..), mkRange, parseDiagnosticsDriver, simpleTest,
  treeDoesNotContainNameTest)

import Test.Tasty.HUnit (Assertion)

testScopes :: HasCallStack => Bool -> IO DiagnosticTest -> Assertion
testScopes shouldTestFromCompiler test = do
  test' <- test
  parseDiagnosticsDriver @Standard test'
  when shouldTestFromCompiler $
    parseDiagnosticsDriver @FromCompiler test'

-- Try to parse a file, and check that the proper error messages are generated
unit_bad_parse :: HasCallStack => Assertion
unit_bad_parse = testScopes True simpleTest

unit_name_not_found :: HasCallStack => Assertion
unit_name_not_found = testScopes True treeDoesNotContainNameTest

externalLibsTest :: FilePath -> Maybe Message -> DiagnosticTest
externalLibsTest dtFile errMsg =
  DiagnosticTest{dtFile, dtAllMsgs = msgGroup, dtFilteredMsgs = msgGroup}
  where
    msgGroup = MessageGroup
      { mgParserMsgs = []
      , mgCompilerMsgs = maybeToList errMsg
      , mgFallbackMsgs = []
      }

installLigoLibrary :: FilePath -> IO ()
installLigoLibrary fp =
  -- 'ligo install' outputs to stderr even in case of a success, so we need to
  -- check the exit code.
  void (callLigo (Just fp) ["install"] Nothing) `UnliftIO.catch` \exc ->
    case cfeExit exc of
      Nothing -> UnliftIO.throwIO exc
      Just ExitSuccess -> pass
      Just (ExitFailure _) -> UnliftIO.throwIO exc

unit_external_libs :: HasCallStack => Assertion
unit_external_libs = do
  let
    testDirectory = contractsDir </> "bugs" </> "LIGO-821" </> "1"
    msg2_lib1 = Message
      (FromLIGO "File \"@ligo/math-lib/rational/rational.mligo\" not found.")
      SeverityError
      (mkRange (1, 1) (1, 53) (testDirectory </> "2" </> "lib1.jsligo"))
    msg3_4_lib2 = Message
      (FromLIGO "File \"@ligo/bigarray/lib/bigarray.mligo\" not found.")
      SeverityError
      (mkRange (1, 1) (1, 55) (testDirectory </> "3" </> "4" </> "lib2.jsligo"))
    -- We don't test 'FromCompiler' because it causes preprocessing errors,
    -- which we don't handle in this scoping system.
    runTest path = testScopes False . pure . externalLibsTest (testDirectory </> path)

  -- Install LIGO libraries before running tests:
  installLigoLibrary (testDirectory </> "2")
  installLigoLibrary testDirectory

  -- Check that we can get the proper diagnostics depending on whether the
  -- libraries are installed or not:
  runTest ("2" </> "lib1.jsligo") (Just msg2_lib1)
  --runTest ("2" </> "lib2.jsligo") Nothing  -- FIXME: fails on latest LIGO release
  runTest ("3" </> "4" </> "lib1.jsligo") Nothing
  runTest ("3" </> "4" </> "lib2.jsligo") (Just msg3_4_lib2)
