module Test.Common.Diagnostics
  ( DiagnosticSource (..)
  , simpleTest
  , treeDoesNotContainNameTest
  , parseDiagnosticsDriver
  ) where

import Data.List (nub)
import System.FilePath ((</>))
import System.Directory (makeAbsolute)
import Language.LSP.Types qualified as J

import AST.Parser (collectAllErrors, parseWithScopes)
import AST.Scope (Fallback, FromCompiler, Standard)
import Log (runNoLoggingT)
import ParseTree (pathToSrc)
import Parser
import Range

import qualified Test.Common.Capabilities.Util as Util (contractsDir)
import Test.Common.FixedExpectations (HasCallStack, shouldMatchList)
import Test.Common.Util (ScopeTester)
import Test.Tasty.HUnit (Assertion)

data DiagnosticSource impl where
  CompilerSource :: DiagnosticSource FromCompiler
  FallbackSource :: DiagnosticSource Fallback
  StandardSource :: DiagnosticSource Standard

data DiagnosticTest = DiagnosticTest
  { dtFile :: FilePath
  , dtCompilerMsgs :: [Message]
  , dtFallbackMsgs :: [Message]
  }

simpleTest :: IO DiagnosticTest
simpleTest = do
  dtFile <- makeAbsolute $ inputDir </> "a.mligo"
  pure DiagnosticTest
    { dtFile
    , dtCompilerMsgs =
      [ Message
        "Ill-formed function parameters.\nAt this point, one of the following is expected:\n  * another parameter as an irrefutable pattern, e.g a variable;\n  * a type annotation starting with a colon ':' for the body;\n  * the assignment symbol '=' followed by an expression.\n"
        SeverityError
        (mkRange (3, 17) (3, 19) dtFile)
      ]
    , dtFallbackMsgs =
      [ Message "Unexpected: :: int"   SeverityError (mkRange (3, 17) (3, 23) dtFile)
      , Message "Unrecognized: :: int" SeverityError (mkRange (3, 17) (3, 23) dtFile)
      , Message "Unrecognized: int"    SeverityError (mkRange (3, 20) (3, 23) dtFile)
      ]
    }

-- LIGO-474 regression test
treeDoesNotContainNameTest :: IO DiagnosticTest
treeDoesNotContainNameTest = do
  dtFile <- makeAbsolute $ inputDir </> "LIGO-474.religo"
  pure DiagnosticTest
    { dtFile
    , dtCompilerMsgs =
      [ Message "Syntax error #200." SeverityError (mkRange (1, 14) (1, 16) dtFile)
      , Message "Syntax error #233." SeverityError (mkRange (1, 17) (1, 18) dtFile)
      ]
    , dtFallbackMsgs =
      [ Message "Unexpected: r"                         SeverityError (mkRange (1, 17) (1, 18) dtFile)
      , Message "Expected to find a name, but got `42`" SeverityError (mkRange (1, 14) (1, 16) dtFile)
      ]
    }

inputDir :: FilePath
inputDir = Util.contractsDir </> "diagnostic"

mkRange :: (J.UInt, J.UInt) -> (J.UInt, J.UInt) -> FilePath -> Range
mkRange (a, b) (c, d) = Range (a, b, 0) (c, d, 0)

-- Try to parse a file, and check that the proper error messages are generated
parseDiagnosticsDriver
  :: forall impl
   . (HasCallStack, ScopeTester impl)
  => DiagnosticSource impl
  -> DiagnosticTest
  -> Assertion
parseDiagnosticsDriver source (DiagnosticTest file fromCompiler fallback) = do
  src <- pathToSrc file
  contract <- runNoLoggingT $ parseWithScopes @impl src
  let
    expectedMsgs = case source of
      CompilerSource -> fromCompiler
      FallbackSource -> fallback
      StandardSource -> fallback <> fromCompiler
    -- FIXME (LIGO-507)
    msgs = nub $ collectAllErrors contract
  msgs `shouldMatchList` expectedMsgs
