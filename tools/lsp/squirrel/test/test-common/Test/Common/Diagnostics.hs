module Test.Common.Diagnostics
  ( DiagnosticSource (..)
  , simpleTest
  , treeDoesNotContainNameTest
  , parseDiagnosticsDriver
  ) where

import Data.Text (Text)
import Data.Word (Word32)
import System.FilePath ((</>))
import System.Directory (makeAbsolute)

import AST.Parser (collectAllErrors, parseWithScopes)
import AST.Scope (Fallback, FromCompiler, Standard)
import AST.Skeleton (Error (..))
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
  , dtCompilerMsgs :: [(Range, Text)]
  , dtFallbackMsgs :: [(Range, Text)]
  }

simpleTest :: IO DiagnosticTest
simpleTest = do
  dtFile <- makeAbsolute $ inputDir </> "a.mligo"
  pure DiagnosticTest
    { dtFile
    , dtCompilerMsgs =
      [ ( mkRange (3, 17) (3, 19) dtFile
        , "Ill-formed function parameters.\nAt this point, one of the following is expected:\n  * another parameter as an irrefutable pattern, e.g a variable;\n  * a type annotation starting with a colon ':' for the body;\n  * the assignment symbol '=' followed by an expression.\n\n"
        )
      ]
    , dtFallbackMsgs =
      [ (mkRange (3, 17) (3, 23) dtFile, "Unexpected: :: int")
      , (mkRange (3, 17) (3, 23) dtFile, "Unrecognized: :: int")
      , (mkRange (3, 20) (3, 23) dtFile, "Unrecognized: int")
      ]
    }

-- LIGO-474 regression test
treeDoesNotContainNameTest :: IO DiagnosticTest
treeDoesNotContainNameTest = do
  dtFile <- makeAbsolute $ inputDir </> "LIGO-474.religo"
  pure DiagnosticTest
    { dtFile
    , dtCompilerMsgs =
      [ (mkRange (1, 14) (1, 16) dtFile, "Syntax error #200.\n")
      ]
    , dtFallbackMsgs =
      [ (mkRange (1, 17) (1, 18) dtFile, "Unexpected: r")
      , (mkRange (1, 14) (1, 16) dtFile, "Expected to find a name, but got `42`")
      , (mkRange (1, 14) (1, 16) dtFile, "Expected to find a name, but got `42`")
      ]
    }

inputDir :: FilePath
inputDir = Util.contractsDir </> "diagnostic"

mkRange :: (Word32, Word32) -> (Word32, Word32) -> FilePath -> Range
mkRange (a, b) (c, d) = Range (a, b, 0) (c, d, 0)

simplifyError :: Msg -> (Range, Text)
simplifyError (range, Error t _) = (range, t)

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
    msgs = collectAllErrors contract
  fmap simplifyError msgs `shouldMatchList` expectedMsgs
