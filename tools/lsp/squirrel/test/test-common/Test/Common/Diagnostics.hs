{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Test.Common.Diagnostics
  ( simpleTest
  , treeDoesNotContainNameTest
  , parseDiagnosticsDriver
  ) where

import Language.LSP.Types qualified as J
import System.FilePath ((</>))
import UnliftIO.Directory (makeAbsolute)

import AST.Parser (collectAllErrors, parseWithScopes)
import AST.Scope (ScopingSystem (..))
import Diagnostic (Message (..), MessageDetail (..), Severity (..), filterDiagnostics)
import Range

import Test.Common.Capabilities.Util qualified as Util (contractsDir)
import Test.Common.FixedExpectations (shouldMatchList)
import Test.Common.Util (ScopeTester)
import Test.Tasty.HUnit (Assertion)


data MessageGroup = MessageGroup
  { mgParserMsgs   :: [Message]
  , mgCompilerMsgs :: [Message]
  , mgFallbackMsgs :: [Message]
  }

data DiagnosticTest = DiagnosticTest
  { dtFile :: FilePath
  , dtAllMsgs :: MessageGroup
  , dtFilteredMsgs :: MessageGroup
  }

simpleTest :: IO DiagnosticTest
simpleTest = do
  dtFile <- makeAbsolute $ inputDir </> "a.mligo"
  let
    unexpectedMsg = Message (Unexpected ":: int") SeverityError (mkRange (3, 17) (3, 23) dtFile)
    compilerMsgs =
      [ Message
        (FromLIGO "Ill-formed function parameters.\nAt this point, one of the following is expected:\n  * another parameter as an irrefutable pattern, e.g a variable;\n  * a type annotation starting with a colon ':' for the body;\n  * the assignment symbol '=' followed by an expression.\n")
        SeverityError
        (mkRange (3, 17) (3, 19) dtFile)
      ]
  pure DiagnosticTest
    { dtFile
    , dtAllMsgs = MessageGroup
      { mgParserMsgs =
        [ unexpectedMsg
        , Message (Unrecognized ":: int") SeverityError (mkRange (3, 17) (3, 23) dtFile)
        , Message (Unrecognized "int")    SeverityError (mkRange (3, 20) (3, 23) dtFile)
        ]
      , mgCompilerMsgs = compilerMsgs
      , mgFallbackMsgs = []
      }
    , dtFilteredMsgs = MessageGroup
      { mgParserMsgs = [unexpectedMsg]
      , mgCompilerMsgs = compilerMsgs
      , mgFallbackMsgs = []
      }
    }

-- LIGO-474 regression test
treeDoesNotContainNameTest :: IO DiagnosticTest
treeDoesNotContainNameTest = do
  dtFile <- makeAbsolute $ inputDir </> "LIGO-474.religo"
  let
    msgGroup = MessageGroup
      { mgParserMsgs =
        [ Message (Unexpected "r") SeverityError (mkRange (1, 17) (1, 18) dtFile)
        ]
      , mgCompilerMsgs =
        [ Message (FromLIGO "Syntax error #200.") SeverityError (mkRange (1, 14) (1, 16) dtFile)
        , Message (FromLIGO "Syntax error #233.") SeverityError (mkRange (1, 17) (1, 18) dtFile)
        , Message
          (FromLIGO "Reasonligo is depreacted, support will be dropped in a few versions.@")
          SeverityWarning
          (mkRange (0, 0) (0, 0) "")
        ]
      , mgFallbackMsgs =
        [ Message (FromLanguageServer "Expected to find a name, but got `42`") SeverityError (mkRange (1, 14) (1, 16) dtFile)
        ]
      }
  pure DiagnosticTest
    { dtFile
    , dtAllMsgs = msgGroup
    , dtFilteredMsgs = msgGroup
    }

inputDir :: FilePath
inputDir = Util.contractsDir </> "diagnostic"

mkRange :: (J.UInt, J.UInt) -> (J.UInt, J.UInt) -> FilePath -> Range
mkRange (a, b) (c, d) = Range (a, b, 0) (c, d, 0)

-- | Try to parse a file, and check that the proper error messages are generated.
parseDiagnosticsDriver
  :: forall impl
   . (HasCallStack, ScopeTester impl)
  => ScopingSystem impl
  -> DiagnosticTest
  -> Assertion
parseDiagnosticsDriver source (DiagnosticTest file expectedAllMsgs expectedFilteredMsgs) = do
  contract <- parseWithScopes @impl file
  let
    catMsgs (MessageGroup parser fromCompiler fallback) = parser <> case source of
      CompilerScopes -> fromCompiler
      FallbackScopes -> fallback
      StandardScopes -> fallback <> fromCompiler
    -- FIXME (LIGO-507): Remove duplicated diagnostics.
    msgs = ordNub $ collectAllErrors contract
  msgs `shouldMatchList` catMsgs expectedAllMsgs
  filterDiagnostics msgs `shouldMatchList` catMsgs expectedFilteredMsgs
