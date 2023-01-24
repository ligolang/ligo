{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Test.Common.Diagnostics
  ( MessageGroup (..), DiagnosticTest (..), mkRange, contractsDir
  , simpleTest
  , treeDoesNotContainNameTest
  , parseDiagnosticsDriver
  ) where

import Language.LSP.Types qualified as J
import System.FilePath ((</>))
import UnliftIO.Directory (makeAbsolute)

import AST.Parser (collectAllErrors, parseWithScopes)
import AST.Scope (KnownScopingSystem (..), ScopingSystem (..))
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

contractsDir :: FilePath
contractsDir = Util.contractsDir </> "diagnostic"

simpleTest :: IO DiagnosticTest
simpleTest = do
  dtFile <- makeAbsolute $ contractsDir </> "a.mligo"
  let
    unexpectedMsg = Message (Unexpected ":: int") SeverityError (mkRange (3, 17) (3, 23) dtFile)
    compilerMsgs =
      [ Message
        (FromLIGO "Ill-formed function parameters.\nAt this point, one of the following is expected:\n  * another parameter as an irrefutable pattern, e.g a variable;\n  * a type annotation starting with a colon ':' for the body;\n  * an arrow '->' followed by the function body;\n  * the assignment symbol '=' followed by an expression.\n")
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
  dtFile <- makeAbsolute $ contractsDir </> "LIGO-474.mligo"
  let
    msgGroup = MessageGroup
      { mgParserMsgs =
        [ Message (Unrecognized "") SeverityError (mkRange (2, 7) (2, 7) dtFile)
        ]
      , mgCompilerMsgs =
        [ Message
          (FromLIGO "Ill-formed type declaration.\nAt this point, one of the following is expected:\n  * the name of the type being defined;\n  * a quoted type parameter, like 'a;\n  * a tuple of quoted type parameters, like ('a, 'b).\n")
          SeverityError
          (mkRange (2, 9) (2, 10) dtFile)
        , Message (FromLIGO "Type \"storage\" not found. ")
          SeverityError
          (mkRange (14, 34) (14, 41) dtFile)
        , Message (FromLIGO "Module \"EURO\" not found.")
          SeverityError
          (mkRange (12, 16) (12, 22) dtFile)
        , Message (FromLIGO "Module \"EURO\" not found.")
          SeverityError
          (mkRange (10, 1) (10, 24) dtFile)
        , Message (FromLIGO "Type \"t\" not found. ")
          SeverityError
          (mkRange (3, 19) (3, 20) dtFile)
        ]
      , mgFallbackMsgs =
        [ Message
          (FromLanguageServer "Expected to find a type name, but got `(* Unrecognized:  *)`")
          SeverityError
          (mkRange (2, 7) (2, 7) dtFile)
        ]
      }
  pure DiagnosticTest
    { dtFile
    , dtAllMsgs = msgGroup
    , dtFilteredMsgs = msgGroup
    }

mkRange :: (J.UInt, J.UInt) -> (J.UInt, J.UInt) -> FilePath -> Range
mkRange (a, b) (c, d) = Range (a, b, 0) (c, d, 0)

-- | Try to parse a file, and check that the proper error messages are generated.
parseDiagnosticsDriver
  :: forall impl
   . (HasCallStack, ScopeTester impl, KnownScopingSystem impl)
  => DiagnosticTest
  -> Assertion
parseDiagnosticsDriver (DiagnosticTest file expectedAllMsgs expectedFilteredMsgs) = do
  contract <- parseWithScopes @impl file
  let
    catMsgs (MessageGroup parser fromCompiler fallback) = parser <> case knownScopingSystem @impl of
      CompilerScopes -> fromCompiler
      FallbackScopes -> fallback
      StandardScopes -> fallback <> fromCompiler
    -- FIXME (LIGO-507): Remove duplicated diagnostics.
    msgs = ordNub $ collectAllErrors contract
  msgs `shouldMatchList` catMsgs expectedAllMsgs
  filterDiagnostics msgs `shouldMatchList` catMsgs expectedFilteredMsgs
