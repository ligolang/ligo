module Test.Common.Capabilities.Completion
  ( completionDriver
  , caseInfos
  ) where

import Algebra.Graph.AdjacencyMap qualified as G
import Data.Maybe (fromJust)
import Language.LSP.Types (CompletionItemKind (..), UInt)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import AST.Capabilities.Completion
import AST.Scope.Common
import Log qualified
import Range (point)

import Test.Common.Capabilities.Util qualified (contractsDir)
import Test.Common.FixedExpectations (expectationFailure, shouldMatchList)
import Test.Common.Util (ScopeTester, parseDirectoryWithScopes)

contractsDir :: FilePath
contractsDir = Test.Common.Capabilities.Util.contractsDir </> "completion"

data TestInfo = TestInfo
  { tiContract :: FilePath
  , tiPosition :: (UInt, UInt)
  , tiExpected :: [Completion]
  , tiGraph :: Includes FilePath
  }

-- Note: Not all completions will be in the completion list for VSCode. This is
-- because VSCode uses a fuzzy matcher, which can't be disabled, which further
-- filters the completion results.
-- See this for a discussion on the matter:
-- https://gitlab.com/serokell/ligo/ligo/-/merge_requests/176#note_679710028
caseInfos :: [TestInfo]
caseInfos =
  [ TestInfo
    { tiContract = "no-prefix.ligo"
    , tiPosition = (1, 46)
    , tiExpected = []
    , tiGraph = Includes G.empty
    }
  , TestInfo
    { tiContract = "yes-prefix.ligo"
    , tiPosition = (1, 48)
    , tiExpected =
      [ Completion (Just CiVariable) (NameCompletion "parameter") (Just $ TypeCompletion "int") (DocCompletion "")
      , CompletionKeyword (NameCompletion "patch")
      ]
    , tiGraph = Includes G.empty
    }

  , TestInfo
    { tiContract = "type-attribute.ligo"
    , tiPosition = (15, 35)
    , tiExpected =
      [ Completion (Just CiField) (NameCompletion "id") (Just $ TypeCompletion "nat") (DocCompletion "")
      , Completion (Just CiField) (NameCompletion "is_admin") (Just $ TypeCompletion "bool") (DocCompletion "")
      , CompletionKeyword (NameCompletion "big_map")
      , CompletionKeyword (NameCompletion "if")
      , CompletionKeyword (NameCompletion "begin")
      , CompletionKeyword (NameCompletion "list")
      , CompletionKeyword (NameCompletion "skip")
      , CompletionKeyword (NameCompletion "in")
      , CompletionKeyword (NameCompletion "while")
      , CompletionKeyword (NameCompletion "is")
      , CompletionKeyword (NameCompletion "nil")
      , CompletionKeyword (NameCompletion "recursive")
      , CompletionKeyword (NameCompletion "contains")
      , CompletionKeyword (NameCompletion "function")
      ]
    , tiGraph = Includes G.empty
    }
  , TestInfo
    { tiContract = "type-attribute.mligo"
    , tiPosition = (13, 33)
    , tiExpected =
      [ Completion (Just CiField) (NameCompletion "id") (Just $ TypeCompletion "nat") (DocCompletion "")
      , Completion (Just CiField) (NameCompletion "is_admin") (Just $ TypeCompletion "bool") (DocCompletion "")
      , CompletionKeyword (NameCompletion "if")
      , CompletionKeyword (NameCompletion "begin")
      , CompletionKeyword (NameCompletion "with")
      , CompletionKeyword (NameCompletion "in")
      ]
    , tiGraph = Includes G.empty
    }
  , TestInfo
    { tiContract = "type-attribute.religo"
    , tiPosition = (13, 33)
    , tiExpected =
      [ Completion (Just CiField) (NameCompletion "id") (Just $ TypeCompletion "nat") (DocCompletion "")
      , Completion (Just CiField) (NameCompletion "is_admin") (Just $ TypeCompletion "bool") (DocCompletion "")
      , CompletionKeyword (NameCompletion "if")
      , CompletionKeyword (NameCompletion "switch")
      ]
    , tiGraph = Includes G.empty
    }
  , TestInfo
    { tiContract = "type-attribute.jsligo"
    , tiPosition = (13, 33)
    , tiExpected =
      [ Completion (Just CiField) (NameCompletion "id") (Just $ TypeCompletion "nat") (DocCompletion "")
      , Completion (Just CiField) (NameCompletion "is_admin") (Just $ TypeCompletion "bool") (DocCompletion "")
      , CompletionKeyword (NameCompletion "if")
      , CompletionKeyword (NameCompletion "switch")
      , CompletionKeyword (NameCompletion "while")
      , CompletionKeyword (NameCompletion "import")
      ]
    , tiGraph = Includes G.empty
    }

  , TestInfo
    { tiContract = "type-constructor.ligo"
    , tiPosition = (5, 21)
    , tiExpected =
      [ Completion (Just CiConstructor) (NameCompletion "Increment") (Just $ TypeCompletion "action") (DocCompletion "")
      ]
    , tiGraph = Includes G.empty
    }
  , TestInfo
    { tiContract = "type-constructor.mligo"
    , tiPosition = (5, 19)
    , tiExpected =
      [ Completion (Just CiConstructor) (NameCompletion "Increment") (Just $ TypeCompletion "action") (DocCompletion "")
      ]
    , tiGraph = Includes G.empty
    }
  , TestInfo
    { tiContract = "type-constructor.religo"
    , tiPosition = (5, 19)
    , tiExpected =
      [ Completion (Just CiConstructor) (NameCompletion "Increment") (Just $ TypeCompletion "action") (DocCompletion "")
      ]
    , tiGraph = Includes G.empty
    }
  , TestInfo
    { tiContract = "type-constructor.jsligo"
    , tiPosition = (5, 19)
    , tiExpected =
      [ Completion (Just CiConstructor) (NameCompletion "Increment") (Just $ TypeCompletion "action") (DocCompletion "")
      ]
    , tiGraph = Includes G.empty
    }

  , TestInfo
    { tiContract = "unfinished-field-name.mligo"
    , tiPosition = (8, 27)
    , tiExpected =
      [ Completion (Just CiField) (NameCompletion "sum") (Just $ TypeCompletion "int") (DocCompletion "")
      , CompletionKeyword (NameCompletion "struct")
      ]
    , tiGraph = Includes G.empty
    }

  , TestInfo
    { tiContract = "nested-fields.ligo"
    , tiPosition = (21, 37)
    , tiExpected =
      [ Completion (Just CiField) (NameCompletion "series") (Just $ TypeCompletion "int") (DocCompletion "")
      , CompletionKeyword (NameCompletion "set")
      , CompletionKeyword (NameCompletion "list")
      , CompletionKeyword (NameCompletion "skip")
      , CompletionKeyword (NameCompletion "case")
      , CompletionKeyword (NameCompletion "else")
      , CompletionKeyword (NameCompletion "step")
      , CompletionKeyword (NameCompletion "is")
      , CompletionKeyword (NameCompletion "recursive")
      , CompletionKeyword (NameCompletion "const")
      , CompletionKeyword (NameCompletion "contains")
      ]
    , tiGraph = Includes G.empty
    }
  , TestInfo
    { tiContract = "nested-fields.mligo"
    , tiPosition = (18, 36)
    , tiExpected =
      [ Completion (Just CiField) (NameCompletion "series") (Just $ TypeCompletion "int") (DocCompletion "")
      , CompletionKeyword (NameCompletion "struct")
      , CompletionKeyword (NameCompletion "lsl")
      , CompletionKeyword (NameCompletion "else")
      , CompletionKeyword (NameCompletion "lsr")
      ]
    , tiGraph = Includes G.empty
    }
  , TestInfo
    { tiContract = "nested-fields.religo"
    , tiPosition = (18, 36)
    , tiExpected =
      [ Completion (Just CiField) (NameCompletion "series") (Just $ TypeCompletion "int") (DocCompletion "")
      , CompletionKeyword (NameCompletion "switch")
      , CompletionKeyword (NameCompletion "lsl")
      , CompletionKeyword (NameCompletion "else")
      , CompletionKeyword (NameCompletion "lsr")
      ]
    , tiGraph = Includes G.empty
    }
  , TestInfo
    { tiContract = "nested-fields.jsligo"
    , tiPosition = (18, 35)
    , tiExpected =
      [ Completion (Just CiField) (NameCompletion "series") (Just $ TypeCompletion "int") (DocCompletion "")
      , CompletionKeyword (NameCompletion "as")
      , CompletionKeyword (NameCompletion "switch")
      , CompletionKeyword (NameCompletion "namespace")
      , CompletionKeyword (NameCompletion "case")
      , CompletionKeyword (NameCompletion "else")
      , CompletionKeyword (NameCompletion "const")
      ]
    , tiGraph = Includes G.empty
    }
  , TestInfo
    { tiContract = "incr.mligo"
    , tiPosition = (3, 13)
    , tiExpected =
      [ Completion (Just CiFunction) (NameCompletion "incr_my_stuff") (Just $ TypeCompletion "nat") (DocCompletion "")
      , CompletionKeyword (NameCompletion "begin")
      , CompletionKeyword (NameCompletion "in")
      ]
    , tiGraph = Includes G.empty
    }
  , TestInfo
    { tiContract = "import/outer-includer.ligo"
    , tiPosition = (1, 11)
    , tiExpected =
        [ ImportCompletion (NameCompletion "outer-includer2.ligo")
        , ImportCompletion (NameCompletion "innerFolder/inner-includer.ligo")
        ]
    , tiGraph = Includes $ G.vertices
        [ "./test/contracts/completion/import/outer-includer.ligo"
        , "./test/contracts/completion/import/outer-includer2.ligo"
        , "./test/contracts/completion/import/innerFolder/inner-includer.ligo"
        ]
    }
  , TestInfo
    { tiContract = "import/innerFolder/inner-includer.ligo"
    , tiPosition = (1, 11)
    , tiExpected =
        [ ImportCompletion (NameCompletion "../outer-includer2.ligo")
        , ImportCompletion (NameCompletion "../outer-includer.ligo")
        ]
    , tiGraph = Includes $ G.vertices
        [ "./test/contracts/completion/import/outer-includer.ligo"
        , "./test/contracts/completion/import/outer-includer2.ligo"
        , "./test/contracts/completion/import/innerFolder/inner-includer.ligo"
        ]
    }
  ]

completionDriver :: forall parser. ScopeTester parser => [TestInfo] -> IO TestTree
completionDriver testInfos = do
  graph <- parseDirectoryWithScopes @parser contractsDir
  pure $ testGroup "Completion" $ map (makeTestCase graph) testInfos
  where
    makeTestCase graph info =
      testCase (tiContract info) do

        let fp = contractsDir </> tiContract info
            pos = uncurry point $ tiPosition info
            contract = fromJust $ lookupContract fp graph
            tree = contractTree contract
            source = _cFile $ _getContract contract
        results <- Log.runNoLoggingT $ withCompleterM (CompleterEnv pos tree source (tiGraph info)) complete
        case (results, tiExpected info) of
          (Nothing, []) -> pure ()
          (Nothing, _) -> expectationFailure "Expected completion items, but got none"
          (Just [], []) -> pure ()
          (Just _, []) -> expectationFailure "Expected no completion items, but got them"
          (Just results', expected') -> results' `shouldMatchList` expected'
