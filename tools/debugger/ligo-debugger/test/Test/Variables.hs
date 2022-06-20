module Test.Variables
  ( test_Variables
  ) where

import Data.Map qualified as M
import Language.LIGO.DAP.Variables (createVariables, runBuilder)
import Language.LIGO.Debugger.CLI.Types
  (LigoExposedStackEntry (LigoExposedStackEntry), LigoStackEntry (LigoStackEntry), LigoVariable (LigoVariable), LigoType (LTUnresolved))
import Language.LIGO.Debugger.Snapshots (StackItem (StackItem))
import Morley.Debugger.Protocol.DAP (Variable (..))
import Morley.Michelson.Typed
  (SingI, SomeConstrainedValue (SomeValue), T (TUnit), Value, Value' (VList, VOption, VUnit))
import Test.HUnit ((@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

mkStackItem :: (SingI t) => Value t -> Maybe Text -> StackItem
mkStackItem v nameMb = StackItem desc (SomeValue v)
  where
    desc =
      LigoStackEntry $ LigoExposedStackEntry (LigoVariable <$> nameMb) LTUnresolved

test_Variables :: TestTree
test_Variables = testGroup "variables"
  [ testOption
  , testList
  ]

testOption :: TestTree
testOption = testGroup "option"
  [ testCase "nothing" do
      let vNothingItem = mkStackItem (VOption @'TUnit Nothing) (Just "nothingVar")
      snd (runBuilder $ createVariables [vNothingItem]) @?=
        M.fromList
          [ (1,
              [ Variable
                  { nameVariable = "nothingVar"
                  , valueVariable = "None"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "None"
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              ]
            )
          ]
  , testCase "contains unit" do
      let vUnitItem = mkStackItem (VOption $ Just VUnit) (Just "someUnit")
      snd (runBuilder $ createVariables [vUnitItem]) @?=
        M.fromList
          [ (1,
              [ Variable
                  { nameVariable = "Some"
                  , valueVariable = "()"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "()"
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
                ]
              )
          , (2,
              [ Variable
                  { nameVariable = "someUnit"
                  , valueVariable = "Some ()"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "Some ()"
                  , variablesReferenceVariable = 1
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              ]
            )
          ]
  ]

testList :: TestTree
testList = testGroup "list"
  [ testCase "empty list" do
      let vList = mkStackItem (VList @'TUnit []) (Just "list")
      snd (runBuilder $ createVariables [vList]) @?=
        M.fromList
          [ (1,
              [ Variable
                  { nameVariable = "list"
                  , valueVariable = "[]"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "[]"
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              ]
            )
          ]
  , testCase "list of two units" do
      let vList = mkStackItem (VList [VUnit, VUnit]) (Just "list")
      snd (runBuilder $ createVariables [vList]) @?=
        M.fromList
          [ (1,
              [ Variable
                  { nameVariable = "1"
                  , valueVariable = "()"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "()"
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              , Variable
                  { nameVariable = "2"
                  , valueVariable = "()"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "()"
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              ]
            )
          , (2,
              [ Variable
                  { nameVariable = "list"
                  , valueVariable = "[(), ()]"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "[(), ()]"
                  , variablesReferenceVariable = 1
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              ]
            )
          ]
  ]
