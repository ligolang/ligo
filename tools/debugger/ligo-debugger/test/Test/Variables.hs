module Test.Variables
  ( test_Variables
  ) where

import Data.Map qualified as M
import Morley.Debugger.Protocol.DAP (Variable (..))
import Morley.Michelson.Typed
  (EpAddress (EpAddress), EpName (UnsafeEpName), MkEntrypointCallRes (MkEntrypointCallRes),
  ParamNotes (pnRootAnn), SingI, SomeConstrainedValue (SomeValue), SomeEntrypointCallT (SomeEpc),
  T (TUnit), Value, Value' (VAddress, VContract, VList, VOption, VUnit), mkEntrypointCall,
  sepcPrimitive, tyImplicitAccountParam)
import Morley.Michelson.Untyped (Annotation (UnsafeAnnotation), pattern DefEpName)
import Morley.Tezos.Address (parseAddress)
import Unsafe (fromJust)

import Test.HUnit ((@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Language.LIGO.DAP.Variables (createVariables, runBuilder)
import Language.LIGO.Debugger.CLI.Types
  (LigoExposedStackEntry (LigoExposedStackEntry), LigoStackEntry (LigoStackEntry),
  LigoType (LTUnresolved), LigoVariable (LigoVariable))
import Language.LIGO.Debugger.Snapshots (StackItem (StackItem))

mkStackItem :: (SingI t) => Value t -> Maybe Text -> StackItem
mkStackItem v nameMb = StackItem desc (SomeValue v)
  where
    desc =
      LigoStackEntry $ LigoExposedStackEntry (LigoVariable <$> nameMb) LTUnresolved

test_Variables :: TestTree
test_Variables = testGroup "variables"
  [ testOption
  , testList
  , testContracts
  , testAddresses
  ]

testAddresses :: TestTree
testAddresses = testGroup "addresses"
  [ testCase "address with entrypoint \"foo\"" do
      let epAddress = EpAddress address (UnsafeEpName "foo")
      let addressItem = mkStackItem (VAddress epAddress) (Just "addr")
      snd (runBuilder $ createVariables [addressItem]) @?=
        M.fromList
          [ (1,
              [ Variable
                  { nameVariable = "address"
                  , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Nothing
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              , Variable
                  { nameVariable = "entrypoint"
                  , valueVariable = "foo"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Nothing
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Nothing
                  }
              ])
          , (2,
              [ Variable
                  { nameVariable = "addr"
                  , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%foo"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%foo"
                  , variablesReferenceVariable = 1
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Just "address"
                  }
              ])
          ]
  , testCase "address without entrypoint" do
      let epAddress = EpAddress address DefEpName
      let addressItem = mkStackItem (VAddress epAddress) (Just "addr")
      snd (runBuilder $ createVariables [addressItem]) @?=
        M.fromList
          [ (1,
              [ Variable
                { nameVariable = "addr"
                , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                , typeVariable = ""
                , presentationHintVariable = Nothing
                , evaluateNameVariable = Just "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                , variablesReferenceVariable = 0
                , namedVariablesVariable = Nothing
                , indexedVariablesVariable = Nothing
                , __vscodeVariableMenuContextVariable = Just "address"
                }
              ])
          ]
  ]
  where
    address = fromRight (error "address parse error")
      $ parseAddress "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"

testContracts :: TestTree
testContracts = testGroup "contracts"
  [ testCase "contract with entrypoint \"foo\"" do
      let paramNotes = tyImplicitAccountParam
            { pnRootAnn = UnsafeAnnotation "foo"
            }
      let mkEntrypoint = fromJust $
            mkEntrypointCall (UnsafeEpName "foo") paramNotes
      case mkEntrypoint of
        MkEntrypointCallRes _ entrypoint -> do
          let contractItem = mkStackItem (VContract address (SomeEpc entrypoint)) (Just "contract")
          snd (runBuilder $ createVariables [contractItem]) @?=
            M.fromList
              [ (1,
                  [ Variable
                      { nameVariable = "address"
                      , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                      , typeVariable = ""
                      , presentationHintVariable = Nothing
                      , evaluateNameVariable = Nothing
                      , variablesReferenceVariable = 0
                      , namedVariablesVariable = Nothing
                      , indexedVariablesVariable = Nothing
                      , __vscodeVariableMenuContextVariable = Nothing
                      }
                  , Variable
                      { nameVariable = "entrypoint"
                      , valueVariable = "foo"
                      , typeVariable = ""
                      , presentationHintVariable = Nothing
                      , evaluateNameVariable = Nothing
                      , variablesReferenceVariable = 0
                      , namedVariablesVariable = Nothing
                      , indexedVariablesVariable = Nothing
                      , __vscodeVariableMenuContextVariable = Nothing
                      }
                  ])
              , (2,
                  [ Variable
                      { nameVariable = "contract"
                      , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%foo"
                      , typeVariable = ""
                      , presentationHintVariable = Nothing
                      , evaluateNameVariable = Just "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%foo"
                      , variablesReferenceVariable = 1
                      , namedVariablesVariable = Nothing
                      , indexedVariablesVariable = Nothing
                      , __vscodeVariableMenuContextVariable = Just "contract"
                      }
                  ])
              ]
  , testCase "contract without entrypoint" do
      let contractItem = mkStackItem (VContract address (sepcPrimitive @'TUnit)) (Just "contract")
      snd (runBuilder $ createVariables [contractItem]) @?=
        M.fromList
          [ (1,
              [ Variable
                  { nameVariable = "contract"
                  , valueVariable = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                  , typeVariable = ""
                  , presentationHintVariable = Nothing
                  , evaluateNameVariable = Just "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                  , variablesReferenceVariable = 0
                  , namedVariablesVariable = Nothing
                  , indexedVariablesVariable = Nothing
                  , __vscodeVariableMenuContextVariable = Just "contract"
                  }
              ])
          ]
  ]
  where
    address = fromRight (error "address parse error")
      $ parseAddress "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"

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
