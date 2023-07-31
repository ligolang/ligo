module Test.Variables
  ( test_Variables
  ) where

import Data.Map qualified as M
import Fmt (pretty)
import Unsafe (fromJust)

import Morley.Michelson.Typed
  (Constrained (SomeValue), EpAddress (EpAddress'), EpName (UnsafeEpName),
  MkEntrypointCallRes (MkEntrypointCallRes), ParamNotes (pnRootAnn), SingI,
  SomeEntrypointCallT (SomeEpc), T (TUnit), Value,
  Value' (VAddress, VContract, VList, VOption, VUnit), mkEntrypointCall, sepcPrimitive,
  tyImplicitAccountParam)
import Morley.Michelson.Untyped (Annotation (UnsafeAnnotation), pattern DefEpName)
import Morley.Tezos.Address (parseAddress)
import Named (defaults, (!))
import Protocol.DAP qualified as DAP

import Test.HUnit ((@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Language.LIGO.AST (Lang (Caml))
import Language.LIGO.DAP.Variables (createVariables, runBuilder)
import Language.LIGO.Debugger.CLI

mkDummyValue :: LigoOrMichValue -> Maybe (Name 'Concise) -> (Text, LigoOrMichValue)
mkDummyValue v nameMb =
  ( maybe unknownVariable pretty nameMb
  , v
  )

mkDummyMichValue :: (SingI t) => Value t -> Maybe (Name 'Concise) -> (Text, LigoOrMichValue)
mkDummyMichValue v = mkDummyValue (MichValue (LigoType Nothing) $ SomeValue v)

mkDummyLigoValue :: LigoValue -> Maybe (Name 'Concise) -> (Text, LigoOrMichValue)
mkDummyLigoValue v = mkDummyValue (LigoValue (LigoType Nothing) v)

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
      let epAddress = EpAddress' address (UnsafeEpName "foo")
      let addressItem = mkDummyMichValue (VAddress epAddress) (Just "addr")
      snd (runBuilder $ createVariables Caml [addressItem]) @?=
        M.fromList
          [ (DAP.VariableId 1,
              [ DAP.mk @DAP.Variable
                  ! #name "address"
                  ! #value "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                  ! defaults
              , DAP.mk @DAP.Variable
                  ! #name "entrypoint"
                  ! #value "foo"
                  ! defaults
              ])
          , (DAP.VariableId 2,
              [ DAP.mk @DAP.Variable
                  ! #name "addr"
                  ! #value "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%foo"
                  ! #evaluateName "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%foo"
                  ! #variablesReference (DAP.VariableId 1)
                  ! #__vscodeVariableMenuContext "address"
                  ! defaults
              ])
          ]
  , testCase "address without entrypoint" do
      let epAddress = EpAddress' address DefEpName
      let addressItem = mkDummyMichValue (VAddress epAddress) (Just "addr")
      snd (runBuilder $ createVariables Caml [addressItem]) @?=
        M.fromList
          [ (DAP.VariableId 1,
              [ DAP.mk @DAP.Variable
                ! #name "addr"
                ! #value "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                ! #evaluateName "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                ! #__vscodeVariableMenuContext "address"
                ! defaults
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
          let contractItem = mkDummyMichValue (VContract address (SomeEpc entrypoint)) (Just "contract")
          snd (runBuilder $ createVariables Caml [contractItem]) @?=
            M.fromList
              [ (DAP.VariableId 1,
                  [ DAP.mk @DAP.Variable
                      ! #name "address"
                      ! #value "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                  ! defaults
                  , DAP.mk @DAP.Variable
                      ! #name "entrypoint"
                      ! #value "foo"
                      ! defaults
                  ])
              , (DAP.VariableId 2,
                  [ DAP.mk @DAP.Variable
                      ! #name "contract"
                      ! #value "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%foo"
                      ! #evaluateName "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%foo"
                      ! #variablesReference (DAP.VariableId 1)
                      ! #__vscodeVariableMenuContext "contract"
                      ! defaults
                  ])
              ]
  , testCase "contract without entrypoint" do
      let contractItem = mkDummyMichValue (VContract address (sepcPrimitive @'TUnit)) (Just "contract")
      snd (runBuilder $ createVariables Caml [contractItem]) @?=
        M.fromList
          [ (DAP.VariableId 1,
              [ DAP.mk @DAP.Variable
                  ! #name "contract"
                  ! #value "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                  ! #evaluateName "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                  ! #__vscodeVariableMenuContext "contract"
                  ! defaults
              ])
          ]
  , testCase "LIGO contract" do
      let contract = LigoContract "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU" (Just "foo")
      let contractItem = mkDummyLigoValue (LVCt $ LCContract contract) (Just "addr")
      snd (runBuilder $ createVariables Caml [contractItem]) @?=
        M.fromList
          [ (DAP.VariableId 1,
              [ DAP.mk @DAP.Variable
                  ! #name "address"
                  ! #value "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
                  ! defaults
              , DAP.mk @DAP.Variable
                  ! #name "entrypoint"
                  ! #value "foo"
                  ! defaults
              ])
          , (DAP.VariableId 2,
              [ DAP.mk @DAP.Variable
                  ! #name "addr"
                  ! #value "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU(foo)"
                  ! #evaluateName "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU(foo)"
                  ! #variablesReference (DAP.VariableId 1)
                  ! #__vscodeVariableMenuContext "contract"
                  ! defaults
              ])
          ]
  ]
  where
    address = fromRight (error "address parse error")
      $ parseAddress "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"

testOption :: TestTree
testOption = testGroup "option"
  [ testCase "nothing" do
      let vNothingItem = mkDummyMichValue (VOption @'TUnit Nothing) (Just "nothingVar")
      snd (runBuilder $ createVariables Caml [vNothingItem]) @?=
        M.fromList
          [ (DAP.VariableId 1,
              [ DAP.mk @DAP.Variable
                  ! #name "nothingVar"
                  ! #value "None"
                  ! #evaluateName "None"
                  ! defaults
              ]
            )
          ]
  , testCase "contains unit" do
      let vUnitItem = mkDummyMichValue (VOption $ Just VUnit) (Just "someUnit")
      snd (runBuilder $ createVariables Caml [vUnitItem]) @?=
        M.fromList
          [ (DAP.VariableId 1,
              [ DAP.mk @DAP.Variable
                  ! #name "Some"
                  ! #value "()"
                  ! #evaluateName "()"
                  ! defaults
                ]
              )
          , (DAP.VariableId 2,
              [ DAP.mk @DAP.Variable
                  ! #name "someUnit"
                  ! #value "Some ()"
                  ! #evaluateName "Some ()"
                  ! #variablesReference (DAP.VariableId 1)
                  ! defaults
              ]
            )
          ]
  , testCase "LIGO Some" do
      let ligoSome = mkDummyLigoValue (LVConstructor ("Some", LVCt LCUnit)) (Just "someUnit")
      snd (runBuilder $ createVariables Caml [ligoSome]) @?=
        M.fromList
          [ (DAP.VariableId 1,
              [ DAP.mk @DAP.Variable
                  ! #name "Some"
                  ! #value "()"
                  ! #evaluateName "()"
                  ! defaults
                ]
              )
          , (DAP.VariableId 2,
              [ DAP.mk @DAP.Variable
                  ! #name "someUnit"
                  ! #value "Some (())"
                  ! #evaluateName "Some (())"
                  ! #variablesReference (DAP.VariableId 1)
                  ! defaults
              ]
            )
          ]
  ]

testList :: TestTree
testList = testGroup "list"
  [ testCase "empty list" do
      let vList = mkDummyMichValue (VList @'TUnit []) (Just "list")
      snd (runBuilder $ createVariables Caml [vList]) @?=
        M.fromList
          [ (DAP.VariableId 1,
              [ DAP.mk @DAP.Variable
                  ! #name "list"
                  ! #value "[]"
                  ! #evaluateName "[]"
                  ! defaults
              ]
            )
          ]
  , testCase "list of two units" do
      let vList = mkDummyMichValue (VList [VUnit, VUnit]) (Just "list")
      snd (runBuilder $ createVariables Caml [vList]) @?=
        M.fromList
          [ (DAP.VariableId 1,
              [ DAP.mk @DAP.Variable
                  ! #name "1"
                  ! #value "()"
                  ! #evaluateName "()"
                  ! defaults
              , DAP.mk @DAP.Variable
                  ! #name "2"
                  ! #value "()"
                  ! #evaluateName "()"
                  ! defaults
              ]
            )
          , (DAP.VariableId 2,
              [ DAP.mk @DAP.Variable
                  ! #name "list"
                  ! #value "[(), ()]"
                  ! #evaluateName "[(), ()]"
                  ! #variablesReference (DAP.VariableId 1)
                  ! defaults
              ]
            )
          ]
  , testCase "LIGO list" do
      let ligoList = mkDummyLigoValue (LVList [LVCt LCUnit, LVCt LCUnit]) (Just "list")
      snd (runBuilder $ createVariables Caml [ligoList]) @?=
        M.fromList
          [ (DAP.VariableId 1,
              [ DAP.mk @DAP.Variable
                  ! #name "1"
                  ! #value "()"
                  ! #evaluateName "()"
                  ! defaults
              , DAP.mk @DAP.Variable
                  ! #name "2"
                  ! #value "()"
                  ! #evaluateName "()"
                  ! defaults
              ]
            )
          , (DAP.VariableId 2,
              [ DAP.mk @DAP.Variable
                  ! #name "list"
                  ! #value "[(); ()]"
                  ! #evaluateName "[(); ()]"
                  ! #variablesReference (DAP.VariableId 1)
                  ! defaults
              ]
            )
          ]
  ]
