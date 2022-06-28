-- | Checking snapshots collection.
module Test.Snapshots
  ( module Test.Snapshots
  ) where

import Data.Singletons (SingI, sing)
import Data.Singletons.Decide (decideEquality)
import Data.Typeable ((:~:) (Refl))
import Fmt (Buildable, pretty)
import Morley.Michelson.Typed qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion)

import Morley.Debugger.Core
  (DebugSource (..), DebuggerState (..), Direction (..), SourceLocation,
  curSnapshot, frozen, groupSourceLocations, move, moveTill, playInterpretHistory, tsAfterInstrs, Frozen)
import Morley.Debugger.DAP.Types.Morley ()
import Morley.Michelson.Runtime.Dummy (dummyContractEnv)

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Debugger.Snapshots
import Test.Util
import Control.Lens (ix)

data ContractRunData =
  forall param st.
  ( T.IsoValue param, T.IsoValue st
  , SingI (T.ToT param), SingI (T.ToT st)
  , T.ForbidOr (T.ToT param)
  )
  => ContractRunData
  { crdProgram :: FilePath
  , crdEntrypoint :: Maybe String
  , crdParam :: param
  , crdStorage :: st
  }

-- | Make snapshots history for simple contract.
mkSnapshotsFor
  :: HasCallStack
  => ContractRunData -> IO (Set SourceLocation, InterpretHistory InterpretSnapshot)
mkSnapshotsFor (ContractRunData file mEntrypoint (param :: param) (st :: st)) = do
  let entrypoint = mEntrypoint ?: "main"
  ligoMapper <- compileLigoContractDebug entrypoint file
  (allLocs, T.SomeContract (contract@T.Contract{} :: T.Contract cp' st')) <-
    case readLigoMapper ligoMapper of
      Right v -> pure v
      Left err -> assertFailure $ pretty err
  Refl <- sing @cp' `decideEquality` sing @(T.ToT param)
    & maybe (assertFailure "Parameter type mismatch") pure
  Refl <- sing @st' `decideEquality` sing @(T.ToT st)
    & maybe (assertFailure "Storage type mismatch") pure
  let his = collectInterpretSnapshots file (fromString entrypoint) contract T.epcPrimitive (T.toVal param) (T.toVal st) dummyContractEnv
  return (allLocs, his)

testWithSnapshots
  :: ContractRunData
  -> StateT (DebuggerState InterpretSnapshot) IO ()
  -> Assertion
testWithSnapshots runData action = do
  (allLocs, his) <- mkSnapshotsFor runData
  let st = DebuggerState
        { _dsSnapshots = playInterpretHistory his
        , _dsSources = DebugSource mempty <$> groupSourceLocations (toList allLocs)
        }
  evalStateT action st

(@?==)
  :: (MonadIO m, MonadReader r m, Eq a, Buildable (TestBuildable a), HasCallStack)
  => Lens' r a -> a -> m ()
len @?== expected = do
  actual <- view len
  actual @?= expected
infixl 0 @?==

checkSnapshot
  :: (MonadState (DebuggerState InterpretSnapshot) m, MonadIO m)
  => (InterpretSnapshot -> Assertion)
  -> m ()
checkSnapshot check = frozen curSnapshot >>= liftIO . check

unexpectedSnapshot
  :: HasCallStack => InterpretSnapshot -> Assertion
unexpectedSnapshot sp =
  assertFailure $ "Unexpected snapshot:\n" <> pretty sp

fromValCasting :: forall a t. (T.IsoValue a, SingI t) => T.Value t -> Maybe a
fromValCasting v = do
  Refl <- sing @(T.ToT a) `decideEquality` sing @t
  return $ T.fromVal v

pattern SomeLorentzValue :: T.IsoValue v => v -> T.SomeValue
pattern SomeLorentzValue v <- T.SomeValue (fromValCasting -> Just v)
  where SomeLorentzValue v =  T.SomeValue (T.toVal v)

test_Snapshots :: TestTree
test_Snapshots = testGroup "Snapshots collection"
  [ testCaseSteps "noop.mligo contract" \step -> do
      let file = contractsDir </> "noop.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        let step' txt = lift $ step txt
        step' "Initial snapshot"
        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretStarted
            , isStackFrames = StackFrame
                { sfName = "main"
                , sfLoc = LigoRange file' (LigoPosition 1 0) (LigoPosition 1 0)
                  -- ↑ We likely want to change this as soon as debug info
                  -- contains info about entering methods
                , sfStack = []
                } :| []
            } | file == file'
              -> pass
          sp -> unexpectedSnapshot sp

        _ <- move Forward
        lift $ step "Intermediate snapshots"

        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretRunning EventExpressionPreview
            , isStackFrames = StackFrame
                { sfName = "main"
                , sfLoc = LigoRange file' (LigoPosition 2 11) (LigoPosition 2 17)
                , sfStack =
                  [ StackItem
                    { siLigoDesc = LigoStackEntry
                        -- should name name "s", but debug info is yet buggy
                        (LigoExposedStackEntry Nothing _)
                    , siValue = SomeLorentzValue (0 :: Integer)
                    }
                  ]
                } :| []
            } | file == file'
              -> pass
          sp -> unexpectedSnapshot sp

        -- Only in this test we have to check all the snapshots quite thoroughly,
        -- so here getting all the remaining snapshots and checking them.
        (_, tsAfterInstrs -> restSnapshots) <- moveTill Forward (pure False)

        ( restSnapshots <&> \InterpretSnapshot{..} ->
            ( isStatus
            , toList isStackFrames <&> \StackFrame{..} ->
                ( sfLoc
                , sfStack <&> \StackItem{..} ->
                    ( case siLigoDesc of
                        LigoHiddenStackEntry -> Nothing
                        LigoStackEntry LigoExposedStackEntry{..} ->
                          leseDeclaration
                    , siValue
                    )
                )
            )
          )
          @?=
          let
            stack0 =
              [ (Nothing, SomeLorentzValue (0 :: Integer))
              ]
            stackWithS2 =
              [ ( Just LigoVariable
                  { lvName = "s2"
                  }
                , SomeLorentzValue (42 :: Integer)
                )
              ]
          in
          [ ( InterpretRunning . EventExpressionEvaluated . Just $
                SomeLorentzValue (42 :: Integer)
            , one
              ( LigoRange file (LigoPosition 2 11) (LigoPosition 2 17)
              , stack0
              )
            )

          , ( InterpretRunning EventExpressionPreview
            , one
              ( LigoRange file (LigoPosition 3 3) (LigoPosition 3 24)
              , stackWithS2
              )
            )

          , ( InterpretRunning . EventExpressionEvaluated . Just $
                SomeLorentzValue ([] :: [T.Operation])
            , one
              ( LigoRange file (LigoPosition 3 3) (LigoPosition 3 24)
              , stackWithS2
              )
            )

          , ( InterpretRunning EventExpressionPreview
            , one
              ( LigoRange file (LigoPosition 3 3) (LigoPosition 3 28)
              , stackWithS2
              )
            )

          , ( InterpretRunning . EventExpressionEvaluated . Just $
                SomeLorentzValue ([] :: [T.Operation], 42 :: Integer)
            , one
              ( LigoRange file (LigoPosition 3 3) (LigoPosition 3 28)
              , stackWithS2
              )
            )

           , ( InterpretTerminatedOk
            , one
              ( LigoRange file (LigoPosition 3 3) (LigoPosition 3 28)
              , stackWithS2
              )
            )

          ]
  , testCaseSteps "Check specific entrypoint" \_step -> do
      let file = contractsDir </> "not-main-entry-point.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Just "not_main"
            , crdParam = ()
            , crdStorage = 42 :: Integer
            }

      testWithSnapshots runData do
        -- Skip starting snapshot
        _ <- move Forward

        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretRunning EventExpressionPreview
            , isStackFrames = StackFrame
                { sfName = "not_main"
                , sfLoc = LigoRange _ (LigoPosition 2 11) (LigoPosition 2 17)
                } :| []
            } -> pass
          sp -> unexpectedSnapshot sp


  , testCaseSteps "check int type in simple-ops" \_step -> do
        let file = contractsDir </> "simple-ops.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdEntrypoint = Nothing
              , crdParam = ()
              , crdStorage = 42 :: Integer
              }

        testWithSnapshots runData do
          -- Skip starting snapshot
          _ <- move Forward

          checkSnapshot \case
            InterpretSnapshot
              { isStatus = InterpretRunning EventExpressionPreview
              , isStackFrames = StackFrame
                  { sfLoc = LigoRange _ (LigoPosition 2 11) (LigoPosition 2 17)
                  , sfStack =
                    [ StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = LTConstant (LigoTypeConstant [] "int")
                            }
                        }
                    ]
                  } :| []
              } -> pass
            sp -> unexpectedSnapshot sp

  , testCaseSteps "pattern-match on option" \_step -> do
      let file = contractsDir </> "match-on-some.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = Just (5 :: Integer)
            }

      testWithSnapshots runData do
        -- Skip starting snapshot
        _ <- move Forward

        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretRunning EventExpressionPreview
            , isStackFrames = StackFrame
                { sfLoc = LigoRange _ (LigoPosition 3 16) (LigoPosition 3 21)
                } :| []
            } -> pass
          sp -> unexpectedSnapshot sp

  , testCaseSteps "multiple contracts" \step -> do
      let modulePath = contractsDir </> "module_contracts"
      let file = modulePath </> "importer.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 10 :: Integer
            }

      testWithSnapshots runData do
        -- Predicate for @moveTill@ which stops on a snapshot with specified file name in loc.
        let stopAtFile
              :: (MonadState (DebuggerState InterpretSnapshot) m)
              => FilePath
              -> Frozen (DebuggerState InterpretSnapshot) m Bool
            stopAtFile filePath = do
              snap <- curSnapshot
              let locMb = snap ^? isStackFramesL . ix 0 . sfLocL
              case locMb of
                Just loc -> pure $ lrFile loc == filePath
                Nothing -> pure False

        lift $ step "Go to nested contract"
        _ <- moveTill Forward $ stopAtFile (modulePath </> "imported.mligo")
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = LigoRange "test/contracts/module_contracts/imported.mligo" (LigoPosition 15 5) (LigoPosition 15 10)
                } :| []
            } -> pass
          sp -> unexpectedSnapshot sp

        lift $ step "Make sure that we went back"
        _ <- moveTill Forward $ stopAtFile file
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = LigoRange "test/contracts/module_contracts/importer.mligo" (LigoPosition 7 12) (LigoPosition 7 21)
                } :| []
            } -> pass
          sp -> unexpectedSnapshot sp

        lift $ step "Check that we can go to more nested contract (and in another dialect)"
        _ <- moveTill Forward $ stopAtFile (modulePath </> "imported2.ligo")
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = LigoRange "test/contracts/module_contracts/imported2.ligo" (LigoPosition 5 11) (LigoPosition 5 18)
                } :| []
            } -> pass
          sp -> unexpectedSnapshot sp

        lift $ step "Make sure that we went back to \"imported.mligo\""
        _ <- moveTill Forward $ stopAtFile (modulePath </> "imported.mligo")
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = LigoRange "test/contracts/module_contracts/imported.mligo" (LigoPosition 19 57) (LigoPosition 19 64)
                } :| []
            } -> pass
          sp -> unexpectedSnapshot sp

        lift $ step "Make sure that we went back to \"importer.mligo\""
        _ <- moveTill Forward $ stopAtFile file
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = LigoRange "test/contracts/module_contracts/importer.mligo" (LigoPosition 10 26) (LigoPosition 10 39)
                } :| []
            } -> pass
          sp -> unexpectedSnapshot sp
  ]
