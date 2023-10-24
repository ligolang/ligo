-- | Checking snapshots collection.
module Test.Snapshots
  ( test_Snapshots
  , test_Contracts_are_sensible
  ) where

import Unsafe qualified

import Control.Category ((>>>))
import Control.Lens (Each (each), has, ix, lens, toListOf, (?~), (^?!))
import Control.Monad.Writer (listen)
import Data.Coerce (coerce)
import Data.Default (Default (def))
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Fmt (pretty)
import System.FilePath (combine, dropExtension, makeRelative)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@=?))
import Test.Util
import Test.Util.Options (minor, reinsuring)
import Text.Interpolation.Nyan hiding (rmode')
import UnliftIO (forConcurrently_)

import Morley.Debugger.Core
  (DebuggerFailure (DebuggerInfiniteLoop), DebuggerState (..), Direction (..),
  FinalStack (ContractFinalStack), FrozenPredicate (FrozenPredicate), HistoryReplayM,
  MovementResult (..), NavigableSnapshot (getExecutedPosition), SourceLocation' (SourceLocation),
  SrcLoc (..), curSnapshot, frozen, matchesSrcType, move, moveTill, tsAfterInstrs, tsAllVisited)
import Morley.Debugger.Core.Breakpoint qualified as N
import Morley.Debugger.DAP.Types.Morley ()
import Morley.Michelson.ErrorPos (ErrorSrcPos (ErrorSrcPos), Pos (Pos), SrcPos (SrcPos))
import Morley.Michelson.Interpret
  (MichelsonFailed (MichelsonExt), MichelsonFailureWithStack (MichelsonFailureWithStack),
  NoStkElMeta (NoStkElMeta), StkEl (MkStkEl))
import Morley.Michelson.Parser.Types (MichelsonSource (MSFile))
import Morley.Michelson.Typed (SomeValue)
import Morley.Michelson.Typed qualified as T

import Lorentz (MText, Rec (RNil, (:&)))
import Lorentz qualified as L
import Lorentz.Value (mt)

import Language.LIGO.AST (scanContracts)
import Language.LIGO.Debugger.CLI
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Handlers.Helpers
import Language.LIGO.Debugger.Handlers.Impl (convertMichelsonValuesToLigo)
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Debugger.Navigate
import Language.LIGO.Debugger.Snapshots
import Language.LIGO.Range

test_Snapshots :: TestTree
test_Snapshots = testGroup "Snapshots collection"
  [ testCaseSteps "noop.mligo contract" \_step -> do
      let file = contractsDir </> "noop.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretRunning EventFacedStatement
            , isStackFrames = StackFrame
                { sfName = "main"
                , sfLoc = Range (LigoPosition 3 3) (LigoPosition 3 18) file'
                , sfStack =
                  [ StackItem
                    { siLigoDesc = LigoStackEntry (LigoExposedStackEntry (Just (LigoVariable "s")) typ _)
                    , siValue = SomeLorentzValue (0 :: Integer)
                    }
                  ]
                } :| _
            } | file == file' && typ == intType
              -> pass
          sp -> unexpectedSnapshot sp

        _ <- move Forward

        -- Only in this test we have to check all the snapshots quite thoroughly,
        -- so here getting all the remaining snapshots and checking them.
        (_, tsAfterInstrs -> restSnapshots) <- listen $ moveTill Forward false

        ( restSnapshots <&> \InterpretSnapshot{..} ->
            ( isStatus
            , toList isStackFrames <&> \StackFrame{..} ->
                ( sfLoc
                , sfStack <&> \StackItem{..} ->
                    ( case siLigoDesc of
                        LigoHiddenStackEntry -> Nothing
                        LigoStackEntry LigoExposedStackEntry{..} ->
                          LigoVariable . pretty @_ @(Name 'Concise) <$> leseDeclaration
                    , siValue
                    )
                )
            )
          )
          @?=
          let
            stackWithS2 =
              [ ( Just LigoVariable
                  { lvName = "s"
                  }
                , SomeLorentzValue (0 :: Integer)
                )
              , ( Just LigoVariable
                  { lvName = "s2"
                  }
                , SomeLorentzValue (42 :: Integer)
                )
              ]
            stackWithS =
              [ ( Just LigoVariable
                  { lvName = "s"
                  }
                , SomeLorentzValue (0 :: Integer)
                )
              ]
            lastStack =
              [ ( Nothing
                , SomeLorentzValue ([] :: [T.Operation], 42 :: Integer)
                )
              , ( Nothing
                , SomeLorentzValue (0 :: Integer)
                )
              ]
            contractOut = MkStkEl NoStkElMeta $ T.toVal ([] :: [T.Operation], 42 :: Integer)

            operationListType' = mkConstantType "List" [mkSimpleConstantType "Operation"]
            operationListType = LigoTypeResolved operationListType'
            opsAndStorageType = LigoTypeResolved $
              mkPairType operationListType' intType'
          in
          [ ( InterpretRunning . EventExpressionEvaluated intType . Just $
                SomeLorentzValue (42 :: Integer)
            , one
              ( Range (LigoPosition 3 12) (LigoPosition 3 18) file
              , stackWithS
              )
            )

          , ( InterpretRunning EventFacedStatement
            , one
              ( Range (LigoPosition 4 3) (LigoPosition 4 30) file
              , stackWithS2
              )
            )

          , ( InterpretRunning (EventExpressionPreview GeneralExpression)
            , one
              ( Range (LigoPosition 4 4) (LigoPosition 4 29) file
              , stackWithS2
              )
            )

          , ( InterpretRunning (EventExpressionPreview GeneralExpression)
            , one
              ( Range (LigoPosition 4 4) (LigoPosition 4 25) file
              , stackWithS2
              )
            )

          , ( InterpretRunning . EventExpressionEvaluated operationListType . Just $
                SomeLorentzValue ([] :: [T.Operation])
            , one
              ( Range (LigoPosition 4 4) (LigoPosition 4 25) file
              , stackWithS2
              )
            )

          , ( InterpretRunning . EventExpressionEvaluated opsAndStorageType . Just $
                SomeLorentzValue ([] :: [T.Operation], 42 :: Integer)
            , one
              ( Range (LigoPosition 4 4) (LigoPosition 4 29) file
              , stackWithS2
              )
            )

           , ( InterpretTerminatedOk $ ContractFinalStack (contractOut :& RNil)
            , one
              ( Range (LigoPosition 4 4) (LigoPosition 4 29) file
              , lastStack
              )
            )

          ]
  , testCaseSteps "Check specific entrypoint" \_step -> do
      let file = contractsDir </> "not-main-entry-point.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Just "not_main"
            , crdParam = ()
            , crdStorage = 42 :: Integer
            }

      testWithSnapshots runData do
        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretRunning EventFacedStatement
            , isStackFrames = StackFrame
                { sfName = "not_main"
                , sfLoc = Range (LigoPosition 3 3) (LigoPosition 3 18) _
                } :| _
            } -> pass
          sp -> unexpectedSnapshot sp


  , testCaseSteps "check int type in simple-ops" \_step -> do
        let file = contractsDir </> "simple-ops.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 42 :: Integer
              }

        testWithSnapshots runData do
          checkSnapshot \case
            InterpretSnapshot
              { isStatus = InterpretRunning EventFacedStatement
              , isStackFrames = StackFrame
                  { sfLoc = Range (LigoPosition 3 3) (LigoPosition 3 18) _
                  , sfStack =
                    [ StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ
                            }
                        }
                    ]
                  } :| _
              } | typ == intType -> pass
            sp -> unexpectedSnapshot sp

  , testCaseSteps "pattern-match on option" \_step -> do
      let file = contractsDir </> "match-on-some.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = Just (5 :: Integer)
            }

      testWithSnapshots runData do
        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretRunning EventFacedStatement
            , isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 3 3) (LigoPosition 5 16) _
                } :| _
            } -> pass
          sp -> unexpectedSnapshot sp

  , testCaseSteps "check shadowing" \step -> do
      let file = contractsDir </> "shadowing.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 4 :: Integer
            }

      testWithSnapshots runData do
        let checkStackItem :: Name 'Concise -> SomeValue -> StackItem 'Concise -> Bool
            checkStackItem expectedVar expectedVal = \case
              StackItem
                { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                    { leseDeclaration = Just (LigoVariable actualVar)
                    }
                , siValue = actualVal
                } -> actualVal == expectedVal && expectedVar == actualVar
              _ -> False

        liftIO $ step [int||Go to second "s1"|]
        moveTill Forward $
          goesAfter (SrcLoc 15 0)
        checkSnapshot \snap -> do
          let stackItems = snap ^?! isStackFramesL . ix 0 . sfStackL

          -- check that current snapshot has "s1" variable and it's type is @VInt@
          unless (any (checkStackItem "s1" $ T.SomeConstrainedValue (T.VInt 8)) stackItems) do
            unexpectedSnapshot snap

        liftIO $ step [int||Go to first "s2"|]
        moveTill Forward $
          goesAfter (SrcLoc 16 0)
        checkSnapshot \snap -> do
          let stackItems = snap ^?! isStackFramesL . ix 0 . sfStackL

          -- we should be confident that we have only one "s1" variable in snapshot
          let s1Count = stackItems
                & filter \case
                    StackItem
                      { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                          { leseDeclaration = Just (LigoVariable "s1")
                          }
                      } -> True
                    _ -> False
                & length

          -- check that current snapshot has "s1" variable and it's type is @VOption VInt@
          unless (any (checkStackItem "s1" $ T.SomeConstrainedValue (T.VOption (Just (T.VInt 16)))) stackItems) do
            unexpectedSnapshot snap

          when (s1Count /= 1) do
            assertFailure [int||Expected 1 "s1" variable, found #{s1Count} "s1" variables|]

        liftIO $ step [int||Check shadowing in switch|]
        moveTill Forward $
          goesAfter (SrcLoc 20 0)
        -- TODO [LIGO-552] We somehow appear at weird place
        -- Breakpoint was pointing to body of `switch`, but we stopped at the switch itself
        _ <- move Forward
        checkSnapshot \snap -> do
          let stackItems = snap ^?! isStackFramesL . ix 0 . sfStackL

          -- check that current snapshot has "s" variable and it's value not 4
          unless (any (checkStackItem "s" $ T.SomeConstrainedValue (T.VInt 96)) stackItems) do
            unexpectedSnapshot snap

  , testCaseSteps "multiple contracts" \step -> do
      let modulePath = contractsDir </> "module_contracts"
      let file = modulePath </> "importer.mligo"
      let nestedFile = modulePath </> "imported.mligo"
      let nestedFile2 = modulePath </> "imported2.jsligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 10 :: Integer
            }

      testWithSnapshots runData do
        -- Predicate for @moveTill@ which stops on a snapshot with specified file name in loc.
        let stopAtFile
              :: (MonadState (DebuggerState (InterpretSnapshot u)) m)
              => FilePath
              -> FrozenPredicate (DebuggerState (InterpretSnapshot u)) m ()
            stopAtFile filePath = FrozenPredicate do
              snap <- curSnapshot
              Just loc <- pure $ snap ^? isStackFramesL . ix 0 . sfLocL
              guard $ _rFile loc == filePath

        liftIO $ step "Go to nested contract"
        _ <- moveTill Forward $ stopAtFile nestedFile
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 15 6) (LigoPosition 15 11) file'
                } :| _
            } | file' == nestedFile -> pass
          sp -> unexpectedSnapshot sp

        liftIO $ step "Make sure that we went back"
        _ <- moveTill Forward $ stopAtFile file
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 7 14) (LigoPosition 7 27) file'
                } :| _
            } | file' == file -> pass
          sp -> unexpectedSnapshot sp

        liftIO $ step "Check that we can go to more nested contract (and in another dialect)"
        _ <- moveTill Forward $ stopAtFile nestedFile2
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 2 3) (LigoPosition 2 20) file'
                } :| _
            } | file' == nestedFile2 -> pass
          sp -> unexpectedSnapshot sp

        liftIO $ step "Make sure that we went back to \"imported.mligo\""
        _ <- moveTill Forward $ stopAtFile nestedFile
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 19 69) (LigoPosition 19 89) file'
                } :| _
            } | file' == nestedFile -> pass
          sp -> unexpectedSnapshot sp

        liftIO $ step "Make sure that we went back to \"importer.mligo\""
        _ <- moveTill Forward $ stopAtFile file
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 11 56) (LigoPosition 11 81) file'
                } :| _
            } | file' == file -> pass
          sp -> unexpectedSnapshot sp

  , testCaseSteps "Pair values are present in variables pane" \step -> do
      let file = contractsDir </> "not-inlined-fst.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        void $ moveTill Forward $
          goesBetween (SrcLoc 6 0) (SrcLoc 7 0)

        liftIO $ step [int||Check that snapshot contains "pair1" and "pair2" variables|]
        checkSnapshot \snap -> do
          let vars = snap
                & isStackFrames
                & head
                & getVariableNamesFromStackFrame

          vars @~=? ["fst", "s", "pair1", "pair2"]

  , minor $ testCaseSteps "functions and variables are not inlined" \step -> do
      let file = contractsDir </> "funcs-and-vars-no-inline.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 4 :: Integer
            }

      testWithSnapshots runData do
        N.switchBreakpoint (MSFile file) (SrcLoc 9 0)

        let checkLinePosition pos = do
              frozen getExecutedPosition >>= \case
                Just (SourceLocation _ (SrcLoc actualPos _) _)
                  | actualPos == pos -> pass
                loc -> liftIO $ assertFailure [int||Expected stopping at line #{pos + 1}, got #{loc}|]

        let stepNextLine :: ReaderT TestCtx (HistoryReplayM (InterpretSnapshot 'Unique) IO) ()
            stepNextLine = void $ moveTill Forward $ FrozenPredicate do
              curSnapshot >>= \case
                InterpretSnapshot
                  { isStatus = InterpretRunning EventFacedStatement
                  } -> pass
                _ -> empty

        let goAndCheckLinePosition pos = do
              stepNextLine
              checkLinePosition pos

        liftIO $ step "check \"func\" function call stepping"
        checkLinePosition 7

        liftIO $ step "check stepping inside \"func\""
        goAndCheckLinePosition 1
        goAndCheckLinePosition 2
        goAndCheckLinePosition 3

        liftIO $ step "check stopping at constant assignment"
        goAndCheckLinePosition 8

        liftIO $ step "check that \"s2\" is not inlined"
        goToNextBreakpoint Forward
        checkSnapshot \snap -> do
          let stackItems = snap ^?! isStackFramesL . ix 0 . sfStackL
          let s2ItemMb = stackItems
                & find \case
                    StackItem
                      { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                          { leseDeclaration = Just (LigoVariable "s2")
                          }
                      } -> True
                    _ -> False

          case s2ItemMb of
            Nothing -> assertFailure [int||Can't find "s2" variable in snapshot #{snap}|]
            _ -> pass

  , testCaseSteps "built-ins work correctly" \step -> do
      let file = contractsDir </> "built-ins.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 42 :: Integer
            }

      testWithSnapshots runData do
        N.switchBreakpoint (MSFile file) (SrcLoc 5 0)

        liftIO $ step "Check that \"fold\" build-in works correctly"
        moveTill Forward isAtBreakpoint
        checkSnapshot \snap -> do
          let stackItems = snap ^?! isStackFramesL . ix 0 . sfStackL
          let sumElemsItemMb = stackItems
                & find \case
                    StackItem
                      { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                          { leseDeclaration = Just (LigoVariable "sum_elems")
                          }
                      , siValue = T.SomeConstrainedValue (T.VInt 6)
                      } -> True
                    _ -> False

          case sumElemsItemMb of
            Nothing ->
              assertFailure
                [int||
                  Can't find "sum_elems" variable \
                  with value 6 in snapshot #{snap}
                |]
            _ -> pass

    -- TODO: enable this test when type unification works properly in @ligo@
    -- (this problem appeared in 0.58.0)
  , minor $ testCaseSteps "monomorphed functions shows pretty" \step -> do
      let file = contractsDir </> "poly.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 42 :: Integer
            }

      testWithSnapshots runData do
        N.switchBreakpoint (MSFile file) (SrcLoc 12 0)

        N.continueUntilBreakpoint N.NextBreak
        liftIO $ step "Check function namings"
        checkSnapshot \snap -> do
          let stackItems = snap ^?! isStackFramesL . ix 0 . sfStackL

          let variables = stackItems
                <&> do \StackItem{..} -> case siLigoDesc of
                        LigoHiddenStackEntry -> ""
                        LigoStackEntry LigoExposedStackEntry{..} ->
                          maybe "" (pretty @_ @Text) leseDeclaration

          unless
            ( and
              $ flip elem variables
              <$> [ "foo"
                  , "foo"
                  , "TestId.One.id"
                  , "List.fold_left"
                  , "poly_troll42_"
                  , "s"
                  ]
            ) do
            assertFailure [int||This snapshot doesn't contain pretty monomorphed variables: #{snap}|]

  , testCaseSteps "Function assignments are skipped" \_ -> do
      let file = contractsDir </> "functions-assignments.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 42 :: Integer
            }

      testWithSnapshots runData do
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 7 3) (LigoPosition 7 45) file'
                } :| _
            } | file' == file -> pass
          snap -> unexpectedSnapshot snap

  , testCaseSteps "Check statements" \step -> do
      let checkLocations runData locs = testWithSnapshots runData do
            let snapPred InterpretSnapshot{..} = case isStatus of
                  InterpretRunning EventFacedStatement -> True
                  _ -> False

            (_, filter snapPred . tsAllVisited -> snaps) <- listen $ moveTill Forward false

            ( snaps
                <&> do \InterpretSnapshot{..} ->
                        head isStackFrames
                          & \StackFrame{..} -> sfLoc
              ) @?= locs

      let file = contractsDir </> "statement-visiting.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 42 :: Integer
            }

      step [int||Checking locations for #{file}|]
      checkLocations
        runData
        [ Range (LigoPosition 10 3) (LigoPosition 10 16) file
        , Range (LigoPosition 11 3) (LigoPosition 11 20) file
        , Range (LigoPosition 4 6) (LigoPosition 4 16) file
        , Range (LigoPosition 6 8) (LigoPosition 6 10) file
        , Range (LigoPosition 12 3) (LigoPosition 12 24) file
        , Range (LigoPosition 1 36) (LigoPosition 1 41) file
        , Range (LigoPosition 12 28) (LigoPosition 12 44) file
        , Range (LigoPosition 13 3) (LigoPosition 13 37) file
        ]

      let file2 = contractsDir </> "statement-visiting.jsligo"
      let runData2 = ContractRunData
            { crdProgram = file2
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 3 :: Integer
            }

      step [int||Checking locations for #{file2}|]
      checkLocations
        runData2 $
          [ Range (LigoPosition 3 3) (LigoPosition 3 20) file2
          , Range (LigoPosition 5 3) (LigoPosition 5 18) file2
          ] ++
          concat
            ( replicate 4
              [ Range (LigoPosition 7 5) (LigoPosition 7 28) file2
              , Range (LigoPosition 8 5) (LigoPosition 8 22) file2
              , Range (LigoPosition 9 5) (LigoPosition 9 14) file2
              ]
            )
          ++ [Range (LigoPosition 12 3) (LigoPosition 12 46) file2]

  , testCaseSteps "Execution history is lazy" \step -> do
      let file = contractsDir </> "infinite_contract.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 42 :: Integer
            }

      (allLocs, his, ep, ligoTypesVec) <- mkSnapshotsFor runData

      let his' = InterpretHistory $
            fromList (take 1000 (toList $ unInterpretHistory his)) <>
            error "Went too far in execution history"

      let tenSeconds = 10 * 1e6

      step "Evaluating prefix of interpret history"
      res <-
        timeout tenSeconds do
          withSnapshots (allLocs, his', ep, ligoTypesVec) do
            replicateM_ 3 $ move Forward
            frozen curSnapshot

      assertBool "Expected history to be evaluated" (isJust res)

  , testCaseSteps "Check snapshot collection logging" \step -> do
      anyWritten <- newIORef False

      let logger :: Text -> IO ()
          logger = const $ writeIORef anyWritten True

      let file = contractsDir </> "noop.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      step [int||Going through all execution history|]
      testWithSnapshotsImpl logger Nothing runData do
        void $ moveTill Forward false

      unlessM (readIORef anyWritten) do
        assertFailure [int||No logs we're dumped during snapshot collection|]

  , minor $ testGroup "comparisons does not produce duplicated snapshots"
    -- ligo used to produce the same location twice, e.g. for both COMPARE and GT

    [ testCaseSteps "comparison in condition" \step -> do
        let file = contractsDir </> "if.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 4 :: Integer
              }

        testWithSnapshots runData do
          -- Jump to GT
          liftIO $ step "jumping to GT"
          moveRes <- moveTill Forward $ FrozenPredicate do
            status <- isStatus <$> curSnapshot
            guard $ preview statusExpressionEvaluatedP status
                  == Just (SomeLorentzValue False)

          liftIO $ assertBool
            "Didn't find the necessary snapshot"
            (moveRes == MovedSuccessfully ())

          srcLocAtGt <- sfLoc . head . isStackFrames <$> frozen curSnapshot

          -- Go back. If for COMPARE we also create a snapshot, our test should fail
          liftIO $ step "jumping back"
          void . moveTill Backward $ FrozenPredicate do
            status <- isStatus <$> curSnapshot
            guard $ has statusExpressionEvaluatedP status


          status <- isStatus <$> frozen curSnapshot
          when
            ( preview statusExpressionEvaluatedP status
              == Just (SomeLorentzValue (-1 :: Integer))
            ) do
              srcLocAtCompare <- sfLoc . head . isStackFrames <$> frozen curSnapshot
              if srcLocAtCompare == srcLocAtGt
                then liftIO $ assertFailure
                  "Created a snapshot at COMPARE that duplicates the snapshot at GT"
                else liftIO $ assertFailure
                  "COMPARE seems to have a different source location than GT, \
                  \please check that this test is still sensible"

    , testCaseSteps "standalone comparison & comparison against 0" \step -> do
        -- Checking this just in case, maybe LIGO behaves differently for
        -- extracted @let cond = a > b@.
        -- Comparisons against 0 do not require `COMPARE`, however
        -- at the moment of writing LIGO still uses it.
        let file = contractsDir </> "bool.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = 5 :: Integer
              , crdStorage = False
              }

        testWithSnapshots runData do
          liftIO $ step "Skip tuple statement"
          _ <- move Forward

          liftIO $ step "Skipping push"
          _ <- moveTill Forward $ goesAfter (SrcLoc 1 0)

          liftIO $ step "Skipping \"ExpressionPreviewEvent\""
          _ <- moveTill Forward $ goesAfter (SrcLoc 1 0)

          liftIO $ step "Checking comparison result"
          do
            status <- isStatus <$> frozen curSnapshot
            preview statusExpressionEvaluatedP status
              @?= Just (SomeLorentzValue True)

    , testCaseSteps "integer division" \step -> do
        -- Division in LIGO also leaves the same locations for
        -- the intermediate computation, 3 times this time.
        let file = contractsDir </> "if.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 6 :: Integer
              }

        testWithSnapshots runData do
          -- Jump to division result
          liftIO $ step "jumping to GT"
          moveRes <- moveTill Forward $ FrozenPredicate do
            status <- isStatus <$> curSnapshot
            guard $ preview statusExpressionEvaluatedP status
                  == Just (SomeLorentzValue (3 :: Integer))

          liftIO $ assertBool
            "Didn't find the necessary snapshot"
            (moveRes == MovedSuccessfully ())
          srcLocAtDiv <- sfLoc . head . isStackFrames <$> frozen curSnapshot

          liftIO $ step "jumping to the previously computed value"
          void . moveTill Backward $ FrozenPredicate do
            hoistMaybe . preview statusExpressionEvaluatedP . isStatus =<< curSnapshot

          srcLocAtBack <- sfLoc . head . isStackFrames <$> frozen curSnapshot
          liftIO $ assertBool
            "Duplicated source range for different snapshots"
            (srcLocAtDiv /= srcLocAtBack)
    ]

  , testCaseSteps "Variables in pattern match" \step -> do
      let file = contractsDir </> "variables-in-pattern-match.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        void $ moveTill Forward $
          goesAfter (SrcLoc 5 0)

        -- Skip statement
        void $ move Forward

        liftIO $ step [int||Extract variables|]
        checkSnapshot \snap -> do
          let vars = snap
                & isStackFrames
                & head
                & getVariableNamesFromStackFrame

          vars @~=? ["a", "b", "pairFunc", "s"]

  , testCaseSteps "Check stack frames on function entering / exiting" \step -> do
      let file = contractsDir </> "recursive.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $ isAtLine 1
        liftIO $ step [int||Check stack frame names on entering "recursive"|]
        checkSnapshot ((@=?) ["recursive", "main"] . getStackFrameNames)

        moveTill Forward $ goesAfter (SrcLoc 4 0)
        liftIO $ step [int||Check that we have only "main" stack frame after leaving function|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

  , testCaseSteps "Calling local function" \step -> do
      let file = contractsDir </> "local-function.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        move Forward

        liftIO $ step [int||Check that we have only one "main" stack frame|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

        moveTill Forward $ isAtLine 3
        liftIO $ step [int||Check that we have "f" stack frame on entering local function|]
        checkSnapshot ((@=?) ["f", "main"] . getStackFrameNames)

  , testCaseSteps "Function calling other function" \step -> do
      let file = contractsDir </> "function-calling-function.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $ isAtLine 2
        liftIO $ step [int||Calling top level function "complex"|]
        checkSnapshot ((@=?) ["complex", "main"] . getStackFrameNames)

        moveTill Forward $ isAtLine 0
        liftIO $ step [int||Calling function "add" from "complex"|]
        checkSnapshot ((@=?) ["add", "complex", "main"] . getStackFrameNames)

  , testCaseSteps "Lambda parameter" \step -> do
      let file = contractsDir </> "lambda-parameter.mligo"

      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = L.mkLambda $ L.unpair @_ @_ @'[] L.# L.add @Integer @Integer
            , crdStorage = 0 :: Integer
            }

      let runDataFailing = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = L.mkLambda $
                L.drop L.# L.push [L.mt|Stick bugged lol|] L.# L.failWith
                  :: L.Lambda (Integer, Integer) Integer
            , crdStorage = 0 :: Integer
            }

      let stackFramesCheck :: ([[Text]] -> Bool) -> ReaderT TestCtx (HistoryReplayM (InterpretSnapshot u) IO) ()
          stackFramesCheck namesCheck = do
            (_, fmap getStackFrameNames . tsAfterInstrs -> stackFrameNames) <-
              listen $ moveTill Forward false
            liftIO $
              assertBool
                [int||Expected to have only one stack frame \
                  with name "main" in each snapshot, got #{stackFrameNames}|]
                (namesCheck stackFrameNames)

      step [int||Check that all snapshots have exactly one stack frame with name "main"|]
      testWithSnapshots runData $ stackFramesCheck (all (== ["main"]))

      step [int||Check that all snapshots with failing lambda|]
      testWithSnapshots runDataFailing $ stackFramesCheck \names ->
        let
          initElems = Unsafe.init names
          lastElem = Unsafe.last names
        in all (== ["main"]) initElems && lastElem == ["p", "p", "main"]

  , testCaseSteps "Check variables in stack frames" \step -> do
      let dir = contractsDir </> "module_contracts"
      let file = dir </> "importer.mligo"
      let nestedFile = dir </> "imported.mligo"
      let nestedFile2 = dir </> "imported2.jsligo"

      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward do
          goesBetween (SrcLoc 8 0) (SrcLoc 12 0)
          matchesSrcType (MSFile nestedFile)

        -- Skip "lst" and "sum(...)"" statements
        replicateM_ 2 do
          void $ move Forward

        liftIO $ step [int||Check variables for "sum" snapshot|]
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = stackFrame@StackFrame
                { sfName = "sum"
                , sfLoc = Range _ _ file'
                } :| _
            } | file' == nestedFile -> getVariableNamesFromStackFrame stackFrame @~=? ["l", "x", "acc"]
          snap -> unexpectedSnapshot snap

        moveTill Forward $
          goesAfter (SrcLoc 5 0) && matchesSrcType (MSFile nestedFile2)
        liftIO $ step [int||Check variables for "strange" snapshot|]
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = stackFrame@StackFrame
                { sfName = "strange"
                , sfLoc = Range _ _ file'
                } :| _
            } | file' == nestedFile2 ->
                  getVariableNamesFromStackFrame stackFrame @~=? ["acc", "c", "b", "a", "i"]
          snap -> unexpectedSnapshot snap

  , testCaseSteps "Two \"main\" stack frames" \step -> do
      let file = contractsDir </> "two-functions-with-main-name.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $
          goesBetween (SrcLoc 3 0) (SrcLoc 5 0)
        liftIO $ step [int||Check that we have two stack frames with "main" name|]
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfName = "EURO.main"
                , sfLoc = loc1
                } :| StackFrame
                      { sfName = "main"
                      , sfLoc = loc2
                      } : _
            } | loc1 /= loc2 -> pass
          snap -> unexpectedSnapshot snap
        checkSnapshot ((@=?) ["EURO.main", "main"] . getStackFrameNames)

  , testCaseSteps "Stack frames in a contract with partially applied function" \step -> do
      let file = contractsDir </> "apply.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $ isAtLine 0

        liftIO $ step [int||Check stack frames after entering "add5"|]
        checkSnapshot ((@=?) ["add", "add5", "main"] . getStackFrameNames)

        moveTill Forward $ goesAfter (SrcLoc 7 0)
        liftIO $ step [int||Check stack frames after leaving "add5"|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

  , testCaseSteps "Partially applied function inside top level function" \step -> do
      let file = contractsDir </> "complex-apply.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        -- Go to function call first.
        moveTill Forward $ isAtLine 3
        moveTill Forward $ isAtLine 1

        liftIO $ step [int||Go into "add5"|]
        checkSnapshot ((@=?) ["add", "add5", "myFunc", "main"] . getStackFrameNames)

        moveTill Forward $ goesAfter (SrcLoc 6 0)

        liftIO $ step [int||Leave "myFunc"|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

  , testCaseSteps "2 times curried function" \step -> do
      let file = contractsDir </> "curry.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $ isAtLine 7

        liftIO $ step [int||Check stack frames for inner "sub"|]
        checkSnapshot ((@=?) ["sub", "f", "partApplied", "applyOp", "main"] . getStackFrameNames)

        moveTill Forward $ isAtLine 5

        liftIO $ step [int||Check stack frames for inner "add"|]
        checkSnapshot ((@=?) ["add", "f", "partApplied", "applyOp", "main"] . getStackFrameNames)

        moveTill Forward $
          goesAfter (SrcLoc 12 0)

        liftIO $ step [int||Leave "applyOp" functions|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

  , testCaseSteps "2 times curried function inside lambda" \step -> do
      let file = contractsDir </> "curry-inside-lambda.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        -- Go to function call first.
        moveTill Forward $ isAtLine 7
        moveTill Forward $ isAtLine 6

        liftIO $ step [int||Check "sub" stack frames inside "lambdaFun"|]
        checkSnapshot ((@=?) ["sub", "f", "apply", "lambdaFun", "main"] . getStackFrameNames)

        moveTill Forward $ isAtLine 5

        liftIO $ step [int||Check "add" stack frames inside "lambdaFun"|]
        checkSnapshot ((@=?) ["add", "f", "apply", "lambdaFun", "main"] . getStackFrameNames)

        moveTill Forward $
          goesAfter (SrcLoc 8 0)

        liftIO $ step [int||Leave "lambdaFun"|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

  , testCaseSteps "Multiple currying" \step -> do
      let file = contractsDir </> "advanced-curry.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $ isAtLine 7

        liftIO $ step [int||Check stack frames in inner "act"|]
        checkSnapshot ((@=?) ["act", "f", "applyOnce", "applyTwice", "applyThrice", "apply", "main"] . getStackFrameNames)

        moveTill Forward $
          goesAfter (SrcLoc 10 0)

        liftIO $ step [int||Check stack frames after leaving "act"|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

  , minor $ testCaseSteps "Skipping constant evaluation and not skipping statement" \step -> do
      let file = contractsDir </> "constant-assignment.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        liftIO $ step [int||Check stopping at statement|]
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 3 3) (LigoPosition 3 13) _
                } :| _
            , isStatus = InterpretRunning EventFacedStatement
            } -> pass
          snap -> unexpectedSnapshot snap

        move Forward

        liftIO $ step [int||Check that we skipped constant evaluation|]
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = loc
                } :| _
            } | loc /= Range (LigoPosition 3 12) (LigoPosition 3 13) file -- position of constant
            -> pass
          snap -> unexpectedSnapshot snap

  , minor $ testCaseSteps "Computations in list" \step -> do
      let file = contractsDir </> "computations-in-list.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = [] :: [Integer]
            }

      testWithSnapshots runData do
        -- Go to list
        void $ moveTill Forward $
          goesAfter (SrcLoc 5 0)

        moveTill Forward false

        liftIO $ step [int||Check that failed location is known|]
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 1 38) (LigoPosition 1 53) _
                , sfName = "failwith"
                } :|
                  StackFrame
                    { sfName = "unsafeCompute"
                    }
                  : _
            , isStatus = InterpretFailed _
            } -> pass
          snap -> unexpectedSnapshot snap

  , testGroup "Complex variable types"
    [ testCaseSteps "Record type" \step -> do
        let file = contractsDir </> "complex-storage.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = (0, 0, [mt|""|]) :: (Integer, Natural, MText)
              }

        testWithSnapshots runData do
          -- Skip arguments
          void $ move Forward

          let expectedType = LigoTypeResolved $ mkRecordType (combLayout ["a", "b", "c"])
                [ ("a", intType')
                , ("b", mkSimpleConstantType "nat")
                , ("c", mkSimpleConstantType "string")
                ]

          liftIO $ step "Check record type"
          checkSnapshot \case
            InterpretSnapshot
              { isStackFrames = StackFrame
                  { sfStack =
                      [ StackItem
                          { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                              { leseType = typ
                              }
                          }
                      ]
                  } :| _
              } | typ == expectedType -> pass
            snap -> unexpectedSnapshot snap

    , testCaseSteps "Sum type" \step -> do
        let file = contractsDir </> "sum-type.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = Left 42 :: Either Integer ()
              , crdStorage = 0 :: Integer
              }

        testWithSnapshots runData do
          let expectedType = LigoTypeResolved $ mkSumType (twoElemTreeLayout "Variant1" "Variant2")
                [ ("Variant1", intType')
                , ("Variant2", unitType')
                ]

          liftIO $ step "Check sum type"
          checkSnapshot \case
            InterpretSnapshot
              { isStackFrames = StackFrame
                  { sfStack = _ :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ
                            }
                        } : _
                  } :| _
              } | typ == expectedType -> pass
            snap -> unexpectedSnapshot snap

    , testCaseSteps "Function type" \step -> do
        let file = contractsDir </> "complex-function-type.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        testWithSnapshots runData do
          let expectedType = LigoTypeResolved
                $  intType'
                ~> mkOptionType (mkSimpleConstantType "nat")
                ~> mkPairType intType' (mkSimpleConstantType "string")
                ~> intType'

          liftIO $ step "Check function type"
          checkSnapshot \case
            InterpretSnapshot
              { isStackFrames = StackFrame
                  { sfStack = _ :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ
                            }
                        } : _
                  } :| _
              } | typ == expectedType -> pass
            snap -> unexpectedSnapshot snap

    , testCaseSteps "Polymorphic functions have types" \step -> do
        let file = contractsDir </> "polymorphic-function.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        testWithSnapshots runData do
          let expectedIntType = LigoTypeResolved (intType' ~> intType')

          let operationList = mkConstantType "List" [mkSimpleConstantType "Operation"]
          let expectedOperationListType = LigoTypeResolved (operationList ~> operationList)

          liftIO $ step "Check types for monomorphed functions"
          checkSnapshot \case
            InterpretSnapshot
              { isStackFrames = StackFrame
                  { sfStack = _ :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ1
                            }
                        } :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ2
                            }
                        }
                      : _
                  } :| _
              } | typ1 == expectedOperationListType
                , typ2 == expectedIntType -> pass
            snap -> unexpectedSnapshot snap

    , testCaseSteps "Nested structures have types" \step -> do
        let file = contractsDir </> "nested-structure-type.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        testWithSnapshots runData do
          moveTill Forward $
            isAtLine 16

          let expectedComplexType = LigoTypeResolved
                $ mkRecordType (combLayout ["simple_field", "complex_field"])
                    [ ("simple_field", mkSimpleConstantType "String")
                    , ("complex_field"
                      , mkRecordType (combLayout ["inner_field1", "inner_field2"])
                          [ ("inner_field1", intType')
                          , ("inner_field2", intType')
                          ]
                      )
                    ]

          liftIO $ step "Check type for nested structure"
          checkSnapshot \case
            InterpretSnapshot
              { isStackFrames = StackFrame
                  { sfStack = _ :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ
                            }
                        } : _
                  } :| _
              } | typ == expectedComplexType -> pass
            snap -> unexpectedSnapshot snap

    , testCaseSteps "Sum with nested record type" \step -> do
        let file = contractsDir </> "sum-with-record-type.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        testWithSnapshots runData do
          let expectedType = LigoTypeResolved
                $ mkSumType (twoElemTreeLayout "A" "B")
                    [ ( "A"
                      , mkRecordType (twoElemTreeLayout "a1" "a2")
                          [ ("a1", intType')
                          , ("a2", mkSimpleConstantType "String")
                          ]
                      )
                    , ( "B"
                      , mkRecordType (twoElemTreeLayout "b1" "b2")
                          [ ("b1", mkSimpleConstantType "String")
                          , ("b2", intType')
                          ]
                      )
                    ]

          void $ moveTill Forward $
            goesAfter (SrcLoc 7 0)

          liftIO $ step "Check type for sum type with inner record"
          checkSnapshot \case
            InterpretSnapshot
              { isStackFrames = StackFrame
                  { sfStack = _ :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ
                            }
                        } : _
                  } :| _
              } | typ == expectedType -> pass
            snap -> unexpectedSnapshot snap

    , testCaseSteps "Never type" \step -> do
        let file = contractsDir </> "never-type.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        testWithSnapshots runData do
          let expectedType = LigoTypeResolved (unitType' ~> mkSimpleConstantType "Never")

          liftIO $ step "Check \"never\" function type"
          checkSnapshot \case
            InterpretSnapshot
              { isStackFrames = StackFrame
                  { sfStack = _ :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ
                            }
                        } : _
                  } :| _
              } | typ == expectedType -> pass
            snap -> unexpectedSnapshot snap

    , testCaseSteps "Some types from stdlib" \step -> do
        let file = contractsDir </> "types-from-stdlib.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        testWithSnapshots runData do
          let bytesType = LigoTypeResolved (mkSimpleConstantType "Bytes")

          let setType = LigoTypeResolved
                $ mkConstantType "Set" [intType']

          let mapType = LigoTypeResolved
                $ mkConstantType "Map"
                    [ intType'
                    , mkSimpleConstantType "String"
                    ]

          void $ moveTill Forward
            $ isAtLine 8

          liftIO $ step "Check some types from stdlib"
          checkSnapshot \case
            InterpretSnapshot
              { isStackFrames = StackFrame
                  { sfStack = _ :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ1
                            }
                        } :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ2
                            }
                        } :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ3
                            }
                        } : _
                  } :| _
              } | typ1 == mapType
                , typ2 == setType
                , typ3 == bytesType -> pass
            snap -> unexpectedSnapshot snap

    , testCaseSteps "Polymorphic values have types" \step -> do
        let file = contractsDir </> "polymorphic-types.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        testWithSnapshots runData do
          let intOptionType = LigoTypeResolved (mkOptionType intType')
          let stringOptionType = LigoTypeResolved (mkOptionType $ mkSimpleConstantType "String")

          void $ moveTill Forward
            $ goesAfter (SrcLoc 4 0)

          liftIO $ step "Check monomorphed \"option\" type"
          checkSnapshot \case
            InterpretSnapshot
              { isStackFrames = StackFrame
                  { sfStack =
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ1
                            }
                        } :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ2
                            }
                        } : _
                  } :| _
              } | typ1 == stringOptionType
                , typ2 == intOptionType -> pass
            snap -> unexpectedSnapshot snap

    , testCaseSteps "Shadowed types" \step -> do
        let file = contractsDir </> "shadowed-types.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        testWithSnapshots runData do
          void $ move Forward

          -- Note that at this moment we're showing raw types.
          -- It means that "type string = int" will have
          -- "int" raw type.
          liftIO $ step "Check shadowed type"
          checkSnapshot \case
            InterpretSnapshot
              { isStackFrames = StackFrame
                  { sfStack =
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ
                            }
                        } : _
                  } :| _
              } | typ == intType -> pass
            snap -> unexpectedSnapshot snap

    , testCaseSteps "Types from Tezos" \step -> do
        let file = contractsDir </> "tezos-types.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        testWithSnapshots runData do
          let tezType = LigoTypeResolved (mkSimpleConstantType "Tez")
          let timestampType = LigoTypeResolved (mkSimpleConstantType "Timestamp")
          let addressType = LigoTypeResolved (mkSimpleConstantType "Address")
          let saplingFooType = LigoTypeResolved
                $  unitType'
                ~> mkConstantType "Sapling_state"
                    [ mkTypeExpression $ LTCSingleton (LTLVInt 42)
                    ]

          void $ moveTill Forward
            $ isAtLine 8

          liftIO $ step "Check types from Tezos module"
          checkSnapshot \case
            InterpretSnapshot
              { isStackFrames = StackFrame
                  { sfStack =
                      _ :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ1
                            }
                        } :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ2
                            }
                        } :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ3
                            }
                        } :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ4
                            }
                        } : _
                  } :| _
              } | typ1 == saplingFooType
                , typ2 == addressType
                , typ3 == timestampType
                , typ4 == tezType -> pass
            snap -> unexpectedSnapshot snap

    , testCaseSteps "Layout comb types" \step -> do
        let file = contractsDir </> "layout-comb-types.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        testWithSnapshots runData do
          let combedType = LigoTypeResolved
                $ mkRecordType (combLayout ["a", "b", "c"])
                    [ ("a", intType')
                    , ("b", mkSimpleConstantType "Nat")
                    , ("c", mkSimpleConstantType "String")
                    ]

          void $ moveTill Forward
            $ isAtLine 10

          liftIO $ step "Check combed type"
          checkSnapshot \case
            InterpretSnapshot
              { isStackFrames = StackFrame
                  { sfStack = _ :
                      StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = typ
                            }
                        } : _
                  } :| _
              } | typ == combedType -> pass
            snap -> unexpectedSnapshot snap
    ]

  , testCaseSteps "Builtin functions have locations" \step -> do
      let file = contractsDir </> "builtins-locations.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 10 :: Integer
            }

      testWithSnapshots runData do
        -- Skip first statement
        void $ move Forward

        liftIO $ step "Check location for \"is_nat\""
        checkSnapshot \case
          InterpretSnapshot
              { isStatus = InterpretRunning EventExpressionPreview{}
              , isStackFrames = StackFrame
                  { sfLoc = Range (LigoPosition 3 11) (LigoPosition 3 19) _
                  } :| _
              } -> pass
          snap -> unexpectedSnapshot snap

        moveTill Forward $
          goesAfter (SrcLoc 3 0)

        -- Skip statement
        void $ move Forward

        liftIO $ step "Check location for \"assert\""
        checkSnapshot \case
          InterpretSnapshot
              { isStatus = InterpretRunning EventExpressionPreview{}
              , isStackFrames = StackFrame
                  { sfLoc = Range (LigoPosition 4 12) (LigoPosition 4 23) _
                  } :| _
              } -> pass
          snap -> unexpectedSnapshot snap

        moveTill Forward $
          goesAfter (SrcLoc 9 0)

        -- Skip statement
        void $ move Forward

        liftIO $ step "Check location for \"List.fold\""
        checkSnapshot \case
          InterpretSnapshot
              { isStatus = InterpretRunning EventExpressionPreview{}
              , isStackFrames = StackFrame
                  { sfLoc = Range (LigoPosition 10 13) (LigoPosition 10 64) _
                  } :| _
              } -> pass
          snap -> unexpectedSnapshot snap

        -- Skip list
        replicateM_ 3 do
          move Forward

        replicateM_ 3 do
          let loc = Range (LigoPosition 10 42) (LigoPosition 10 51) file

          liftIO $ step "Aux function body in \"fold\" is statement"
          checkSnapshot \case
            InterpretSnapshot
              { isStatus = InterpretRunning EventFacedStatement
              , isStackFrames = StackFrame
                  { sfLoc = loc'
                  } :| _
              } | loc == loc' -> pass
            snap -> unexpectedSnapshot snap

          void $ move Forward

          checkSnapshot \case
            InterpretSnapshot
              { isStatus = InterpretRunning EventExpressionEvaluated{}
              , isStackFrames = StackFrame
                  { sfLoc = loc'
                  } :| _
              } | loc' == loc -> pass
            snap -> unexpectedSnapshot snap

          void $ move Forward

  , testCaseSteps "Unit value is skipped" \step -> do
      let file = contractsDir </> "contract-with-unit.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        liftIO $ step [int||Check that we have a statement with unit value|]
        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretRunning EventFacedStatement
            , isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 3 3) (LigoPosition 3 18) file'
                } :| _
            } | file' == file -> pass
          snap -> unexpectedSnapshot snap

        void $ move Forward

        liftIO $ step [int||Check that we skipped unit evaluation|]
        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretRunning EventExpressionPreview{}
            , isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 3 16) (LigoPosition 3 18) file'
                } :| _
            } | file' == file -> assertFailure "Unit is evaluated"
          _ -> pass

  , testGroup "Statement locations"
    let
      getStatementLocations :: ContractRunData -> IO [Range]
      getStatementLocations runData = do
        let getLocations :: NonEmpty (InterpretSnapshot u) -> [Range]
            getLocations =
                  NE.filter
                    do \InterpretSnapshot{..} -> isStatus == InterpretRunning EventFacedStatement
              >>> toListOf (each . isStackFramesL . ix 0 . sfLocL)

        (_, getLocations . coerce -> locs, _, _) <- mkSnapshotsFor runData
        pure locs
    in
    [ testCase "Last statement in function" do
        let file = contractsDir </> "last-statement-in-function.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        (getStatementLocations runData) @@?
          (  elem (Range (LigoPosition 3 3) (LigoPosition 3 29) file)
          )

    , testCase "Last statement in let-in" do
        let file = contractsDir </> "last-statement-in-let-in.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        (getStatementLocations runData) @@?
          (  elem (Range (LigoPosition 5 3) (LigoPosition 5 33) file)
          && (not ... elem) (Range (LigoPosition 4 3) (LigoPosition 5 33) file)
          )

    , testCase "Statement in match branch" do
        let file = contractsDir </> "statement-in-match-branch.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        (getStatementLocations runData) @@?
          (  elem (Range (LigoPosition 6 17) (LigoPosition 6 22) file)
          )

    , testCase "Statements in if" do
        let file = contractsDir </> "statements-in-if.mligo"
        let runData1 = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 0 :: Integer
              }

        (getStatementLocations runData1) @@?
          (  elem (Range (LigoPosition 3 14) (LigoPosition 3 20) file)
          && elem (Range (LigoPosition 3 38) (LigoPosition 3 44) file)
          )

        let runData2 = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = ()
              , crdStorage = 15 :: Integer
              }

        (getStatementLocations runData2) @@?
          (  elem (Range (LigoPosition 3 14) (LigoPosition 3 20) file)
          && elem (Range (LigoPosition 3 26) (LigoPosition 3 32) file)
          )

    , testCase "Statements in case branch" do
        let file = contractsDir </> "statements-in-case-branch.jsligo"
        let runData1 = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = [L.mt|Variant1|]
              , crdStorage = 0 :: Integer
              }

        (getStatementLocations runData1) @@?
          (  elem (Range (LigoPosition 6 7) (LigoPosition 6 18) file)
          )

        let runData2 = ContractRunData
              { crdProgram = file
              , crdModuleName = Nothing
              , crdParam = [L.mt|Variant2|]
              , crdStorage = 0 :: Integer
              }

        (getStatementLocations runData2) @@?
          (  elem (Range (LigoPosition 9 7) (LigoPosition 9 18) file)
          )
    ]

  , testCaseSteps "EventExpressionPreview is skipped after EventFacedStatement" \step -> do
      let runData = ContractRunData
            { crdProgram = contractsDir </> "evaluated-event-after-statement.mligo"
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        void $ moveTill Forward (isAtLine 4)
        void $ moveTill Forward (isAtLine 0)

        liftIO $ step "Check \"EventFacedStatement\" event"
        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretRunning EventFacedStatement
            , isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 1 36) (LigoPosition 1 41) _
                } :| _
            } -> pass
          snap -> unexpectedSnapshot snap

        void $ move Forward

        liftIO $ step
          [int||Check that EventExpressionPreview is skipped and \
          we go to EventExpressionEvaluated immediately|]

        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretRunning EventExpressionEvaluated{}
            , isStackFrames = StackFrame
                { sfLoc = Range (LigoPosition 1 36) (LigoPosition 1 41) _
                } :| _
            } -> pass
          snap -> unexpectedSnapshot snap

  , testCaseSteps "Decompile values in snapshot" \step -> do
      let runData = ContractRunData
            { crdProgram = contractsDir </> "complex_value_decompilation.mligo"
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        ligoTypesVec <- asks tcLigoTypesVec

        liftIO $ step "Go to some line"
        void $ moveTill Forward
          $ goesAfter (SrcLoc 20 0)

        snap <- frozen curSnapshot

        liftIO $ step "Extract values and types"
        let convertInfos = extractConvertInfos ligoTypesVec snap

        let largeValue = LVConstructor
              ( "Go"
              , LVRecord $ HM.fromList
                  [ (LLabel "ch", LVCt $ LCChainId "NetXH12Aer3be93")
                  , (LLabel "state", LVConstructor ("A", LVCt $ LCTimestamp "100"))
                  , (LLabel "s", LVCt $ LCString "large")
                  ]
              )

        let expected =
              [ largeValue
              , largeValue
              , LVCt $ LCInt "0"
              , LVCt $ LCString "<fun>"
              ]

        liftIO $ step "Decompile"
        actual <-
          liftIO (convertMichelsonValuesToLigo dummyLoggingFunction convertInfos)
            <&> mapMaybe (\case{LigoValue _ v -> Just v ; _ -> Nothing})

        actual @?= expected

  , testCaseSteps "Check max steps" \step -> do
      let runData = ContractRunData
            { crdProgram = contractsDir </> "simple-ops.mligo"
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshotsImpl dummyLoggingFunction (Just 5) runData do
        void $ moveTill Forward false

        liftIO $ step "Check infinite loop exception"
        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretFailed
                ( MichelsonFailureWithStack
                    (MichelsonExt DebuggerInfiniteLoop)
                    (ErrorSrcPos (SrcPos (Pos 3) (Pos 11)))
                )
            } -> pass
          snap -> unexpectedSnapshot snap

  , testCaseSteps "Check new storage in LIGO format" \step -> do
      let runData = ContractRunData
            { crdProgram = contractsDir </> "complex-storage.mligo"
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = (0 :: Integer, 0 :: Natural, [mt|!|])
            }

      testWithSnapshots runData do
        ep <- asks tcEntrypointType

        liftIO $ step "Go to last snapshot"
        void $ moveTill Forward false

        snap <- frozen curSnapshot

        let (_, storageType, _) = getParameterStorageAndOpsTypes ep

        liftIO $ step "Extract new storage convert info"
        let someValues = snap ^.. isStackFramesL . ix 0 . sfStackL . each . siValueL

        let storageConvertInfo = PreLigoConvertInfo (someValues Unsafe.!! 1) storageType

        let expected =
              [ LVRecord $ HM.fromList
                  [ (LLabel "a", LVCt $ LCInt "0")
                  , (LLabel "b", LVCt $ LCNat "0")
                  , (LLabel "c", LVCt $ LCString "!")
                  ]
              ]

        liftIO $ step "Decompile"
        actual <-
          liftIO (convertMichelsonValuesToLigo dummyLoggingFunction [storageConvertInfo])
            <&> mapMaybe (\case{LigoValue _ v -> Just v ; _ -> Nothing})

        actual @?= expected

  , testCaseSteps "Check evaluated record in LIGO format" \step -> do
      let runData = ContractRunData
            { crdProgram = contractsDir </> "record-evaluated.mligo"
            , crdModuleName = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        void $ moveTill Forward $
          isAtLine 2

        liftIO $ step "Skip preview, go to evaluated"
        void $ move Forward

        checkSnapshot \case
          InterpretSnapshot
            { isStatus = InterpretRunning (EventExpressionEvaluated typ (Just value))
            } -> do
              let expected =
                    [ LVRecord $ HM.fromList
                        [ (LLabel "a", LVCt $ LCInt "42")
                        , (LLabel "b", LVCt $ LCNat "0")
                        , (LLabel "c", LVCt $ LCString "!")
                        ]
                    ]

              liftIO $ step "Decompile evaluated value"
              actual <-
                liftIO (convertMichelsonValuesToLigo dummyLoggingFunction [PreLigoConvertInfo value typ])
                  <&> mapMaybe (\case{LigoValue _ v -> Just v ; _ -> Nothing})

              actual @?= expected
          snap -> unexpectedSnapshot snap
  ]

-- | Special options for checking contract.
data CheckingOptions = CheckingOptions
  { coModuleName           :: Maybe ModuleName
  , coCheckSourceLocations :: Bool
  , coCheckEntrypointsList :: Bool
  } deriving stock (Show)

coModuleNameL :: Lens' CheckingOptions (Maybe ModuleName)
coModuleNameL = lens
  do \CheckingOptions{..} -> coModuleName
  do \(CheckingOptions _ locs eps) modName -> CheckingOptions modName locs eps

coCheckSourceLocationsL :: Lens' CheckingOptions Bool
coCheckSourceLocationsL = lens
  do \CheckingOptions{..} -> coCheckSourceLocations
  do \(CheckingOptions ep _ eps) locs -> CheckingOptions ep locs eps

instance Default CheckingOptions where
  def =
    CheckingOptions
      { coModuleName           = Nothing
      , coCheckSourceLocations = True
      , coCheckEntrypointsList = True
      }

-- | This test is checking that @readLigoMapper@ produces ok result for all contracts from @contractsDir@.
-- Also this test can check contracts with special options
-- (for e.g. with special entrypoint or should it check source locations for sensibility)
test_Contracts_are_sensible :: TestTree
test_Contracts_are_sensible = reinsuring $ testCase "Contracts are sensible" do
  contracts <- makeRelative contractsDir <<$>> scanContracts (`notElem` badContracts) contractsDir
  forConcurrently_ contracts testContract
  where
    testContract :: FilePath -> Assertion
    testContract contractName = do
      let CheckingOptions{..} = fromMaybe def (specialContracts M.!? dropExtension contractName)

      ligoMapper <-
        compileLigoContractDebug
          (fromMaybe "$main" coModuleName)
          (contractsDir </> contractName)

      LigoMapperResult{..} <-
        case readLigoMapper ligoMapper of
          Right v -> pure v
          Left err -> assertFailure $ pretty err

      when coCheckSourceLocations do
        forM_ (getAllSourceLocations lmrExpressionLocation) \srcLoc@(SourceLocation loc _ _) -> do
          case loc of
            MSFile path ->
              -- Some paths can be empty in @SourceLocation@ because of some ligo issues.
              -- So, we want to check them for sensibility.
              when (path == "") do
                assertFailure [int||Expected non-empty file name in loc #{srcLoc} in contract #{contractName}|]
            _ ->
              assertFailure [int||Unexpected source location in loc field of #{srcLoc} in contract #{contractName}|]

      when coCheckEntrypointsList do
        try @_ @SomeException (getAvailableModules (contractsDir </> contractName)) >>= \case
          Right _ -> pass
          Left exc -> do
            assertFailure [int||Something unexpected happened with contract #{contractName}:
              #{displayException exc}
            |]

    -- Contracts with special checking options
    specialContracts :: Map FilePath CheckingOptions
    specialContracts = M.fromList
      [ -- we use built-in functions in next contract and they are having weird source locations.
        ("built-ins", def & coCheckSourceLocationsL .~ False)
      , ("poly", def & coCheckSourceLocationsL .~ False)
      , ("self", def & coCheckSourceLocationsL .~ False)
      , ("iterate-big-map", def & coCheckSourceLocationsL .~ False)
      , ("big-map-storage", def & coCheckSourceLocationsL .~ False)
      , ("two-module-names", def & coModuleNameL ?~ "Main1.$main")
      , ("if-no-else", def & coCheckSourceLocationsL .~ False) -- no filename at some locations
      , ("statement-visiting", def & coCheckSourceLocationsL .~ False) -- no filename at some locations
      , ("computations-in-list", def & coCheckSourceLocationsL .~ False) -- no filename at some locations
      , ("complex-function-type", def & coCheckSourceLocationsL .~ False) -- no filename at some locations
      , ("builtins-locations", def & coCheckSourceLocationsL .~ False) -- no filename at some locations
      , ("module-entrypoints", def & coModuleNameL ?~ "IncDec.$main")
      ]

    -- Valid contracts that can't be used in debugger for some reason.
    badContracts :: [FilePath]
    badContracts = combine contractsDir <$>
      [ "no-modules.mligo" -- this file doesn't have any entrypoint
      , "module_contracts" </> "imported.mligo" -- this file doesn't have any entrypoint
      , "module_contracts" </> "imported2.jsligo" -- this file doesn't have any entrypoint
      , "malformed.mligo" -- incorrect contract
      , "dupped-ticket.mligo" -- illegal intentionally
      , "pretty" </> "sum-type.mligo" -- this file doesn't have any entrypoint
      , "pretty" </> "sum-type.jsligo" -- this file doesn't have any entrypoint
      ]
