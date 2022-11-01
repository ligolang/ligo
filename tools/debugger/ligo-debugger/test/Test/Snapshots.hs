{-# LANGUAGE NumDecimals #-}

-- | Checking snapshots collection.
module Test.Snapshots
  ( module Test.Snapshots
  ) where

import Unsafe qualified

import AST (scanContracts)
import Control.Lens (has, ix, makeLensesWith, (?~), (^?!))
import Data.Default (Default (def))
import Data.Map qualified as M
import Fmt (pretty)
import System.FilePath (combine, dropExtension, makeRelative)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@=?))
import Test.Util
import Text.Interpolation.Nyan
import UnliftIO (forConcurrently_)

import Morley.Debugger.Core
  (DebuggerState (..), Direction (..), FrozenPredicate (FrozenPredicate), MovementResult (..),
  NavigableSnapshot (getExecutedPosition), SourceLocation (SourceLocation), SourceType (..),
  curSnapshot, frozen, goesAfter, matchesSrcType, move, moveTill, tsAfterInstrs, twoElemFromList)
import Morley.Debugger.Core.Breakpoint qualified as N
import Morley.Debugger.Core.Snapshots qualified as N
import Morley.Debugger.DAP.Types.Morley ()
import Morley.Michelson.ErrorPos (Pos (Pos), SrcPos (SrcPos))
import Morley.Michelson.Typed (SomeValue)
import Morley.Michelson.Typed qualified as T
import Morley.Util.Lens (postfixLFields)

import Lorentz qualified as L

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Debugger.Snapshots

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
            { isStatus = InterpretRunning EventFacedStatement
            , isStackFrames = StackFrame
                { sfName = "main"
                , sfLoc = LigoRange file' (LigoPosition 2 2) (LigoPosition 2 17)
                , sfStack =
                  [ StackItem
                    { siLigoDesc = LigoStackEntry (LigoExposedStackEntry (Just (LigoVariable "s")) typ)
                    , siValue = SomeLorentzValue (0 :: Integer)
                    }
                  ]
                } :| []
            } | file == file' && typ == intType
              -> pass
          sp -> unexpectedSnapshot sp

        _ <- move Forward

        -- Only in this test we have to check all the snapshots quite thoroughly,
        -- so here getting all the remaining snapshots and checking them.
        (_, tsAfterInstrs -> restSnapshots) <- moveTill Forward (FrozenPredicate $ pure False)

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
          in
          [ ( InterpretRunning . EventExpressionEvaluated . Just $
                SomeLorentzValue (42 :: Integer)
            , one
              ( LigoRange file (LigoPosition 2 15) (LigoPosition 2 17)
              , stackWithS
              )
            )

          , ( InterpretRunning EventExpressionPreview
            , one
              ( LigoRange file (LigoPosition 2 11) (LigoPosition 2 17)
              , stackWithS
              )
            )

          , ( InterpretRunning . EventExpressionEvaluated . Just $
                SomeLorentzValue (42 :: Integer)
            , one
              ( LigoRange file (LigoPosition 2 11) (LigoPosition 2 17)
              , stackWithS
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
              , lastStack
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
            { isStatus = InterpretRunning EventFacedStatement
            , isStackFrames = StackFrame
                { sfName = "not_main"
                , sfLoc = LigoRange _ (LigoPosition 2 2) (LigoPosition 2 17)
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
          -- Skipping snapshots till snapshot with 'int' variable
          _ <- move Forward

          checkSnapshot \case
            InterpretSnapshot
              { isStatus = InterpretRunning EventFacedStatement
              , isStackFrames = StackFrame
                  { sfLoc = LigoRange _ (LigoPosition 2 2) (LigoPosition 2 17)
                  , sfStack =
                    [ StackItem
                        { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                            { leseType = LTConstant (LigoTypeConstant [] ("Int" :| []))
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
            { isStatus = InterpretRunning EventFacedStatement
            , isStackFrames = StackFrame
                { sfLoc = LigoRange _ (LigoPosition 2 2) (LigoPosition 4 17)
                } :| []
            } -> pass
          sp -> unexpectedSnapshot sp

  , testCaseSteps "check shadowing" \_step -> do
      let file = contractsDir </> "shadowing.religo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 4 :: Integer
            }

      testWithSnapshots runData do
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 12) (Pos 0))
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 13) (Pos 0))
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 18) (Pos 0))

        let checkStackItem :: Name 'Concise -> SomeValue -> StackItem 'Concise -> Bool
            checkStackItem expectedVar expectedVal = \case
              StackItem
                { siLigoDesc = LigoStackEntry LigoExposedStackEntry
                    { leseDeclaration = Just (LigoVariable actualVar)
                    }
                , siValue = actualVal
                } -> actualVal == expectedVal && expectedVar == actualVar
              _ -> False

        goToNextBreakpoint
        checkSnapshot \snap -> do
          let stackItems = snap ^?! isStackFramesL . ix 0 . sfStackL

          -- check that current snapshot has "s1" variable and it's type is @VInt@
          unless (any (checkStackItem "s1" $ T.SomeConstrainedValue (T.VInt 8)) stackItems) do
            unexpectedSnapshot snap

        goToNextBreakpoint
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

        goToNextBreakpoint
        checkSnapshot \snap -> do
          let stackItems = snap ^?! isStackFramesL . ix 0 . sfStackL

          -- check that current snapshot has "s" variable and it's value not 4
          unless (any (checkStackItem "s" $ T.SomeConstrainedValue (T.VInt 96)) stackItems) do
            unexpectedSnapshot snap

  , testCaseSteps "multiple contracts" \step -> do
      let modulePath = contractsDir </> "module_contracts"
      let file = modulePath </> "importer.mligo"
      let nestedFile = modulePath </> "imported.mligo"
      let nestedFile2 = modulePath </> "imported2.ligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 10 :: Integer
            }

      testWithSnapshots runData do
        -- Predicate for @moveTill@ which stops on a snapshot with specified file name in loc.
        let stopAtFile
              :: (MonadState (DebuggerState (InterpretSnapshot u)) m)
              => FilePath
              -> FrozenPredicate (DebuggerState (InterpretSnapshot u)) m
            stopAtFile filePath = FrozenPredicate do
              snap <- curSnapshot
              let locMb = snap ^? isStackFramesL . ix 0 . sfLocL
              case locMb of
                Just loc -> pure $ lrFile loc == filePath
                Nothing -> pure False

        lift $ step "Go to nested contract"
        _ <- moveTill Forward $ stopAtFile nestedFile
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = LigoRange file' (LigoPosition 5 0) (LigoPosition 5 18)
                } :| []
            } | file' == nestedFile -> pass
          sp -> unexpectedSnapshot sp

        lift $ step "Make sure that we went back"
        _ <- moveTill Forward $ stopAtFile file
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = LigoRange file' (LigoPosition 6 2) (LigoPosition 6 26)
                } :| []
            } | file' == file -> pass
          sp -> unexpectedSnapshot sp

        lift $ step "Check that we can go to more nested contract (and in another dialect)"
        _ <- moveTill Forward $ stopAtFile nestedFile2
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = LigoRange file' (LigoPosition 2 2) (LigoPosition 2 20)
                } :| _
            } | file' == nestedFile2 -> pass
          sp -> unexpectedSnapshot sp

        lift $ step "Make sure that we went back to \"imported.mligo\""
        _ <- moveTill Forward $ stopAtFile nestedFile
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = LigoRange file' (LigoPosition 19 57) (LigoPosition 19 64)
                } :| _
            } | file' == nestedFile -> pass
          sp -> unexpectedSnapshot sp

        lift $ step "Make sure that we went back to \"importer.mligo\""
        _ <- moveTill Forward $ stopAtFile file
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfLoc = LigoRange file' (LigoPosition 10 26) (LigoPosition 10 39)
                } :| []
            } | file' == file -> pass
          sp -> unexpectedSnapshot sp

    -- [LIGO-658]: write a test that checks that we have 'pair1' and 'pair2' in 'not-inlined-fst.mligo' contract.

  , testCaseSteps "functions and variables are not inlined" \step -> do
      let file = contractsDir </> "funcs-and-vars-no-inline.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 4 :: Integer
            }

      testWithSnapshots runData do
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 1) (Pos 0))
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 2) (Pos 0))
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 6) (Pos 0))
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 7) (Pos 0))
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 8) (Pos 0))

        let checkLinePosition pos = do
              goToNextBreakpoint
              frozen getExecutedPosition >>= \case
                Just (SourceLocation _ (SrcPos (Pos actualPos) _))
                  | actualPos == pos -> pass
                loc -> lift $ assertFailure [int||Expected stopping at line #{pos + 1}, got #{loc}|]

        lift $ step "check \"func\" function call stepping"
        checkLinePosition 6

        lift $ step "check stepping inside \"func\""
        checkLinePosition 1
        checkLinePosition 2

        lift $ step "check stopping at constant assignment"
        checkLinePosition 7

        lift $ step "check that \"s2\" is not inlined"
        goToNextBreakpoint
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
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 42 :: Integer
            }

      testWithSnapshots runData do
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 4) (Pos 0))

        lift $ step "Check that \"fold\" build-in works correctly"
        N.continueUntilBreakpoint N.NextBreak
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

  , testCaseSteps "monomorphed functions shows pretty" \step -> do
      let file = contractsDir </> "poly.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 42 :: Integer
            }

      testWithSnapshots runData do
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 11) (Pos 0))

        N.continueUntilBreakpoint N.NextBreak
        lift $ step "Check function namings"
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
              <$> [ "foo$1"
                  , "foo$4"
                  , "id$2"
                  , "fold_left$3"
                  , "poly_troll42_"
                  ]
            ) do
            assertFailure [int||This snapshot doesn't contain pretty monomorphed variables: #{snap}|]

  , testCaseSteps "Function assignments are skipped" \_ -> do
      let file = contractsDir </> "functions-assignments.mligo"
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
            { isStackFrames = StackFrame
                { sfLoc = LigoRange file' (LigoPosition 7 2) (LigoPosition 7 25)
                } :| []
            } | file' == file -> pass
          snap -> unexpectedSnapshot snap

  , testCaseSteps "Check statements" \step -> do
      let checkLocations runData locs = testWithSnapshots runData do
            let snapPred InterpretSnapshot{..} = case isStatus of
                  InterpretRunning EventFacedStatement -> True
                  _ -> False

            (_, filter snapPred . tsAfterInstrs -> snaps) <- moveTill Forward (FrozenPredicate $ pure False)

            ( snaps
                <&> do \InterpretSnapshot{..} ->
                        head isStackFrames
                          & \StackFrame{..} -> sfLoc
              ) @?= locs

      let file = contractsDir </> "statement-visiting.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 42 :: Integer
            }

      step [int||Checking locations for #{file}|]
      checkLocations
        runData
        [ LigoRange file (LigoPosition 9 2) (LigoPosition 9 15)
        , LigoRange file (LigoPosition 10 2) (LigoPosition 10 19)
        , LigoRange file (LigoPosition 11 2) (LigoPosition 11 23)
        , LigoRange file (LigoPosition 11 27) (LigoPosition 11 43)
        ]

      let file2 = contractsDir </> "statement-visiting.ligo"
      let runData2 = ContractRunData
            { crdProgram = file2
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 3 :: Integer
            }

      step [int||Checking locations for #{file2}|]
      checkLocations
        runData2
        ( [LigoRange file2 (LigoPosition 2 2) (LigoPosition 2 20)]
        <>
          concat
            ( replicate 3
              [ LigoRange file2 (LigoPosition 4 4) (LigoPosition 4 28)
              , LigoRange file2 (LigoPosition 5 4) (LigoPosition 5 22)
              ]
            )
        )

  , testCaseSteps "Execution history is lazy" \step -> do
      let file = contractsDir </> "infinite_contract.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 42 :: Integer
            }

      (allLocs, his) <- mkSnapshotsFor runData

      let his' = InterpretHistory $
            (Unsafe.fromJust . twoElemFromList) (take 1000 (toList $ unInterpretHistory his)) <>
            error "Went too far in execution history"

      let tenSeconds = 10 * 1e6

      step "Evaluating prefix of interpret history"
      res <-
        timeout tenSeconds do
          withSnapshots (allLocs, his') do
            replicateM_ 3 $ move Forward
            frozen curSnapshot

      assertBool "Expected history to be evaluated" (isJust res)

  , testCaseSteps "Check snapshot collection logging" \step -> do
      anyWritten <- newIORef False

      let logger :: String -> IO ()
          logger = const $ writeIORef anyWritten True

      let file = contractsDir </> "noop.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      step [int||Going through all execution history|]
      testWithSnapshotsImpl logger runData do
        void $ moveTill Forward $ FrozenPredicate $ pure False

      unlessM (readIORef anyWritten) do
        assertFailure [int||No logs we're dumped during snapshot collection|]

  , testGroup "comparisons does not produce duplicated snapshots"
    -- ligo used to produce the same location twice, e.g. for both COMPARE and GT

    [ testCaseSteps "comparison in condition" \step -> do
        let file = contractsDir </> "if.mligo"
        let runData = ContractRunData
              { crdProgram = file
              , crdEntrypoint = Nothing
              , crdParam = ()
              , crdStorage = 4 :: Integer
              }

        testWithSnapshots runData do
          -- Jump to GT
          lift $ step "jumping to GT"
          (moveRes, _) <- moveTill Forward $ FrozenPredicate do
            status <- isStatus <$> curSnapshot
            return $ preview statusExpressionEvaluatedP status
                  == Just (SomeLorentzValue False)

          lift $ assertBool
            "Didn't find the necessary snapshot"
            (moveRes == MovedSuccessfully)

          srcLocAtGt <- sfLoc . head . isStackFrames <$> frozen curSnapshot

          -- Go back. If for COMPARE we also create a snapshot, our test should fail
          lift $ step "jumping back"
          void . moveTill Backward $ FrozenPredicate do
            status <- isStatus <$> curSnapshot
            return $ has statusExpressionEvaluatedP status


          status <- isStatus <$> frozen curSnapshot
          when
            ( preview statusExpressionEvaluatedP status
              == Just (SomeLorentzValue (-1 :: Integer))
            ) do
              srcLocAtCompare <- sfLoc . head . isStackFrames <$> frozen curSnapshot
              if srcLocAtCompare == srcLocAtGt
                then lift $ assertFailure
                  "Created a snapshot at COMPARE that duplicates the snapshot at GT"
                else lift $ assertFailure
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
              , crdEntrypoint = Nothing
              , crdParam = 5 :: Integer
              , crdStorage = False
              }

        testWithSnapshots runData do
          lift $ step "Skipping push"
          _ <- moveTill Forward $ goesAfter (SrcPos (Pos 1) (Pos 0))
          _ <- move Forward  -- skip "upon expression" state
          do
            status <- isStatus <$> frozen curSnapshot
            preview statusExpressionEvaluatedP status
              @?= Just (SomeLorentzValue (0 :: Integer))

          lift $ step "Checking comparison result"
          replicateM_ 2 $ move Forward
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
              , crdEntrypoint = Nothing
              , crdParam = ()
              , crdStorage = 6 :: Integer
              }

        testWithSnapshots runData do
          -- Jump to division result
          lift $ step "jumping to GT"
          (moveRes, _) <- moveTill Forward $ FrozenPredicate do
            status <- isStatus <$> curSnapshot
            return $ preview statusExpressionEvaluatedP status
                  == Just (SomeLorentzValue (3 :: Integer))

          lift $ assertBool
            "Didn't find the necessary snapshot"
            (moveRes == MovedSuccessfully)
          srcLocAtDiv <- sfLoc . head . isStackFrames <$> frozen curSnapshot

          lift $ step "jumping to the previously computed value"
          void . moveTill Backward $ FrozenPredicate do
            has statusExpressionEvaluatedP . isStatus <$> curSnapshot

          srcLocAtBack <- sfLoc . head . isStackFrames <$> frozen curSnapshot
          lift $ assertBool
            "Duplicated source range for different snapshots"
            (srcLocAtDiv /= srcLocAtBack)
    ]

  , testCaseSteps "Variables in pattern match" \step -> do
      let file = contractsDir </> "variables-in-pattern-match.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        void $ moveTill Forward $
          goesAfter (SrcPos (Pos 4) (Pos 0))

        lift $ step [int||Extract variables|]
        checkSnapshot \snap -> do
          let vars = snap
                & isStackFrames
                & head
                & getVariableNamesFromStackFrame

          vars @~=? ["a", "b"]

  , testCaseSteps "Check stack frames on function entering / exiting" \step -> do
      let file = contractsDir </> "recursive.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $
          goesBetween (SrcPos (Pos 1) (Pos 0)) (SrcPos (Pos 2) (Pos 0))
        lift $ step [int||Check stack frame names on entering "recursive"|]
        checkSnapshot ((@=?) ["recursive", "main"] . getStackFrameNames)

        moveTill Forward $ goesAfter (SrcPos (Pos 4) (Pos 0))
        lift $ step [int||Check that we have only "main" stack frame after leaving function|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

  , testCaseSteps "Calling local function" \step -> do
      let file = contractsDir </> "local-function.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        move Forward

        lift $ step [int||Check that we have only one "main" stack frame|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

        moveTill Forward $
          goesBetween (SrcPos (Pos 1) (Pos 0)) (SrcPos (Pos 2) (Pos 0))
        lift $ step [int||Check that we have "f" stack frame on entering local function|]
        checkSnapshot ((@=?) ["f", "main"] . getStackFrameNames)

  , testCaseSteps "Function calling other function" \step -> do
      let file = contractsDir </> "function-calling-function.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $
          goesBetween (SrcPos (Pos 2) (Pos 0)) (SrcPos (Pos 3) (Pos 0))
        lift $ step [int||Calling top level function "complex"|]
        checkSnapshot ((@=?) ["complex", "main"] . getStackFrameNames)

        moveTill Forward $
          goesBetween (SrcPos (Pos 0) (Pos 0)) (SrcPos (Pos 1) (Pos 0))
        lift $ step [int||Calling function "add" from "complex"|]
        checkSnapshot ((@=?) ["add", "complex", "main"] . getStackFrameNames)

  , testCaseSteps "Lambda parameter" \step -> do
      let file = contractsDir </> "lambda-parameter.mligo"

      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = L.unpair @_ @_ @'[] L.# L.add @Integer @Integer
            , crdStorage = 0 :: Integer
            }

      let runDataFailing = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = L.drop L.# L.push [L.mt|Stick bugged lol|] L.# L.failWith :: '[(Integer, Integer)] L.:-> '[Integer]
            , crdStorage = 0 :: Integer
            }

      let stackFramesCheck :: ([[Text]] -> Bool) -> StateT (DebuggerState (InterpretSnapshot u)) IO ()
          stackFramesCheck namesCheck = do
            (_, fmap getStackFrameNames . tsAfterInstrs -> stackFrameNames) <- moveTill Forward (FrozenPredicate $ pure False)
            lift $
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
        in all (== ["main"]) initElems && lastElem == ["p", "main"]

  , testCaseSteps "Check variables in stack frames" \step -> do
      let dir = contractsDir </> "module_contracts"
      let file = dir </> "importer.mligo"
      let nestedFile = dir </> "imported.mligo"
      let nestedFile2 = dir </> "imported2.ligo"

      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $
          goesBetween (SrcPos (Pos 8) (Pos 0)) (SrcPos (Pos 12) (Pos 0))
          && matchesSrcType (N.SourcePath nestedFile)
        lift $ step [int||Check variables for "sum" snapshot|]
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = stackFrame@StackFrame
                { sfName = "sum"
                , sfLoc = LigoRange file' _ _
                } :| _
            } | file' == nestedFile -> getVariableNamesFromStackFrame stackFrame @~=? ["l", "x", "acc"]
          snap -> unexpectedSnapshot snap

        moveTill Forward $
          goesAfter (SrcPos (Pos 4) (Pos 0)) && matchesSrcType (N.SourcePath nestedFile2)
        lift $ step [int||Check variables for "strange" snapshot|]
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = stackFrame@StackFrame
                { sfName = "strange"
                , sfLoc = LigoRange file' _ _
                } :| _
            } | file' == nestedFile2 ->
                  getVariableNamesFromStackFrame stackFrame @~=? ["acc", "c", "b", "a"]
          snap -> unexpectedSnapshot snap

  , testCaseSteps "Two \"main\" stack frames" \step -> do
      let file = contractsDir </> "two-functions-with-main-name.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $
          goesBetween (SrcPos (Pos 3) (Pos 0)) (SrcPos (Pos 5) (Pos 0))
        lift $ step [int||Check that we have two stack frames with "main" name|]
        checkSnapshot \case
          InterpretSnapshot
            { isStackFrames = StackFrame
                { sfName = "main"
                , sfLoc = loc1
                } :|
                  [ StackFrame
                      { sfName = "main"
                      , sfLoc = loc2
                      }
                  ]
            } | loc1 /= loc2 -> pass
          snap -> unexpectedSnapshot snap
        checkSnapshot ((@=?) ["main", "main"] . getStackFrameNames)

  , testCaseSteps "Stack frames in a contract with partially applied function" \step -> do
      let file = contractsDir </> "apply.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $
          goesBetween (SrcPos (Pos 0) (Pos 0)) (SrcPos (Pos 1) (Pos 0))

        lift $ step [int||Check stack frames after entering "add5"|]
        checkSnapshot ((@=?) ["add", "add5", "main"] . getStackFrameNames)

        moveTill Forward $ goesAfter (SrcPos (Pos 7) (Pos 0))
        lift $ step [int||Check stack frames after leaving "add5"|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

  , testCaseSteps "Paritally applied function inside top level function" \step -> do
      let file = contractsDir </> "complex-apply.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $
          goesBetween (SrcPos (Pos 1) (Pos 0)) (SrcPos (Pos 2) (Pos 0))

        lift $ step [int||Go into "add5"|]
        checkSnapshot ((@=?) ["add", "add5", "myFunc", "main"] . getStackFrameNames)

        moveTill Forward $ goesAfter (SrcPos (Pos 6) (Pos 0))

        lift $ step [int||Leave "myFunc"|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

  , testCaseSteps "2 times curried function" \step -> do
      let file = contractsDir </> "curry.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $
          goesBetween (SrcPos (Pos 7) (Pos 0)) (SrcPos (Pos 8) (Pos 0))

        lift $ step [int||Check stack frames for inner "sub"|]
        checkSnapshot ((@=?) ["sub", "f", "partApplied", "applyOp", "main"] . getStackFrameNames)

        moveTill Forward $
          goesBetween (SrcPos (Pos 5) (Pos 0)) (SrcPos (Pos 6) (Pos 0))

        lift $ step [int||Check stack frames for inner "add"|]
        checkSnapshot ((@=?) ["add", "f", "partApplied", "applyOp", "main"] . getStackFrameNames)

        moveTill Forward $
          goesAfter (SrcPos (Pos 12) (Pos 0))

        lift $ step [int||Leave "applyOp" functions|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

  , testCaseSteps "2 times curried function inside lambda" \step -> do
      let file = contractsDir </> "curry-inside-lambda.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $
          goesBetween (SrcPos (Pos 5) (Pos 0)) (SrcPos (Pos 6) (Pos 0))

        lift $ step [int||Check "sub" stack frames inside "lambdaFun"|]

        -- Actually here should be only one stack frame with name "f"
        -- but LIGO source mapper treats these "f"s from this contract as different.
        checkSnapshot ((@=?) ["sub", "f", "f", "apply", "lambdaFun", "main"] . getStackFrameNames)

        moveTill Forward $
          goesBetween (SrcPos (Pos 4) (Pos 0)) (SrcPos (Pos 5) (Pos 0))

        lift $ step [int||Check "add" stack frames inside "lambdaFun"|]

        -- The same here.
        checkSnapshot ((@=?) ["add", "f", "f", "apply", "lambdaFun", "main"] . getStackFrameNames)

        moveTill Forward $
          goesAfter (SrcPos (Pos 7) (Pos 0))

        lift $ step [int||Leave "lambdaFun"|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)

  , testCaseSteps "Multiple currying" \step -> do
      let file = contractsDir </> "advanced-curry.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 0 :: Integer
            }

      testWithSnapshots runData do
        moveTill Forward $
          goesBetween (SrcPos (Pos 7) (Pos 0)) (SrcPos (Pos 8) (Pos 0))

        lift $ step [int||Check stack frames in inner "act"|]

        -- The same as in the test above.
        checkSnapshot ((@=?) ["act", "f", "f", "applyOnce", "applyTwice", "applyThrice", "apply", "main"] . getStackFrameNames)

        moveTill Forward $
          goesAfter (SrcPos (Pos 10) (Pos 0))

        lift $ step [int||Check stack frames after leaving "act"|]
        checkSnapshot ((@=?) ["main"] . getStackFrameNames)
  ]

-- | Special options for checking contract.
data CheckingOptions = CheckingOptions
  { coEntrypoint :: Maybe String
  , coCheckSourceLocations :: Bool
  , coCheckEntrypointsList :: Bool
  } deriving stock (Show)
makeLensesWith postfixLFields ''CheckingOptions

instance Default CheckingOptions where
  def =
    CheckingOptions
      { coEntrypoint = Nothing
      , coCheckSourceLocations = True
      , coCheckEntrypointsList = True
      }

-- | This test is checking that @readLigoMapper@ produces ok result for all contracts from @contractsDir@.
-- Also this test can check contracts with special options
-- (for e.g. with special entrypoint or should it check source locations for sensibility)
unit_Contracts_are_sensible :: Assertion
unit_Contracts_are_sensible = do
  contracts <- makeRelative contractsDir <<$>> scanContracts (`notElem` badContracts) contractsDir
  forConcurrently_ contracts testContract
  where
    testContract :: FilePath -> Assertion
    testContract contractName = do
      let CheckingOptions{..} = fromMaybe def (specialContracts M.!? dropExtension contractName)

      ligoMapper <- compileLigoContractDebug (fromMaybe "main" coEntrypoint) (contractsDir </> contractName)

      (locations, _, _) <-
        case readLigoMapper ligoMapper typesReplaceRules instrReplaceRules of
          Right v -> pure v
          Left err -> assertFailure $ pretty err

      when coCheckSourceLocations do
        forM_ locations \srcLoc@(SourceLocation loc _) -> do
          case loc of
            SourcePath path ->
              -- Some paths can be empty in @SourceLocation@ because of some ligo issues.
              -- So, we want to check them for sensibility.
              when (path == "") do
                assertFailure [int||Expected non-empty file name in loc #{srcLoc} in contract #{contractName}|]
            LorentzContract ->
              assertFailure [int||Unexpected "Lorentz contract" in loc #{srcLoc} in contract #{contractName}|]

      when coCheckEntrypointsList do
        try @_ @SomeException (getAvailableEntrypoints (contractsDir </> contractName)) >>= \case
          Right _ -> pass
          Left exc -> do
            assertFailure [int||Something unexpected happened with contract #{contractName}:
              #{displayException exc}
            |]

    -- Contracts with special checking options
    specialContracts :: Map FilePath CheckingOptions
    specialContracts = M.fromList
      [ ("not-main-entry-point", def & coEntrypointL ?~ "not_main")
      -- we use built-in functions in next contract and they are having weird source locations.
      , ("built-ins", def & coCheckSourceLocationsL .~ False)
      , ("poly", def & coCheckSourceLocationsL .~ False)
      , ("self", def & coCheckSourceLocationsL .~ False)
      , ("iterate-big-map", def & coCheckSourceLocationsL .~ False)
      , ("two-entrypoints", def & coEntrypointL ?~ "main1")
      ]

    -- Valid contracts that can't be used in debugger for some reason.
    badContracts :: [FilePath]
    badContracts = combine contractsDir <$>
      [ "no-entrypoint.mligo" -- this file doesn't have any entrypoint
      , "module_contracts" </> "imported.mligo" -- this file doesn't have any entrypoint
      , "module_contracts" </> "imported2.ligo" -- this file doesn't have any entrypoint
      , "malformed.mligo" -- incorrect contract
      ]
