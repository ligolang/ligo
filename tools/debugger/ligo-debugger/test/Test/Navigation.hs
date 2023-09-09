{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tests on navigation through the snapshots and
-- responding to stepping commands.
module Test.Navigation
  ( test_StepIn_golden
  , test_Seq_node_doesn't_have_location
  , test_constant_as_statement
  , test_top_level_function_with_preprocessor_don't_have_locations
  , test_big_tuples_have_correct_evaluated_value
  , test_values_inside_switch_and_match_with_are_statements
  , test_local_function_assignments_are_statements
  , test_record_update_is_statement
  , test_Module_entrypoints
  , test_Next_golden
  , test_StepOut_golden
  , test_Continue_golden
  , test_StepBackReversed
  , test_ContinueReversed
  ) where

import Fmt (pretty)
import Hedgehog (forAllWith, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Text.Interpolation.Nyan hiding (rmode')

import Morley.Debugger.Core
  (Direction (..), FrozenPredicate (..), NavigableSnapshot (getExecutedPosition),
  SourceLocation' (..), SrcLoc (..), curSnapshot, frozen, moveTill, switchBreakpoint)
import Morley.Debugger.Core.TestUtil.Reversible
import Morley.Debugger.DAP.Types (StepCommand' (..))
import Morley.Michelson.Parser.Types (MichelsonSource (..))
import Morley.Michelson.Text (mt)

import Language.LIGO.Debugger.Navigate
import Language.LIGO.Debugger.Snapshots

import Test.Util
import Test.Util.Golden

-- Note: see the docs to 'processLigoStep' on rules for the expected
-- stepping behaviour

basicCaseRun :: Lang -> ContractRunData
basicCaseRun dialect = ContractRunData
  { crdProgram = contractsDir </> "functions-visiting" <.> langExtension dialect
  , crdModuleName = Nothing
  , crdParam = ()
  , crdStorage = 0 :: Integer
  }

{-

[LIGO-951]: finalize the tests.

-}
test_StepIn_golden :: TestTree
test_StepIn_golden = testGroup "StepIn" do
  gran <- allLigoStepGranularities
  let doStep = processLigoStep (CStepIn gran)
  pure $ testGroup [int||Granularity of #{gran}|] do
    dialect <- case gran of
      -- for statements the behaviour may differ for different dialects
      -- as they have custom format of code blocks
      GStmt -> allLangs
      _ -> one Caml
    [ goldenTestWithSnapshots [int||basic case, #s{dialect}, #{gran} granularity|]
        "StepIn"
        (basicCaseRun dialect)
        (dumpAllSnapshotsWithStep doStep)

      ]

test_Seq_node_doesn't_have_location :: TestTree
test_Seq_node_doesn't_have_location =
  let
    runData = ContractRunData
      { crdProgram = contractsDir </> "seq-nodes-without-locations.mligo"
      , crdModuleName = Nothing
      , crdParam = ()
      , crdStorage = 0 :: Integer
      }

    doStep = processLigoStep (CStepIn GExpExt)
  in goldenTestWithSnapshots
      "seq nodes dont have expression locations in snapshots"
      "StepIn"
      runData
      (dumpAllSnapshotsWithStep doStep)

test_constant_as_statement :: TestTree
test_constant_as_statement =
  let
    runData = ContractRunData
      { crdProgram = contractsDir </> "constant_as_statement.mligo"
      , crdModuleName = Nothing
      , crdParam = ()
      , crdStorage = 0 :: Integer
      }

    doStep = processLigoStep (CStepIn GStmt)
  in goldenTestWithSnapshots
      "Constant recognized as statement"
      "StepIn"
      runData
      (dumpAllSnapshotsWithStep doStep)

test_top_level_function_with_preprocessor_don't_have_locations :: TestTree
test_top_level_function_with_preprocessor_don't_have_locations =
  let
    runData = ContractRunData
      { crdProgram = contractsDir </> "contract-with-preprocessor.mligo"
      , crdModuleName = Nothing
      , crdParam = ()
      , crdStorage = 0 :: Integer
      }

    doStep = processLigoStep (CStepIn GExpExt)
  in goldenTestWithSnapshots
      "top-level functions with preprocessor don't have expression locations in snapshots"
      "StepIn"
      runData
      (dumpAllSnapshotsWithStep doStep)

test_big_tuples_have_correct_evaluated_value :: TestTree
test_big_tuples_have_correct_evaluated_value =
  let
    runData = ContractRunData
      { crdProgram = contractsDir </> "tuple-with-size-five.mligo"
      , crdModuleName = Nothing
      , crdParam = ()
      , crdStorage = 0 :: Integer
      }

    doStep = processLigoStep (CStepIn GExpExt)
  in goldenTestWithSnapshots
      "big tuples have correct evaluated value"
      "StepIn"
      runData
      (dumpAllSnapshotsWithStep doStep)

test_values_inside_switch_and_match_with_are_statements :: TestTree
test_values_inside_switch_and_match_with_are_statements =
  testGroup "Values inside \"switch\" and \"match ... with\" are statements" $
    [ ContractRunData
      { crdProgram = contractsDir </> "statement-in-match-branch.mligo"
      , crdModuleName = Nothing
      , crdParam = ()
      , crdStorage = 0 :: Integer
      }

    , ContractRunData
      { crdProgram = contractsDir </> "statements-in-case-branch.jsligo"
      , crdModuleName = Nothing
      , crdParam = [mt|Variant1|]
      , crdStorage = 0 :: Integer
      }
    ] <&> \runData -> do
      let doStep = processLigoStep (CStepIn GStmt)
      goldenTestWithSnapshots
        [int||checking for #{crdProgram runData} contract|]
        "StepIn"
        runData
        (dumpAllSnapshotsWithStep doStep)

test_local_function_assignments_are_statements :: TestTree
test_local_function_assignments_are_statements =
  let
    runData = ContractRunData
      { crdProgram = contractsDir </> "local-function-assignments.mligo"
      , crdModuleName = Nothing
      , crdParam = ()
      , crdStorage = 0 :: Integer
      }

    doStep = processLigoStep (CStepIn GStmt)
  in goldenTestWithSnapshots
      "local function assignments are statements"
      "StepIn"
      runData
      (dumpAllSnapshotsWithStep doStep)

test_record_update_is_statement :: TestTree
test_record_update_is_statement =
  let
    runData = ContractRunData
      { crdProgram = contractsDir </> "record-update-is-statement.mligo"
      , crdModuleName = Nothing
      , crdParam = ()
      , crdStorage = (0 :: Integer, [mt|"str"|], False)
      }

    doStep = processLigoStep (CStepIn GStmt)
  in goldenTestWithSnapshots
      "record update is statement"
      "StepIn"
      runData
      (dumpAllSnapshotsWithStep doStep)

test_Module_entrypoints :: TestTree
test_Module_entrypoints = testGroup "Module entrypoints"
  [ goldenTestWithSnapshots [int||Module entrypoint #{paramName}|]
    "StepIn"
    ContractRunData
      { crdProgram = contractsDir </> "module-entrypoints.mligo"
      , crdModuleName = Just "IncDec.$main"
      , crdParam = param
      , crdStorage = 100 :: Integer
      }
    (dumpAllSnapshotsWithStep doStep)
  | (param, paramName) <-
      [ (decrementParam, "decrement" :: Text)
      , (incrementParam, "increment")
      , (resetParam, "reset")
      ]
  ]
  where
    decrementParam :: Either () (Either Integer Integer)
    decrementParam = Right $ Left 42

    incrementParam :: Either () (Either Integer Integer)
    incrementParam = Right $ Right 42

    resetParam :: Either () (Either Integer Integer)
    resetParam = Left ()

    doStep = processLigoStep (CStepIn GExpExt)

test_Next_golden :: TestTree
test_Next_golden = testGroup "Next" do
  gran <- allLigoStepGranularities
  let doStep = processLigoStep (CNext Forward gran)
  pure $ testGroup [int||Granularity of #{gran}|]
    [ goldenTestWithSnapshots [int||go over top-level, granularity #{gran}|]
        "Next"
        (basicCaseRun Caml)
        do
          dumpAllSnapshotsWithStep $
            doStep <* do
              Just (SourceLocation _ (SrcLoc line _) _) <-
                lift $ frozen getExecutedPosition
              when (gran == GStmt && line < 10) do
                liftIO $ assertFailure [int||
                  We appeared at line #{line}, but we were not expected to visit
                  the functions `f` and `g`.
                  |]

    , goldenTestWithSnapshots [int||Go from within `f`, granularity #{gran}|]
        "Next"
        (basicCaseRun Caml)
        do
          moveTill Forward $ isAtLine 1
          [int||
            Starting from `f`'s beginning,
            should traverse `f`, skip `g`, and go to the end
          |]
          dumpAllSnapshotsWithStep doStep

    , testGroup "Interaction with breakpoints"
      [ goldenTestWithSnapshots
          [int||Breakpoint causes interrupt when we are going over function, \
                granularity #{gran}|]
          "Next"
          (basicCaseRun Caml)
          do
            switchBreakpoint
              (MSFile $ crdProgram $ basicCaseRun Caml)
              (SrcLoc 1 0)
            [int||Enabled breakpoint within `f`, but not within `g`|]
            dumpAllSnapshotsWithStep doStep

      ]

    ]

test_StepOut_golden :: TestTree
test_StepOut_golden = testGroup "StepOut" do
  gran <- allLigoStepGranularities
  let doStep = processLigoStep (CStepOut gran)
  pure $ testGroup [int||Granularity of #{gran}|]
    [ goldenTestWithSnapshots "Exit main function"
        "StepOut"
        (basicCaseRun Caml)
        do
          [int||Steping out immediately on start|]
          doStep
          dumpCurSnapshot

    , goldenTestWithSnapshots "Exit `f`"
        "StepOut"
        (basicCaseRun Caml)
        do
          moveTill Forward $ isAtLine 1
          [int||Should be within `f` now:|]
          dumpCurSnapshot

          [int||Jumping out of `f`:|]
          doStep
          dumpCurSnapshot

          [int||Jumping out of `main`:|]
          doStep
          dumpCurSnapshot

    , goldenTestWithSnapshots "Is not interfered by breakpoints"
        "StepOut"
        (basicCaseRun Caml)
        do
          switchBreakpoint
            (MSFile $ crdProgram $ basicCaseRun Caml)
            (SrcLoc 1 0)
          switchBreakpoint
            (MSFile $ crdProgram $ basicCaseRun Caml)
            (SrcLoc 2 0)
          processLigoStep (CContinue Forward)
          [int||Should be within `f` now:|]
          dumpCurSnapshot

          [int|n|
            Stepping out, now should appear outside of the method despite
            the breakpoint
          |]
          doStep
          dumpCurSnapshot

    ]

test_Continue_golden :: TestTree
test_Continue_golden = testGroup "Continue"
  let doStep = processLigoStep (CContinue Forward) in
  [ goldenTestWithSnapshots "Simple case with breakpoints"
      "Continue"
      (basicCaseRun Caml)
      do
        switchBreakpoint
          (MSFile $ crdProgram $ basicCaseRun Caml)
          (SrcLoc 12 0)

        [int||Breakpoint is set at `f` & `g` call|]
        dumpAllSnapshotsWithStep doStep
  ]

{-# ANN test_StepBackReversed ("HLint: ignore Redundant <$>" :: Text) #-}

test_StepBackReversed :: IO TestTree
test_StepBackReversed = fmap (testGroup "Step back is the opposite to Next") $
  [ ContractRunData
    { crdProgram = contractsDir </> "simple-ops.mligo"
    , crdModuleName = Nothing
    , crdParam = ()
    , crdStorage = 0 :: Integer
    }
  , ContractRunData
    { crdProgram = contractsDir </> "functions-assignments.mligo"
    , crdModuleName = Nothing
    , crdParam = ()
    , crdStorage = 0 :: Integer
    }
  , ContractRunData
    { crdProgram = contractsDir </> "module_contracts" </> "importer.mligo"
    , crdModuleName = Nothing
    , crdParam = ()
    , crdStorage = 0 :: Integer
    }

  ] `forM` \runData -> do
    locsHisEpAndLigoTypes <- liftIO $ mkSnapshotsForImpl dummyLoggingFunction Nothing runData
    return $ testProperty [int||On example of "#{crdProgram runData}"|] $
      property $ withSnapshots locsHisEpAndLigoTypes do
        let liftProp = lift . lift . lift

        granularity <- liftProp $ forAllWith pretty genStepGranularity
        oneStepReversed
          liftProp
          defaultOneStepReversedSettings
          { osrsDiscardAtStartWhenHolds = FrozenPredicate do
              -- The property doesn't hold for @GStmt@ granularity
              -- in case of stopping at function call since we tend to make an
              -- artificial stop there.
              guard (granularity == GStmt)
              InterpretRunning (EventExpressionPreview FunctionCall) <- isStatus <$> curSnapshot
              pass
          }
          processLigoStep
          granularity

test_ContinueReversed :: TestTree
test_ContinueReversed = testGroup "Continueing back is the opposite to Continue"
  [  -- TODO: [LIGO-949]
  ]
