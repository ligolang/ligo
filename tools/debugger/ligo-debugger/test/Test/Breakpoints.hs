module Test.Breakpoints
  ( module Test.Breakpoints
  ) where

import Control.Lens (has)
import Test.Tasty (TestTree)

import Morley.Debugger.Core.Breakpoint qualified as N
import Morley.Debugger.Core.Navigate qualified as N
import Morley.Debugger.Core.Snapshots qualified as N
import Morley.Debugger.DAP.Types.Morley ()
import Morley.Michelson.ErrorPos (Pos (..), SrcPos (..))

import Language.LIGO.Debugger.Snapshots
import Test.Snapshots
import Test.Util

test_test :: [TestTree]
test_test =
  [ testCaseSteps "Simple case" \step -> do
      let file = contractsDir </> "simple-ops-sequence.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 5 :: Integer
            }

      testWithSnapshots runData do
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 2) (Pos 0))
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 5) (Pos 0))

        lift $ step "Visiting 1st breakpoint"
        N.continueUntilBreakpoint N.NextBreak
        N.frozen do
          isStatus <$> N.curSnapshot @@?= InterpretRunning EventExpressionPreview
          -- here and further we don't expect any concrete column number
          view N.slSrcPos <<$>> N.getExecutedPosition @@?= Just (SrcPos (Pos 2) (Pos 11))

        lift $ step "Visiting 1st breakpoint after expr evaluation"
        N.continueUntilBreakpoint N.NextBreak
        N.frozen do
          -- here and further we don't expect any concrete column number
          view N.slSrcPos <<$>> N.getExecutedPosition @@?= Just (SrcPos (Pos 2) (Pos 11))
          isStatus <$> N.curSnapshot @@? has (_InterpretRunning . _EventExpressionEvaluated)

        lift $ step "Visiting 2nd breakpoint"
        N.continueUntilBreakpoint N.NextBreak
        N.frozen do
          isStatus <$> N.curSnapshot @@?= InterpretRunning EventExpressionPreview
          view N.slSrcPos <<$>> N.getExecutedPosition @@?= Just (SrcPos (Pos 5) (Pos 3))

        lift $ step "Visiting back 1st breakpoint"
        N.reverseContinue N.NextBreak
        N.frozen do
          isStatus <$> N.curSnapshot
            @@? has (_InterpretRunning . _EventExpressionEvaluated)
          view N.slSrcPos <<$>> N.getExecutedPosition @@?= Just (SrcPos (Pos 2) (Pos 11))

  , testCaseSteps "Pausing in different contracts" \step -> do
      let modulePath = contractsDir </> "module_contracts"
      let file = modulePath </> "importer.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 5 :: Integer
            }

      let nestedFile = modulePath </> "imported.mligo"
      let nestedFile2 = modulePath </> "imported2.ligo"

      testWithSnapshots runData do
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 5) (Pos 0))
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 7) (Pos 0))
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 8) (Pos 0))
        N.switchBreakpoint (N.SourcePath file) (SrcPos (Pos 9) (Pos 0))

        N.switchBreakpoint (N.SourcePath nestedFile) (SrcPos (Pos 8) (Pos 0))
        N.switchBreakpoint (N.SourcePath nestedFile) (SrcPos (Pos 13) (Pos 0))
        N.switchBreakpoint (N.SourcePath nestedFile) (SrcPos (Pos 18) (Pos 0))

        N.switchBreakpoint (N.SourcePath nestedFile2) (SrcPos (Pos 0) (Pos 0))

        lift $ step "Go to first breakpoint"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just (N.SourceLocation (N.SourcePath file) (SrcPos (Pos 5) (Pos 21)))

        lift $ step "Go to next breakpoint (switch file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just (N.SourceLocation (N.SourcePath nestedFile) (SrcPos (Pos 14) (Pos 5)))

        lift $ step "Go to next breakpoint (go to start file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just (N.SourceLocation (N.SourcePath file) (SrcPos (Pos 7) (Pos 23)))

        lift $ step "Go to next breakpoint (switch file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just (N.SourceLocation (N.SourcePath nestedFile) (SrcPos (Pos 11) (Pos 18)))

        lift $ step "Go to next breakpoint (go to start file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just (N.SourceLocation (N.SourcePath file) (SrcPos (Pos 8) (Pos 27)))

        lift $ step "Go to next breakpoint (more nested file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just (N.SourceLocation (N.SourcePath nestedFile2) (SrcPos (Pos 1) (Pos 19)))

        lift $ step "Go to next breakpoint (go back)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just (N.SourceLocation (N.SourcePath nestedFile) (SrcPos (Pos 18) (Pos 45)))

        lift $ step "Go to previous breakpoint"
        goToPreviousBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just (N.SourceLocation (N.SourcePath nestedFile2) (SrcPos (Pos 1) (Pos 19)))
  ]
