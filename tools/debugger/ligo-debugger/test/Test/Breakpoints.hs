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

  ]
