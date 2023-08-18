module Test.Breakpoints
  ( module Test.Breakpoints
  ) where

import Test.Tasty (TestTree)
import Test.Util
import Test.Util.Options (minor)
import Text.Interpolation.Nyan hiding (rmode')

import Morley.Debugger.Core.Breakpoint qualified as N
import Morley.Debugger.Core.Common (SrcLoc (..))
import Morley.Debugger.Core.Navigate (Direction (..))
import Morley.Debugger.Core.Navigate qualified as N
import Morley.Debugger.Core.Snapshots qualified as N
import Morley.Debugger.DAP.Types.Morley ()
import Morley.Michelson.Parser.Types (MichelsonSource (MSFile))

import Language.LIGO.Debugger.Snapshots

test_test :: [TestTree]
test_test = minor <$>
  [ testCaseSteps "Simple case" \step -> do
      let file = contractsDir </> "simple-ops-sequence.mligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 5 :: Integer
            }

      testWithSnapshots runData do
        N.switchBreakpoint (MSFile file) (SrcLoc 3 0)
        N.switchBreakpoint (MSFile file) (SrcLoc 6 0)

        liftIO $ step "Visiting 1st breakpoint"
        goToNextBreakpoint Forward
        N.frozen do
          isStatus <$> N.curSnapshot @@?= InterpretRunning EventFacedStatement
          -- here and further we don't expect any concrete column number
          view N.slStart <<$>> N.getExecutedPosition @@?= Just (SrcLoc 3 2)

        liftIO $ step "Visiting 2nd breakpoint"
        goToNextBreakpoint Forward
        N.frozen do
          isStatus <$> N.curSnapshot @@?= InterpretRunning (EventExpressionPreview GeneralExpression)
          -- here and further we don't expect any concrete column number
          view N.slStart <<$>> N.getExecutedPosition @@?= Just (SrcLoc 3 11)

        liftIO $ step "Visiting 3rd breakpoint"
        goToNextBreakpoint Forward
        N.frozen do
          isStatus <$> N.curSnapshot @@?= InterpretRunning EventFacedStatement
          view N.slStart <<$>> N.getExecutedPosition @@?= Just (SrcLoc 6 2)

        -- TODO: uncomment this when visiting multiple source locations is available in morley-debugger

        -- lift $ step "Visiting 1st breakpoint"
        -- N.continueUntilBreakpoint N.NextBreak
        -- N.frozen do
        --   isStatus <$> N.curSnapshot @@?= InterpretRunning EventExpressionPreview
        --   -- here and further we don't expect any concrete column number
        --   view N.slSrcPos <<$>> N.getExecutedPosition @@?= Just (SrcLoc 2 11)

        -- lift $ step "Visiting 1st breakpoint after expr evaluation"
        -- N.continueUntilBreakpoint N.NextBreak
        -- N.frozen do
        --   -- here and further we don't expect any concrete column number
        --   view N.slSrcPos <<$>> N.getExecutedPosition @@?= Just (SrcLoc 2 11)
        --   isStatus <$> N.curSnapshot @@? has (_InterpretRunning . _EventExpressionEvaluated)

        -- lift $ step "Visiting 2nd breakpoint"
        -- N.continueUntilBreakpoint N.NextBreak
        -- N.frozen do
        --   isStatus <$> N.curSnapshot @@?= InterpretRunning EventExpressionPreview
        --   view N.slSrcPos <<$>> N.getExecutedPosition @@?= Just (SrcLoc 5 3)

        -- lift $ step "Visiting back 1st breakpoint"
        -- N.reverseContinue N.NextBreak
        -- N.frozen do
        --   isStatus <$> N.curSnapshot
        --     @@? has (_InterpretRunning . _EventExpressionEvaluated)
        --   view N.slSrcPos <<$>> N.getExecutedPosition @@?= Just (SrcLoc 2 11)

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
      let nestedFile2 = modulePath </> "imported2.jsligo"

      testWithSnapshots runData do
        N.switchBreakpoint (MSFile file) (SrcLoc 6 0)
        N.switchBreakpoint (MSFile file) (SrcLoc 8 0)
        N.switchBreakpoint (MSFile file) (SrcLoc 9 0)
        N.switchBreakpoint (MSFile file) (SrcLoc 10 0)

        N.switchBreakpoint (MSFile nestedFile) (SrcLoc 8 0)
        N.switchBreakpoint (MSFile nestedFile) (SrcLoc 13 0)
        N.switchBreakpoint (MSFile nestedFile) (SrcLoc 18 0)

        N.switchBreakpoint (MSFile nestedFile2) (SrcLoc 1 0)

        liftIO $ step "Go to first breakpoint"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 6 13)
              (SrcLoc 6 26)
            )

        liftIO $ step "Calculate arguments"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 6 21)
              (SrcLoc 6 25)
            )

        liftIO $ step "Go to next breakpoint (switch file)"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcLoc 14 5)
              (SrcLoc 14 10)
            )

        liftIO $ step "Go to next breakpoint (go to start file)"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 6 13)
              (SrcLoc 6 26)
            )

        liftIO $ step "Stop at statement"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 8 2)
              (SrcLoc 8 30)
            )

        liftIO $ step "Stop at function call"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 8 15)
              (SrcLoc 8 30)
            )

        liftIO $ step "Calculate arguments"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 8 23)
              (SrcLoc 8 29)
            )

        liftIO $ step "Go to next breakpoint (nested file, statement)"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcLoc 9 8)
              (SrcLoc 9 11)
            )

        liftIO $ step "Go to next breakpoint (go to start file)"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 8 15)
              (SrcLoc 8 30)
            )

        liftIO $ step "Stop at statement"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 9 2)
              (SrcLoc 9 45)
            )

        liftIO $ step "Stop at function call"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 9 19)
              (SrcLoc 9 45)
            )

        liftIO $ step "Calculate arguments"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 9 27)
              (SrcLoc 9 44)
            )

        liftIO $ step "Stop at function call"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 9 19)
              (SrcLoc 9 45)
            )

        liftIO $ step "Stop at statement"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 10 2)
              (SrcLoc 10 81)
            )

        liftIO $ step "Stop inside tuple"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 10 3)
              (SrcLoc 10 80)
            )

        liftIO $ step "Stop at second element"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 10 26)
              (SrcLoc 10 80)
            )

        liftIO $ step "Stop at \"what\" call"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 10 55)
              (SrcLoc 10 80)
            )

        liftIO $ step "Calculate arguments for \"what\""
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 10 64)
              (SrcLoc 10 79)
            )

        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 10 74)
              (SrcLoc 10 79)
            )

        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 10 67)
              (SrcLoc 10 72)
            )

        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 10 64)
              (SrcLoc 10 79)
            )

        liftIO $ step "Stop at statement"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcLoc 18 45)
              (SrcLoc 18 88)
            )

        liftIO $ step "Stop at \"strange\" call"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcLoc 18 68)
              (SrcLoc 18 88)
            )

        liftIO $ step "Calculate arguments for \"strange\""
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcLoc 18 80)
              (SrcLoc 18 87)
            )

        liftIO $ step "Go to next breakpoint (more nested file)"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile2)
              (SrcLoc 1 2)
              (SrcLoc 1 19)
            )

        liftIO $ step "Go to next breakpoint (go back)"
        goToNextBreakpoint Forward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcLoc 18 68)
              (SrcLoc 18 88)
            )

        liftIO $ step "Go to previous breakpoint"
        goToNextBreakpoint Backward
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile2)
              (SrcLoc 1 2)
              (SrcLoc 1 19)
            )

    -- Cover regression in
    -- https://gitlab.com/morley-framework/morley-debugger/-/issues/91
  , testCaseSteps "Breakpoint ids in different files are unique" \step -> do
      let modulePath = contractsDir </> "module_contracts"
      let file = modulePath </> "importer.mligo"
      let nestedFile  = modulePath </> "imported.mligo"
      let nestedFile2 = modulePath </> "imported2.jsligo"
      let runData = ContractRunData
            { crdProgram = file
            , crdEntrypoint = Nothing
            , crdParam = ()
            , crdStorage = 5 :: Integer
            }

      testWithSnapshots runData do
        N.switchBreakpoint (MSFile file) (SrcLoc 8 0)
        N.switchBreakpoint (MSFile nestedFile) (SrcLoc 18 0)
        N.switchBreakpoint (MSFile nestedFile2) (SrcLoc 3 0)
        N.switchBreakpoint (MSFile nestedFile2) (SrcLoc 9 0)

        liftIO $ step "Fly through all the breakpoints"
        -- Doing a bit more steps than breakpoints just in case
        let stepsNum = 6
        visitedBreakpoints <- replicateM stepsNum (goToNextBreakpointLine' Forward)
        let uniqueBreakpointsNum = length (ordNub visitedBreakpoints)
        if
          | uniqueBreakpointsNum == 4 -> pass
          | uniqueBreakpointsNum < 4 ->
            liftIO $ assertFailure [int||
              Visited only #{uniqueBreakpointsNum} breakpoints, expected 4
            |]
          | uniqueBreakpointsNum == stepsNum ->
            liftIO $ assertFailure [int||
              All breakpoints seem to have unique ids, something's odd
            |]
          | otherwise -> pass

  ]
