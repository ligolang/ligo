module Test.Breakpoints
  ( module Test.Breakpoints
  ) where

import Test.Tasty (TestTree)
import Test.Util
import Test.Util.Options (minor)

import Morley.Debugger.Core.Breakpoint qualified as N
import Morley.Debugger.Core.Common (SrcLoc (..))
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
        N.switchBreakpoint (MSFile file) (SrcLoc 2 0)
        N.switchBreakpoint (MSFile file) (SrcLoc 5 0)

        liftIO $ step "Visiting 1st breakpoint"
        goToNextBreakpoint
        N.frozen do
          isStatus <$> N.curSnapshot @@?= InterpretRunning EventFacedStatement
          -- here and further we don't expect any concrete column number
          view N.slStart <<$>> N.getExecutedPosition @@?= Just (SrcLoc 2 2)

        liftIO $ step "Visiting 2nd breakpoint"
        goToNextBreakpoint
        N.frozen do
          isStatus <$> N.curSnapshot @@?= InterpretRunning (EventExpressionPreview GeneralExpression)
          -- here and further we don't expect any concrete column number
          view N.slStart <<$>> N.getExecutedPosition @@?= Just (SrcLoc 2 11)

        liftIO $ step "Visiting 3rd breakpoint"
        goToNextBreakpoint
        N.frozen do
          isStatus <$> N.curSnapshot @@?= InterpretRunning EventFacedStatement
          view N.slStart <<$>> N.getExecutedPosition @@?= Just (SrcLoc 5 2)

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
        N.switchBreakpoint (MSFile file) (SrcLoc 5 0)
        N.switchBreakpoint (MSFile file) (SrcLoc 7 0)
        N.switchBreakpoint (MSFile file) (SrcLoc 8 0)
        N.switchBreakpoint (MSFile file) (SrcLoc 9 0)

        N.switchBreakpoint (MSFile nestedFile) (SrcLoc 8 0)
        N.switchBreakpoint (MSFile nestedFile) (SrcLoc 13 0)
        N.switchBreakpoint (MSFile nestedFile) (SrcLoc 18 0)

        N.switchBreakpoint (MSFile nestedFile2) (SrcLoc 1 0)

        liftIO $ step "Go to first breakpoint"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 5 13)
              (SrcLoc 5 26)
            )

        liftIO $ step "Calculate arguments"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 5 21)
              (SrcLoc 5 25)
            )

        liftIO $ step "Go to next breakpoint (switch file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcLoc 14 5)
              (SrcLoc 14 10)
            )

        liftIO $ step "Go to next breakpoint (go to start file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 5 13)
              (SrcLoc 5 26)
            )

        liftIO $ step "Stop at statement"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 7 2)
              (SrcLoc 7 30)
            )

        liftIO $ step "Stop at function call"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 7 15)
              (SrcLoc 7 30)
            )

        liftIO $ step "Calculate arguments"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 7 23)
              (SrcLoc 7 29)
            )

        liftIO $ step "Go to next breakpoint (nested file, statement)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcLoc 10 10)
              (SrcLoc 10 13)
            )

        liftIO $ step "Go to next breakpoint (go to start file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 7 15)
              (SrcLoc 7 30)
            )

        liftIO $ step "Stop at statement"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 8 2)
              (SrcLoc 8 45)
            )

        liftIO $ step "Stop at function call"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 8 19)
              (SrcLoc 8 45)
            )

        liftIO $ step "Calculate arguments"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 8 27)
              (SrcLoc 8 44)
            )

        liftIO $ step "Stop at function call"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 8 19)
              (SrcLoc 8 45)
            )

        liftIO $ step "Stop at statement"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 9 2)
              (SrcLoc 9 81)
            )

        liftIO $ step "Stop at \"what\" call"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 9 55)
              (SrcLoc 9 80)
            )

        liftIO $ step "Calculate arguments for \"what\""
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 9 74)
              (SrcLoc 9 79)
            )

        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 9 67)
              (SrcLoc 9 72)
            )

        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcLoc 9 64)
              (SrcLoc 9 79)
            )

        liftIO $ step "Stop at statement"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcLoc 18 45)
              (SrcLoc 18 88)
            )

        liftIO $ step "Stop at \"strange\" call"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcLoc 18 68)
              (SrcLoc 18 88)
            )

        liftIO $ step "Calculate arguments for \"strange\""
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcLoc 18 80)
              (SrcLoc 18 87)
            )

        liftIO $ step "Go to next breakpoint (more nested file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile2)
              (SrcLoc 1 2)
              (SrcLoc 1 19)
            )

        liftIO $ step "Go to next breakpoint (go back)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcLoc 18 68)
              (SrcLoc 18 88)
            )

        liftIO $ step "Go to previous breakpoint"
        goToPreviousBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile2)
              (SrcLoc 1 2)
              (SrcLoc 1 19)
            )
  ]
