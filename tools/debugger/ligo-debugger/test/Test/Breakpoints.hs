module Test.Breakpoints
  ( module Test.Breakpoints
  ) where

import Test.Tasty (TestTree)
import Test.Util
import Test.Util.Options (minor)

import Morley.Debugger.Core.Breakpoint qualified as N
import Morley.Debugger.Core.Navigate qualified as N
import Morley.Debugger.Core.Snapshots qualified as N
import Morley.Debugger.DAP.Types.Morley ()
import Morley.Michelson.ErrorPos (Pos (..), SrcPos (..))
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
        N.switchBreakpoint (MSFile file) (SrcPos (Pos 2) (Pos 0))
        N.switchBreakpoint (MSFile file) (SrcPos (Pos 5) (Pos 0))

        liftIO $ step "Visiting 1st breakpoint"
        goToNextBreakpoint
        N.frozen do
          isStatus <$> N.curSnapshot @@?= InterpretRunning EventFacedStatement
          -- here and further we don't expect any concrete column number
          view N.slStart <<$>> N.getExecutedPosition @@?= Just (SrcPos (Pos 2) (Pos 2))

        liftIO $ step "Visiting 2nd breakpoint"
        goToNextBreakpoint
        N.frozen do
          isStatus <$> N.curSnapshot @@?= InterpretRunning EventExpressionPreview
          -- here and further we don't expect any concrete column number
          view N.slStart <<$>> N.getExecutedPosition @@?= Just (SrcPos (Pos 2) (Pos 11))

        liftIO $ step "Visiting 3rd breakpoint"
        goToNextBreakpoint
        N.frozen do
          isStatus <$> N.curSnapshot @@?= InterpretRunning EventExpressionPreview
          view N.slStart <<$>> N.getExecutedPosition @@?= Just (SrcPos (Pos 5) (Pos 3))

        -- TODO: uncomment this when visiting multiple source locations is available in morley-debugger

        -- lift $ step "Visiting 1st breakpoint"
        -- N.continueUntilBreakpoint N.NextBreak
        -- N.frozen do
        --   isStatus <$> N.curSnapshot @@?= InterpretRunning EventExpressionPreview
        --   -- here and further we don't expect any concrete column number
        --   view N.slSrcPos <<$>> N.getExecutedPosition @@?= Just (SrcPos (Pos 2) (Pos 11))

        -- lift $ step "Visiting 1st breakpoint after expr evaluation"
        -- N.continueUntilBreakpoint N.NextBreak
        -- N.frozen do
        --   -- here and further we don't expect any concrete column number
        --   view N.slSrcPos <<$>> N.getExecutedPosition @@?= Just (SrcPos (Pos 2) (Pos 11))
        --   isStatus <$> N.curSnapshot @@? has (_InterpretRunning . _EventExpressionEvaluated)

        -- lift $ step "Visiting 2nd breakpoint"
        -- N.continueUntilBreakpoint N.NextBreak
        -- N.frozen do
        --   isStatus <$> N.curSnapshot @@?= InterpretRunning EventExpressionPreview
        --   view N.slSrcPos <<$>> N.getExecutedPosition @@?= Just (SrcPos (Pos 5) (Pos 3))

        -- lift $ step "Visiting back 1st breakpoint"
        -- N.reverseContinue N.NextBreak
        -- N.frozen do
        --   isStatus <$> N.curSnapshot
        --     @@? has (_InterpretRunning . _EventExpressionEvaluated)
        --   view N.slSrcPos <<$>> N.getExecutedPosition @@?= Just (SrcPos (Pos 2) (Pos 11))

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
        N.switchBreakpoint (MSFile file) (SrcPos (Pos 5) (Pos 0))
        N.switchBreakpoint (MSFile file) (SrcPos (Pos 7) (Pos 0))
        N.switchBreakpoint (MSFile file) (SrcPos (Pos 8) (Pos 0))
        N.switchBreakpoint (MSFile file) (SrcPos (Pos 9) (Pos 0))

        N.switchBreakpoint (MSFile nestedFile) (SrcPos (Pos 8) (Pos 0))
        N.switchBreakpoint (MSFile nestedFile) (SrcPos (Pos 13) (Pos 0))
        N.switchBreakpoint (MSFile nestedFile) (SrcPos (Pos 18) (Pos 0))

        N.switchBreakpoint (MSFile nestedFile2) (SrcPos (Pos 1) (Pos 0))

        liftIO $ step "Go to first breakpoint"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcPos (Pos 5) (Pos 2))
              (SrcPos (Pos 5) (Pos 26))
            )

        liftIO $ step "Calculate arguments"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcPos (Pos 5) (Pos 21))
              (SrcPos (Pos 5) (Pos 25))
            )

        liftIO $ step "Go to next breakpoint (switch file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcPos (Pos 14) (Pos 5))
              (SrcPos (Pos 14) (Pos 10))
            )

        liftIO $ step "Go to next breakpoint (go to start file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcPos (Pos 7) (Pos 2))
              (SrcPos (Pos 7) (Pos 30))
            )

        liftIO $ step "Calculate arguments"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcPos (Pos 7) (Pos 23))
              (SrcPos (Pos 7) (Pos 29))
            )

        replicateM_ 4 do
          liftIO $ step "Go to next breakpoint (nested file)"
          goToNextBreakpoint
          N.frozen do
            N.getExecutedPosition @@?= Just
              (N.SourceLocation
                (MSFile nestedFile)
                (SrcPos (Pos 11) (Pos 18))
                (SrcPos (Pos 11) (Pos 25))
              )

          liftIO $ step "Go to next breakpoint (outer expression)"
          goToNextBreakpoint
          N.frozen do
            N.getExecutedPosition @@?= Just
              (N.SourceLocation
                (MSFile nestedFile)
                (SrcPos (Pos 11) (Pos 18))
                (SrcPos (Pos 11) (Pos 28))
              )

        liftIO $ step "Go to next breakpoint (go to start file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcPos (Pos 8) (Pos 2))
              (SrcPos (Pos 8) (Pos 45))
            )

        liftIO $ step "Calculate arguments"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcPos (Pos 8) (Pos 27))
              (SrcPos (Pos 8) (Pos 44))
            )

        liftIO $ step "Calculate arguments for \"what\""
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcPos (Pos 9) (Pos 74))
              (SrcPos (Pos 9) (Pos 79))
            )

        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcPos (Pos 9) (Pos 67))
              (SrcPos (Pos 9) (Pos 72))
            )

        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile file)
              (SrcPos (Pos 9) (Pos 64))
              (SrcPos (Pos 9) (Pos 79))
            )

        liftIO $ step "Calculate arguments for \"strange\""
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcPos (Pos 18) (Pos 80))
              (SrcPos (Pos 18) (Pos 87))
            )

        liftIO $ step "Go to next breakpoint (more nested file)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile2)
              (SrcPos (Pos 1) (Pos 2))
              (SrcPos (Pos 1) (Pos 19))
            )

        liftIO $ step "Go to next breakpoint (go back)"
        goToNextBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile)
              (SrcPos (Pos 18) (Pos 57))
              (SrcPos (Pos 18) (Pos 64))
            )

        liftIO $ step "Go to previous breakpoint"
        goToPreviousBreakpoint
        N.frozen do
          N.getExecutedPosition @@?= Just
            (N.SourceLocation
              (MSFile nestedFile2)
              (SrcPos (Pos 1) (Pos 2))
              (SrcPos (Pos 1) (Pos 19))
            )
  ]
