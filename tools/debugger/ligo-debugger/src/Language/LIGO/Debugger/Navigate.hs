module Language.LIGO.Debugger.Navigate
  ( isAtBreakpoint
  , LigoStepGranularity
  , processLigoStep
  ) where

import Control.Lens (ix)
import Data.Set qualified as Set

import Morley.Debugger.Core
  (DebugSource (..), DebuggerState (..), Direction (..), FrozenPredicate (..), HistoryReplay,
  MovementResult, NavigableSnapshot, NavigableSnapshotWithMethods, SourceLocation (..), dsSources,
  getExecutedPosition, move, moveOutsideMethod, moveSkippingMethods, moveTill)
import Morley.Debugger.DAP.Types (StepCommand (..))
import Morley.Michelson.ErrorPos (Pos (..), SrcPos (..))

-- | Whether are we at breakpoint.
--
-- We introduce our custom function compared to morley-debugger since its
-- semantics is inconvenient for us.
isAtBreakpoint
  :: (MonadState (DebuggerState is) m, NavigableSnapshot is)
  => FrozenPredicate (DebuggerState is) m
isAtBreakpoint = FrozenPredicate $ fmap isJust . runMaybeT $ do
  Just curLoc <- getExecutedPosition
  sources <- view dsSources
  Just sourceInfo <- pure $ sources ^? ix (_slPath curLoc)
  let curLine = srcLine (_slStart curLoc)

  -- Find if any breakpoint is at the same line as we are
  Just nextPosAfterBreakpoint <-
        pure $ Set.lookupGT (SrcPos curLine (Pos 0)) (_dsBreakpoints sourceInfo)
  guard (srcLine nextPosAfterBreakpoint == curLine)

  -- TODO [LIGO-543]: something that we want to change
type LigoStepGranularity = ()

processLigoStep
  :: (HistoryReplay is m, NavigableSnapshotWithMethods is)
  => StepCommand LigoStepGranularity
  -> m MovementResult
processLigoStep = \case
  CNext dir () -> moveSkippingMethods dir
  CStepIn () -> move Forward
  CStepOut () -> moveOutsideMethod
  CContinue dir -> moveTill dir isAtBreakpoint
