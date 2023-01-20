module Language.LIGO.Debugger.Navigate
  ( isAtBreakpoint
  , LigoStepGranularity (..)
  , allLigoStepGranularities
  , processLigoStep
  ) where

import Control.Lens (ix)
import Data.Default (Default (..))
import Data.Set qualified as Set
import Fmt (Buildable (..))

import Morley.Debugger.Core.Common
import Morley.Debugger.Core.Navigate hiding (isAtBreakpoint)
import Morley.Debugger.DAP.Types (StepCommand, StepCommand' (..))

import Language.LIGO.Debugger.Snapshots

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
  let curLine = slLine (_slStart curLoc)

  -- Find if any breakpoint is at the same line as we are
  Just nextPosAfterBreakpoint <-
        pure $ Set.lookupGT (SrcLoc curLine 0) (_dsBreakpoints sourceInfo)
  guard (slLine nextPosAfterBreakpoint == curLine)

-- | Stepping granularity allowed in our debugger.
data LigoStepGranularity
  = GStmt
    -- ^ Statement granularity.
  | GExp
    -- ^ Expression granularity.
    -- This is 'GStmt' + stop after every sub-expression evaluation.
    -- Stops before expression if and only if it's a function call.
  | GExpExt
    -- ^ Expression-extended granularity.
    -- This is 'GStmt' + stop before & after every sub-expression evaluation.
  deriving stock (Show, Eq)

-- | All possible granularities.
allLigoStepGranularities :: [LigoStepGranularity]
allLigoStepGranularities = [GExpExt, GExp, GStmt]

instance Default LigoStepGranularity where
  def = GStmt

instance Buildable LigoStepGranularity where
  build = \case
    GStmt -> "statement"
    GExp -> "expression"
    GExpExt -> "expression (pre + post)"

-- | Whether given event is interesting with respect to the current granularity
-- (throwing away other factors related to stepping).
granularityMatchesEvent :: LigoStepGranularity -> InterpretEvent -> Bool
granularityMatchesEvent = \case
  GStmt -> \case
    EventFacedStatement{} -> True
    _ -> False

  GExp -> \case
    EventFacedStatement{} -> True
    EventExpressionPreview FunctionCall -> True
    EventExpressionPreview GeneralExpression -> False
    EventExpressionEvaluated{} -> True

  GExpExt -> const True

-- | Whether given position is interesting with respect to the current granularity
-- (throwing away other factors related to stepping).
matchesGranularityP
  :: Monad m
  => LigoStepGranularity
  -> FrozenPredicate (DebuggerState (InterpretSnapshot u)) m
matchesGranularityP gran = FrozenPredicate do
  statusMatches . isStatus <$> curSnapshot
  where
    statusMatches :: InterpretStatus -> Bool
    statusMatches = \case
      InterpretRunning event -> granularityMatchesEvent gran event
      -- key status are interesting in either way
      _ -> True

{-# ANN isFunctionCallP ("HLint: ignore Redundant <$>" :: Text) #-}

-- | Like @matchesGranularityP@ but discards the current granularity.
isFunctionCallP :: Monad m => FrozenPredicate (DebuggerState (InterpretSnapshot u)) m
isFunctionCallP = FrozenPredicate do
  isStatus <$> curSnapshot >>= \case
    InterpretRunning (EventExpressionPreview FunctionCall) -> pure True
    _ -> pure False

-- Necessary due to https://gitlab.com/morley-framework/morley/-/issues/912
{-# ANN processLigoStep ("HLint: ignore Use or" :: Text) #-}

-- | Handle a stepping command.
{- Implementation notes:

We've decided to follow several rules here:

I. We tend to stop at all visited breakpoints even if they do not match
the current granularity

Motivation:
  1. Sometimes it seems convenient - as a user, I don't have to make sure
     I've put my breakpoint at an appropriate place (e.g. statement beginning)
     to stop on it.
  2. `continue` and `reverseContinue` physically don't have granularities,
     meaning that DAP assumes granularities not being able to affect anything
     for these commands.
  3. Debuggers for other languages seem to do so too (that's just my, @martoon's,
     intuition).

II. `StepOut` ignores the current granularity and jumps at "expression evaluated"
event.

Motivation:

Imagine `let r = f() + g() + h()` statement, the user has statement granularity.
If he entered `f()`, on exit he oftentimes may want to step into `g()`.
Stopping right after `f()` lets him step into `g()` without extra hassle.

And the opposite user's scenario does not suffer much: if he doesn't want
to step into `g()` and want to go to the next expression, he will just need
to click `Step over` one extra time.

If we made `StepOut` respect the statement granularity, then in case the user
wanted to visit `g()` and clicks `StepOut`, he would appear at the next line and
would have to step back several times.

III. `StepOut` ignores the breakpoints.

Motivation:

This is subjective, but `StepOver` is a big hammer and the user should be sure
that it works with one click. If the user is forced to invoke it over and over,
there is a great chance of overstepping.

IV. `StepIn` always stops at function calls.

Motivation:

Suppose we're using a `statement` granularity. We're stepping with `Next` button and
went to `let v = f() + g()` statement. I'm interested in `g` function call and it would
be really convenient if we can step to it with one button.

V. `Expression` granularity stops at `ExpressionPreview` if and only if it's a function call.

Motivation:

Suppose we have `let v = f() + g() + h()` and we're interested in `f` function call.
We can step with `Next` till it and use `StepIn` to go inside.

-}
processLigoStep
  :: (HistoryReplay (InterpretSnapshot u) m)
  => StepCommand LigoStepGranularity
  -> m MovementResult
processLigoStep = \case
  CContinue dir ->
    moveTill dir isAtBreakpoint

  CStepIn gran ->
    moveTill Forward $ foldr1 (||) $ fromList
      [ matchesGranularityP gran
      , isAtBreakpoint
      , isFunctionCallP
      ]

  CNext dir gran -> do
    methodNestingLevelBeforeStep <- frozen getCurMethodBlockLevel
    let notWithinNestedMethod = FrozenPredicate do
          curMethodNestingLevel <- getCurMethodBlockLevel
          return (curMethodNestingLevel <= methodNestingLevelBeforeStep)
    let notWithinCurrentMethod = FrozenPredicate do
          curMethodNestingLevel <- getCurMethodBlockLevel
          return (curMethodNestingLevel < methodNestingLevelBeforeStep)

    moveTill dir $ foldr1 (||) $ fromList
      [ -- In common scenario we skip entering into methods and skip
        -- too small granularities.
        -- For instance, at statement granularity we really want to skip
        -- entire expressions having multiple function calls.
        notWithinNestedMethod && matchesGranularityP gran
        -- If we stepped out of the method, it's fine to first step at
        -- the event where our function call is evaluated.
      , notWithinCurrentMethod
        -- Any breakpoint is a good reason to interrupt.
      , isAtBreakpoint
      ]

  CStepOut _gran ->
    moveOutsideMethod

  -- Copypasted from @defaultProcessStep@
  CRestartFrame amt -> moveToStartFrame amt
