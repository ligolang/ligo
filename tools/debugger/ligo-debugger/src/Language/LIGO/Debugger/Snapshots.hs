-- | Evaluation of snapshots in LIGO code execution.
module Language.LIGO.Debugger.Snapshots
  ( StackItem (..)
  , StackFrame (..)
  , InterpretStatus (..)
  , InterpretEvent (..)
  , InterpretSnapshot (..)
  , CollectorState (..)
  , InterpretHistory (..)
  , EmbeddedLigoMeta
  , runInstrCollect
  , runCollectInterpretSnapshots
  , collectInterpretSnapshots

    -- * Lenses
  , sfNameL
  , sfLocL
  , sfStackL

  , isStatusL
  , isStackFramesL

  , csInterpreterStateL
  , csStackFramesL
  , csActiveStackFrameL

  , _InterpretStarted
  , _InterpretRunning
  , _InterpretTerminatedOk
  , _InterpretFailed

  , _EventFacedStatement
  , _EventExpressionPreview
  , _EventExpressionEvaluated
  ) where

import Control.Lens (Zoom (zoom), makeLensesWith, makePrisms, (.=))
import Control.Monad.RWS.Strict (RWST (..))
import Data.Conduit (ConduitT)
import Data.Conduit qualified as C
import Data.Conduit.Lift qualified as CL
import Data.List.NonEmpty qualified as NE
import Data.Typeable (cast)
import Data.Vinyl (Rec (..))
import Fmt (Buildable (..), genericF)
import Morley.Michelson.Interpret
  (ContractEnv, InstrRunner, InterpreterState, InterpreterStateMonad (..),
  MichelsonFailureWithStack, MorleyLogsBuilder, StkEl, initInterpreterState, mkInitStack,
  runInstrImpl, seValue)
import Morley.Michelson.Typed as T
import Morley.Util.Lens (postfixLFields)

import Morley.Debugger.Core.Navigate
  (Direction (Backward), MovementResult (ReachedBoundary), NavigableSnapshot (..),
  NavigableSnapshotWithMethods (..), SnapshotEdgeStatus (..), curSnapshot, frozen, move,
  unfreezeLocally)
import Morley.Debugger.Core.Snapshots (InterpretHistory (..))

import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Common
import Morley.Michelson.Runtime.Dummy (dummyBigMapCounter, dummyGlobalCounter)

-- | Stack element, likely with an associated variable.
data StackItem = StackItem
  { siLigoDesc :: LigoStackEntry
  , siValue :: SomeValue
  } deriving stock (Show, Eq, Generic)

instance Buildable StackItem where
  build = genericF

-- | Stack frame provides information about execution in some scope.
--
-- This includes the currently executed instruction,
-- portion of stack with variables available in that scope.
--
-- When we execute a function call, the current stack frame gets frozen and
-- a new one is added for the scope of that function call.
data StackFrame = StackFrame
  { sfName :: Text
    -- ^ Stack frame name.
  , sfLoc :: LigoRange
    -- ^ Source location related to the current snapshot
    -- (and referred by 'sfInstrNo').
  , sfStack :: [StackItem]
    -- ^ Ligo stack available at the current position of this stack frame.
  } deriving stock (Show, Eq, Generic)

instance Buildable StackFrame where
  build = genericF

-- | Snapshot type, depends on which event has triggered the snapshot
-- recording.
data InterpretStatus
    -- | Just started interpretation.
  = InterpretStarted

    -- | Interpretation is in the middle, we made a snapshot because of
    -- the given event.
  | InterpretRunning InterpretEvent

    -- | Termination finished successfully.
  | InterpretTerminatedOk

    -- | Interpretation failed.
  | InterpretFailed MichelsonFailureWithStack

  deriving stock (Show, Eq)

instance Buildable InterpretStatus where
  build = \case
    InterpretStarted -> "started"
    InterpretRunning ev -> "running / " <> build ev
    InterpretTerminatedOk -> "terminated ok"
    InterpretFailed err -> "failed with " <> build err

-- | An interesting event in interpreter that is worth a snapshot.
data InterpretEvent
    -- | Start of the new statement.
  = EventFacedStatement

    -- | We faced an expression (or sub-expression), going to evaluate it.
    --
    -- Recording such events is important:
    -- 1. There are expressions that hide large evaluation, like @EXEC@;
    -- 2. There are failing expressions.
    --
    -- If stopping at such events is undesired, they can be easily skipped later.
  | EventExpressionPreview

    -- | We have evaluated expression, with the given result.
    --
    -- 'ExpressionPreview' does /not/ always have the enclosing
    -- 'ExpressionEvaluated' since some expressions fail.
    --
    -- Normally, this always contains some value; 'Nothing' means that
    -- something went wrong (but we don't want to crash the entire debugger).
  | EventExpressionEvaluated (Maybe SomeValue)

  deriving stock (Show, Eq)

instance Buildable InterpretEvent where
  build = \case
    EventFacedStatement ->
      "faced statement"
    EventExpressionPreview ->
      "upon expression"
    EventExpressionEvaluated mval ->
      "expression evaluated (" <> maybe "-" build mval <> ")"

-- | Information about execution state at a point where the debugger can
-- potentially stop.
data InterpretSnapshot = InterpretSnapshot
  { isStatus :: InterpretStatus
    -- ^ Type of snapshot.
  , isStackFrames :: NonEmpty StackFrame
    -- ^ Stack frames, top-level frame goes last.
  } deriving stock (Show, Eq, Generic)

instance Buildable InterpretSnapshot where
  build = genericF

instance NavigableSnapshot InterpretSnapshot where
  getExecutedPosition = do
    locRange <- sfLoc . head . isStackFrames <$> curSnapshot
    return . Just $ ligoRangeToSourceLocation locRange
  getLastExecutedPosition = unfreezeLocally do
    move Backward >>= \case
      ReachedBoundary -> return Nothing
      _ -> frozen getExecutedPosition

  pickSnapshotEdgeStatus is = case isStatus is of
    InterpretStarted -> SnapshotAtStart
    InterpretRunning _ -> SnapshotIntermediate
    InterpretTerminatedOk -> SnapshotAtEnd pass
    InterpretFailed err -> SnapshotAtEnd (Left err)

instance NavigableSnapshotWithMethods InterpretSnapshot where
  getCurMethodBlockLevel = length . isStackFrames <$> curSnapshot

-- | State at some point of execution, used by Morley interpreter and by
-- our snapshots collector.
data CollectorState = CollectorState
  { csInterpreterState :: InterpreterState
    -- ^ State of the Morley interpreter.
  , csStackFrames :: NonEmpty StackFrame
    -- ^ Stack frames at this point, top-level frame goes last.
  } deriving stock (Show)

makeLensesWith postfixLFields ''StackFrame
makeLensesWith postfixLFields ''InterpretSnapshot
makeLensesWith postfixLFields ''CollectorState

-- | Lens giving an access to the bottom-most frame - which is also
-- the only active one.
csActiveStackFrameL :: Lens' CollectorState StackFrame
csActiveStackFrameL = csStackFramesL . __head
  where
    __head :: Lens' (NonEmpty a) a
    __head f (x :| xs) = (:| xs) <$> f x

-- | Our monadic stack, allows running interpretation and making snapshot
-- records.
type CollectingEvalOp =
  -- Including ConduitT to build snapshots sequence lazily.
  -- Normally ConduitT lies on top of the stack, but here we put it under
  -- ExceptT to make it record things even when a failure occurs.
  ExceptT MichelsonFailureWithStack $
  ConduitT () InterpretSnapshot $
  RWST ContractEnv MorleyLogsBuilder CollectorState $
  Identity

-- TODO: Consider making CollectingEvalOp a newtype to avoid this overlapping
-- instance.
instance {-# OVERLAPS #-} InterpreterStateMonad CollectingEvalOp where
  stateInterpreterState f =
    lift $ lift $ zoom csInterpreterStateL $ state f

stkElValue :: StkEl v -> SomeValue
stkElValue stkEl = let v = seValue stkEl in withValueTypeSanity v (SomeValue v)

-- | Executes the code and collects snapshots of execution.
runInstrCollect :: InstrRunner CollectingEvalOp
runInstrCollect = \case
  -- TODO: use ConcreteMeta from Morley once available
  instr@(T.Meta (T.SomeMeta (cast -> Just (embeddedMeta :: EmbeddedLigoMeta))) _) -> \stack -> do
    preExecutedStage embeddedMeta (refineStack stack)
    newStack <- runInstrImpl runInstrCollect instr stack
    postExecutedStage embeddedMeta (refineStack stack) (refineStack newStack)
    return newStack
  other -> runInstrImpl runInstrCollect other
  where
    -- What is done upon executing instruction.
    preExecutedStage LigoIndexedInfo{..} stack = do
      whenJust liiLocation \loc -> do
        recordSnapshot loc EventExpressionPreview

      whenJust liiEnvironment \env -> do
        -- Here stripping occurs, as the second list keeps the entire stack,
        -- while the first list (@env@) - only stack related to the current
        -- stack frame. And this is good.
        let stackHere = zipWith StackItem env stack
        csActiveStackFrameL . sfStackL .= stackHere

        -- TODO: record snapshot as soon as there is location associated
        -- with "environment" LIGO's debug info
        -- recordSnapshot (instrNo, _) EventFacedStatement

    -- What is done right after the instruction is executed.
    postExecutedStage LigoIndexedInfo{..} _oldStack newStack = do
      whenJust liiLocation \loc -> do
        -- `location` point to instructions that end expression evaluation,
        -- we can record the computed value
        let evaluatedVal = safeHead newStack
        recordSnapshot loc (EventExpressionEvaluated evaluatedVal)

    -- Save a snapshot.
    --
    -- It is not strictly necessary for this function to accept instrNo and
    -- location, but that's a sanity check: if an event is not associated with
    -- some location, then it is likely not worth recording.
    recordSnapshot
      :: LigoRange
      -> InterpretEvent
      -> CollectingEvalOp ()
    recordSnapshot loc event = do
      csActiveStackFrameL . sfLocL .= loc

      isStackFrames <- use csStackFramesL

      lift $ C.yield InterpretSnapshot
        { isStatus = InterpretRunning event
        , ..
        }

    -- Leave only information that matters in LIGO.
    refineStack :: Rec StkEl st -> [SomeValue]
    refineStack =
      -- Note: it is important for this function to be lazy if we don't
      -- want to have full copy of stack skeleton (which is sequence of `:&`)
      -- in each snapshot, that would take O(snapshots num * avg stack size) memory.
      --
      -- And 'Rec' is strict datatype, so using functions like 'rmap' would not
      -- fit our purpose.
      \case
        RNil -> []
        stkEl :& st -> stkElValue stkEl : refineStack st


runCollectInterpretSnapshots
  :: CollectingEvalOp a
  -> ContractEnv
  -> CollectorState
  -> InterpretHistory InterpretSnapshot
runCollectInterpretSnapshots act env initSt =
  InterpretHistory $
  -- This is safe because we yield at least two snapshots
  NE.fromList $
  runIdentity $
  C.sourceToList $ do
    C.yield InterpretSnapshot
      { isStackFrames = csStackFrames initSt
      , isStatus = InterpretStarted
      }
    (outcome, endState, _) <-
      CL.runRWSC env initSt $ runExceptT act
    let endStatus = either InterpretFailed (const InterpretTerminatedOk) outcome
    C.yield InterpretSnapshot
      { isStatus = endStatus
      , isStackFrames = csStackFrames endState
      }

-- | Execute contract similarly to 'interpret' function, but in result
-- produce an entire execution history.
collectInterpretSnapshots
  :: forall cp st arg.
     FilePath
  -> Text
  -> Contract cp st
  -> EntrypointCallT cp arg
  -> Value arg
  -> Value st
  -> ContractEnv
  -> InterpretHistory InterpretSnapshot
collectInterpretSnapshots mainFile entrypoint Contract{..} epc param initStore env =
  runCollectInterpretSnapshots
    (runInstrCollect cCode initStack)
    env
    collSt
  where
    initStack = mkInitStack (liftCallArg epc param) cParamNotes initStore cStoreNotes
    initSt = initInterpreterState dummyGlobalCounter dummyBigMapCounter env
    collSt = CollectorState
      { csInterpreterState = initSt
      , csStackFrames = one StackFrame
          { sfName = entrypoint
          , sfStack = []
          , sfLoc = LigoRange
            { lrFile = mainFile
            , lrStart = LigoPosition 1 0
            , lrEnd = LigoPosition 1 0
            }
          }
      }

makePrisms ''InterpretStatus
makePrisms ''InterpretEvent
