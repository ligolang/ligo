{-# LANGUAGE UndecidableInstances #-}

-- | Evaluation of snapshots in LIGO code execution.
module Language.LIGO.Debugger.Snapshots
  ( StackItem (..)
  , StackFrame (..)
  , InterpretStatus (..)
  , EventExpressionReason (..)
  , InterpretEvent (..)
  , statusExpressionEvaluatedP
  , InterpretSnapshot (..)
  , LambdaMeta
  , LambdaMeta' (..)
  , ContractEnv
  , CollectorState (..)
  , InterpretHistory (..)
  , EmbeddedLigoMeta
  , runInstrCollect
  , runCollectInterpretSnapshots
  , collectInterpretSnapshots
  , makeConciseSnapshots

    -- * Lenses
  , siLigoDescL
  , siValueL

  , sfNameL
  , sfLocL
  , sfStackL

  , isStatusL
  , isStackFramesL

  , csInterpreterStateL
  , csStackFramesL
  , csActiveStackFrameL

  , _InterpretRunning
  , _InterpretTerminatedOk
  , _InterpretFailed

  , _EventFacedStatement
  , _EventExpressionPreview
  , _EventExpressionEvaluated
  ) where

import Control.Lens
  (At (at), Each (each), Ixed (ix), Zoom (zoom), lens, makeLensesWith, makePrisms, (%=), (.=),
  (<<.=), (?=))
import Control.Lens.Prism (_Just)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.RWS.Strict (RWST (..))
import Data.Conduit (ConduitT)
import Data.Conduit qualified as C
import Data.Conduit.Lazy (MonadActive, lazyConsume)
import Data.Conduit.Lift qualified as CL
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List ((\\))
import Data.List.NonEmpty (cons)
import Data.Text qualified as Text
import Data.Vinyl (Rec (..))
import Fmt.Buildable (Buildable, build, pretty)
import Text.Interpolation.Nyan hiding (rmode')
import UnliftIO (MonadUnliftIO, throwIO)
import Util

import Morley.Debugger.Core.Common (fromCanonicalLoc)
import Morley.Debugger.Core.Navigate
  (Direction (Backward), MonadWriter, MovementResult (HitBoundary), NavigableSnapshot (..),
  NavigableSnapshotWithMethods (..), SnapshotEdgeStatus (..),
  SnapshotEndedWith (SnapshotEndedWithFail, SnapshotEndedWithOk), curSnapshot, frozen, moveRaw,
  unfreezeLocally)
import Morley.Debugger.Core.Snapshots
  (DebuggerFailure (DebuggerInfiniteLoop), FinalStack (ContractFinalStack, ViewFinalStack),
  InterpretHistory (..))
import Morley.Michelson.ErrorPos (ErrorSrcPos (ErrorSrcPos), Pos (Pos), SrcPos (SrcPos))
import Morley.Michelson.Interpret
  (ContractEnv' (ceMaxSteps), InstrRunner, InterpreterState (InterpreterState),
  InterpreterStateMonad (..), MichelsonFailed (MichelsonExt, MichelsonFailedWith),
  MichelsonFailureWithStack (MichelsonFailureWithStack, mfwsErrorSrcPos, mfwsFailed),
  MorleyLogsBuilder, NoStkElMeta (NoStkElMeta), StkEl (MkStkEl, StkEl), isRemainingSteps,
  runInstrImpl)
import Morley.Michelson.Runtime.Dummy (dummyBigMapCounter, dummyGlobalCounter)
import Morley.Michelson.TypeCheck.Helpers (handleError)
import Morley.Michelson.Typed as T
import Morley.Util.Lens (postfixLFields)

import Duplo (leq)

import Language.LIGO.AST (LIGO, Lang (Caml))
import Language.LIGO.Debugger.CLI
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Error
import Language.LIGO.Debugger.Functions
import Language.LIGO.Parser (ParsedInfo)
import Language.LIGO.Range (HasRange (getRange), LigoPosition (LigoPosition), Range (..))
import Language.LIGO.Scope

-- | Stack element, likely with an associated variable.
data StackItem u = StackItem
  { siLigoDesc :: LigoStackEntry u
  , siValue :: SomeValue
  } deriving stock (Generic)
    deriving anyclass (Buildable)

deriving stock instance (Show (LigoTypeF u)) => Show (StackItem u)
deriving stock instance Eq (StackItem 'Concise)

-- | Stack frame provides information about execution in some scope.
--
-- This includes the currently executed instruction,
-- portion of stack with variables available in that scope.
--
-- When we execute a function call, the current stack frame gets frozen and
-- a new one is added for the scope of that function call.
data StackFrame u = StackFrame
  { sfName :: Text
    -- ^ Stack frame name.
  , sfLoc :: Range
    -- ^ Source location related to the current snapshot
    -- (and referred by 'sfInstrNo').
  , sfStack :: [StackItem u]
    -- ^ Ligo stack available at the current position of this stack frame.
    -- Top of the stack goes first.
  } deriving stock (Generic)
    deriving anyclass (Buildable)

deriving stock instance (Show (LigoTypeF u)) => Show (StackFrame u)
deriving stock instance Eq (StackFrame 'Concise)

-- | Snapshot type, depends on which event has triggered the snapshot
-- recording.
data InterpretStatus
    -- | Interpretation is in progress, we made a snapshot because of
    -- the given event.
  = InterpretRunning InterpretEvent

    -- | Termination finished successfully.
  | InterpretTerminatedOk FinalStack

    -- | Interpretation failed.
  | InterpretFailed (MichelsonFailureWithStack DebuggerFailure)

  deriving stock (Show, Eq)

instance Buildable InterpretStatus where
  build = \case
    InterpretRunning ev -> "running / " <> build ev
    InterpretTerminatedOk{} -> "terminated ok"
    InterpretFailed err -> "failed with " <> build err

-- | Type of the expression that we met.
data EventExpressionReason
  -- | Just a regular expression. We'll stop at it
  -- only with at least @GExp@ granularity
  = GeneralExpression
  -- | Function call. We stop at it always while
  -- using @StepIn@ action.
  | FunctionCall
  deriving stock (Show, Eq)

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
  | EventExpressionPreview EventExpressionReason

    -- | We have evaluated expression, with the given result.
    --
    -- 'ExpressionPreview' does /not/ always have the enclosing
    -- 'ExpressionEvaluated' since some expressions fail.
    --
    -- Normally, this always contains some value; 'Nothing' means that
    -- something went wrong (but we don't want to crash the entire debugger).
  | EventExpressionEvaluated LigoType (Maybe SomeValue)

  deriving stock (Show, Eq)

instance Buildable InterpretEvent where
  build = \case
    EventFacedStatement ->
      "faced statement"
    EventExpressionPreview GeneralExpression ->
      "upon expression"
    EventExpressionPreview FunctionCall ->
      "upon function call"
    EventExpressionEvaluated typ mval ->
      "expression evaluated (" <> maybe "-" [int|m|#{id} : #{const $ buildType Caml typ}|] mval <> ")"

-- | Information about execution state at a point where the debugger can
-- potentially stop.
data InterpretSnapshot u = InterpretSnapshot
  { isStatus :: InterpretStatus
    -- ^ Type of snapshot.
  , isStackFrames :: NonEmpty (StackFrame u)
    -- ^ Stack frames, top-level frame goes last.
  } deriving stock (Generic)
    deriving anyclass (Buildable)

deriving stock instance (Show (LigoTypeF u)) => Show (InterpretSnapshot u)
deriving stock instance Eq (InterpretSnapshot 'Concise)

instance NavigableSnapshot (InterpretSnapshot u) where
  getExecutedPosition = do
    locRange <- sfLoc . head . isStackFrames <$> curSnapshot
    return . Just $ rangeToSourceLocation locRange
  getLastExecutedPosition = unfreezeLocally do
    moveRaw Backward >>= \case
      HitBoundary -> return Nothing
      _ -> frozen getExecutedPosition

  pickSnapshotEdgeStatus is = case isStatus is of
    InterpretRunning _ -> SnapshotIntermediate
    InterpretTerminatedOk stack -> SnapshotAtEnd (SnapshotEndedWithOk stack)
    InterpretFailed err -> SnapshotAtEnd (SnapshotEndedWithFail err)

instance NavigableSnapshotWithMethods (InterpretSnapshot u) where
  getCurMethodBlockLevel = length . isStackFrames <$> curSnapshot

-- | A key for cache map in @CollectorState@.
data CacheKey = CacheKey
  { ckVariableName :: Text
  , ckStackFrameName :: Text
  , ckFileName :: FilePath
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, Hashable)

-- | State at some point of execution, used by Morley interpreter and by
-- our snapshots collector.
data CollectorState m = CollectorState
  { csInterpreterState :: InterpreterState
    -- ^ State of the Morley interpreter.
  , csStackFrames :: NonEmpty (StackFrame 'Unique)
    -- ^ Stack frames at this point, top-level frame goes last.
  , csLastRecordedSnapshot :: Maybe (InterpretSnapshot 'Unique)
    -- ^ Last recorded snapshot.
    -- We can pick @[operation] * storage@ value from it.
  , csParsedFiles :: HashMap FilePath (LIGO ParsedInfo)
    -- ^ Parsed contracts.
  , csRecordedStatementRanges :: HashSet Range
    -- ^ Ranges of recorded statement snapshots.
  , csRecordedExpressionRanges :: HashSet Range
    -- ^ Ranges of recorded expression snapshots.
    -- Note that these ranges refer only to snapshots, that
    -- have @EventExpressionPreview@ event.
  , csLoggingFunction :: Text -> m ()
    -- ^ Function for logging some useful debugging info.
  , csLastRangeMb :: Maybe Range
    -- ^ Last range. We can use it to get
    -- the latest position where some
    -- failure occurred.
  , csLambdaLocs :: HashSet Range
    -- ^ Locations for lambdas. We need them to ignore statements recording
    -- in cases when some location is present in this set and it is not
    -- associated with @LAMBDA@ instruction.
  , csCheckStepsAmount :: Bool
    -- ^ We have a max steps field in the settings. If it's blank
    -- then we perform an infinite amount of steps.
  , csRecordedFirstTime :: Bool
    -- ^ A flag which indicates whether we recorded a location
    -- with @EventExpressionPreview@/@EventFacedStatement@ event
    -- for a first time.
    --
    -- We need it because we want to record a paired @EventExpressionEvaluated@
    -- for the same location.
  , csLigoTypesVec :: LigoTypesVec
    -- ^ Vector with LIGO types. It's needed to map @LigoTypeRef@ into bare @LigoType@.
  , csScopes :: HashMap FilePath [Scope]
    -- ^ Scopes for each file.
  , csVariablesCache :: HashMap CacheKey (StackItem 'Unique)
    -- ^ A cache with stack items. When a variable becomes unused then it
    -- disappears from the variables pane. We can fill the snapshot with
    -- these variables by using this cache and scopes above.
  }

makeLensesWith postfixLFields ''StackItem
makeLensesWith postfixLFields ''StackFrame
makeLensesWith postfixLFields ''InterpretSnapshot
makeLensesWith postfixLFields ''CollectorState

makeConciseSnapshots :: LigoTypesVec -> InterpretSnapshot 'Unique -> InterpretSnapshot 'Concise
makeConciseSnapshots vec snap =
  snap & isStackFramesL . each . sfStackL . each . siLigoDescL %~ makeConciseLigoStackEntry vec

-- | Lens giving an access to the top-most frame - which is also
-- the only active one.
csActiveStackFrameL :: Lens' (CollectorState m) (StackFrame 'Unique)
csActiveStackFrameL = csStackFramesL . __head
  where
    __head :: Lens' (NonEmpty a) a
    __head = lens head setHead
      where
        setHead :: NonEmpty a -> a -> NonEmpty a
        setHead (_ :| xs) x' = x' :| xs

type ContractEnv m = ContractEnv' (CollectingEvalOp m)

-- | Our monadic stack, allows running interpretation and making snapshot
-- records.
newtype CollectingEvalOp m a = CollectingEvalOp
  -- Including ConduitT to build snapshots sequence lazily.
  -- Normally ConduitT lies on top of the stack, but here we put it under
  -- ExceptT to make it record things even when a failure occurs.
  { runCollectingEvalOp :: ExceptT (MichelsonFailureWithStack DebuggerFailure)
      (ConduitT () (InterpretSnapshot 'Unique)
      (RWST (ContractEnv m) MorleyLogsBuilder (CollectorState m) m)) a
  }
  deriving newtype
    ( MonadError (MichelsonFailureWithStack DebuggerFailure)
    , MonadState (CollectorState m)
    , MonadWriter MorleyLogsBuilder
    , MonadReader (ContractEnv m)
    , Monad
    , Applicative
    , Functor
    )

instance MonadTrans CollectingEvalOp where
  lift = CollectingEvalOp . lift . lift . lift

instance (Monad m) => InterpreterStateMonad (CollectingEvalOp m) where
  stateInterpreterState f = CollectingEvalOp $
    lift $ lift $ zoom csInterpreterStateL $ state f

makePrisms ''InterpretStatus
makePrisms ''InterpretEvent

statusExpressionEvaluatedP :: Traversal' InterpretStatus SomeValue
statusExpressionEvaluatedP = _InterpretRunning . _EventExpressionEvaluated . _2 . _Just

logMessage :: (Monad m) => (ForInternalUse => Text) -> CollectingEvalOp m ()
logMessage msg = logMessageM (pure msg)

logMessageM :: (Monad m) => (ForInternalUse => CollectingEvalOp m Text) -> CollectingEvalOp m ()
logMessageM mkMsg = do
  logger <- use csLoggingFunctionL
  msg <- itIsForInternalUse mkMsg
  lift $ logger [int||[SnapshotCollecting] #{msg}|]

-- | Executes the code and collects snapshots of execution.
runInstrCollect :: forall m. (Monad m) => InstrRunner NoStkElMeta (CollectingEvalOp m)
runInstrCollect = \instr oldStack -> michFailureHandler `handleError` do
  let (embeddedMetaMb, inner) = getMetaMbAndUnwrap instr

  whenJust embeddedMetaMb \embeddedMeta -> do
    logMessage
      [int||
        Got meta: #{embeddedMeta}
        for instruction: #{instr}
      |]

    whenJust (liiLocation embeddedMeta) \loc -> do
      contracts <- use csParsedFilesL

      logMessage
        [int||
          Would be ignored: #{shouldIgnoreMeta loc inner contracts}
        |]

  ligoTypesVec <- use csLigoTypesVecL

  let stack = maybe oldStack (embedFunctionNames ligoTypesVec oldStack) (liiEnvironment =<< embeddedMetaMb)

  preExecutedStage embeddedMetaMb inner stack
  newStack <- surroundExecutionInner embeddedMetaMb (runInstrImpl runInstrCollect) inner stack
  postExecutedStage embeddedMetaMb inner newStack <* (csRecordedFirstTimeL .= False)
  where
    michFailureHandler :: MichelsonFailureWithStack DebuggerFailure -> CollectingEvalOp m a
    michFailureHandler err = use csLastRangeMbL >>= \case
      Nothing -> throwError err
      Just (ligoPositionToSrcLoc . _rStart -> lastSrcLoc)
        -> throwError err { mfwsErrorSrcPos = ErrorSrcPos $ fromCanonicalLoc lastSrcLoc }

    -- What is done upon executing instruction.
    preExecutedStage
      :: Maybe EmbeddedLigoMeta
      -> Instr i o
      -> Rec (StkEl meta) i
      -> CollectingEvalOp m ()
    preExecutedStage embeddedMetaMb instr stack = case embeddedMetaMb of
      Just LigoIndexedInfo{..} -> do
        whenJust liiLocation \loc -> do
          fillSnapshotWithCachedValues loc
          statements <- getStatements instr loc

          forM_ statements \statement -> do
            unlessM (HS.member statement <$> use csRecordedStatementRangesL) do
              recordSnapshot statement EventFacedStatement
              csRecordedStatementRangesL %= HS.insert statement

          contracts <- use csParsedFilesL
          lastLoc <- use csLastRangeMbL

          recordedExpressionLocs <- use csRecordedExpressionRangesL

          unless (shouldIgnoreMeta loc instr contracts || HS.member loc recordedExpressionLocs) do
            csRecordedExpressionRangesL %= HS.insert loc
            csRecordedFirstTimeL .= True

            -- There is no reason to record a snapshot with @EventExpressionPreview@
            -- event if we already recorded a @statement@ snapshot with the same location.
            unless (lastLoc == Just loc) do
              let eventExpressionReason =
                    if isLocationForFunctionCall loc contracts
                    then FunctionCall
                    else GeneralExpression

              recordSnapshot loc (EventExpressionPreview eventExpressionReason)

        whenJust liiEnvironment \env -> do
          -- Here stripping occurs, as the second list keeps the entire stack,
          -- while the first list (@env@) - only stack related to the current
          -- stack frame. And this is good.
          let stackHere = zipWith StackItem env (refineStack stack)
          logMessage
            [int||
              Stack at preExecutedStage: #{stackHere}
            |]

          -- Lets put these values in cache.
          stackFrameName <- use (csActiveStackFrameL . sfNameL)
          forM_ stackHere \item@StackItem{..} ->
            case siLigoDesc of
              LigoStackEntry (LigoExposedStackEntry (Just (LigoVariable (Name name))) _ (Just fileName)) ->
                csVariablesCacheL %= HM.insert (CacheKey name stackFrameName fileName) item
              _ -> pass

          csActiveStackFrameL . sfStackL .= stackHere

      Nothing -> pass

    -- What is done right after the instruction is executed.
    postExecutedStage
      :: Maybe EmbeddedLigoMeta
      -> Instr i o
      -> Rec (StkEl meta) o
      -> CollectingEvalOp m (Rec (StkEl meta) o)
    postExecutedStage embeddedMetaMb instr newStack = case embeddedMetaMb of
      Just LigoIndexedInfo{..} -> do
        whenJust liiLocation \loc -> do
          -- `location` point to instructions that end expression evaluation,
          -- we can record the computed value
          let evaluatedVal = safeHead (refineStack newStack)

          logMessage
            [int||
              Just evaluated: #{evaluatedVal}
            |]

          contracts <- use csParsedFilesL

          ligoTypesVec <- use csLigoTypesVecL

          whenM (use csRecordedFirstTimeL) do
            unless (shouldIgnoreMeta loc instr contracts) do
              recordSnapshot loc (EventExpressionEvaluated (ligoTypesVec `readLigoType` join liiSourceType) evaluatedVal)

        pure newStack
      Nothing -> pure newStack

    -- When we want to go inside a function call or loop-like thing (e,g, @for-of@ or @while@)
    -- we need to clean up remembered locations and after interpreting restore them.
    wrapAction :: Instr i o -> CollectingEvalOp m (Rec (StkEl meta) o) -> CollectingEvalOp m (Rec (StkEl meta) o)
    wrapAction instr act
      | isExecOrLoopLike instr = do
          -- <<.= sets a variable and returns its previous value (yeah, it's a bit confusing)
          oldExprRanges <- csRecordedExpressionRangesL <<.= HS.empty
          oldStatementRanges <- csRecordedStatementRangesL <<.= HS.empty

          stack <- wrappedAct

          csRecordedExpressionRangesL .= oldExprRanges
          csRecordedStatementRangesL .= oldStatementRanges

          pure stack
      | otherwise = wrappedAct
      where
        wrappedAct = do
          oldRecordedFirstTime <- csRecordedFirstTimeL <<.= False
          stack <- act
          csRecordedFirstTimeL .= oldRecordedFirstTime
          pure stack

        isExecOrLoopLike = \case
          EXEC{} -> True
          LOOP{} -> True
          LOOP_LEFT{} -> True
          ITER{} -> True
          _ -> False

    -- What is done both before and after the instruction is executed.
    -- This function is executed after 'preExecutedStage' and before 'postExecutedStage'.
    surroundExecutionInner
      :: Maybe EmbeddedLigoMeta
      -> (Instr i o -> Rec (StkEl meta) i -> CollectingEvalOp m (Rec (StkEl meta) o))
      -> Instr i o
      -> Rec (StkEl meta) i
      -> CollectingEvalOp m (Rec (StkEl meta) o)
    surroundExecutionInner _embeddedMetaMb runInstr instr stack = wrapAction instr $
      case (instr, stack) of

        -- We're on a way to execute a function.
        -- Let's get our created meta from executed lambda
        -- and create necessary stack frames.
        --
        -- Here and in @postExecutedStage@ we care only about
        -- @EXEC@ and don't take into account @APPLY@ because
        -- user-defined partially applied functions are generated by
        -- creating a @lambda arg1 (lambda arg2 res)@ value
        -- and using @EXEC@ after it to perform application.
        (EXEC{}, _ :& StkEl lam :& _) -> do
          let meta = getLambdaMeta lam
          logMessage
            [int||
              Meta #{meta} for lambda #{lam}
            |]

          oldStackFrames <- use csStackFramesL

          forM_ (lmAllFuncNames meta) \name -> do
            let Name uniqueName = name

            -- We don't want to create a stack frame for "Some.Module.$name"
            -- functions since they contain not so interesting for the debugging
            -- info (parameter, storage and "main" function). They will appear
            -- in the latter stack frames.
            unless (generatedMainName `Text.isSuffixOf` uniqueName) do
              let sfName = pretty name

              curStackFrame <- use csActiveStackFrameL
              let loc = curStackFrame ^. sfLocL

              let newStackFrame = StackFrame
                    { sfLoc = loc
                    , sfStack = []
                    , ..
                    }

              -- Sometimes debugging starts not in the main function (e.g. calculating top-level values).
              -- We assign to this stack frame a @<start>@ name. When we're
              -- going to do the first @EXEC@ then we're entering the main function.
              --
              -- So, we need to replace the name of the @<start>@ stack frame with
              -- actual main entrypoint one.
              let currentName = curStackFrame ^. sfNameL

              if currentName == beforeMainStackFrameName
              then csActiveStackFrameL . sfNameL .= sfName
              else csStackFramesL %= cons newStackFrame

          newStack <- runInstr instr stack

          csStackFramesL .= oldStackFrames
          logMessageM [int|m|
            Restored stack frames, new active frame: #{sfName <$> use csActiveStackFrameL}
            |]

          case newStack of
            MkStkEl m newLam@VLam{} :& stkEls -> do
              -- There might be a case when after executing function
              -- we'll get another function. In order not to lose stack frames
              -- we need to embed all future stack frame names into resulting
              -- function.
              let embeddedLam = newLam & lambdaMetaL .~ view lambdaMetaL lam
              logMessage [int|n|
                Embedding old meta
                #{view lambdaMetaL lam}
                into lambda #{embeddedLam}
                |]

              return $ MkStkEl m embeddedLam :& stkEls
            _ -> return newStack

        _ -> runInstr instr stack

    -- Fills the snapshot with values that became unused.
    --
    -- It uses the next strategy:
    -- 1. Pick the closest inner scope for the given range.
    -- 2. Pick the names on stack.
    -- 3. Fill the current snapshot with missing values.
    --
    -- We're picking the closest inner scope because it always
    -- contains the variables that are present in the given range.
    -- E.g. on the current range we can see variables "a" and "b".
    -- The closest inner scope contains a superset of ["a", "b"].
    -- So, we won't lose these variables.
    fillSnapshotWithCachedValues :: Range -> CollectingEvalOp m ()
    fillSnapshotWithCachedValues loc = do
      let fileName = _rFile loc
      scopesMb <- use (csScopesL . at fileName)

      logMessage [int||Getting cached values for #{loc}|]

      whenJust (getClosestInnerScope loc =<< scopesMb) \scope@Scope{..} -> do
        logMessage [int||Picked scope: #{scope}|]

        currentStack <- use (csActiveStackFrameL . sfStackL)
        stackFrameName <- use (csActiveStackFrameL . sfNameL)

        let nameExtractor = \case
              LigoStackEntry (LigoExposedStackEntry (Just (LigoVariable (Name name))) _ _) -> Just name
              _ -> Nothing

        let namesOnStack = mapMaybe (nameExtractor . siLigoDesc) currentStack
        let missingNames = sVariables \\ namesOnStack

        logMessage [int||Values on stack: #{namesOnStack}. Missing values: #{missingNames}|]

        cache <- use csVariablesCacheL

        let missingItems = missingNames
              & mapMaybe \name -> cache HM.!? (CacheKey name stackFrameName fileName)

        csActiveStackFrameL . sfStackL %= (++) missingItems

    -- Save a snapshot.
    --
    -- It is not strictly necessary for this function to accept instrNo and
    -- location, but that's a sanity check: if an event is not associated with
    -- some location, then it is likely not worth recording.
    recordSnapshot
      :: Range
      -> InterpretEvent
      -> CollectingEvalOp m ()
    recordSnapshot loc event = do
      whenM (use csCheckStepsAmountL) do
        rs <- isRemainingSteps <$> getInterpreterState
        if rs == 0
        then throwError
          $ MichelsonFailureWithStack
              (MichelsonExt DebuggerInfiniteLoop)
              -- We can set there a dummy location.
              -- @michFailureHandler@ will handle it properly.
              (ErrorSrcPos (SrcPos (Pos 0) (Pos 0)))
        else modifyInterpreterState \s -> s{ isRemainingSteps = rs - 1 }

      logMessage
        [int||
          Recording location #{loc}
          For event: #{event}
        |]

      csActiveStackFrameL . sfLocL .= loc
      isStackFrames <- use csStackFramesL

      let newSnap = InterpretSnapshot
            { isStatus = InterpretRunning event
            , ..
            }

      logMessage
        [int||
          Recorded snapshot: #{newSnap}
        |]

      csLastRecordedSnapshotL ?= newSnap
      csLastRangeMbL ?= loc
      CollectingEvalOp $ lift $ C.yield newSnap

    getStatements :: Instr i o -> Range -> CollectingEvalOp m [Range]
    getStatements instr range@Range{..} = do
      {-
        Here we need to clarify some implementation moments.

        For each @range@ we're trying to find the nearest scope
        (like function call or loop). With loops everything seems straightforward
        but for functions we're doing one trick.

        In LIGO we can define functions in 2 ways:
        1. Binding definition (like @let foo (a, b : int * int) = a + b@)
        2. Lambda definition (like @let foo = fun (a, b : int * int) -> a + b@)

        Both definitions compile to @LAMBDA@ instr with some source location, but
        for (1) we'll have location for function arguments and for (2) location for the whole
        @fun .. -> ..@ body.

        In (1) case we wan't to record a statement location for not top-level function assignment and
        at this moment we can do this only with these arguments locations, so, we can't just ignore them.
        Moreover, these argument locations appear in the @LAMBDA@s body.

        This is handled by matching on the current instruction. If it's a @LAMBDA@ then we'll record statements.
        If it's not a @LAMBDA@, but range is associated with some @LAMBDA@ instr then we'll just return empty
        statements list.
      -}

      let isLambda =
            case instr of
              Nested LAMBDA{} -> True
              Nested (LAMBDA{} :# _) -> True
              _ -> False

      parsedLigo <-
        fromMaybe
          (error [int||File #{_rFile} is not parsed for some reason|])
          <$> use (csParsedFilesL . at _rFile)

      isLambdaLoc <- HS.member range <$> use csLambdaLocsL

      if isLambdaLoc && not isLambda
      then pure []
      else do
        let statements = filterAndReverseStatements isLambda isLambdaLoc (getRange parsedLigo) $ spineAtPoint range parsedLigo

        pure $ getRange <$> statements
      where
        -- Here we're looking for statements and the nearest scope locations.
        -- These statements are filtered by strict inclusivity of their ranges to this scope.
        filterAndReverseStatements :: Bool -> Bool -> Range -> [LIGO ParsedInfo] -> [LIGO ParsedInfo]
        filterAndReverseStatements isLambda = \isLambdaLoc startRange nodes ->
          let (statements, scopeRange) = usingState startRange $ go [] nodes isLambdaLoc False
          in filter (\(getRange -> stmtRange) -> stmtRange `leq` scopeRange && stmtRange /= scopeRange) statements
          where
            go :: [LIGO ParsedInfo] -> [LIGO ParsedInfo] -> Bool -> Bool -> State Range [LIGO ParsedInfo]
            go acc nodes isLambdaLoc ignore =
              tryToProcessLigoStatement
                onSuccess
                onFail
                (pure acc)
                nodes
              where
                -- Note, that @BFunction@ binding not always is a scope. Sometimes we want
                -- to treat it as a statement. From observations we'll see that it's a statement
                -- if and only if it's discovered as a first statement that cover the given range.

                onSuccess x xs =
                  let newAcc
                        -- We want to ignore body location if it's assigned
                        -- to @LAMBDA@ instr.
                        | getRange x == range && isLambda = acc
                        | otherwise = x : acc
                  in
                  decide True x (go newAcc xs)
                onFail x xs = decide False x (go acc xs)

                decide :: Bool -> LIGO ParsedInfo -> (Bool -> Bool -> State Range [LIGO ParsedInfo]) -> State Range [LIGO ParsedInfo]
                decide isOnSuccess x cont
                  | ignore = cont isLambdaLoc ignore
                  | getRange x /= range && isScopeForStatements isLambdaLoc x
                      = put (getRange x) >> cont isLambdaLoc True
                  | otherwise =
                      let
                        isFunctionAssignment = isScopeForStatements (not isLambdaLoc) x && isOnSuccess
                      in cont (isLambdaLoc && not isFunctionAssignment) ignore

runCollectInterpretSnapshots
  :: (MonadUnliftIO m, MonadActive m)
  => CollectingEvalOp m FinalStack
  -> ContractEnv m
  -> CollectorState m
  -> Value st
  -> m (InterpretHistory (InterpretSnapshot 'Unique))
runCollectInterpretSnapshots act env initSt initStorage =
  -- This should be safe because we yield at least one snapshot in the end
  InterpretHistory . fromList <$>
  lazyConsume do
    (outcome, endState, _) <- CL.runRWSC env initSt $ runExceptT $ runCollectingEvalOp act
    case outcome of
      Left stack -> do
        case mfwsFailed stack of
          MichelsonFailedWith val -> do
            whenJust (replacementErrorValueToException val) \exc -> do
              throwIO exc
          _ -> pass

        let stackFrames = case csLastRangeMb endState of
              Nothing -> csStackFrames endState
              Just range -> csStackFrames endState & ix 0 . sfLocL .~ range

        C.yield InterpretSnapshot
          { isStatus = InterpretFailed stack
          , isStackFrames = stackFrames
          }

      Right finalStack -> do
        let lastValue =
              case finalStack of
                ContractFinalStack (StkEl x :& RNil) -> SomeValue x
                ViewFinalStack (StkEl x :& RNil) -> SomeValue x

        let stackItemWithOpsAndStorage = StackItem
              { siLigoDesc = LigoStackEntry $ LigoExposedStackEntry Nothing Nothing Nothing
              , siValue = lastValue
              }
        let oldStorage = StackItem
              { siLigoDesc = LigoStackEntry $ LigoExposedStackEntry Nothing Nothing Nothing
              , siValue = withValueTypeSanity initStorage (SomeValue initStorage)
              }

        lastSnap <-
          maybe
            (throwIO $ ImpossibleHappened "No snapshots were recorded while interpreting Michelson code")
            pure
            (csLastRecordedSnapshot endState)

        let isStackFrames = (lastSnap ^. isStackFramesL)
              & ix 0 . sfStackL .~ [stackItemWithOpsAndStorage, oldStorage]

        C.yield InterpretSnapshot
          { isStatus = InterpretTerminatedOk finalStack
          , ..
          }

-- | Sometimes before calling an entrypoint
-- we may step through some top-levels. It would be
-- convenient to assign this name to the first stack frame.
beforeMainStackFrameName :: Text
beforeMainStackFrameName = "<start>"

-- | Execute contract similarly to 'interpret' function, but in result
-- produce an entire execution history.
collectInterpretSnapshots
  :: forall m cp st arg.
     (MonadUnliftIO m, MonadActive m)
  => FilePath
  -> Contract cp st
  -> EntrypointCallT cp arg
  -> Value arg
  -> Value st
  -> ContractEnv m
  -> HashMap FilePath (LIGO ParsedInfo)
  -> (Text -> m ())
  -> HashSet Range
  -> Bool -- ^ should we track steps amount
  -> LigoTypesVec
  -> HashMap FilePath [Scope]
  -> m (InterpretHistory (InterpretSnapshot 'Unique))
collectInterpretSnapshots
  mainFile
  Contract{..}
  epc
  param
  initStore
  env
  parsedContracts
  logger
  lambdaLocs
  trackMaxSteps
  ligoTypesVec
  scopesMap =
    runCollectInterpretSnapshots
      (ContractFinalStack <$> runInstrCollect (stripDuplicates $ unContractCode cCode) initStack)
      env
      collSt
      initStore
    where
      initStack = MkStkEl NoStkElMeta (T.VPair (liftCallArg epc param, initStore)) :& RNil
      initSt =
        InterpreterState (ceMaxSteps env)
          dummyGlobalCounter dummyBigMapCounter
      collSt = CollectorState
        { csInterpreterState = initSt
        , csStackFrames = one StackFrame
            { sfName = beforeMainStackFrameName
            , sfStack = []
            , sfLoc = Range
              { _rFile = mainFile
              , _rStart = LigoPosition 1 1
              , _rFinish = LigoPosition 1 1
              }
            }
        , csLastRecordedSnapshot = Nothing
        , csParsedFiles = parsedContracts
        , csRecordedStatementRanges = HS.empty
        , csRecordedExpressionRanges = HS.empty
        , csLoggingFunction = logger
        , csLastRangeMb = Nothing
        , csLambdaLocs = lambdaLocs
        , csCheckStepsAmount = trackMaxSteps
        , csRecordedFirstTime = False
        , csLigoTypesVec = ligoTypesVec
        , csScopes = scopesMap
        , csVariablesCache = HM.empty
        }

      -- Strip duplicate locations.
      --
      -- In practice it happens that LIGO produces snapshots for intermediate
      -- computations. For instance, @a > 10@ will translate to @COMPARE; GT@,
      -- both having the same @location@ meta; we don't want the user to
      -- see that.
      stripDuplicates :: forall i o. Instr i o -> Instr i o
      stripDuplicates = evaluatingState Nothing . dfsTraverseInstr def{ dsGoToValues = True, dsCtorEffectsApp = recursionImpl }
        where
          -- Note that this dfs is implemented in such a way that
          -- it applies actions in bottom-up manner.
          recursionImpl :: CtorEffectsApp $ State (Maybe Range)
          recursionImpl = CtorEffectsApp "Strip duplicates" $ flip \mkNewInstr -> \case
            ConcreteMeta (embeddedMeta :: EmbeddedLigoMeta) _ -> case liiLocation embeddedMeta of
              Just loc ->
                ifM ((== Just loc) <$> get)
                  do
                    -- @mkNewInstr@ will return meta-wrapped instruction after traversal.
                    -- We in this branch we should replace location meta with @Nothing@.
                    mkNewInstr >>= \case
                      ConcreteMeta (_ :: EmbeddedLigoMeta) inner'
                        -> pure $ Meta (SomeMeta $ embeddedMeta & liiLocationL .~ Nothing) inner'
                      other -> pure other
                  do
                    put (Just loc)
                    mkNewInstr
              _ -> mkNewInstr
            _ -> mkNewInstr
