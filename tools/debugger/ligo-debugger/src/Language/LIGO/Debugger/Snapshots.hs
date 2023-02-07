-- | Evaluation of snapshots in LIGO code execution.
module Language.LIGO.Debugger.Snapshots
  ( StackItem (..)
  , StackFrame (..)
  , InterpretStatus (..)
  , InterpretEvent (..)
  , statusExpressionEvaluatedP
  , InterpretSnapshot (..)
  , LambdaMeta (..)
  , CollectorState (..)
  , InterpretHistory (..)
  , EmbeddedLigoMeta
  , runInstrCollect
  , runCollectInterpretSnapshots
  , collectInterpretSnapshots
  , stripSuffixHashFromSnapshots

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


import AST (Binding, Expr, LIGO)
import AST qualified
import Control.Lens
  (At (at), Each (each), Ixed (ix), Zoom (zoom), lens, makeLensesWith, makePrisms, (%=), (.=), (?=))
import Control.Lens.Prism (Prism', _Just)
import Control.Monad.Except (throwError)
import Control.Monad.RWS.Strict (RWST (..))
import Data.Conduit (ConduitT)
import Data.Conduit qualified as C
import Data.Conduit.Lazy (MonadActive, lazyConsume)
import Data.Conduit.Lift qualified as CL
import Data.HashSet qualified as HS
import Data.List.NonEmpty (cons)
import Data.Vinyl (Rec (..))
import Duplo (layer)
import Fmt (Buildable (..), genericF, pretty)
import Parser (ParsedInfo)
import Range (HasRange (getRange), Range (..))
import Text.Interpolation.Nyan
import UnliftIO (MonadUnliftIO, throwIO)

import Morley.Debugger.Core.Navigate
  (Direction (Backward), MovementResult (ReachedBoundary), NavigableSnapshot (..),
  NavigableSnapshotWithMethods (..), SnapshotEdgeStatus (..), curSnapshot, frozen, moveRaw,
  unfreezeLocally)
import Morley.Debugger.Core.Snapshots (DebuggerFailure, InterpretHistory (..))
import Morley.Michelson.ErrorPos (ErrorSrcPos (ErrorSrcPos))
import Morley.Michelson.Interpret
  (ContractEnv, InstrRunner, InterpreterState, InterpreterStateMonad (..),
  MichelsonFailed (MichelsonFailedWith), MichelsonFailureWithStack (mfwsErrorSrcPos, mfwsFailed),
  MorleyLogsBuilder, StkEl (StkEl), initInterpreterState, mkInitStack, runInstrImpl)
import Morley.Michelson.Runtime.Dummy (dummyBigMapCounter, dummyGlobalCounter)
import Morley.Michelson.TypeCheck.Helpers (handleError)
import Morley.Michelson.Typed as T
import Morley.Util.Lens (postfixLFields)

import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Functions

-- | Stack element, likely with an associated variable.
data StackItem u = StackItem
  { siLigoDesc :: LigoStackEntry u
  , siValue :: SomeValue
  } deriving stock (Show, Generic)

deriving stock instance Eq (StackItem 'Concise)

instance (SingI u) => Buildable (StackItem u) where
  build = genericF

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
  , sfLoc :: LigoRange
    -- ^ Source location related to the current snapshot
    -- (and referred by 'sfInstrNo').
  , sfStack :: [StackItem u]
    -- ^ Ligo stack available at the current position of this stack frame.
    -- Top of the stack goes first.
  } deriving stock (Show, Generic)

deriving stock instance Eq (StackFrame 'Concise)

instance (SingI u) => Buildable (StackFrame u) where
  build = genericF

-- | Snapshot type, depends on which event has triggered the snapshot
-- recording.
data InterpretStatus
    -- | Interpretation is in progress, we made a snapshot because of
    -- the given event.
  = InterpretRunning InterpretEvent

    -- | Termination finished successfully.
  | InterpretTerminatedOk

    -- | Interpretation failed.
  | InterpretFailed (MichelsonFailureWithStack DebuggerFailure)

  deriving stock (Show, Eq)

instance Buildable InterpretStatus where
  build = \case
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
data InterpretSnapshot u = InterpretSnapshot
  { isStatus :: InterpretStatus
    -- ^ Type of snapshot.
  , isStackFrames :: NonEmpty (StackFrame u)
    -- ^ Stack frames, top-level frame goes last.
  } deriving stock (Show, Generic)

deriving stock instance Eq (InterpretSnapshot 'Concise)

instance (SingI u) => Buildable (InterpretSnapshot u) where
  build = genericF

instance NavigableSnapshot (InterpretSnapshot u) where
  getExecutedPosition = do
    locRange <- sfLoc . head . isStackFrames <$> curSnapshot
    return . Just $ ligoRangeToSourceLocation locRange
  getLastExecutedPosition = unfreezeLocally do
    moveRaw Backward >>= \case
      ReachedBoundary -> return Nothing
      _ -> frozen getExecutedPosition

  pickSnapshotEdgeStatus is = case isStatus is of
    InterpretRunning _ -> SnapshotIntermediate
    InterpretTerminatedOk -> SnapshotAtEnd pass
    InterpretFailed err -> SnapshotAtEnd (Left err)

instance NavigableSnapshotWithMethods (InterpretSnapshot u) where
  getCurMethodBlockLevel = length . isStackFrames <$> curSnapshot

-- | An entry for each recursive function or cycle.
-- It is needed because we want to track which statement snapshots
-- were already recorded in the current iteration.
data RecursiveOrCycleEntry = RecursiveOrCycleEntry
  { roceStart :: LigoRange
    -- ^ First snapshot in cycle or recursive function
  , roceRecordedStatements :: HashSet LigoRange
    -- ^ All statements that were recorded in one iteration
  }

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
  , csRecordedRanges :: HashSet LigoRange
    -- ^ Ranges of recorded statement snapshots.
  , csRecursiveOrCycleEntries :: HashMap LigoRange RecursiveOrCycleEntry
    -- ^ @RecursiveOrCycleEntry@ for each cycle or
    -- recursive function expression.
  , csLoggingFunction :: String -> m ()
    -- ^ Function for logging some useful debugging info.
  , csMainFunctionName :: Name 'Unique
    -- ^ Name of main entrypoint.
    -- We need to store it in order not to create an extra stack frame.
  , csLastRangeMb :: Maybe LigoRange
    -- ^ Last range. We can use it to get
    -- the latest position where some
    -- failure occurred.
  }

makeLensesWith postfixLFields ''StackItem
makeLensesWith postfixLFields ''StackFrame
makeLensesWith postfixLFields ''InterpretSnapshot
makeLensesWith postfixLFields ''RecursiveOrCycleEntry
makeLensesWith postfixLFields ''CollectorState

stripSuffixHashFromSnapshots :: InterpretSnapshot 'Unique -> InterpretSnapshot 'Concise
stripSuffixHashFromSnapshots snap =
  snap & isStackFramesL . each . sfStackL . each . siLigoDescL %~ stripSuffixHashLigoStackEntry

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

-- | Our monadic stack, allows running interpretation and making snapshot
-- records.
type CollectingEvalOp m =
  -- Including ConduitT to build snapshots sequence lazily.
  -- Normally ConduitT lies on top of the stack, but here we put it under
  -- ExceptT to make it record things even when a failure occurs.
  ExceptT (MichelsonFailureWithStack DebuggerFailure) $
  ConduitT () (InterpretSnapshot 'Unique) $
  RWST ContractEnv MorleyLogsBuilder (CollectorState m) $
  m

-- TODO: Consider making CollectingEvalOp a newtype to avoid this overlapping
-- instance.
instance {-# OVERLAPS #-} (Monad m) => InterpreterStateMonad (CollectingEvalOp m) where
  stateInterpreterState f =
    lift $ lift $ zoom csInterpreterStateL $ state f

makePrisms ''InterpretStatus
makePrisms ''InterpretEvent

statusExpressionEvaluatedP :: Prism' InterpretStatus SomeValue
statusExpressionEvaluatedP = _InterpretRunning . _EventExpressionEvaluated . _Just

logMessage :: (Monad m) => String -> CollectingEvalOp m ()
logMessage str = do
  logger <- use csLoggingFunctionL
  lift $ lift $ lift $ logger [int||[SnapshotCollecting] #{str}|]

-- | Executes the code and collects snapshots of execution.
runInstrCollect :: forall m. (Monad m) => InstrRunner (CollectingEvalOp m)
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

  let stack = maybe oldStack (embedFunctionNames oldStack) (liiEnvironment =<< embeddedMetaMb)

  preExecutedStage embeddedMetaMb inner stack
  newStack <- surroundExecutionInner embeddedMetaMb (runInstrImpl runInstrCollect) inner stack
  postExecutedStage embeddedMetaMb inner newStack
  where
    michFailureHandler :: MichelsonFailureWithStack DebuggerFailure -> CollectingEvalOp m a
    michFailureHandler err = use csLastRangeMbL >>= \case
      Nothing -> throwError err
      Just (ligoPositionToSrcPos . lrStart -> lastSrcPos)
        -> throwError err { mfwsErrorSrcPos = ErrorSrcPos lastSrcPos }

    -- What is done upon executing instruction.
    preExecutedStage
      :: Maybe EmbeddedLigoMeta
      -> Instr i o
      -> Rec StkEl i
      -> CollectingEvalOp m ()
    preExecutedStage embeddedMetaMb instr stack = case embeddedMetaMb of
      Just LigoIndexedInfo{..} -> do
        whenJust liiLocation \loc -> do
          statements <- getStatements loc

          forM_ statements \statement -> do
            recordSnapshot statement EventFacedStatement
            csRecordedRangesL %= HS.insert statement

          contracts <- use csParsedFilesL

          unless (shouldIgnoreMeta loc instr contracts) do
            recordSnapshot loc EventExpressionPreview

        whenJust liiEnvironment \env -> do
          -- Here stripping occurs, as the second list keeps the entire stack,
          -- while the first list (@env@) - only stack related to the current
          -- stack frame. And this is good.
          let stackHere = zipWith StackItem env (refineStack stack)
          logMessage
            [int||
              Stack at preExecutedStage: #{stackHere}
            |]

          csActiveStackFrameL . sfStackL .= stackHere
      Nothing -> pass

    -- What is done right after the instruction is executed.
    postExecutedStage
      :: Maybe EmbeddedLigoMeta
      -> Instr i o
      -> Rec StkEl o
      -> CollectingEvalOp m (Rec StkEl o)
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

          csLastRangeMbL ?= loc

          contracts <- use csParsedFilesL

          unless (shouldIgnoreMeta loc instr contracts) do
            recordSnapshot loc (EventExpressionEvaluated evaluatedVal)

        pure newStack
      Nothing -> pure newStack

    -- What is done both before and after the instruction is executed.
    -- This function is executed after 'preExecutedStage' and before 'postExecutedStage'.
    surroundExecutionInner
      :: Maybe EmbeddedLigoMeta
      -> (Instr i o -> Rec StkEl i -> CollectingEvalOp m (Rec StkEl o))
      -> Instr i o
      -> Rec StkEl i
      -> CollectingEvalOp m (Rec StkEl o)
    surroundExecutionInner _embeddedMetaMb runInstr instr stack =
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
          let meta@LambdaMeta{..} = getLambdaMeta lam
          logMessage
            [int||
              Meta #{meta} for lambda #{lam}
            |]

          oldStackFrames <- use csStackFramesL

          forM_ lmVariables \name -> do
            let sfName = pretty name
            loc <- use $ csActiveStackFrameL . sfLocL
            let newStackFrame = StackFrame
                  { sfLoc = loc
                  , sfStack = []
                  , ..
                  }

            mainFunctionName <- use csMainFunctionNameL

            unless (mainFunctionName `matchesUniqueLambdaName` name) do
              logMessage
                [int||
                  Created new stack frame #{newStackFrame}
                |]

              csStackFramesL %= cons newStackFrame

          newStack <- runInstr instr stack

          csStackFramesL .= oldStackFrames
          logMessage =<< [int|m|
            Restored stack frames, new active frame: #{sfName <$> use csActiveStackFrameL}
            |]

          case newStack of
            StkEl newLam@VLam{} :& stkEls -> do
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

              return $ StkEl embeddedLam :& stkEls
            _ -> return newStack

        _ -> runInstr instr stack

    -- Save a snapshot.
    --
    -- It is not strictly necessary for this function to accept instrNo and
    -- location, but that's a sanity check: if an event is not associated with
    -- some location, then it is likely not worth recording.
    recordSnapshot
      :: LigoRange
      -> InterpretEvent
      -> CollectingEvalOp m ()
    recordSnapshot loc event = do
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
      lift $ C.yield newSnap

    getStatements :: LigoRange -> CollectingEvalOp m [LigoRange]
    getStatements ligoRange = do
      let range@Range{..} = ligoRangeToRange ligoRange

      parsedLigo <-
        fromMaybe
          (error [int||File #{_rFile} is not parsed for some reason|])
          <$> use (csParsedFilesL . at _rFile)

      statements <- filterAndReverseStatements $ spineAtPoint range parsedLigo
      pure $ rangeToLigoRange . getRange <$> statements
      where
        filterAndReverseStatements :: [LIGO ParsedInfo] -> CollectingEvalOp m [LIGO ParsedInfo]
        filterAndReverseStatements = go []
          where
            go :: [LIGO ParsedInfo] -> [LIGO ParsedInfo] -> CollectingEvalOp m [LIGO ParsedInfo]
            go acc nodes =
              tryToProcessLigoStatement
                (`decide` acc)
                (\xs -> go acc xs)
                (pure acc)
                nodes

            decide :: LIGO ParsedInfo -> [LIGO ParsedInfo] -> [LIGO ParsedInfo] -> CollectingEvalOp m [LIGO ParsedInfo]
            decide x acc xs = do
              let accept = go (x : acc) xs
              let deny = go acc xs

              let cycleNode = xs
                    & find \el ->
                      case layer @Expr el of
                        Just AST.WhileLoop{} -> containsNode el x
                        Just AST.ForOfLoop{} -> containsNode el x
                        _ -> False

              let recNode = xs
                    & find \el ->
                      case layer @Binding el of
                        Just (AST.BFunction isRec _ _ _ _ _) -> isRec && containsNode el x
                        _ -> False

              ranges <- use csRecordedRangesL
              let range = rangeToLigoRange $ getRange x

              -- Here we want to check should we record this statement or not.
              case cycleNode <|> recNode of
                -- In this case our statement is present in some recursive function or loop.
                -- Further we'll call @recursive function or loop@ as @repetitive action@.
                Just (rangeToLigoRange . getRange -> nodeRange) -> do
                  recOrCycleEntry <- use $ csRecursiveOrCycleEntriesL . at nodeRange
                  -- Let's check if we created an entry
                  -- for repetitive action or not.
                  case recOrCycleEntry of
                    -- In this case an entry for repetitive action is present.
                    -- So, we need to perform some checks and decide
                    -- should we record this statement or not.
                    Just entry -> do
                      if | entry ^. roceStartL == ligoRange -> do
                            -- Here we are sure that we went on the first expression
                            -- in repetitive action. It means that we should
                            -- forget about all recorded statements in this action.
                            csRecursiveOrCycleEntriesL . at nodeRange . _Just . roceRecordedStatementsL .= HS.singleton range
                            accept

                         -- Otherwise, we're in a process of repetitive action.
                         -- Let's check if we recorded this statement or not.
                         | range `HS.member` (entry ^. roceRecordedStatementsL) -> deny
                         | otherwise -> do
                            csRecursiveOrCycleEntriesL . at nodeRange . _Just . roceRecordedStatementsL %= HS.insert range
                            accept

                    -- In this case we stepped in repetitive action for the first time.
                    -- It means that we should create an entry for it.
                    Nothing -> do
                      let newEntry = RecursiveOrCycleEntry
                            { roceStart = ligoRange
                            , roceRecordedStatements = HS.singleton range
                            }
                      csRecursiveOrCycleEntriesL . at nodeRange ?= newEntry
                      accept

                -- In this case our statement is not in repetitive action.
                -- So, we just need to check that it is not recorded.
                Nothing ->
                  if not $ range `HS.member` ranges
                  then accept
                  else deny

runCollectInterpretSnapshots
  :: (MonadUnliftIO m, MonadActive m)
  => CollectingEvalOp m a
  -> ContractEnv
  -> CollectorState m
  -> Value st
  -> m (InterpretHistory (InterpretSnapshot 'Unique))
runCollectInterpretSnapshots act env initSt initStorage =
  -- This should be safe because we yield at least one snapshot in the end
  InterpretHistory . fromList <$>
  lazyConsume do
    (outcome, endState, _) <- CL.runRWSC env initSt $ runExceptT act
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

      Right _ -> do
        let isStackFrames = either error id do
              lastSnap <-
                maybeToRight
                  "Internal error: No snapshots were recorded while interpreting Michelson code"
                  do csLastRecordedSnapshot endState

              case isStatus lastSnap of
                InterpretRunning (EventExpressionEvaluated (Just val)) -> do
                  let stackItemWithOpsAndStorage = StackItem
                        { siLigoDesc = LigoHiddenStackEntry
                        , siValue = val
                        }
                  let oldStorage = StackItem
                        { siLigoDesc = LigoHiddenStackEntry
                        , siValue = withValueTypeSanity initStorage (SomeValue initStorage)
                        }
                  pure
                    $ (lastSnap ^. isStackFramesL)
                        & ix 0 . sfStackL .~ [stackItemWithOpsAndStorage, oldStorage]
                status ->
                  throwError
                    [int||
                    Internal error:
                    Expected "Interpret running" status with evaluated expression status.

                    Got #{status}|]
        C.yield InterpretSnapshot
          { isStatus = InterpretTerminatedOk
          , ..
          }

-- | Execute contract similarly to 'interpret' function, but in result
-- produce an entire execution history.
collectInterpretSnapshots
  :: forall m cp st arg.
     (MonadUnliftIO m, MonadActive m)
  => FilePath
  -> Text
  -> Contract cp st
  -> EntrypointCallT cp arg
  -> Value arg
  -> Value st
  -> ContractEnv
  -> HashMap FilePath (LIGO ParsedInfo)
  -> (String -> m ())
  -> m (InterpretHistory (InterpretSnapshot 'Unique))
collectInterpretSnapshots mainFile entrypoint Contract{..} epc param initStore env parsedContracts logger =
  runCollectInterpretSnapshots
    (runInstrCollect (unContractCode cCode) initStack)
    env
    collSt
    initStore
  where
    initStack = mkInitStack (liftCallArg epc param) initStore
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
      , csLastRecordedSnapshot = Nothing
      , csParsedFiles = parsedContracts
      , csRecordedRanges = HS.empty
      , csRecursiveOrCycleEntries = mempty
      , csLoggingFunction = logger
      , csMainFunctionName = Name entrypoint
      , csLastRangeMb = Nothing
      }
