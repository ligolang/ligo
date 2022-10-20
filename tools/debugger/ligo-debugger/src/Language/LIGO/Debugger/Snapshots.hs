-- | Evaluation of snapshots in LIGO code execution.
module Language.LIGO.Debugger.Snapshots
  ( StackItem (..)
  , StackFrame (..)
  , InterpretStatus (..)
  , InterpretEvent (..)
  , statusExpressionEvaluatedP
  , InterpretSnapshot (..)
  , CollectorState (..)
  , InterpretHistory (..)
  , EmbeddedLigoMeta
  , runInstrCollect
  , runCollectInterpretSnapshots
  , collectInterpretSnapshots

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

  , _InterpretStarted
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
  (At (at), Ixed (ix), Zoom (zoom), has, makeLensesWith, makePrisms, (%=), (.=), (?=))
import Control.Lens.Prism (Prism', _Just)
import Control.Monad.Except (throwError)
import Control.Monad.RWS.Strict (RWST (..))
import Data.Conduit (ConduitT)
import Data.Conduit qualified as C
import Data.Conduit.Lazy (MonadActive, lazyConsume)
import Data.Conduit.Lift qualified as CL
import Data.HashSet qualified as HS
import Data.List.NonEmpty qualified as NE
import Data.Typeable (cast)
import Data.Vinyl (Rec (..))
import Duplo (layer)
import Fmt (Buildable (..), genericF)
import Parser (Info)
import Range (HasRange (getRange), Range (..))
import Text.Interpolation.Nyan
import UnliftIO (MonadUnliftIO)
import Unsafe qualified

import Morley.Debugger.Core.Navigate
  (Direction (Backward), MovementResult (ReachedBoundary), NavigableSnapshot (..),
  NavigableSnapshotWithMethods (..), SnapshotEdgeStatus (..), curSnapshot, frozen, move,
  unfreezeLocally)
import Morley.Debugger.Core.Snapshots (InterpretHistory (..), twoElemFromList)
import Morley.Michelson.Interpret
  (ContractEnv, InstrRunner, InterpreterState, InterpreterStateMonad (..),
  MichelsonFailureWithStack, MorleyLogsBuilder, StkEl, initInterpreterState, mkInitStack,
  runInstrImpl, seValue)
import Morley.Michelson.Runtime.Dummy (dummyBigMapCounter, dummyGlobalCounter)
import Morley.Michelson.Typed as T
import Morley.Util.Lens (postfixLFields)

import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Common

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
  , csStackFrames :: NonEmpty StackFrame
    -- ^ Stack frames at this point, top-level frame goes last.
  , csLastRecordedSnapshot :: Maybe InterpretSnapshot
    -- ^ Last recorded snapshot.
    -- We can pick @[operation] * storage@ value from it.
  , csParsedFiles :: HashMap FilePath (LIGO Info)
    -- ^ Parsed contracts.
  , csRecordedRanges :: HashSet LigoRange
    -- ^ Ranges of recorded statement snapshots.
  , csRecursiveOrCycleEntries :: HashMap LigoRange RecursiveOrCycleEntry
    -- ^ @RecursiveOrCycleEntry@ for each cycle or
    -- recursive function expression.
  , csLoggingFunction :: String -> m ()
    -- ^ Function for logging some useful debugging info.
  }

makeLensesWith postfixLFields ''StackItem
makeLensesWith postfixLFields ''StackFrame
makeLensesWith postfixLFields ''InterpretSnapshot
makeLensesWith postfixLFields ''RecursiveOrCycleEntry
makeLensesWith postfixLFields ''CollectorState

-- | Lens giving an access to the bottom-most frame - which is also
-- the only active one.
csActiveStackFrameL :: Lens' (CollectorState m) StackFrame
csActiveStackFrameL = csStackFramesL . __head
  where
    __head :: Lens' (NonEmpty a) a
    __head f (x :| xs) = (:| xs) <$> f x

-- | Our monadic stack, allows running interpretation and making snapshot
-- records.
type CollectingEvalOp m =
  -- Including ConduitT to build snapshots sequence lazily.
  -- Normally ConduitT lies on top of the stack, but here we put it under
  -- ExceptT to make it record things even when a failure occurs.
  ExceptT MichelsonFailureWithStack $
  ConduitT () InterpretSnapshot $
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

stkElValue :: StkEl v -> SomeValue
stkElValue stkEl = let v = seValue stkEl in withValueTypeSanity v (SomeValue v)

logMessage :: (Monad m) => String -> CollectingEvalOp m ()
logMessage str = do
  logger <- use csLoggingFunctionL
  lift $ lift $ lift $ logger [int||[SnapshotCollecting] #{str}|]

-- | Executes the code and collects snapshots of execution.
runInstrCollect :: forall m. (Monad m) => InstrRunner (CollectingEvalOp m)
runInstrCollect = \case
  -- TODO: use ConcreteMeta from Morley once available
  instr@(T.Meta (T.SomeMeta (cast -> Just (embeddedMeta :: EmbeddedLigoMeta))) _) -> \stack -> do
    logMessage
      [int||
        Got meta: #{embeddedMeta}
        for instruction: #{instr}
      |]

    preExecutedStage embeddedMeta (refineStack stack)
    newStack <- runInstrImpl runInstrCollect instr stack
    postExecutedStage embeddedMeta (refineStack stack) (refineStack newStack)
    return newStack
  other -> runInstrImpl runInstrCollect other
  where

    -- What is done upon executing instruction.
    preExecutedStage LigoIndexedInfo{..} stack = do
      whenJust liiLocation \loc -> do
        statements <- getStatements loc

        forM_ statements \statement -> do
          recordSnapshot statement EventFacedStatement
          csRecordedRangesL %= HS.insert statement

        recordSnapshot loc EventExpressionPreview

      whenJust liiEnvironment \env -> do
        -- Here stripping occurs, as the second list keeps the entire stack,
        -- while the first list (@env@) - only stack related to the current
        -- stack frame. And this is good.
        let stackHere = zipWith StackItem env stack

        logMessage
          [int||
            Stack at preExecutedStage: #{stackHere}
          |]

        csActiveStackFrameL . sfStackL .= stackHere

    -- What is done right after the instruction is executed.
    postExecutedStage LigoIndexedInfo{..} _oldStack newStack = do
      whenJust liiLocation \loc -> do
        -- `location` point to instructions that end expression evaluation,
        -- we can record the computed value
        let evaluatedVal = safeHead newStack

        logMessage
          [int||
            Just evaluated: #{evaluatedVal}
          |]

        recordSnapshot loc (EventExpressionEvaluated evaluatedVal)

    -- Save a snapshot.
    --
    -- It is not strictly necessary for this function to accept instrNo and
    -- location, but that's a sanity check: if an event is not associated with
    -- some location, then it is likely not worth recording.
    recordSnapshot
      :: LigoRange
      -> InterpretEvent
      -> CollectingEvalOp m ()
    recordSnapshot loc event = unless (isLigoStdLib $ lrFile loc) do
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

      csLastRecordedSnapshotL ?= newSnap
      lift $ C.yield newSnap

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

    ligoRangeToRange :: LigoRange -> Range
    ligoRangeToRange LigoRange{..} = Range
      { _rStart = toPosition lrStart
      , _rFinish = toPosition lrEnd
      , _rFile = lrFile
      }
      where
        toPosition LigoPosition{..} = (Unsafe.fromIntegral lpLine, Unsafe.fromIntegral $ lpCol + 1, 0)

    rangeToLigoRange :: Range -> LigoRange
    rangeToLigoRange Range{..} = LigoRange
      { lrStart = toLigoPosition _rStart
      , lrEnd = toLigoPosition _rFinish
      , lrFile = _rFile
      }
      where
        toLigoPosition (line, col, _) = LigoPosition (Unsafe.fromIntegral line) (Unsafe.fromIntegral $ col - 1)

    getStatements :: LigoRange -> CollectingEvalOp m [LigoRange]
    getStatements ligoRange
      | isLigoStdLib $ lrFile ligoRange = pure []
      | otherwise = do
          let range@Range{..} = ligoRangeToRange ligoRange

          parsedLigo <-
            fromMaybe
              (error [int||File #{_rFile} is not parsed for some reason|])
              <$> use (csParsedFilesL . at _rFile)

          statements <- filterAndReverseStatements $ spineAtPoint range parsedLigo
          pure $ rangeToLigoRange . getRange <$> statements
      where
        filterAndReverseStatements :: [LIGO Info] -> CollectingEvalOp m [LIGO Info]
        filterAndReverseStatements = go []
          where
            go :: [LIGO Info] -> [LIGO Info] -> CollectingEvalOp m [LIGO Info]
            go acc [] = pure acc
            go acc (x@(layer -> Just AST.Assign{}) : xs) = decide x acc xs
            go acc (x@(layer -> Just AST.BConst{}) : xs) = decide x acc xs
            go acc (x@(layer -> Just AST.BVar{}) : xs) = decide x acc xs
            go acc (x@(layer -> Just AST.Apply{}) : xs@((layer @Expr -> Just ctor) : _)) =
              case ctor of
                AST.Let{} -> decide x acc xs
                AST.Seq{} -> decide x acc xs
                _ -> go acc xs
            go acc (_ : xs) = go acc xs

            decide :: LIGO Info -> [LIGO Info] -> [LIGO Info] -> CollectingEvalOp m [LIGO Info]
            decide x acc xs = do
              let accept = go (x : acc) xs
              let deny = go acc xs

              let cycleNode = xs
                    & find \el ->
                      case layer @Expr el of
                        Just AST.ForLoop{} -> containsNode el x
                        Just AST.WhileLoop{} -> containsNode el x
                        Just AST.ForOfLoop{} -> containsNode el x
                        Just AST.ForBox{} -> containsNode el x
                        _ -> False

              let recNode = xs
                    & find \el ->
                      case layer @Binding el of
                        Just (AST.BFunction isRec _ _ _ _) -> isRec && containsNode el x
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
              where
                -- Comparing by ranges because @Eq@ instance behaves
                -- weird with @LIGO Info@.
                containsNode :: LIGO Info -> LIGO Info -> Bool
                containsNode tree node = getRange node `elem` nodes
                  where
                    nodes = getRange <$> spineAtPoint (getRange node) tree

runCollectInterpretSnapshots
  :: (MonadUnliftIO m, MonadActive m)
  => CollectingEvalOp m a
  -> ContractEnv
  -> CollectorState m
  -> Value st
  -> m (InterpretHistory InterpretSnapshot)
runCollectInterpretSnapshots act env initSt initStorage =
  -- This should be safe because in practice we yield at least two snapshots
  InterpretHistory . fromMaybe (error "Unexpectedly < 2 snapshots") . twoElemFromList .
  filterDupLocSnapshots <$>
  lazyConsume do
    C.yield InterpretSnapshot
      { isStackFrames = csStackFrames initSt
      , isStatus = InterpretStarted
      }

    (outcome, endState, _) <- CL.runRWSC env initSt $ runExceptT act
    case outcome of
      Left stack ->
        C.yield InterpretSnapshot
          { isStatus = InterpretFailed stack
          , isStackFrames = csStackFrames endState
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

{-# ANN filterDupLocSnapshots ("HLint: ignore Redundant lambda" :: Text) #-}

-- | Strip adjacent snapshots that point to the same location.
--
-- In practice it happens that LIGO produces snapshots for intermediate
-- computations. For instance, @a > 10@ will translate to @COMPARE; GT@,
-- both having the same @location@ meta; we don't want the user to
-- see that.
filterDupLocSnapshots :: [InterpretSnapshot] -> [InterpretSnapshot]
filterDupLocSnapshots = \snaps -> snaps
  & NE.groupBy ((==) `on` stackFrameId . isStackFrames)
  & concatMap (cleanupGroup . toList)
  where
    stackFrameId = sfLoc . head &&& length &&& length . sfStack . head

    cleanupGroup :: [InterpretSnapshot] -> [InterpretSnapshot]
    cleanupGroup = \snaps -> snaps
      & reverse
      & dropAllMatchingExceptFirst
        (isStatusL . _InterpretRunning . _EventExpressionPreview)
      & dropAllMatchingExceptFirst
        (isStatusL . _InterpretRunning . _EventExpressionEvaluated)
      & reverse

    dropAllMatchingExceptFirst pri =
      evaluatingState False . filterM \e -> state \alreadyFaced ->
        ( not $ has pri e && alreadyFaced  -- whether to retain
        , alreadyFaced || has pri e  -- new state
        )

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
  -> HashMap FilePath (LIGO Info)
  -> (String -> m ())
  -> m (InterpretHistory InterpretSnapshot)
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
      }
