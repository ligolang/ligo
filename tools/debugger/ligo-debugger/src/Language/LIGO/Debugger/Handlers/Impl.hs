{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Implementation of DAP handlers.
module Language.LIGO.Debugger.Handlers.Impl
  ( LIGO
  , ligoHandlers

    -- * Helpers
  , initDebuggerState
  , convertMichelsonValuesToLigo
  , initContractEnv
  ) where

import Prelude hiding (try)

import Debug qualified
import Unsafe qualified

import Control.Lens (Each (each), _Just, ix, magnify, traversed, (+~), (.=), (^?!))
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as M
import Data.Singletons (demote)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt.Buildable (blockListF, fmt, pretty)
import Fmt.Utils (Doc)
import Named (defaults, (!))
import System.FilePath (takeFileName, (<.>), (</>))
import Text.Interpolation.Nyan hiding (rmode')
import UnliftIO (UnliftIO (..), askUnliftIO, withRunInIO)
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Exception (Handler (..), catches, throwIO, try)
import UnliftIO.STM (modifyTVar)
import Util

import Control.AbortingThreadPool qualified as AbortingThreadPool
import Control.DelayedValues (Manager (mComputation))
import Control.DelayedValues qualified as DV

import Protocol.DAP (respond, respondAndAlso, submitEvent)
import Protocol.DAP qualified as DAP hiding (mkHandler)
import Protocol.DAP qualified as DAP.LowLevel (mkHandler)
import Protocol.DAP.Serve.IO (StopAdapter (..))

import Morley.Debugger.Core
  (DebuggerState (..), NavigableSnapshot (getLastExecutedPosition), PausedReason (..),
  SnapshotEdgeStatus (SnapshotAtEnd),
  SnapshotEndedWith (SnapshotEndedWithFail, SnapshotEndedWithOk), SourceLocation, _slPath,
  curSnapshot, groupSourceLocations, mkDebugSource, pickSnapshotEdgeStatus, playInterpretHistory,
  slEnd, toCanonicalLoc)
import Morley.Debugger.DAP.Handlers
  (VSCodeSrcLoc (..), fromMichelsonSource, readDAPSessionState, toVSCodeLoc, updateDAPSessionState)
import Morley.Debugger.DAP.Handlers qualified as MorleyHandlers
import Morley.Debugger.DAP.LanguageServer (JsonFromBuildable (..))
import Morley.Debugger.DAP.RIO (logMessage, openLogHandle)
import Morley.Debugger.DAP.Types
  (DAPSessionState (..), HasSpecificMessages (..), MonadRIO, RIO, RioContext (..),
  StopEventDesc (..), StoppedReason (..), dsDebuggerState, dsVariables, resetDAPState,
  unMichelsonJson)
import Morley.Michelson.ErrorPos (ErrorSrcPos (ErrorSrcPos), Pos (Pos), SrcPos (SrcPos))
import Morley.Michelson.Interpret
  (ContractEnv' (..), MichelsonFailed (MichelsonFailedWith),
  MichelsonFailureWithStack (mfwsErrorSrcPos), RemainingSteps (RemainingSteps), ceContracts,
  ceMaxSteps, mfwsFailed)
import Morley.Michelson.Parser.Types (MichelsonSource (MSFile))
import Morley.Michelson.Printer.Util (RenderDoc (renderDoc), doesntNeedParens)
import Morley.Michelson.Runtime (ContractState (..), mkVotingPowers)
import Morley.Michelson.Runtime.Dummy (dummyMaxSteps)
import Morley.Michelson.Typed
  (Constrained (SomeValue), Contract, Contract' (..), ContractCode' (unContractCode),
  SomeContract (..), pattern DefEpName)
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import Morley.Tezos.Address (Constrained (Constrained), ta)
import Morley.Tezos.Core (Timestamp (Timestamp), dummyChainId, tz)
import Morley.Tezos.Crypto (parseHash)

import Language.LIGO.DAP.Variables
import Language.LIGO.Debugger.CLI
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Error
import Language.LIGO.Debugger.Handlers.Helpers
import Language.LIGO.Debugger.Handlers.Types
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Debugger.Navigate
import Language.LIGO.Debugger.Snapshots
import Language.LIGO.Extension (UnsupportedExtension (..), getExt)
import Language.LIGO.Range

data LIGO

instance HasLigoClient (RIO LIGO) where
  getLigoClientEnv = do
    lServ <- asks _rcLSState >>= readTVarIO
    lift $ getClientEnv lServ
    where
      getClientEnv :: Maybe LigoLanguageServerState -> IO LigoClientEnv
      getClientEnv = \case
        Just lServ -> do
          let maybeEnv = LigoClientEnv <$> lsBinaryPath lServ
          maybe getLigoClientEnv pure maybeEnv
        Nothing -> getLigoClientEnv

ligoCustomHandlers :: DAP.HandlersSet (RIO LIGO)
ligoCustomHandlers =
  [ resolveConfigFromLigo
  , initializeLoggerHandler
  , setLigoConfigHandler
  , setProgramPathHandler
  , validateEntrypointHandler
  , getContractMetadataHandler
  , validateValueHandler
  , validateConfigHandler
  ]

ligoHandlers :: StopAdapter -> DAP.HandlersSet (RIO LIGO)
ligoHandlers stopAdapter = MorleyHandlers.safenHandler <$> concat
  [ [ MorleyHandlers.initializeHandler
    , launchHandler
    , MorleyHandlers.configurationDoneHandler
    , terminateHandler
    , MorleyHandlers.disconnectHandler stopAdapter
    ]
  , MorleyHandlers.stateRequestHandlers
  , MorleyHandlers.stepHandlers
      & traversed %~ DAP.embedHandler onStep
  , [MorleyHandlers.setBreakpointsHandler]
  , [ MorleyHandlers.evaluateRequestHandlerDummy ]

  , ligoCustomHandlers
  ]

instance HasSpecificMessages LIGO where
  type LanguageServerStateExt LIGO = LigoLanguageServerState
  type InterpretSnapshotExt LIGO = InterpretSnapshot 'Unique
  type StopEventExt LIGO = InterpretEvent
  type StepGranularityExt LIGO = LigoStepGranularity
  type PausedReasonExt LIGO = LigoSpecificPausedReason

  reportErrorAndStoppedEvent dapState = \case
    ExceptionMet exception -> writeException True exception
    Paused reason label -> writeStoppedEvent reason label
    TerminatedOkMet{} -> writeTerminatedEvent
    PastFinish -> do
      let snap = curSnapshot $ _dsDebuggerState dapState
      case pickSnapshotEdgeStatus snap of
        SnapshotAtEnd outcome -> case outcome of
          SnapshotEndedWithFail exception -> writeException False exception
          SnapshotEndedWithOk{} ->
            -- At the moment we never expect this, as in case of ok termination
            -- we should have already existed when stepping on last snapshot;
            -- but let's handle this condition gracefully anyway.
            writeTerminatedEvent
        _ -> error "Unexpected status"
    ReachedStart -> writeStoppedEvent PlainPaused "entry"
    where
      writeTerminatedEvent = do
        entrypointType <- getEntrypointType

        let (_, storageType, opsAndStorageType) = getParameterStorageAndOpsTypes entrypointType

        let InterpretSnapshot{..} = curSnapshot $ _dsDebuggerState dapState
        let someValues = head isStackFrames ^.. sfStackL . each . siValueL
        let types = [opsAndStorageType, storageType]

        let convertInfos = zipWith PreLigoConvertInfo someValues types

        lServVar <- getServerState @LIGO
        let DV.Manager{..} = lsToLigoValueConverter lServVar

        -- @writeTerminatedEvent@ would be executed only once (on last snapshot)
        -- So, we can call this heavy computation in a synchronous way.
        decompiled <- liftIO $ mComputation convertInfos

        -- TODO [#1987]: Get rid of all those `error` calls
        (opsText, storeText, oldStoreText) <- case decompiled of
          [someValue, someStorage] -> do
            let (ops, store) = buildOpsAndNewStorage someValue
            let oldStore = buildOldStorage someStorage
            pure (ops, store, oldStore)
          _ -> error [int||
            Expected the stack to only have 2 elements, but its length is \
            #{length someValues}.
            |]

        submitEvent $ [int||
          Execution completed.
          Operations:
          #{opsText}
          Storage:
          #{storeText}

          Old storage:
          #{oldStoreText}
          |]
          { DAP.category = Just #stdout
          }
        submitEvent $ Just DAP.TerminatedEvent{ restart = Nothing }
          where
            buildOpsAndNewStorage :: LigoOrMichValue -> (Doc, Doc)
            buildOpsAndNewStorage = \case
              LigoValue ligoType ligoValue -> case toTupleMaybe ligoValue of
                Just [LVList ops, st] ->
                  let (fstType, sndType) = maybe (LigoType Nothing, LigoType Nothing) (bimap LigoType LigoType) do
                        LTCRecord LigoTypeTable{..} <- _lteTypeContent <$> unLigoType ligoType
                        let fstTyp = HM.lookup "0" _lttFields
                        let sndTyp = HM.lookup "1" _lttFields
                        pure (fstTyp, sndTyp)
                  in
                    ( blockListF (buildLigoValue fstType <$> ops)
                    , buildLigoValue sndType st
                    )
                _ ->
                  error badLastElement
              MichValue _ (SomeValue val) -> case val of
                (T.VPair (T.VList ops, r :: T.Value r)) ->
                  case T.valueTypeSanity r of
                    T.Dict ->
                      case T.checkTPresence T.SPSOp (T.sing @r) of
                        T.TAbsent -> (blockListF ops, renderDoc doesntNeedParens r)
                        _ -> error invalidStorage
                _ -> error badLastElement
              _ -> error notComputed

            buildOldStorage :: LigoOrMichValue -> Doc
            buildOldStorage = \case
              LigoValue ligoType ligoValue -> buildLigoValue ligoType ligoValue
              MichValue _ (SomeValue (st :: T.Value r)) ->
                case T.valueTypeSanity st of
                  T.Dict ->
                    case T.checkTPresence T.SPSOp (T.sing @r) of
                      T.TAbsent -> renderDoc doesntNeedParens st
                      _ -> error invalidStorage
              _ -> error notComputed

            invalidStorage = "Invalid storage type"
            badLastElement = "Expected the last element to be a pair of operations and storage"
            notComputed = "Expected value to be computed"

      writeStoppedEvent reason label = do
        let hitBreakpointIds = case reason of
              BreakpointPaused ids -> Just ids
              _ -> Nothing

        (mDesc, mLongDesc) <- usingReaderT dapState $ magnify dsDebuggerState $
          getStopEventInfo @LIGO Proxy
        let fullLabel = case mDesc of
              Nothing -> label
              Just (StopEventDesc desc) -> label <> ", " <> desc
        submitEvent DAP.StoppedEvent
          { DAP.reason = fromString $ toString fullLabel
            -- ↑ By putting moderately large text we slightly violate DAP spec,
            -- but it seems to be worth it
          , DAP.description = Nothing
          , DAP.threadId = Just theThreadId
          , DAP.allThreadsStopped = Just True
          , DAP.text = Just $ maybe "" pretty mLongDesc
          , DAP.hitBreakpointIds = hitBreakpointIds
          , DAP.preserveFocusHint = Nothing
          }

      writeException writeLog exception = do
        let msg = case mfwsFailed exception of
              -- [LIGO-862] display this value as LIGO one
              MichelsonFailedWith val ->
                let
                  ErrorSrcPos (SrcPos (Pos l) (Pos c)) = mfwsErrorSrcPos exception
                in
                  [int||Contract failed with value: #{val}
                  On line #{l + 1} char #{c + 1}|]
              _ -> pretty exception

        lastPos <- usingReaderT (dapState ^. dsDebuggerState) $
          getLastExecutedPosition
            <&> fromMaybe (error "Unexpectedly no last position")
        let lastSrcLoc = toVSCodeLoc $ view slEnd lastPos

        submitEvent DAP.StoppedEvent
          { DAP.reason = #exception
          , DAP.threadId = Just theThreadId
          , DAP.allThreadsStopped = Just True
          , DAP.description = Just "Paused on exception"
          , DAP.text = Just msg
          , DAP.preserveFocusHint = Nothing
          , DAP.hitBreakpointIds = Nothing
          }
        when writeLog do
          submitEvent (fmt $ pretty msg <> "\n")
            { DAP.category = Just #stderr
            , DAP.source = Just $ fromMichelsonSource $ _slPath lastPos
            , DAP.line   = Just $ vsLine lastSrcLoc
            , DAP.column = Just $ vsCol lastSrcLoc
            }

  launchHandler = mkLigoHandler \req@LigoLaunchRequest{} -> do
    st <- lift $ initDebuggerSession req

    lift do
      logMessage "Launching contract with arguments\n"
      logMessage $ Debug.show req <> "\n"
    asks _rcDAPState >>= \var -> atomically $ writeTVar var (Just st)
    respond ()

  handleStackTraceRequest DAP.StackTraceRequest{} = do
    -- We mostly follow morley-debugger's implementation, but here we don't need
    -- to look at the next snapshot, the current one is what we want.
    snap <- curSnapshot . _dsDebuggerState <$> readDAPSessionState
    let frames = toDAPStackFrames snap
    respond DAP.StackTraceResponse
      { DAP.stackFrames = frames
      , DAP.totalFrames = Just $ Unsafe.fromIntegral @Int @Word $ length frames
      }
    where
      toDAPStackFrames snap =
        let frames = toList $ isStackFrames snap
        in zip [topFrameId ..] frames <&>
          \(i, frame) ->
          let Range{..} = sfLoc frame
              (VSCodeSrcLoc startRow startCol, VSCodeSrcLoc endRow endCol) =
                 over each (toVSCodeLoc . toCanonicalLoc) (_rStart, _rFinish)
          in DAP.StackFrame
            { DAP.id = i
            , DAP.name = sfName frame
            , DAP.source = Just $ fromMichelsonSource (MSFile _rFile)
            , DAP.line = startRow
            , DAP.column = startCol
            , DAP.endLine = Just endRow
            , DAP.endColumn = Just endCol
            , DAP.canRestart = Just False
            , instructionPointerReference = Nothing
            , moduleId = Nothing
            , presentationHint = Nothing
            }

  handleScopesRequest req = do
    lServVar <- getServerState
    ligoTypesVec <- getLigoTypesVec
    -- We follow the implementation from morley-debugger
    snap <- curSnapshot . _dsDebuggerState <$> readDAPSessionState

    let currentStackFrame = snap
          & isStackFrames
          & flip (^?!) (ix (DAP.unStackFrameId req.frameId - 1))

    let stackItems = currentStackFrame
          & sfStack
          & reverse -- stack's top should go to the end of the variables list

    -- Here we can see one problem. Variables types would be prettified
    -- in the dialect from the current file.
    -- But some variables can come from, for example, a @CameLIGO@ contract
    -- and the other ones from a @PascaLIGO@ one.
    lang <-
      currentStackFrame ^. sfLocL . rFile
        & getExt @(Either UnsupportedExtension)
        & either throwM pure

    let valConvertManager = lsToLigoValueConverter lServVar

    ligoVals <- fmap catMaybes $ forM stackItems \stackItem ->
      runMaybeT do
        StackItem desc michVal <- pure stackItem
        LigoStackEntry (LigoExposedStackEntry mDecl typRef) <- pure desc
        let typ = readLigoType ligoTypesVec typRef
        let varName = maybe (pretty unknownVariable) pretty mDecl

        ligoVal <- decompileValue (PreLigoConvertInfo michVal typ) valConvertManager
        return (varName :: Text, ligoVal)

    builder <-
      case isStatus snap of
        InterpretRunning (EventExpressionEvaluated typ (Just value))
          -- We want to show $it variable only in the top-most stack frame.
          | req.frameId == DAP.StackFrameId 1 -> do
            itVal <- decompileValue (PreLigoConvertInfo value typ) valConvertManager
            pure do
              idx <- createVariables lang ligoVals
              itVar <- buildVariable lang itVal "$it"
              insertToIndex idx [itVar]
        _ -> pure $ createVariables lang ligoVals

    let (varReference, variables) = runBuilder builder

    updateDAPSessionState $ dsVariables .= variables

    let moveIdAtUpdate = lsMoveId lServVar

    -- TODO [LIGO-304]: show detailed scopes
    let theScope = DAP.mk @DAP.Scope
          ! #name "all variables"
          ! #variablesReference varReference
          ! defaults

    DAP.ScopesResponse
      { DAP.scopes = [theScope] }
      `respondAndAlso`
      do
      -- We have to request for the LIGO values conversion.
      -- It produces a call to @ligo@ and may take time, so we spawn
      -- a thread for this.
      -- We are fine with the thread's death if it gets outdated, it's better
      -- than having 100500 @ligo@ threads running at a time.
      logMessage "Going to compute pending variables"
      AbortingThreadPool.runAbortableAsync (lsVarsComputeThreadPool lServVar) do
        computedSmthNew <- DV.runPendingComputations valConvertManager

        when computedSmthNew do
          logMessage "Computed LIGO values"

        -- Now let's ask VSCode to request for new values.
        -- If moveId has changed, then we have made a new step and it invokes
        -- its own invalidation procedure, we better leave everything on it.
        curMoveId <- lsMoveId <$> getServerState
        when (curMoveId == moveIdAtUpdate && computedSmthNew) do
          submitEvent DAP.InvalidatedEvent
            { DAP.areas = Just ["variables"]
            , DAP.threadId = Nothing
            , DAP.stackFrameId = Just topFrameId
            }

  reportContractLogs _ _ = pass

  getStopEventInfo Proxy = curSnapshot <&> \snap -> case isStatus snap of
    InterpretRunning event ->
      let
        shortDesc = case event of
          EventFacedStatement -> Just $ StopEventDesc "at statement"
          EventExpressionPreview GeneralExpression -> Just $ StopEventDesc "upon exp"
          EventExpressionPreview FunctionCall -> Just $ StopEventDesc "upon func call"
          EventExpressionEvaluated{} -> Just $ StopEventDesc "computed exp"
      in (shortDesc, Just event)
    _ -> (Nothing, Nothing)

  handleVariablesRequest req = do
    vars <- _dsVariables <$> readDAPSessionState
    case vars ^? ix req.variablesReference of
      Nothing ->
        throwM $ PluginCommunicationException "The referred variable does not exist"
      Just vs ->
        respond $ DAP.VariablesResponse vs

  parseStepGranularity = \case
    Nothing -> pure def
    Just t -> case t of
      "statement" -> pure GStmt
      "expression" -> pure GExp
      "expressionSurrounded" -> pure GExpExt
      other -> Left [int||Unknown granularity `#{other}`|]

  processStep = processLigoStep

terminateHandler :: DAP.Handler (RIO LIGO)
terminateHandler = mkLigoHandler \req@DAP.TerminateRequest{} -> do
  resetDAPState
  unless (req.restart == Just True) do
    lServState <- getServerState
    AbortingThreadPool.close (lsVarsComputeThreadPool lServState)

    lServVar <- asks _rcLSState
    atomically $ writeTVar lServVar Nothing

  respondAndAlso () do
    submitEvent "Debug session terminated"
      { DAP.category = Just #console
      }
    submitEvent $ Just DAP.TerminatedEvent{ DAP.restart = Nothing }
    logMessage "Terminating the contract\n"

onStep :: MonadRIO LIGO m => m ()
onStep = do
  lServVar <- asks _rcLSState
  atomically $ modifyTVar' lServVar $ _Just . lsMoveIdL +~ 1

-- | DAP handler specialized to LIGO monad.
type LigoHandler = DAP.Handler (RIO LIGO)

-- | Body of the handler, what is passed to 'mkHandler'.
type LigoHandlerBody r = r -> LigoHandlerBodyM (RIO LIGO) (DAP.ResponseFor r)

-- | Monad of the handler body.
type LigoHandlerBodyM m a =
  DAP.EventSubmitIO m $
    DAP.ResponseAndLaterActions (DAP.EventSubmitIO m) a

-- | Construct a handler in LIGO way.
--
-- The supplied function is expected to manage exceptions in IO, not via `ExceptT`
-- as in `DAP.Handler`.
--
-- Note however, that a handler can do actions after the response is submitted,
-- and there is no way to sensibly report errors in those actions to the client.
-- Thus this function does nothing in regard to those exceptions, which is
-- potentially dangerous (such an exception would abort the requests processing).
-- Prevent this by adding a catch-all to the handlers, e.g. by using
-- 'MorleyHandlers.safenHandler'.
mkLigoHandler
  :: (HasCallStack, DAP.IsRequestToAdapter r)
  => LigoHandlerBody r -> LigoHandler
mkLigoHandler body = DAP.LowLevel.mkHandler (excHandlersWrapper . body)

excHandlersWrapper
  :: (HasLigoClient m)
  => DAP.EventSubmitIO m
       (DAP.ResponseAndLaterActions (DAP.EventSubmitIO m) a)
  -> ExceptT DAP.Error (DAP.EventSubmitIO m)
       (DAP.ResponseAndLaterActions (DAP.EventSubmitIO m) a)
excHandlersWrapper = \action ->
  ExceptT $ catches (Right <$> action) excHandlers
  where
    toErrResponse
      :: forall e m a. (DebuggerException e, Monad m)
      => DAP.ErrorDetails -> m (Either DAP.Error a)
    toErrResponse errBody =
      pure $ Left DAP.Error
        { DAP.message = Just $ demote @(ExceptionTag e)
        , DAP.body = Just errBody
        }

    excHandlers =
      [ Handler \(SomeDebuggerException (err :: excType)) -> do
          versionIssuesDetails <- case debuggerExceptionType err of
            -- TODO: make this pure, carry version in the LS state
            MidLigoLayerException -> lift getVersionIssuesDetails
            _ -> pure Nothing

          toErrResponse @excType $
            [int||#{displayException err}|]
            { DAP.variables = mconcat
                [ one ("origin", pretty (debuggerExceptionType err))
                , maybe mempty (one . ("versionIssues", )) versionIssuesDetails
                , one ("shouldInterruptDebuggingSession", Text.toLower $ pretty $ shouldInterruptDebuggingSession @excType)
                , debuggerExceptionData err
                ]
            }

      , Handler \(SomeException err) -> do
          toErrResponse @ImpossibleHappened $
            [int||Internal (unhandled) error: #exc{err}|]
      ]

-- | We have only one thread.
theThreadId :: DAP.ThreadId
theThreadId = DAP.ThreadId 1

-- | Id of the top (currently active) stack frame.
topFrameId :: DAP.StackFrameId
topFrameId = DAP.StackFrameId 1

decompileValue
  :: (MonadIO m)
  => PreLigoConvertInfo
  -> Manager PreLigoConvertInfo LigoOrMichValue
  -> m LigoOrMichValue
decompileValue convertInfo@(PreLigoConvertInfo val typ) manager = do
  whenJust (tryDecompilePrimitive val) \dec -> do
    atomically $ DV.putComputed
      manager
      convertInfo
      (LigoValue typ dec)

  mLigoVal <- DV.compute manager convertInfo
  pure $ fromMaybe ToBeComputed mLigoVal

resolveConfigFromLigo :: DAP.Handler (RIO LIGO)
resolveConfigFromLigo = mkLigoHandler \req@LigoResolveConfigFromLigoRequest{} ->
  respond =<< resolveConfig req.configPath

initializeLoggerHandler :: DAP.Handler (RIO LIGO)
initializeLoggerHandler = mkLigoHandler \req@LigoInitializeLoggerRequest{} -> do
  let file = req.file
  let logFileMb = do
        dir <- req.logDir
        Just $ dir </> takeFileName file <.> "log"
  whenJust logFileMb openLogHandle

  unlessM (doesFileExist file) do
    throwIO $ ConfigurationException [int||Contract file not found: #{toText file}|]

  respondAndAlso () do
    logMessage [int||Initializing logger for #{file} finished|]

convertMichelsonValuesToLigo :: (HasLigoClient m) => (Text -> m ()) -> [PreLigoConvertInfo] -> m [LigoOrMichValue]
convertMichelsonValuesToLigo logger inps = do
  let typesAndValues = inps
        <&> \(PreLigoConvertInfo val typ) -> (typ, val)

  decompiledValues <- decompileLigoValues typesAndValues

  logger [int||
    Decompilation contract: begin
    #{generateDecompilation typesAndValues}

    the end.
  |]

  pure $
    zipWith
      do \(t, michValue) dec -> maybe (MichValue t michValue) (LigoValue t) dec
      typesAndValues
      decompiledValues

setLigoConfigHandler :: DAP.Handler (RIO LIGO)
setLigoConfigHandler = mkLigoHandler \req@LigoSetLigoConfigRequest{} -> do
  let maxStepsMb = RemainingSteps <$> req.maxSteps

  UnliftIO unliftIO <- askUnliftIO

  lServVar <- asks _rcLSState
  varsComputeThreadPool <- AbortingThreadPool.newPool 10
  toLigoValueConverter <- DV.newManager (unliftIO . convertMichelsonValuesToLigo logMessage)
  atomically $ writeTVar lServVar $ Just LigoLanguageServerState
    { lsProgram = Nothing
    , lsCollectedRunInfo = Nothing
    , lsEntrypoint = Nothing
    , lsAllLocs = Nothing
    , lsBinaryPath = req.binaryPath
    , lsParsedContracts = Nothing
    , lsLambdaLocs = Nothing
    , lsLigoTypesVec = Nothing
    , lsToLigoValueConverter = toLigoValueConverter
    , lsVarsComputeThreadPool = varsComputeThreadPool
    , lsMoveId = 0
    , lsMaxSteps = maxStepsMb
    , lsEntrypointType = Nothing
    }
  do let binaryPath = req.binaryPath
     logMessage [int||Set LIGO binary path: #s{binaryPath}|]

  rawVersion <- getLigoVersion
  logMessage [int||Ligo version: #{getVersion rawVersion}|]

  -- Pro-actively check that ligo version is supported
  runMaybeT do
    Just ligoVer <- pure $ parseLigoVersion rawVersion
    VersionUnsupported <- pure $ isSupportedVersion ligoVer
    throwIO $ UnsupportedLigoVersionException ligoVer

  DAP.respond ()

setProgramPathHandler :: DAP.Handler (RIO LIGO)
setProgramPathHandler = mkLigoHandler \req@LigoSetProgramPathRequest{} -> do
  getExt req.program
    & either throwIO (void . pure)

  EntrypointsList{..} <- getAvailableEntrypoints req.program

  lServVar <- asks _rcLSState
  atomically $ modifyTVar lServVar $ fmap \lServ -> lServ
    { lsProgram = Just req.program
    }
  LigoSetProgramPathResponse
    { entrypoints = unEntrypoints
        <&> \ep@EntrypointName{..} -> ([int||#{enModule}.#{enName}|], pretty ep)
    }
    `respondAndAlso` do

    let program = req.program
    logMessage [int||Setting program path #{program} is finished|]

validateEntrypointHandler :: DAP.Handler (RIO LIGO)
validateEntrypointHandler = mkLigoHandler \req@LigoValidateEntrypointRequest{} -> do
  program <- getProgram
  result <- try @_ @LigoCallException (checkCompilation (mkEntrypointName req.entrypoint) program)

  respond $ ligoValidateFromEither (first pretty result)

getContractMetadataHandler :: DAP.Handler (RIO LIGO)
getContractMetadataHandler = mkLigoHandler \req@LigoGetContractMetadataRequest{} -> do
  lServVar <- asks (_rcLSState @LIGO)
  program <- getProgram

  unlessM (doesFileExist program) $
    throwIO $ ConfigurationException [int||Contract file not found: #{toText program}|]

  let entrypointName = mkEntrypointName req.entrypoint

  -- Here we're catching exception explicitly in order to store it
  -- inside language server state and rethrow it in @initDebuggerSession@
  try (compileLigoContractDebug entrypointName program) >>= \case
    Left (LigoCallException msg) -> do
      -- Since we're packing this exception in @handleGetContractMetadata@
      -- when calling @compileLigoContractDebug@ this exception signalizes
      -- about the problem with an entrypoint.
      --
      -- Here is explanation. @compileLigoContractDebug@ can fail in 3 cases:
      -- 1. Something is wrong with @ligo@ binary.
      -- 2. Our contract is malformed.
      -- 3. Can't compile the contract with the given entrypoint.
      --
      -- 1 and 2 are cutting of after @handleSetProgramPath@ because of calling
      -- @getAvailableEntrypoints@.
      throwIO $ ConfigurationException msg

    Right ligoDebugInfo -> do
      logMessage $ "Successfully read the LIGO debug output for " <> pretty program

      (exprLocs, someContract, allFiles, lambdaLocs, entrypointType, ligoTypesVec) <-
        readLigoMapper ligoDebugInfo
        & either (throwIO . MichelsonDecodeException) pure

      do
        SomeContract (contract@Contract{} :: Contract cp st) <- pure someContract
        logMessage $ pretty (unContractCode $ cCode contract)

        parsedContracts <- parseContracts allFiles

        let statementLocs = getStatementLocs (getAllSourceLocations exprLocs) parsedContracts
        let allLocs = getInterestingSourceLocations parsedContracts exprLocs <> statementLocs

        let
          paramNotes = cParamNotes contract
          michelsonEntrypoints =
            T.flattenEntrypoints U.WithImplicitDefaultEp paramNotes
            -- Now we have only module entrypoints. If the contract has
            -- only one @entry then we should provide default entrypoint.
            -- Otherwise, we can provide only a list of entrypoints without
            -- a default one.
            & \mp -> if M.size mp > 1 then M.delete DefEpName mp else mp

        atomically $ modifyTVar lServVar $ fmap \lServ -> lServ
          { lsCollectedRunInfo = Just $ onlyContractRunInfo contract
          , lsAllLocs = Just allLocs
          , lsParsedContracts = Just parsedContracts
          , lsLambdaLocs = Just lambdaLocs
          , lsLigoTypesVec = Just ligoTypesVec
          , lsEntrypointType = Just entrypointType
          }

        lServerState <- getServerState

        logMessage [int||
          Got metadata for contract #{program}:
            Server state: #{lServerState}
            Michelson entrypoints: #{keys michelsonEntrypoints}
          |]

        respond ContractMetadata
          { parameterMichelsonType =
              JsonFromBuildable (T.convertParamNotes paramNotes)
          , storageMichelsonType =
              JsonFromBuildable (T.mkUType $ T.cStoreNotes contract)
          , michelsonEntrypoints =
              JsonFromBuildable <$> michelsonEntrypoints
          }

validateValueHandler :: DAP.Handler (RIO LIGO)
validateValueHandler = mkLigoHandler \req@LigoValidateValueRequest{} -> do
  CollectedRunInfo
    { criContract = contract@Contract{} :: Contract param storage
    } <- getCollectedRunInfo

  program <- getProgram
  entrypointType <- getEntrypointType

  let (parameterType, storageType, _) = getParameterStorageAndOpsTypes entrypointType

  parseRes <- case req.category of
    "parameter" ->
      withMichelsonEntrypoint contract req.pickedMichelsonEntrypoint $
        \(_ :: T.Notes arg) _ ->
        void <$> parseValue @arg program
          req.category req.value req.valueLang parameterType

    "storage" ->
      void <$> parseValue @storage program
        req.category req.value req.valueLang storageType

    other ->
      throwIO $ PluginCommunicationException [int||Unexpected category #{other}|]

  respond $ ligoValidateFromEither parseRes

validateConfigHandler :: DAP.Handler (RIO LIGO)
validateConfigHandler = mkLigoHandler \req@LigoValidateConfigRequest{} -> do
  -- Getting a contract here because of GHC complains:
  --  • Couldn't match type ‘a0’ with ‘()’
  --         ‘a0’ is untouchable
  CollectedRunInfo
    { criContract = contract@Contract{} :: Contract cp st
    } <- getCollectedRunInfo

  lServVar <- asks _rcLSState

  program <- getProgram
  entrypointType <- getEntrypointType

  let (parameterType, storageType, _) = getParameterStorageAndOpsTypes entrypointType

  withMichelsonEntrypoint contract req.michelsonEntrypoint
    \(_ :: T.Notes arg) epc -> do
      do let param = req.parameter; paramLang = req.parameterLang
         logMessage [int||
          Checking parameter #{param} with lang #{paramLang}
         |]
      param <- parseValue @arg program "parameter" req.parameter req.parameterLang parameterType
        >>= either (throwIO . ConfigurationException) pure

      do let storage = req.storage; storageLang = req.storageLang
         logMessage [int||
          Checking storage #{storage} with lang #{storageLang}
         |]
      stor <- parseValue @st program "storage" req.storage req.storageLang storageType
        >>= either (throwIO . ConfigurationException) pure

      atomically $ modifyTVar lServVar $ fmap \lServ -> lServ
        { lsCollectedRunInfo = Just $ CollectedRunInfo
            { criContract = contract
            , criEpcMb = Just epc
            , criParameterMb = Just param
            , criStorageMb = Just stor
            }
        }

  respond ()

initDebuggerSession
  :: LigoLaunchRequest
  -> RIO LIGO (DAPSessionState (InterpretSnapshot 'Unique))
initDebuggerSession req = do
  program <- getProgram

  CollectedRunInfo
    { criContract = contract
    , criEpcMb = epcMb
    , criParameterMb = paramMb
    , criStorageMb = storMb
    } <- getCollectedRunInfo

  epc <- "Entrypoint call is not initialized" `expectInitialized` pure epcMb
  param <- "Parameter is not initialized" `expectInitialized` pure paramMb
  stor <- "Storage is not initialized" `expectInitialized` pure storMb

  lambdaLocs <- getLambdaLocs

  -- We're adding our own contract in order to use
  -- @{ SELF_ADDRESS; CONTRACT }@ replacement
  -- (we need to have this contract state to use @CONTRACT@ instruction).
  let contractState = ContractState
        { csBalance = [tz|0u|]
        , csContract = contract
        , csStorage = stor
        , csDelegate = Nothing
        }

  allLocs <- getAllLocs
  parsedContracts <- getParsedContracts

  logMessage [int||Contract state: #{contractState}|]

  maxStepsMb <- getMaxStepsMb

  contractEnv <-
    initContractEnv
      contractState
      (req.contractEnv ?: def)
      (fromMaybe dummyMaxSteps maxStepsMb)

  ligoTypesVec <- getLigoTypesVec

  his <-
    withRunInIO \unlifter ->
      collectInterpretSnapshots
        program
        contract
        epc
        param
        stor
        contractEnv
        parsedContracts
        (unlifter . logMessage)
        lambdaLocs
        (isJust maxStepsMb)
        ligoTypesVec

  let ds = initDebuggerState his allLocs

  pure $ DAPSessionState ds mempty mempty

initContractEnv
  :: (MonadIO m)
  => ContractState -> LigoContractEnv -> RemainingSteps -> m (ContractEnv IO)
initContractEnv selfState env ceMaxSteps = do
  ceNow <- liftIO $
    maybe (Timestamp <$> getPOSIXTime) (pure . unMichelsonJson) env.now

  let ceBalance = maybe [tz|1|] unMichelsonJson env.balance
  let ceAmount = maybe [tz|0|] unMichelsonJson env.amount

  let ceSelf =
        env.self
        ?: [ta|KT1XQcegsEtio9oGbLUHA8SKX4iZ2rpEXY9b|]
  let ceSource =
        env.source
        ?: Constrained [ta|tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY|]
  let ceSender =
        env.sender
        ?: Constrained [ta|tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY|]

  let ceChainId = maybe dummyChainId unMichelsonJson env.chainId
  let ceLevel = maybe 10000 unMichelsonJson env.level

  ceVotingPowers <- case env.votingPowers of
    Nothing -> pure $ mkVotingPowers
      [ (unsafe $ parseHash "tz1aZcxeRT4DDZZkYcU3vuBaaBRtnxyTmQRr", 100)
      ]
    Just spec -> case spec of
      SimpleVotingPowers (SimpleVotingPowersInfo vps) ->
        pure $ mkVotingPowers $
          bimap unMichelsonJson unMichelsonJson <$> toPairs vps

  -- ↑ It's good to keep the default addresses in match with default
  -- custom configuration in package.json.

  let ceErrorSrcPos = def
  let ceMinBlockTime = def
  let ceContracts addr = pure $ selfState <$ guard (addr == ceSelf)

  let ceOperationHash = Nothing
  pure ContractEnv{ceMetaWrapper = id, ..}

initDebuggerState :: InterpretHistory is -> Set SourceLocation -> DebuggerState is
initDebuggerState his allLocs = DebuggerState
  { _dsSnapshots = playInterpretHistory his
  , _dsSources =
      evaluatingState 0 $
      mapM @(Map MichelsonSource)
        (\locs -> mkDebugSource $ map fst $ toList @(Set _) locs)
        (groupSourceLocations $ toList allLocs)
  }
