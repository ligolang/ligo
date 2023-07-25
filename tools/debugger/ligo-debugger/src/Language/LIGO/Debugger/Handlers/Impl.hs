-- | Implementation of DAP handlers.
module Language.LIGO.Debugger.Handlers.Impl
  ( LIGO

    -- * Helpers
  , initDebuggerState
  , convertMichelsonValuesToLigo
  , initContractEnv
  ) where

import Prelude hiding (try)

import Debug qualified
import Unsafe qualified

import Control.Lens (Each (each), _Just, ix, uses, zoom, (+~), (.=), (^?!))
import Control.Monad.STM.Class (MonadSTM, liftSTM)
import Data.Char (toLower)
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Singletons (demote)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt.Buildable (blockListF, pretty)
import Fmt.Utils (Doc)
import GHC.Conc (unsafeIOToSTM)
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

import Morley.Debugger.Core
  (DebugSource (..), DebuggerState (..), NavigableSnapshot (getLastExecutedPosition),
  PausedReason (..), SnapshotEdgeStatus (SnapshotAtEnd),
  SnapshotEndedWith (SnapshotEndedWithFail, SnapshotEndedWithOk), SourceLocation, SrcLoc (..),
  _slPath, curSnapshot, frozen, groupSourceLocations, pickSnapshotEdgeStatus, playInterpretHistory,
  slEnd)
import Morley.Debugger.DAP.Handlers (fromMichelsonSource, runSTMHandler, writePostAction)
import Morley.Debugger.DAP.LanguageServer (JsonFromBuildable (..))
import Morley.Debugger.DAP.RIO (logMessage, openLogHandle)
import Morley.Debugger.DAP.Types
  (DAPOutputMessage (..), DAPSessionState (..),
  DAPSpecificEvent (InvalidatedEvent, OutputEvent, StoppedEvent, TerminatedEvent),
  DAPSpecificResponse (..), HandlerEnv (..), HasSpecificMessages (..), RIO, RequestBase (..),
  RioContext (..), ShouldStop (..), StopEventDesc (..), StoppedReason (..), dsDebuggerState,
  dsVariables, pushMessage, unMichelsonJson)
import Morley.Debugger.Protocol.DAP (ScopesRequestArguments (frameIdScopesRequestArguments))
import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Michelson.ErrorPos (ErrorSrcPos (ErrorSrcPos), Pos (Pos), SrcPos (SrcPos))
import Morley.Michelson.Interpret
  (ContractEnv' (..), MichelsonFailed (MichelsonFailedWith),
  MichelsonFailureWithStack (mfwsErrorSrcPos), RemainingSteps (RemainingSteps), ceContracts,
  ceMaxSteps, mfwsFailed)
import Morley.Michelson.Parser.Types (MichelsonSource)
import Morley.Michelson.Printer.Util (RenderDoc (renderDoc), doesntNeedParens)
import Morley.Michelson.Runtime (ContractState (..), mkVotingPowers)
import Morley.Michelson.Runtime.Dummy (dummyMaxSteps)
import Morley.Michelson.Typed
  (Constrained (SomeValue), Contract, Contract' (..), ContractCode' (unContractCode),
  SomeContract (..))
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

instance HasSpecificMessages LIGO where
  type Launch LIGO = LigoLaunchRequest
  type ExtraRequestExt LIGO = LigoSpecificRequest
  type ExtraEventExt LIGO = Void
  type ExtraResponseExt LIGO = LigoSpecificResponse
  type LanguageServerStateExt LIGO = LigoLanguageServerState
  type InterpretSnapshotExt LIGO = InterpretSnapshot 'Unique
  type StopEventExt LIGO = InterpretEvent
  type StepGranularityExt LIGO = LigoStepGranularity
  type PausedReasonExt LIGO = LigoSpecificPausedReason

  reportErrorAndStoppedEvent = \case
    ExceptionMet exception -> writeException True exception
    Paused reason label -> writeStoppedEvent reason label
    TerminatedOkMet{} -> writeTerminatedEvent
    PastFinish -> do
      snap <- zoom dsDebuggerState $ frozen curSnapshot
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
        entrypointType <- getEntrypointTypeH

        let (_, storageType, opsAndStorageType) = getParameterStorageAndOpsTypes entrypointType

        InterpretSnapshot{..} <- zoom dsDebuggerState $ frozen curSnapshot
        let someValues = head isStackFrames ^.. sfStackL . each . siValueL
        let types = [opsAndStorageType, storageType]

        let convertInfos = zipWith PreLigoConvertInfo someValues types

        lServVar <- getServerStateH @LIGO
        let DV.Manager{..} = lsToLigoValueConverter lServVar

        -- @writeTerminatedEvent@ would be executed only once (on last snapshot)
        -- So, we can call this heavy computation in a synchronous way.
        -- Moreover, this STM computation is one-threaded. Thus means that
        -- we shouldn't be afraid of restarting transactions.
        decompiled <- liftSTM $ unsafeIOToSTM (mComputation convertInfos)

        (opsText, storeText, oldStoreText) <- case decompiled of
          [someValue, someStorage] -> do
            (ops, store) <- buildOpsAndNewStorage someValue
            oldStore <- buildOldStorage someStorage
            pure (ops, store, oldStore)
          _ -> throwM $ ImpossibleHappened [int||
            Expected the stack to only have 2 elements, but its length is \
            #{length someValues}.
            |]

        pushMessage $ DAPEvent $ OutputEvent $ DAP.defaultOutputEvent
          { DAP.bodyOutputEvent = DAP.defaultOutputEventBody
            { DAP.categoryOutputEventBody = "stdout"
            , DAP.outputOutputEventBody =
                [int||
                Execution completed.
                Operations:
                #{opsText}
                Storage:
                #{storeText}

                Old storage:
                #{oldStoreText}
                |]
            }
          }
        pushMessage $ DAPEvent $ TerminatedEvent $ DAP.defaultTerminatedEvent
          where
            buildOpsAndNewStorage :: (MonadThrow m) => LigoOrMichValue -> m (Doc, Doc)
            buildOpsAndNewStorage = \case
              LigoValue ligoType ligoValue -> case toTupleMaybe ligoValue of
                Just [LVList ops, st] -> do
                  let (fstType, sndType) = maybe (LigoType Nothing, LigoType Nothing) (bimap LigoType LigoType) do
                        LTCRecord LigoTypeTable{..} <- _lteTypeContent <$> unLigoType ligoType
                        let fstTyp = HM.lookup "0" _lttFields
                        let sndTyp = HM.lookup "1" _lttFields
                        pure (fstTyp, sndTyp)
                  pure
                    ( blockListF (buildLigoValue fstType <$> ops)
                    , buildLigoValue sndType st
                    )
                _ ->
                  throwM badLastElement
              MichValue _ (SomeValue val) -> case val of
                (T.VPair (T.VList ops, r :: T.Value r)) ->
                  case T.valueTypeSanity r of
                    T.Dict ->
                      case T.checkTPresence T.SPSOp (T.sing @r) of
                        T.TAbsent -> pure (blockListF ops, renderDoc doesntNeedParens r)
                        _ -> throwM invalidStorage
                _ -> throwM badLastElement
              _ -> throwM notComputed

            buildOldStorage :: (MonadThrow m) => LigoOrMichValue -> m Doc
            buildOldStorage = \case
              LigoValue ligoType ligoValue -> pure $ buildLigoValue ligoType ligoValue
              MichValue _ (SomeValue (st :: T.Value r)) ->
                case T.valueTypeSanity st of
                  T.Dict ->
                    case T.checkTPresence T.SPSOp (T.sing @r) of
                      T.TAbsent -> pure $ renderDoc doesntNeedParens st
                      _ -> throwM invalidStorage
              _ -> throwM notComputed

            invalidStorage = ImpossibleHappened "Invalid storage type"
            badLastElement = ImpossibleHappened "Expected the last element to be a pair of operations and storage"
            notComputed = ImpossibleHappened "Expected value to be computed"

      writeStoppedEvent reason label = do
        let hitBreakpointIds = case reason of
              -- Wait for this issue to be resolved:
              -- https://gitlab.com/morley-framework/morley-debugger/-/issues/91
              -- BreakpointPaused ids -> unBreakpointId <$> toList ids
              _ -> []

        (mDesc, mLongDesc) <- zoom dsDebuggerState $ frozen (getStopEventInfo @LIGO Proxy)
        let fullLabel = case mDesc of
              Nothing -> label
              Just (StopEventDesc desc) -> label <> ", " <> desc
        pushMessage $ DAPEvent $ StoppedEvent $ DAP.defaultStoppedEvent
          { DAP.bodyStoppedEvent = DAP.defaultStoppedEventBody
            { DAP.reasonStoppedEventBody = toString fullLabel
              -- ↑ By putting moderately large text we slightly violate DAP spec,
              -- but it seems to be worth it
            , DAP.threadIdStoppedEventBody = 1
            , DAP.allThreadsStoppedStoppedEventBody = True
            , DAP.textStoppedEventBody = maybe "" pretty mLongDesc
            , DAP.hitBreakpointIdsStoppedEventBody = hitBreakpointIds
            }
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

        lastPosMb <- uses dsDebuggerState getLastExecutedPosition
        let mSrcLoc = view slEnd <$> lastPosMb

        pushMessage $ DAPEvent $ StoppedEvent $ DAP.defaultStoppedEvent
          { DAP.bodyStoppedEvent = DAP.defaultStoppedEventBody
            { DAP.reasonStoppedEventBody = "exception"
            , DAP.threadIdStoppedEventBody = 1
            , DAP.allThreadsStoppedStoppedEventBody = True
            , DAP.descriptionStoppedEventBody = "Paused on exception"
            , DAP.textStoppedEventBody = msg
            }
          }
        when writeLog do
          pushMessage $ DAPEvent $ OutputEvent $ DAP.defaultOutputEvent
            { DAP.bodyOutputEvent = withSrc lastPosMb $ withSrcPos mSrcLoc DAP.defaultOutputEventBody
              { DAP.categoryOutputEventBody = "stderr"
              , DAP.outputOutputEventBody = msg <> "\n"
              }
            }
        where
          withSrc lastPosMb event = case lastPosMb of
            Nothing -> event
            Just pos -> event { DAP.sourceOutputEventBody = fromMichelsonSource $ _slPath pos }

          withSrcPos mSrcPos event = case mSrcPos of
            Nothing -> event
            Just (SrcLoc row col) -> event
              { DAP.lineOutputEventBody   = Unsafe.fromIntegral $ row + 1
              , DAP.columnOutputEventBody = Unsafe.fromIntegral $ col + 1
              }

  handleLaunch LigoLaunchRequest {..} = do
    st <- lift . lift $
      initDebuggerSession argumentsLigoLaunchRequest

    lift $ lift do
      logMessage "Launching contract with arguments\n"
      logMessage $ Debug.show argumentsLigoLaunchRequest <> "\n"
    put (Just st)
    pushMessage $ DAPResponse $ LaunchResponse DAP.defaultLaunchResponse
      { DAP.successLaunchResponse = True
      , DAP.request_seqLaunchResponse = seqLigoLaunchRequest
      }

  handleStackTraceRequest DAP.StackTraceRequest{..} = zoom dsDebuggerState do
    -- We mostly follow morley-debugger's implementation, but here we don't need
    -- to look at the next snapshot, the current one is what we want.
    snap <- frozen curSnapshot
    let frames = toDAPStackFrames snap
    pushMessage $ DAPResponse $ StackTraceResponse $ DAP.defaultStackTraceResponse
      { DAP.successStackTraceResponse = True
      , DAP.request_seqStackTraceResponse = seqStackTraceRequest
      , DAP.bodyStackTraceResponse = DAP.defaultStackTraceResponseBody
        { DAP.stackFramesStackTraceResponseBody = frames
        , DAP.totalFramesStackTraceResponseBody = length frames
        }
      }
    where
      toDAPStackFrames snap =
        let frames = toList $ isStackFrames snap
        in zip [topFrameId ..] frames <&> \(i, frame) ->
          let Range{..} = sfLoc frame
          in DAP.StackFrame
            { DAP.idStackFrame = i
            , DAP.nameStackFrame = toString $ sfName frame
            , DAP.sourceStackFrame = DAP.defaultSource
              { DAP.nameSource = Just $ takeFileName _rFile
              , DAP.pathSource = _rFile
              }
              -- TODO: use `IsSourceLoc` conversion capability
              -- Once morley-debugger#44 is merged
            , DAP.lineStackFrame = Unsafe.fromIntegral $ _lpLine _rStart
            , DAP.columnStackFrame = Unsafe.fromIntegral $ _lpCol _rStart
            , DAP.endLineStackFrame = Unsafe.fromIntegral $ _lpLine _rFinish
            , DAP.endColumnStackFrame = Unsafe.fromIntegral $ _lpCol _rFinish
            , DAP.canRestartStackFrame = False
            }

  handleScopesRequest DAP.ScopesRequest{..} = do
    lServVar <- getServerStateH
    ligoTypesVec <- getLigoTypesVecH
    -- We follow the implementation from morley-debugger
    snap <- zoom dsDebuggerState $ frozen curSnapshot

    let currentStackFrame = snap
          & isStackFrames
          & flip (^?!) (ix (frameIdScopesRequestArguments argumentsScopesRequest - 1))

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
          | frameIdScopesRequestArguments argumentsScopesRequest == 1 -> do
            itVal <- decompileValue (PreLigoConvertInfo value typ) valConvertManager
            pure do
              idx <- createVariables lang ligoVals
              itVar <- buildVariable lang itVal "$it"
              insertToIndex idx [itVar]
        _ -> pure $ createVariables lang ligoVals

    let (varReference, variables) = runBuilder builder

    dsVariables .= variables

    let moveIdAtUpdate = lsMoveId lServVar
    writePostAction do
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
          void . runSTMHandler $
            pushMessage $ DAPEvent $ InvalidatedEvent $ DAP.defaultInvalidatedEvent
              { DAP.bodyInvalidatedEvent = DAP.InvalidatedEventBody
                { DAP.areasInvalidatedEventBody = ["variables"]
                , DAP.threadIdInvalidatedEventBody = Nothing
                , DAP.stackFrameIdInvalidatedEventBody = Just topFrameId
                }
              }

    -- TODO [LIGO-304]: show detailed scopes
    let theScope = DAP.defaultScope
          { DAP.nameScope = "all variables"
          , DAP.variablesReferenceScope = varReference
          }
    pushMessage $ DAPResponse $ ScopesResponse $ DAP.defaultScopesResponse
      { DAP.successScopesResponse = True
      , DAP.request_seqScopesResponse = seqScopesRequest
      , DAP.bodyScopesResponse = DAP.ScopesResponseBody
        { DAP.scopesScopesResponseBody = [theScope]
        }
      }

  handlersWrapper RequestBase{..} = flip catches
    [ Handler \(SomeDebuggerException (err :: excType)) -> do
        versionIssuesDetails <- case debuggerExceptionType err of
          -- TODO: make this pure, carry version in the LS state
          MidLigoLayerException -> getVersionIssuesDetails
          _ -> pure Nothing

        writeErrResponse @excType $ DAP.defaultMessage
          { DAP.formatMessage = displayException err
          , DAP.variablesMessage = Just $ mconcat
              [ one ("origin", pretty (debuggerExceptionType err))
              , maybe mempty (one . ("versionIssues", ) . toString) versionIssuesDetails
              , one ("shouldInterruptDebuggingSession", map toLower $ pretty $ shouldInterruptDebuggingSession @excType)
              , debuggerExceptionData err
              ]
          }

        return $ ShouldStop False

    , Handler \(SomeException err) -> do
        writeErrResponse @ImpossibleHappened $
          [int||Internal (unhandled) error: #exc{err}|]
        return $ ShouldStop False
    ]
    where
      writeErrResponse
        :: forall e ext. DebuggerException e
        => DAP.Message -> RIO ext ()
      writeErrResponse errBody =
        writeResponse $ ErrorResponse DAP.defaultErrorResponse
          { DAP.request_seqErrorResponse = seqRequestBase
          , DAP.commandErrorResponse = commandRequestBase
          , DAP.messageErrorResponse = Just $ toString $ demote @(ExceptionTag e)
          , DAP.bodyErrorResponse = DAP.ErrorResponseBody $ Just errBody
          }


  handleRequestExt = \case
    InitializeLoggerRequest req -> handleInitializeLogger req
    SetLigoConfigRequest req -> handleSetLigoConfig req
    SetProgramPathRequest req -> handleSetProgramPath req
    ValidateEntrypointRequest req -> handleValidateEntrypoint req
    GetContractMetadataRequest req -> handleGetContractMetadata req
    ValidateValueRequest req -> handleValidateValue req
    ValidateConfigRequest req -> handleValidateConfig req

  reportContractLogs _ = pass

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

  handleVariablesRequest DAP.VariablesRequest{..} = do
    let ref = DAP.variablesReferenceVariablesRequestArguments argumentsVariablesRequest
    vars <- gets _dsVariables
    case vars ^? ix ref of
      Nothing ->
        throwM $ PluginCommunicationException "The referred variable does not exist"
      Just vs ->
        pushMessage $ DAPResponse $ VariablesResponse $ DAP.defaultVariablesResponse
          { DAP.successVariablesResponse = True
          , DAP.request_seqVariablesResponse = seqVariablesRequest
          , DAP.bodyVariablesResponse = DAP.VariablesResponseBody vs
          }

  parseStepGranularity = \case
    Nothing -> pure def
    Just t -> case t of
      "statement" -> pure GStmt
      "expression" -> pure GExp
      "expressionSurrounded" -> pure GExpExt
      other -> Left [int||Unknown granularity `#{other}`|]

  processStep = processLigoStep

  handleSetPreviousStack = pure ()

  onTerminate restart = unless restart do
    lServState <- getServerStateH
    AbortingThreadPool.close (lsVarsComputeThreadPool lServState)

    lServVar <- asks heLSState
    atomically $ writeTVar lServVar Nothing

  onStep = do
    lServVar <- asks heLSState
    liftSTM $ modifyTVar' lServVar $ _Just . lsMoveIdL +~ 1

-- | Id of the top (currently active) stack frame.
topFrameId :: Int
topFrameId = 1

decompileValue
  :: (MonadSTM m, Monad m)
  => PreLigoConvertInfo
  -> Manager PreLigoConvertInfo LigoOrMichValue
  -> m LigoOrMichValue
decompileValue convertInfo@(PreLigoConvertInfo val typ) manager = do
  whenJust (tryDecompilePrimitive val) \dec -> do
    DV.putComputed
      manager
      convertInfo
      (LigoValue typ dec)

  mLigoVal <- DV.computeSTM manager convertInfo
  pure $ fromMaybe ToBeComputed mLigoVal

handleInitializeLogger :: LigoInitializeLoggerRequest -> RIO LIGO ()
handleInitializeLogger LigoInitializeLoggerRequest {..} = do
  let file = fileLigoInitializeLoggerRequestArguments argumentsLigoInitializeLoggerRequest
  let logFileMb = do
        dir <- logDirLigoInitializeLoggerRequestArguments argumentsLigoInitializeLoggerRequest
        Just $ dir </> takeFileName file <.> "log"
  whenJust logFileMb openLogHandle

  unlessM (doesFileExist file) do
    throwIO $ ConfigurationException [int||Contract file not found: #{toText file}|]

  writeResponse $ ExtraResponse $ InitializeLoggerResponse LigoInitializeLoggerResponse
    { seqLigoInitializeLoggerResponse = 0
    , request_seqLigoInitializeLoggerResponse = seqLigoInitializeLoggerRequest
    , successLigoInitializeLoggerResponse = True
    }

  logMessage [int||Initializing logger for #{file} finished|]

convertMichelsonValuesToLigo :: (HasLigoClient m) => (String -> m ()) -> [PreLigoConvertInfo] -> m [LigoOrMichValue]
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

handleSetLigoConfig :: LigoSetLigoConfigRequest -> RIO LIGO ()
handleSetLigoConfig LigoSetLigoConfigRequest {..} = do
  let LigoSetLigoConfigRequestArguments{..} = argumentsLigoSetLigoConfigRequest
  let binaryPathMb = binaryPathLigoSetLigoConfigRequestArguments

  let maxStepsMb = RemainingSteps <$> maxStepsLigoSetLigoConfigRequestArguments

  let binaryPath = Debug.show @Text binaryPathMb

  UnliftIO unliftIO <- askUnliftIO

  lServVar <- asks _rcLSState
  varsComputeThreadPool <- AbortingThreadPool.newPool 10
  toLigoValueConverter <- DV.newManager (unliftIO . convertMichelsonValuesToLigo logMessage)
  atomically $ writeTVar lServVar $ Just LigoLanguageServerState
    { lsProgram = Nothing
    , lsCollectedRunInfo = Nothing
    , lsEntrypoint = Nothing
    , lsAllLocs = Nothing
    , lsBinaryPath = binaryPathMb
    , lsParsedContracts = Nothing
    , lsLambdaLocs = Nothing
    , lsLigoTypesVec = Nothing
    , lsToLigoValueConverter = toLigoValueConverter
    , lsVarsComputeThreadPool = varsComputeThreadPool
    , lsMoveId = 0
    , lsMaxSteps = maxStepsMb
    , lsEntrypointType = Nothing
    }
  logMessage [int||Set LIGO binary path: #{binaryPath}|]

  rawVersion <- getLigoVersion
  logMessage [int||Ligo version: #{getVersion rawVersion}|]

  -- Pro-actively check that ligo version is supported
  runMaybeT do
    Just ligoVer <- pure $ parseLigoVersion rawVersion
    VersionUnsupported <- pure $ isSupportedVersion ligoVer
    throwIO $ UnsupportedLigoVersionException ligoVer

  writeResponse $ ExtraResponse $ SetLigoConfigResponse LigoSetLigoConfigResponse
    { seqLigoSetLigoConfigResponse = 0
    , request_seqLigoSetLigoConfigResponse = seqLigoSetLigoConfigRequest
    , successLigoSetLigoConfigResponse = True
    }

handleSetProgramPath :: LigoSetProgramPathRequest -> RIO LIGO ()
handleSetProgramPath LigoSetProgramPathRequest{..} = do
  let LigoSetProgramPathRequestArguments{..} = argumentsLigoSetProgramPathRequest
  let programPath = programLigoSetProgramPathRequestArguments

  getExt programPath
    & either throwIO (void . pure)

  EntrypointsList{..} <- getAvailableEntrypoints programPath

  lServVar <- asks _rcLSState
  atomically $ modifyTVar lServVar $ fmap \lServ -> lServ
    { lsProgram = Just programPath
    }

  writeResponse $ ExtraResponse $ SetProgramPathResponse LigoSetProgramPathResponse
    { seqLigoSetProgramPathResponse = 0
    , request_seqLigoSetProgramPathResponse = seqLigoSetProgramPathRequest
    , successLigoSetProgramPathResponse = True
    , entrypointsLigoSetProgramPathResponse = unEntrypoints
    }

  logMessage [int||Setting program path #{programPath} is finished|]

handleValidateEntrypoint :: LigoValidateEntrypointRequest -> RIO LIGO ()
handleValidateEntrypoint LigoValidateEntrypointRequest{..} = do
  let LigoValidateEntrypointRequestArguments{..} = argumentsLigoValidateEntrypointRequest
  let pickedEntrypoint = entrypointLigoValidateEntrypointRequestArguments

  program <- getProgram
  result <- try @_ @LigoCallException (checkCompilation pickedEntrypoint program)

  writeResponse $ ExtraResponse $ ValidateEntrypointResponse LigoValidateEntrypointResponse
    { seqLigoValidateEntrypointResponse = 0
    , request_seqLigoValidateEntrypointResponse = seqLigoValidateEntrypointRequest
    , successLigoValidateEntrypointResponse = True
    , messageLigoValidateEntrypointResponse = pretty <$> leftToMaybe result
    }

handleGetContractMetadata :: LigoGetContractMetadataRequest -> RIO LIGO ()
handleGetContractMetadata LigoGetContractMetadataRequest{..} = do
  let LigoGetContractMetadataRequestArguments{..} = argumentsLigoGetContractMetadataRequest
  let entrypoint = entrypointLigoGetContractMetadataRequestArguments

  lServVar <- asks _rcLSState
  program <- getProgram

  unlessM (doesFileExist program) $
    throwIO $ ConfigurationException [int||Contract file not found: #{toText program}|]

  -- Here we're catching exception explicitly in order to store it
  -- inside language server state and rethrow it in @initDebuggerSession@
  try (compileLigoContractDebug entrypoint program) >>= \case
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

        writeResponse $ ExtraResponse $ GetContractMetadataResponse LigoGetContractMetadataResponse
          { seqLigoGetContractMetadataResponse = 0
          , request_seqLigoGetContractMetadataResponse =
              seqLigoGetContractMetadataRequest
          , successLigoGetContractMetadataResponse = True
          , contractMetadataLigoGetContractMetadataResponse = ContractMetadata
              { parameterMichelsonTypeContractMetadata =
                  JsonFromBuildable (T.convertParamNotes paramNotes)
              , storageMichelsonTypeContractMetadata =
                  JsonFromBuildable (T.mkUType $ T.cStoreNotes contract)
              , michelsonEntrypointsContractMetadata =
                  JsonFromBuildable <$> michelsonEntrypoints
              }
          }

handleValidateValue :: LigoValidateValueRequest -> RIO LIGO ()
handleValidateValue LigoValidateValueRequest {..} = do
  let LigoValidateValueRequestArguments
        { valueLigoValidateValueRequestArguments =
            value
        , categoryLigoValidateValueRequestArguments =
            (toText -> category)
        , valueLangLigoValidateValueRequestArguments =
            (toText -> valueLang)
        , pickedMichelsonEntrypointLigoValidateValueRequestArguments =
            michelsonEntrypoint
        } = argumentsLigoValidateValueRequest

  CollectedRunInfo
    { criContract = contract@Contract{} :: Contract param storage
    } <- getCollectedRunInfo

  program <- getProgram
  entrypointType <- getEntrypointType

  let (parameterType, storageType, _) = getParameterStorageAndOpsTypes entrypointType

  parseRes <- case category of
    "parameter" ->
      withMichelsonEntrypoint contract michelsonEntrypoint $
        \(_ :: T.Notes arg) _ ->
        void <$> parseValue @arg program category (toText value) valueLang parameterType

    "storage" ->
      void <$> parseValue @storage program category (toText value) valueLang storageType

    other ->
      throwIO $ PluginCommunicationException [int||Unexpected category #{other}|]

  writeResponse $ ExtraResponse $ ValidateValueResponse LigoValidateValueResponse
    { seqLigoValidateValueResponse = 0
    , request_seqLigoValidateValueResponse = seqLigoValidateValueRequest
    , successLigoValidateValueResponse = True
    , messageLigoValidateValueResponse = toString <$> leftToMaybe parseRes
    }

handleValidateConfig :: LigoValidateConfigRequest -> RIO LIGO ()
handleValidateConfig LigoValidateConfigRequest{..} = do
  let LigoValidateConfigRequestArguments
        { michelsonEntrypointLigoValidateConfigRequestArguments = michelsonEntrypointMb
        , parameterLigoValidateConfigRequestArguments = toText -> parameter
        , parameterLangLigoValidateConfigRequestArguments = toText -> parameterLang
        , storageLigoValidateConfigRequestArguments = toText -> storage
        , storageLangLigoValidateConfigRequestArguments = toText -> storageLang
        } = argumentsLigoValidateConfigRequest

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

  withMichelsonEntrypoint contract michelsonEntrypointMb
    \(_ :: T.Notes arg) epc -> do
      logMessage [int||
        Checking parameter #{parameter} with lang #{parameterLang}
      |]
      param <- parseValue @arg program "parameter" parameter parameterLang parameterType
        >>= either (throwIO . ConfigurationException) pure

      logMessage [int||
        Checking storage #{storage} with lang #{storageLang}
      |]
      stor <- parseValue @st program "storage" storage storageLang storageType
        >>= either (throwIO . ConfigurationException) pure

      atomically $ modifyTVar lServVar $ fmap \lServ -> lServ
        { lsCollectedRunInfo = Just $ CollectedRunInfo
            { criContract = contract
            , criEpcMb = Just epc
            , criParameterMb = Just param
            , criStorageMb = Just stor
            }
        }

  writeResponse $ ExtraResponse $ ValidateConfigResponse LigoValidateConfigResponse
    { seqLigoValidateConfigResponse = 0
    , request_seqLigoValidateConfigResponse = seqLigoValidateConfigRequest
    , successLigoValidateConfigResponse = True
    }

initDebuggerSession
  :: LigoLaunchRequestArguments
  -> RIO LIGO (DAPSessionState (InterpretSnapshot 'Unique))
initDebuggerSession LigoLaunchRequestArguments {..} = do
  entrypoint <- checkArgument "entrypoint" entrypointLigoLaunchRequestArguments
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
      (contractEnvLigoLaunchRequestArguments ?: def)
      (fromMaybe dummyMaxSteps maxStepsMb)

  ligoTypesVec <- getLigoTypesVec

  his <-
    withRunInIO \unlifter ->
      collectInterpretSnapshots
        program
        (fromString entrypoint)
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
  => ContractState -> LigoContractEnvArguments -> RemainingSteps -> m (ContractEnv IO)
initContractEnv selfState LigoContractEnvArguments{..} ceMaxSteps = do
  ceNow <- liftIO $
    maybe (Timestamp <$> getPOSIXTime) (pure . unMichelsonJson)
    nowLigoContractEnvArguments

  let ceBalance = maybe [tz|1|] unMichelsonJson balanceLigoContractEnvArguments
  let ceAmount = maybe [tz|0|] unMichelsonJson amountLigoContractEnvArguments

  let ceSelf =
        selfLigoContractEnvArguments
        ?: [ta|KT1XQcegsEtio9oGbLUHA8SKX4iZ2rpEXY9b|]
  let ceSource =
        sourceLigoContractEnvArguments
        ?: Constrained [ta|tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY|]
  let ceSender =
        senderLigoContractEnvArguments
        ?: Constrained [ta|tz1hTK4RYECTKcjp2dddQuRGUX5Lhse3kPNY|]

  let ceChainId = maybe dummyChainId unMichelsonJson chainIdLigoContractEnvArguments
  let ceLevel = maybe 10000 unMichelsonJson levelLigoContractEnvArguments

  ceVotingPowers <- case votingPowersLigoContractEnvArguments of
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
      fmap @(Map MichelsonSource)
        (\locs -> DebugSource mempty (fromList . map fst $ toList @(Set _) locs) 0)
        (groupSourceLocations $ toList allLocs)
  }

checkArgument :: MonadIO m => Text -> Maybe a -> m a
checkArgument _    (Just a) = pure a
checkArgument name Nothing  = throwIO $ ConfigurationException
  [int||Required configuration option "#{name}" not found in launch.json.|]
