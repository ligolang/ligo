-- | Implementation of DAP handlers.
module Language.LIGO.Debugger.Handlers.Impl
  ( LIGO
  ) where

import Prelude hiding (try)

import Debug qualified
import Unsafe qualified

import Cli (HasLigoClient (getLigoClientEnv), LigoClientEnv (..))
import Control.Lens (Each (each), ix, uses, zoom, (.=), (^?!))
import Data.Map qualified as M
import Data.Text qualified as Text
import Fmt (Builder, blockListF, pretty)
import Morley.Debugger.Core (slSrcPos)
import Morley.Debugger.Core.Navigate
  (DebugSource (..), DebuggerState (..), NavigableSnapshot (getLastExecutedPosition), curSnapshot,
  frozen, groupSourceLocations, playInterpretHistory)
import Morley.Debugger.DAP.LanguageServer (JsonFromBuildable (..))
import Morley.Debugger.DAP.RIO (logMessage, openLogHandle)
import Morley.Debugger.DAP.Types
  (DAPOutputMessage (..), DAPSessionState (..),
  DAPSpecificEvent (OutputEvent, StoppedEvent, TerminatedEvent), DAPSpecificResponse (..),
  HasSpecificMessages (..), RIO, RequestBase (..), RioContext (..), StopEventDesc (..),
  StoppedReason (..), dsDebuggerState, dsVariables, pushMessage)
import Morley.Debugger.Protocol.DAP (ScopesRequestArguments (frameIdScopesRequestArguments))
import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Michelson.ErrorPos (Pos (Pos), SrcPos (SrcPos))
import Morley.Michelson.Interpret (ContractEnv (ceSelf), ceContracts)
import Morley.Michelson.Printer.Util (RenderDoc (renderDoc), doesntNeedParens, printDocB)
import Morley.Michelson.Runtime (AddressState (ASContract), ContractState (..))
import Morley.Michelson.Runtime.Dummy (dummyContractEnv)
import Morley.Michelson.Typed
  (Contract, Contract' (..), ContractCode' (unContractCode), SomeConstrainedValue (SomeValue),
  SomeContract (..))
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import Morley.Tezos.Address (ta)
import Morley.Tezos.Core (tz)
import System.FilePath (takeFileName, (<.>), (</>))
import Text.Interpolation.Nyan
import UnliftIO (withRunInIO)
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Exception (Handler (Handler), catches, fromEither, throwIO, try)
import UnliftIO.STM (modifyTVar)

import Cli qualified as LSP.Cli

import Language.LIGO.DAP.Variables

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Common (getStatementLocs)
import Language.LIGO.Debugger.Handlers.Helpers
import Language.LIGO.Debugger.Handlers.Types
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Debugger.Snapshots

data LIGO

instance HasLigoClient (RIO LIGO) where
  getLigoClientEnv = do
    lServ <- asks _rcLSState >>= readTVarIO
    lift $ getClientEnv lServ
    where
      getClientEnv :: Maybe LigoLanguageServerState -> IO LigoClientEnv
      getClientEnv = \case
        Just lServ -> do
          let maybeEnv = pure . LigoClientEnv <$> lsBinaryPath lServ
          fromMaybe getLigoClientEnv maybeEnv
        Nothing -> getLigoClientEnv

instance HasSpecificMessages LIGO where
  type Launch LIGO = LigoLaunchRequest
  type ExtraRequestExt LIGO = LigoSpecificRequest
  type ExtraEventExt LIGO = Void
  type ExtraResponseExt LIGO = LigoSpecificResponse
  type LanguageServerStateExt LIGO = LigoLanguageServerState
  type InterpretSnapshotExt LIGO = InterpretSnapshot 'Unique
  type StopEventExt LIGO = InterpretEvent

  reportErrorAndStoppedEvent = \case
    ExceptionMet exception -> writeException exception
    Paused reason -> writeStoppedEvent reason
    Terminated -> writeTerminatedEvent
    ReachedStart -> writeStoppedEvent "Reached start"
    where
      writeTerminatedEvent = do
        InterpretSnapshot{..} <- zoom dsDebuggerState $ frozen curSnapshot
        let someValues = head isStackFrames ^.. sfStackL . each . siValueL

        let result = case someValues of
              [someValue, someStorage] -> buildStoreOps someValue someStorage
              _ -> Left
                  [int||
                  Internal Error: Expected the stack to only have 2 elements, but its length is \
                  #{length someValues}.
                  |]

        case result of
          Right (opsText, storeText, oldStoreText) -> do
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

          Left errMsg ->
            pushMessage . DAPResponse $ ErrorResponse DAP.defaultErrorResponse
            { DAP.bodyErrorResponse = DAP.ErrorResponseBody $ Just
                (DAP.defaultMessage { DAP.formatMessage = errMsg })
            }
          where
            buildStoreOps :: T.SomeValue -> T.SomeValue -> Either String (Builder, Builder, Builder)
            buildStoreOps (SomeValue val) (SomeValue (st :: T.Value r')) = case val of
              (T.VPair (T.VList ops, r :: T.Value r)) ->
                case (T.valueTypeSanity r, T.valueTypeSanity st) of
                  (T.Dict, T.Dict) ->
                    case (T.checkOpPresence (T.sing @r), T.checkOpPresence (T.sing @r')) of
                      (T.OpAbsent, T.OpAbsent) -> Right
                        ( blockListF ops
                        , printDocB False $ renderDoc doesntNeedParens r
                        , printDocB False $ renderDoc doesntNeedParens st
                        )
                      _ -> Left "Internal Error: Invalid storage type."

              _ -> Left "Internal Error: Expected the last element to be a pair of operations and storage."

      writeStoppedEvent reason = do
        (mDesc, mLongDesc) <- zoom dsDebuggerState $ frozen (getStopEventInfo @LIGO Proxy)
        let fullReason = case mDesc of
              Nothing -> reason
              Just (StopEventDesc desc) -> reason <> ", " <> desc
        pushMessage $ DAPEvent $ StoppedEvent $ DAP.defaultStoppedEvent
          { DAP.bodyStoppedEvent = DAP.defaultStoppedEventBody
            { DAP.reasonStoppedEventBody = toString fullReason
              -- ↑ By putting moderately large text we slightly violate DAP spec,
              -- but it seems to be worth it
            , DAP.threadIdStoppedEventBody = 1
            , DAP.allThreadsStoppedStoppedEventBody = True
            , DAP.textStoppedEventBody = maybe "" pretty mLongDesc
            }
          }

      writeException exception = do
        st <- get
        let msg = pretty exception
        mSrcLoc <- view slSrcPos <<$>> uses dsDebuggerState getLastExecutedPosition
        pushMessage $ DAPEvent $ StoppedEvent $ DAP.defaultStoppedEvent
          { DAP.bodyStoppedEvent = DAP.defaultStoppedEventBody
            { DAP.reasonStoppedEventBody = "exception"
            , DAP.threadIdStoppedEventBody = 1
            , DAP.allThreadsStoppedStoppedEventBody = True
            , DAP.descriptionStoppedEventBody = "Paused on exception"
            , DAP.textStoppedEventBody = msg
            }
          }
        pushMessage $ DAPEvent $ OutputEvent $ DAP.defaultOutputEvent
          { DAP.bodyOutputEvent = withSrc st $ withSrcPos mSrcLoc DAP.defaultOutputEventBody
            { DAP.categoryOutputEventBody = "stderr"
            , DAP.outputOutputEventBody = msg <> "\n"
            }
          }
        where
          withSrc st event = event { DAP.sourceOutputEventBody = mkSource st }

          mkSource DAPSessionState{..} = DAP.defaultSource
            { DAP.nameSource = Just $ takeFileName _dsSource
            , DAP.pathSource = _dsSource
            }

          withSrcPos mSrcPos event = case mSrcPos of
            Nothing -> event
            Just (SrcPos (Pos row) (Pos col)) -> event
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
        in zip [1..] frames <&> \(i, frame) ->
          let LigoRange{..} = sfLoc frame
          in DAP.StackFrame
            { DAP.idStackFrame = i
            , DAP.nameStackFrame = toString $ sfName frame
            , DAP.sourceStackFrame = DAP.defaultSource
              { DAP.nameSource = Just $ takeFileName lrFile
              , DAP.pathSource = lrFile
              }
              -- TODO: use `IsSourceLoc` conversion capability
              -- Once morley-debugger#44 is merged
            , DAP.lineStackFrame = Unsafe.fromIntegral $ lpLine lrStart
            , DAP.columnStackFrame = Unsafe.fromIntegral $ lpCol lrStart + 1
            , DAP.endLineStackFrame = Unsafe.fromIntegral $ lpLine lrEnd
            , DAP.endColumnStackFrame = Unsafe.fromIntegral $ lpCol lrEnd + 1
            }

  handleScopesRequest DAP.ScopesRequest{..} = do
    -- We follow the implementation from morley-debugger
    snap <- zoom dsDebuggerState $ frozen curSnapshot
    let stackItems = snap
          & isStackFrames
          & flip (^?!) (ix (frameIdScopesRequestArguments argumentsScopesRequest - 1))
          & sfStack

    let builder =
          case isStatus snap of
            InterpretRunning (EventExpressionEvaluated (Just (SomeValue value)))
              -- We want to show $it variable only in the top-most stack frame.
              | frameIdScopesRequestArguments argumentsScopesRequest == 1 -> do
                idx <- createVariables stackItems
                -- TODO: get the type of "$it" value
                itVar <- buildVariable LTUnresolved value "$it"
                insertToIndex idx [itVar]
            _ -> createVariables stackItems

    let (varReference, variables) = runBuilder builder

    dsVariables .= variables

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

  handlersWrapper RequestBase{..} =
    let
      writeErrResponse :: Text -> DAP.Message -> RIO ext ()
      writeErrResponse msgTag fullMsg =
        writeResponse $ ErrorResponse DAP.defaultErrorResponse
          { DAP.request_seqErrorResponse = seqRequestBase
          , DAP.commandErrorResponse = commandRequestBase
          , DAP.messageErrorResponse = Just (toString msgTag)
          , DAP.bodyErrorResponse = DAP.ErrorResponseBody $ Just fullMsg
          }
    in flip catches
      [ Handler \(e :: LigoException) -> do
          writeErrResponse (leMessage e) (pretty e)

      , Handler \(DapMessageException msg :: DapMessageException) -> do
          writeErrResponse (toText $ DAP.formatMessage msg) msg

      , Handler \(e :: UnsupportedLigoVersionException) -> do
          writeErrResponse (pretty e) (pretty e)
      ]

  handleRequestExt = \case
    InitializeLoggerRequest req -> handleInitializeLogger req
    SetLigoBinaryPathRequest req -> handleSetLigoBinaryPath req
    SetProgramPathRequest req -> handleSetProgramPath req
    ValidateEntrypointRequest req -> handleValidateEntrypoint req
    GetContractMetadataRequest req -> handleGetContractMetadata req
    ValidateValueRequest req -> handleValidateValue req

  reportContractLogs _ = pass

  getStopEventInfo Proxy = curSnapshot <&> \snap -> case isStatus snap of
    InterpretRunning event ->
      let
        shortDesc = case event of
          EventFacedStatement -> Just $ StopEventDesc "at statement"
          EventExpressionPreview -> Just $ StopEventDesc "upon exp"
          EventExpressionEvaluated{} -> Just $ StopEventDesc "computed exp"
      in (shortDesc, Just event)
    _ -> (Nothing, Nothing)

  handleVariablesRequest DAP.VariablesRequest{..} = do
    let ref = DAP.variablesReferenceVariablesRequestArguments argumentsVariablesRequest
    vars <- gets _dsVariables
    case vars ^? ix ref of
      Nothing -> do
        pushMessage $ DAPResponse $ ErrorResponse $ DAP.defaultErrorResponse
          { DAP.request_seqErrorResponse = seqVariablesRequest
          , DAP.commandErrorResponse = commandVariablesRequest
          }
      Just vs ->
        pushMessage $ DAPResponse $ VariablesResponse $ DAP.defaultVariablesResponse
          { DAP.successVariablesResponse = True
          , DAP.request_seqVariablesResponse = seqVariablesRequest
          , DAP.bodyVariablesResponse = DAP.VariablesResponseBody vs
          }

  handleSetPreviousStack = pure ()

handleInitializeLogger :: LigoInitializeLoggerRequest -> RIO LIGO ()
handleInitializeLogger LigoInitializeLoggerRequest {..} = do
  let file = fileLigoInitializeLoggerRequestArguments argumentsLigoInitializeLoggerRequest
  let logFileMb = do
        dir <- logDirLigoInitializeLoggerRequestArguments argumentsLigoInitializeLoggerRequest
        Just $ dir </> takeFileName file <.> "log"
  whenJust logFileMb openLogHandle

  unlessM (doesFileExist file) do
    throwIO $ DapMessageException $ DAP.mkErrorMessage "Contract file not found" $ toText file

  writeResponse $ ExtraResponse $ InitializeLoggerResponse LigoInitializeLoggerResponse
    { seqLigoInitializeLoggerResponse = 0
    , request_seqLigoInitializeLoggerResponse = seqLigoInitializeLoggerRequest
    , successLigoInitializeLoggerResponse = True
    }

  logMessage [int||Initializing logger for #{file} finished|]

handleSetLigoBinaryPath :: LigoSetLigoBinaryPathRequest -> RIO LIGO ()
handleSetLigoBinaryPath LigoSetLigoBinaryPathRequest {..} = do
  let LigoSetLigoBinaryPathRequestArguments{..} = argumentsLigoSetLigoBinaryPathRequest
  let binaryPathMb = binaryPathLigoSetLigoBinaryPathRequestArguments

  let binaryPath = Debug.show @Text binaryPathMb

  lServVar <- asks _rcLSState
  atomically $ writeTVar lServVar $ Just LigoLanguageServerState
    { lsProgram = Nothing
    , lsContract = Nothing
    , lsEntrypoint = Nothing
    , lsAllLocs = Nothing
    , lsBinaryPath = binaryPathMb
    , lsParsedContracts = Nothing
    , lsSwallowedException = Nothing
    }
  logMessage [int||Set LIGO binary path: #{binaryPath}|]

  rawVersion <- getLigoVersion
  logMessage [int||Ligo version: #{LSP.Cli.getVersion rawVersion}|]

  -- Pro-actively check that ligo version is supported
  runMaybeT do
    Just ligoVer <- pure $ parseLigoVersion rawVersion
    VersionUnsupported <- pure $ isSupportedVersion ligoVer
    throwIO $ UnsupportedLigoVersionException ligoVer

  writeResponse $ ExtraResponse $ SetLigoBinaryPathResponse LigoSetLigoBinaryPathResponse
    { seqLigoSetLigoBinaryPathResponse = 0
    , request_seqLigoSetLigoBinaryPathResponse = seqLigoSetLigoBinaryPathRequest
    , successLigoSetLigoBinaryPathResponse = True
    }

handleSetProgramPath :: LigoSetProgramPathRequest -> RIO LIGO ()
handleSetProgramPath LigoSetProgramPathRequest{..} = do
  let LigoSetProgramPathRequestArguments{..} = argumentsLigoSetProgramPathRequest
  let programPath = programLigoSetProgramPathRequestArguments

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
  result <- void <$> try @_ @LigoException (compileLigoContractDebug pickedEntrypoint program)

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
    throwIO @_ @DapMessageException [int||Contract file not found: #{toText program}|]

  -- Here we're catching exception explicitly in order to store it
  -- inside language server state and rethrow it in @initDebuggerSession@
  try @_ @SomeDebuggerException (compileLigoContractDebug entrypoint program) >>= \case
    Left exc -> do
      atomically $ modifyTVar lServVar $ fmap \lServ ->
        lServ { lsSwallowedException = Just exc }

      writeResponse $ ExtraResponse $ GetContractMetadataResponse LigoGetContractMetadataResponse
        { seqLigoGetContractMetadataResponse = 0
        , request_seqLigoGetContractMetadataResponse =
            seqLigoGetContractMetadataRequest
        , successLigoGetContractMetadataResponse = True
        , contractMetadataLigoGetContractMetadataResponse = Nothing
        }
    Right ligoDebugInfo -> do
      logMessage $ "Successfully read the LIGO debug output for " <> pretty program

      (exprLocs, someContract, allFiles) <-
        readLigoMapper ligoDebugInfo typesReplaceRules instrReplaceRules
        & first [int|m|Failed to process contract: #{id}|]
        & fromEither @DapMessageException

      do
        SomeContract (contract@Contract{} :: Contract cp st) <- pure someContract
        logMessage $ pretty (unContractCode $ cCode contract)

        parsedContracts <- parseContracts allFiles

        let statementLocs = getStatementLocs exprLocs parsedContracts
        let allLocs = exprLocs <> statementLocs

        let
          paramNotes = cParamNotes contract
          michelsonEntrypoints =
            T.flattenEntrypoints paramNotes
            <> one (U.DefEpName, T.mkUType $ T.pnNotes paramNotes)

        atomically $ modifyTVar lServVar $ fmap \lServ -> lServ
          { lsContract = Just someContract
          , lsAllLocs = Just allLocs
          , lsParsedContracts = Just parsedContracts
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
          , contractMetadataLigoGetContractMetadataResponse = Just ContractMetadata
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
        , valueTypeLigoValidateValueRequestArguments =
            (toText -> valueType)
        , pickedMichelsonEntrypointLigoValidateValueRequestArguments =
            michelsonEntrypoint
        } = argumentsLigoValidateValueRequest

  SomeContract (contract@Contract{} :: Contract param storage) <- getContract
  program <- getProgram

  parseRes <- try @_ @SomeDebuggerException case category of
    "parameter" ->
      withMichelsonEntrypoint contract michelsonEntrypoint $
        \(_ :: T.Notes arg) _ ->
        void $ parseValue @arg program category (toText value) valueType

    "storage" ->
      void $ parseValue @storage program category (toText value) valueType

    other -> error [int||Unexpected category #{other}|]

  writeResponse $ ExtraResponse $ ValidateValueResponse LigoValidateValueResponse
    { seqLigoValidateValueResponse = 0
    , request_seqLigoValidateValueResponse = seqLigoValidateValueRequest
    , successLigoValidateValueResponse = True
    , messageLigoValidateValueResponse = displayException <$> leftToMaybe parseRes
    }

initDebuggerSession
  :: LigoLaunchRequestArguments
  -> RIO LIGO (DAPSessionState (InterpretSnapshot 'Unique))
initDebuggerSession LigoLaunchRequestArguments {..} = do
  storageT <- toText <$> checkArgument "storage" storageLigoLaunchRequestArguments
  paramT <- toText <$> checkArgument "parameter" parameterLigoLaunchRequestArguments
  entrypoint <- checkArgument "entrypoint" entrypointLigoLaunchRequestArguments

  lServVar <-
    asks _rcLSState >>= readTVarIO >>= \case
      Nothing -> throwIO @_ @DapMessageException [int||Language server state is not initialized|]
      Just var -> pure var

  program <- getProgram

  -- Here we're rethrowing exception which we swallowed
  -- at @getContractMetadata@ stage.
  whenJust (lsSwallowedException lServVar) throwIO

  let splitValueAndType value what = do
        if '@' `elem` value then do
          -- Sometimes we can find '@' in LIGO values but the last one should be definitely value type
          pure $ first (Text.dropEnd 1) $ Text.breakOnEnd "@" value
        else do
          throwIO @_ @DapMessageException [int||
            Can't find value type in #{what}.
            It should be separated with '@' sign.
          |]

  (stor, storageType) <- splitValueAndType storageT ("storage" :: Text)
  (parameter, parameterType) <- splitValueAndType paramT ("parameter" :: Text)

  -- This do is purely for scoping, otherwise GHC trips up:
  --     • Couldn't match type ‘a0’ with ‘()’
  --         ‘a0’ is untouchable
  do
    SomeContract contract@Contract{} <- getContract

    withMichelsonEntrypoint contract michelsonEntrypointLigoLaunchRequestArguments
      \(_ :: T.Notes arg) epc -> do

        arg <- parseValue program "parameter" parameter parameterType
        storage <- parseValue program "storage" stor storageType

        allLocs <- getAllLocs
        parsedContracts <- getParsedContracts

        let contractState = ContractState
              { csBalance = [tz|0u|]
              , csContract = contract
              , csStorage = storage
              , csDelegate = Nothing
              }

        -- TODO: remove it when we migrate to morley-1.18.0
        let self = [ta|KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB|]

        logMessage [int||Self address: #{self}|]
        logMessage [int||Contract state: #{contractState}|]

        his <-
          withRunInIO \unlifter ->
            collectInterpretSnapshots
              program
              (fromString entrypoint)
              contract
              epc
              arg
              storage
              -- We're adding our own contract in order to use
              -- @{ SELF_ADDRESS; CONTRACT }@ replacement
              -- (we need to have this contract state to use @CONTRACT@ instruction).
              dummyContractEnv { ceContracts = M.fromList [(self, ASContract contractState)], ceSelf = self }
              parsedContracts
              (unlifter . logMessage)

        let ds = DebuggerState
              { _dsSnapshots = playInterpretHistory his
              , _dsSources =
                  DebugSource mempty <$>
                  groupSourceLocations (toList allLocs)
              }

        pure $ DAPSessionState ds mempty mempty program

checkArgument :: MonadIO m => Text -> Maybe a -> m a
checkArgument _    (Just a) = pure a
checkArgument name Nothing  = throwIO @_ @DapMessageException
  [int||Required configuration option "#{name}" not found in launch.json.|]
