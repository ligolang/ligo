-- | Implementation of DAP handlers.
module Language.LIGO.Debugger.Handlers.Impl
  ( LIGO
  ) where

import Debug qualified
import Unsafe qualified

import Control.Lens (Each (each), ix, uses, zoom, (.=), (^?!))
import Control.Monad.Except (MonadError (..), liftEither, withExceptT)
import Data.Text qualified as Text
import Fmt (Builder, blockListF, build, pretty)
import Fmt.Internal.Core (FromBuilder (fromBuilder))
import Morley.Debugger.Core (slSrcPos)
import Morley.Debugger.Core.Navigate
  (DebugSource (..), DebuggerState (..), NavigableSnapshot (getLastExecutedPosition), curSnapshot,
  frozen, groupSourceLocations, playInterpretHistory)
import Morley.Debugger.DAP.LanguageServer (JsonFromBuildable (..))
import Morley.Debugger.DAP.RIO (logMessage, openLogHandle)
import Morley.Debugger.DAP.Types
  (DAPOutputMessage (..), DAPSessionState (..),
  DAPSpecificEvent (OutputEvent, StoppedEvent, TerminatedEvent), DAPSpecificResponse (..),
  HasSpecificMessages (..), RIO, RioContext (..), StopEventDesc (..), StoppedReason (..),
  dsDebuggerState, dsVariables, pushMessage)
import Morley.Debugger.Protocol.DAP (ScopesRequestArguments (frameIdScopesRequestArguments))
import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Michelson.ErrorPos (Pos (Pos), SrcPos (SrcPos))
import Morley.Michelson.Printer.Util (RenderDoc (renderDoc), doesntNeedParens, printDocB)
import Morley.Michelson.Runtime.Dummy (dummyContractEnv)
import Morley.Michelson.Typed
  (Contract, Contract' (..), ContractCode' (unContractCode), SomeConstrainedValue (SomeValue),
  SomeContract (..))
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import System.FilePath (takeFileName, (<.>), (</>))
import Text.Interpolation.Nyan
import UnliftIO.Directory (doesFileExist)
import UnliftIO.STM (modifyTVar)

import Language.LIGO.Debugger.Handlers.Helpers
import Language.LIGO.Debugger.Handlers.Types

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Debugger.Snapshots

import Language.LIGO.DAP.Variables

data LIGO

instance HasSpecificMessages LIGO where
  type Launch LIGO = LigoLaunchRequest
  type ExtraRequestExt LIGO = LigoSpecificRequest
  type ExtraEventExt LIGO = Void
  type ExtraResponseExt LIGO = LigoSpecificResponse
  type LanguageServerStateExt LIGO = LigoLanguageServerState
  type InterpretSnapshotExt LIGO = InterpretSnapshot
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
    initRes <- lift . lift $
      runExceptT (initDebuggerSession argumentsLigoLaunchRequest)
    case initRes of
      Left msg ->
        pushMessage $ DAPResponse $ ErrorResponse DAP.defaultErrorResponse
          { DAP.request_seqErrorResponse = seqLigoLaunchRequest
          , DAP.commandErrorResponse = commandLigoLaunchRequest
          , DAP.bodyErrorResponse = DAP.ErrorResponseBody $ Just msg
          }
      Right st -> do
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
            InterpretRunning (EventExpressionEvaluated (Just (SomeValue value))) -> do
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

  handleRequestExt = \case
    InitializeLoggerRequest req -> handleInitializeLogger req
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

  program <- runExceptT $ ifM (doesFileExist file)
    (pure file)
    (throwError $ DAP.mkErrorMessage "Contract file not found" $ toText file)

  result <- case program of
    Left msg -> do
      writeResponse $ ErrorResponse DAP.defaultErrorResponse
        { DAP.request_seqErrorResponse = seqLigoInitializeLoggerRequest
        , DAP.commandErrorResponse = commandLigoInitializeLoggerRequest
        , DAP.messageErrorResponse = Just $ DAP.formatMessage msg
        , DAP.bodyErrorResponse = DAP.ErrorResponseBody $ Just msg
        }
      pure $ Left msg
    Right _ -> do
      writeResponse $ ExtraResponse $ InitializeLoggerResponse LigoInitializeLoggerResponse
        { seqLigoInitializeLoggerResponse = 0
        , request_seqLigoInitializeLoggerResponse = seqLigoInitializeLoggerRequest
        , successLigoInitializeLoggerResponse = True
        }
      pure $ Right ()
  logMessage [int||Initializing logger for #{file} finished: #s{result}|]

handleSetProgramPath :: LigoSetProgramPathRequest -> RIO LIGO ()
handleSetProgramPath LigoSetProgramPathRequest{..} = do
  let LigoSetProgramPathRequestArguments{..} = argumentsLigoSetProgramPathRequest
  let programPath = programLigoSetProgramPathRequestArguments

  entrypointsE <- runExceptT $ getAvailableEntrypoints programPath

  result <-
    case entrypointsE of
      Left e -> do
        let msg = fromBuilder . build $ e
        writeResponse $ ErrorResponse DAP.defaultErrorResponse
          { DAP.request_seqErrorResponse = seqLigoSetProgramPathRequest
          , DAP.commandErrorResponse = commandLigoSetProgramPathRequest
          , DAP.messageErrorResponse = Just $ DAP.formatMessage msg
          , DAP.bodyErrorResponse = DAP.ErrorResponseBody $ Just msg
          }
        pure $ Left msg
      Right EntrypointsList{..} -> do
        lServVar <- asks _rcLSState
        atomically $ writeTVar lServVar $ Just LigoLanguageServerState
          { lsProgram = Just programPath
          , lsContract = Nothing
          , lsAllLocs = Nothing
          }

        writeResponse $ ExtraResponse $ SetProgramPathResponse LigoSetProgramPathResponse
          { seqLigoSetProgramPathResponse = 0
          , request_seqLigoSetProgramPathResponse = seqLigoSetProgramPathRequest
          , successLigoSetProgramPathResponse = True
          , entrypointsLigoSetProgramPathResponse = unEntrypoints
          }
        pure $ Right ()

  logMessage [int||Setting program path #{programPath} is finished: #s{result}|]

handleValidateEntrypoint :: LigoValidateEntrypointRequest -> RIO LIGO ()
handleValidateEntrypoint LigoValidateEntrypointRequest{..} = do
  let LigoValidateEntrypointRequestArguments{..} = argumentsLigoValidateEntrypointRequest
  let pickedEntrypoint = entrypointLigoValidateEntrypointRequestArguments

  program <- getProgram
  result <- void <<$>> runExceptT $ compileLigoContractDebug pickedEntrypoint program

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
  lServerStateOld <- getServerState
  let program = fromMaybe (error "Program is not initialized") $ lsProgram lServerStateOld

  result <- runExceptT do
    unlessM (doesFileExist program) $
      throwError [int||Contract file not found: #{toText program}|]

    ligoDebugInfo <- compileLigoContractDebug entrypoint program
    logMessage $ "Successfully read the LIGO debug output for " <> pretty program

    (allLocs, someContract) <-
      readLigoMapper ligoDebugInfo
      & first [int|m|"Failed to process contract: #{id}|]
      & liftEither

    () <- do
      SomeContract (contract@Contract{} :: Contract cp st) <- pure someContract
      logMessage $ pretty (unContractCode $ cCode contract)

    pure (someContract, allLocs)

  case result of
    Left e -> do
      let msg = fromBuilder . build $ e
      logMessage [int||Getting metadata for contract #{program} failed: #{msg}|]
      writeResponse $
        ErrorResponse $
          DAP.defaultErrorResponse
            { DAP.request_seqErrorResponse = seqLigoGetContractMetadataRequest
            , DAP.commandErrorResponse = commandLigoGetContractMetadataRequest
            , DAP.messageErrorResponse = Just $ toString . leMessage $ e
            , DAP.bodyErrorResponse = DAP.ErrorResponseBody $ Just msg
            }
    Right (someContract@(SomeContract contract), allLocs) -> do
      let
        paramNotes = cParamNotes contract
        michelsonEntrypoints =
          T.flattenEntrypoints paramNotes
          <> one (U.DefEpName, T.mkUType $ T.pnNotes paramNotes)

      atomically $ modifyTVar lServVar $ fmap \lServ -> lServ
        { lsContract = Just someContract
        , lsAllLocs = Just allLocs
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
        , valueTypeLigoValidateValueRequestArguments =
            (toText -> valueType)
        , pickedMichelsonEntrypointLigoValidateValueRequestArguments =
            michelsonEntrypoint
        } = argumentsLigoValidateValueRequest

  SomeContract (contract@Contract{} :: Contract param storage) <- getContract
  program <- getProgram

  parseRes <- runExceptT case category of
    "parameter" ->
      withMichelsonEntrypoint contract michelsonEntrypoint id $
        \(_ :: T.Notes arg) _ ->
        void $ parseValue @arg program category (toText value) valueType

    "storage" ->
      void $ parseValue @storage program category (toText value) valueType

    other -> error [int||Unexpected category #{other}|]

  writeResponse $ ExtraResponse $ ValidateValueResponse LigoValidateValueResponse
    { seqLigoValidateValueResponse = 0
    , request_seqLigoValidateValueResponse = seqLigoValidateValueRequest
    , successLigoValidateValueResponse = True
    , messageLigoValidateValueResponse = toString <$> leftToMaybe parseRes
    }

initDebuggerSession
  :: LigoLaunchRequestArguments
  -> ExceptT DAP.Message (RIO LIGO) (DAPSessionState InterpretSnapshot)
initDebuggerSession LigoLaunchRequestArguments {..} = do
  storageT <- toText <$> checkArgument "storage" storageLigoLaunchRequestArguments
  paramT <- toText <$> checkArgument "parameter" parameterLigoLaunchRequestArguments
  entrypoint <- checkArgument "entrypoint" entrypointLigoLaunchRequestArguments

  let splitValueAndType value what = do
        if '@' `elem` value then do
          -- Sometimes we can find '@' in LIGO values but the last one should be definitely value type
          pure $ first (Text.dropEnd 1) $ Text.breakOnEnd "@" value
        else do
          throwDAPError [int||
            Can't find value type in #{what}.
            It should be separated with '@' sign.
          |]

  (stor, storageType) <- splitValueAndType storageT ("storage" :: Text)
  (parameter, parameterType) <- splitValueAndType paramT ("parameter" :: Text)

  asks _rcLSState >>= readTVarIO >>= \case
    Nothing -> throwDAPError "Language server state is not initialized"
    Just _ -> pass

  program <- lift getProgram

  -- This do is purely for scoping, otherwise GHC trips up:
  --     • Couldn't match type ‘a0’ with ‘()’
  --         ‘a0’ is untouchable
  do
    SomeContract contract@Contract{} <- lift getContract

    withMichelsonEntrypoint contract michelsonEntrypointLigoLaunchRequestArguments
      (pretty :: Text -> DAP.Message)
      \(_ :: T.Notes arg) epc -> do

        arg <- parseValue program "parameter" parameter parameterType
          & withExceptT (pretty :: Text -> DAP.Message)
        storage <- parseValue program "storage" stor storageType
          & withExceptT (pretty :: Text -> DAP.Message)

        allLocs <- lift getAllLocs

        let his = collectInterpretSnapshots program (fromString entrypoint) contract epc arg storage dummyContractEnv
            ds = DebuggerState
              { _dsSnapshots = playInterpretHistory his
              , _dsSources =
                  DebugSource mempty <$>
                  groupSourceLocations (toList allLocs)
              }
        logMessage [int||All snapshots: #{his}|]
        pure $ DAPSessionState ds mempty mempty program

checkArgument :: MonadError DAP.Message m => Text -> Maybe a -> m a
checkArgument _    (Just a) = pure a
checkArgument name Nothing  = throwError DAP.defaultMessage
  { DAP.formatMessage = toString $ "Required configuration option \"" <> name <> "\" not found in launch.json."
  }
