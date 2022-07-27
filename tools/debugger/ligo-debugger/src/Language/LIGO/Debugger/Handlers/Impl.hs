-- | Implementation of DAP handlers.
module Language.LIGO.Debugger.Handlers.Impl
  ( LIGO
  ) where

import Debug qualified
import Unsafe qualified

import Control.Lens (ix, zoom, (.=), (^?!))
import Control.Monad.Except (MonadError (..), liftEither, withExceptT)
import Fmt (pretty)
import Morley.Debugger.Core.Navigate
  (DebugSource (..), DebuggerState (..), curSnapshot, frozen, groupSourceLocations,
  playInterpretHistory)
import Morley.Debugger.DAP.LanguageServer (JsonFromBuildable (..))
import Morley.Debugger.DAP.RIO (logMessage, openLogHandle)
import Morley.Debugger.DAP.Types
  (DAPOutputMessage (..), DAPSessionState (..), DAPSpecificResponse (..), HasSpecificMessages (..),
  RIO, RioContext (..), StopEventDesc (..), dsDebuggerState, dsVariables, pushMessage)
import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Michelson.Runtime.Dummy (dummyContractEnv)
import Morley.Michelson.Typed
  (Contract, Contract' (..), SomeConstrainedValue (SomeValue), SomeContract (..))
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import System.FilePath (takeFileName, (<.>), (</>))
import Text.Interpolation.Nyan
import UnliftIO.Directory (doesFileExist)

import Language.LIGO.Debugger.Handlers.Helpers
import Language.LIGO.Debugger.Handlers.Types

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Debugger.Snapshots

import Language.LIGO.DAP.Variables
import Morley.Debugger.Protocol.DAP (ScopesRequestArguments (frameIdScopesRequestArguments))

data LIGO

instance HasSpecificMessages LIGO where
  type Launch LIGO = LigoLaunchRequest
  type ExtraRequestExt LIGO = LigoSpecificRequest
  type ExtraEventExt LIGO = Void
  type ExtraResponseExt LIGO = LigoSpecificResponse
  type LanguageServerStateExt LIGO = LigoLanguageServerState
  type InterpretSnapshotExt LIGO = InterpretSnapshot
  type StopEventExt LIGO = InterpretEvent

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

handleGetContractMetadata :: LigoGetContractMetadataRequest -> RIO LIGO ()
handleGetContractMetadata LigoGetContractMetadataRequest{..} = do
  let LigoGetContractMetadataRequestArguments{..} = argumentsLigoGetContractMetadataRequest
  let program = fileLigoGetContractMetadataRequestArguments
  let entrypoint = fromMaybe "main" entrypointLigoGetContractMetadataRequestArguments
  lServVar <- asks _rcLSState

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
      logMessage $ pretty (cCode contract)

    return LigoLanguageServerState
      { lsProgram = program
      , lsContract = someContract
      , lsEntrypoint = entrypoint
      , lsAllLocs = allLocs
      }

  case result of
    Left msg -> do
      writeResponse $ ErrorResponse $ DAP.defaultErrorResponse
        { DAP.request_seqErrorResponse = seqLigoGetContractMetadataRequest
        , DAP.commandErrorResponse = commandLigoGetContractMetadataRequest
        , DAP.messageErrorResponse = Just $ DAP.formatMessage msg
        , DAP.bodyErrorResponse = DAP.ErrorResponseBody $ Just msg
        }
      logMessage [int||Getting metadata for contract #{program} failed: #{msg}|]
    Right lServerState -> case lsContract lServerState of
      SomeContract contract -> do
        let
          paramNotes = cParamNotes contract
          michelsonEntrypoints =
            T.flattenEntrypoints paramNotes
            <> one (U.DefEpName, T.mkUType $ T.pnNotes paramNotes)

        logMessage [int||
          Got metadata for contract #{program}:
            Server state: #{lServerState}
            Michelson entrypoints: #{keys michelsonEntrypoints}
          |]

        atomically $ writeTVar lServVar (Just lServerState)
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
        , pickedMichelsonEntrypointLigoValidateValueRequestArguments =
            michelsonEntrypoint
        } = argumentsLigoValidateValueRequest

  lServerStateVar <- asks _rcLSState
  lServerState <- readTVarIO lServerStateVar >>= \case
    Nothing -> error "Language server state is not initialized"
    Just st -> pure st
  SomeContract (contract@Contract{} :: Contract param storage) <-
    pure $ lsContract lServerState

  parseRes <- runExceptT case category of
    "parameter" ->
      withMichelsonEntrypoint contract michelsonEntrypoint id $
        \(_ :: T.Notes arg) _ ->
        void $ parseValue @arg (lsProgram lServerState) category (toText value)

    "storage" ->
      void $ parseValue @storage (lsProgram lServerState) category (toText value)

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

  lServerState <- asks _rcLSState >>= readTVarIO >>= \case
    Nothing -> throwDAPError "Language server state is not initialized"
    Just s -> return s
  let program = lsProgram lServerState
  let entrypoint = lsEntrypoint lServerState

  -- This do is purely for scoping, otherwise GHC trips up:
  --     • Couldn't match type ‘a0’ with ‘()’
  --         ‘a0’ is untouchable
  do
    SomeContract contract@Contract{} <- pure $ lsContract lServerState

    withMichelsonEntrypoint contract michelsonEntrypointLigoLaunchRequestArguments
      (pretty :: Text -> DAP.Message)
      \(_ :: T.Notes arg) epc -> do

        arg <- parseValue program "parameter" paramT
          & withExceptT (pretty :: Text -> DAP.Message)
        storage <- parseValue program "storage" storageT
          & withExceptT (pretty :: Text -> DAP.Message)

        let his = collectInterpretSnapshots program (fromString entrypoint) contract epc arg storage dummyContractEnv
            ds = DebuggerState
              { _dsSnapshots = playInterpretHistory his
              , _dsSources =
                  DebugSource mempty <$>
                  groupSourceLocations (toList $ lsAllLocs lServerState)
              }
        pure $ DAPSessionState ds mempty mempty program

checkArgument :: MonadError DAP.Message m => Text -> Maybe a -> m a
checkArgument _    (Just a) = pure a
checkArgument name Nothing  = throwError DAP.defaultMessage
  { DAP.formatMessage = toString $ "Required configuration option \"" <> name <> "\" not found in launch.json."
  }
