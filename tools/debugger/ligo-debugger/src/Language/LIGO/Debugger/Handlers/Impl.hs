-- | Implementation of DAP handlers.
module Language.LIGO.Debugger.Handlers.Impl
  ( LIGO
  ) where

import Debug qualified
import Unsafe qualified

import Control.Concurrent.STM (writeTChan)
import Control.Lens (zoom, (.=), ix, (^?!))
import Control.Monad.Except (MonadError (..), liftEither)
import Fmt (pretty)
import Morley.Debugger.Core.Common (typeCheckingForDebugger)
import Morley.Debugger.Core.Navigate
  (DebugSource (..), DebuggerState (..), SourceType (..), curSnapshot, frozen, groupSourceLocations,
  playInterpretHistory)
import Morley.Debugger.DAP.RIO (logMessage, openLogHandle)
import Morley.Debugger.DAP.Types
  (DAPOutputMessage (..), DAPSessionState (..), DAPSpecificResponse (..), HasSpecificMessages (..),
  RIO, RioContext (..), StopEventDesc (..), dsDebuggerState, dsVariables, pushMessage)
import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Michelson.Parser qualified as P
import Morley.Michelson.Runtime.Dummy (dummyContractEnv)
import Morley.Michelson.TypeCheck (typeVerifyParameter, typeVerifyStorage)
import Morley.Michelson.Typed (Contract, Contract' (..), SomeContract (..), SomeConstrainedValue (SomeValue))
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import System.FilePath (takeFileName, (<.>), (</>))
import Text.Interpolation.Nyan
import UnliftIO.Directory (doesFileExist)

import Language.LIGO.Debugger.Handlers.Types

import Language.LIGO.Debugger.CLI.Call
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Debugger.Snapshots

import Language.LIGO.DAP.Variables
import Morley.Debugger.Protocol.DAP (ScopesRequestArguments(frameIdScopesRequestArguments))

data LIGO

instance HasSpecificMessages LIGO where
  type Launch LIGO = LigoLaunchRequest
  type ExtraRequestExt LIGO = LigoSpecificRequest
  type ExtraEventExt LIGO = Void
  type ExtraResponseExt LIGO = LigoSpecificResponse
  type LanguageServerStateExt LIGO = Void
  type InterpretSnapshotExt LIGO = InterpretSnapshot
  type StopEventExt LIGO = InterpretEvent

  handleLaunch LigoLaunchRequest {..} = do
    initRes <- lift . lift $
      runExceptT (initDebuggerSession logMessage argumentsLigoLaunchRequest)
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
            InterpretRunning (EventExpressionEvaluated (Just (SomeValue value))) ->
              createVariables stackItems >>= \idx -> buildVariable value "$it" >>= insertToIndex idx . (:[])
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

handleInitializeLogger :: LigoInitializeLoggerRequest -> RIO LIGO ()
handleInitializeLogger LigoInitializeLoggerRequest {..} = do
  let file = fileLigoInitializeLoggerRequestArguments argumentsLigoInitializeLoggerRequest
  let logFileMb = do
        dir <- logDirLigoInitializeLoggerRequestArguments argumentsLigoInitializeLoggerRequest
        Just $ dir </> takeFileName file <.> "log"
  whenJust logFileMb openLogHandle

  ch <- asks _rcOutputChannel

  program <- runExceptT $ ifM (doesFileExist file)
    (pure file)
    (throwError $ DAP.mkErrorMessage "Contract file not found" $ toText file)

  result <- case program of
    Left msg -> do
      atomically . writeTChan ch $ DAPResponse $ ErrorResponse DAP.defaultErrorResponse
        { DAP.request_seqErrorResponse = seqLigoInitializeLoggerRequest
        , DAP.commandErrorResponse = commandLigoInitializeLoggerRequest
        , DAP.messageErrorResponse = Just $ DAP.formatMessage msg
        , DAP.bodyErrorResponse = DAP.ErrorResponseBody $ Just msg
        }
      pure $ Left msg
    Right _ -> do
      atomically $ writeTChan ch $ DAPResponse $ ExtraResponse $ InitializeLoggerResponse LigoInitializeLoggerResponse
        { seqLigoInitializeLoggerResponse = 0
        , request_seqLigoInitializeLoggerResponse = seqLigoInitializeLoggerRequest
        , successLigoInitializeLoggerResponse = True
        }
      pure $ Right ()
  logMessage [int||Initializing logger for #{file} finished: #s{result}|]

initDebuggerSession
  :: (String -> RIO LIGO ())
     -- ^ Logger to file
  -> LigoLaunchRequestArguments
  -> ExceptT DAP.Message (RIO LIGO) (DAPSessionState InterpretSnapshot)
initDebuggerSession logger LigoLaunchRequestArguments {..} = do
  program <- checkArgument "program" programLigoLaunchRequestArguments
  storageT <- checkArgument "storage" storageLigoLaunchRequestArguments
  paramT <- checkArgument "parameter" parameterLigoLaunchRequestArguments
  let entrypoint = fromMaybe "main" entrypointLigoLaunchRequestArguments

  unlessM (doesFileExist program) $
    throwError $ DAP.mkErrorMessage "Contract file not found" $ toText program

  let src = P.MSFile program

  ligoDebugInfo <- compileLigoContractDebug entrypoint program
  lift . logger $ "Successfully read the LIGO debug output for " <> pretty program

  -- TODO [LIGO-554]: use LIGO for parsing it
  uParam <- P.parseExpandValue src (toText paramT) &
    either (throwError . DAP.mkErrorMessage "Could not parse parameter" . pretty) pure

  -- TODO [LIGO-554]: use LIGO for parsing it
  uStorage <- P.parseExpandValue src (toText storageT) &
    either (throwError . DAP.mkErrorMessage "Could not parse storage" . pretty) pure

  -- This do is purely for scoping, otherwise GHC trips up:
  --     • Couldn't match type ‘a0’ with ‘()’
  --         ‘a0’ is untouchable
  do
    (allLocs, SomeContract (contract@Contract{} :: Contract cp st)) <-
      readLigoMapper ligoDebugInfo
      & first (DAP.mkErrorMessage "Failed to process contract: " . pretty)
      & liftEither

    lift $ logger $ pretty (cCode contract)

    -- Entrypoint is default because in @ParamNotes@ we don't have @EpName@ at all.
    epcRes <- T.mkEntrypointCall U.DefEpName (cParamNotes contract) &
      maybe (throwError $ DAP.mkErrorMessage "Entrypoint not found" $ pretty entrypoint) pure

    case epcRes of
      T.MkEntrypointCallRes (_ :: T.Notes arg) epc -> do
        arg <- typeVerifyParameter @arg mempty uParam
          & typeCheckingForDebugger
          & either (throwError . DAP.mkErrorMessage "Parameter does not typecheck") pure
        storage <- typeVerifyStorage @st uStorage
          & typeCheckingForDebugger
          & either (throwError . DAP.mkErrorMessage "Storage does not typecheck") pure

        let his = collectInterpretSnapshots program (fromString entrypoint) contract epc arg storage dummyContractEnv
            ds = DebuggerState
              { _dsSourceOrigin = SourcePath program
              , _dsSnapshots = playInterpretHistory his
              , _dsSources = DebugSource mempty <$> groupSourceLocations (toList allLocs)
              }
        pure $ DAPSessionState ds mempty program

checkArgument :: MonadError DAP.Message m => Text -> Maybe a -> m a
checkArgument _    (Just a) = pure a
checkArgument name Nothing  = throwError DAP.defaultMessage
  { DAP.formatMessage = toString $ "Required configuration option \"" <> name <> "\" not found in launch.json."
  }
