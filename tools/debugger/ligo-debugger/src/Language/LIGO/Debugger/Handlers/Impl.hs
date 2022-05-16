-- | Implementation of DAP handlers.
module Language.LIGO.Debugger.Handlers.Impl
  ( LIGO
  ) where

import Debug qualified

import Control.Concurrent.STM (writeTChan)
import Control.Monad.Except (MonadError (..))
import Data.IntMap qualified as IntMap
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.IO.Utf8 qualified as Utf8
import Fmt (pretty)
import Morley.Debugger.Core.Common (typeCheckingForDebugger)
import Morley.Debugger.Core.Navigate
  (DebugSource (..), DebuggerState (..), SourceLocation (..), SourceMapper (..), SourceType (..),
  mkTapeL)
import Morley.Debugger.Core.Snapshots
  (InstrNo (..), InterpretHistory (..), annotateInstrWith, collectInterpretSnapshots)
import Morley.Debugger.DAP.RIO (logMessage, openLogHandle)
import Morley.Debugger.DAP.Types
  (DAPOutputMessage (..), DAPSessionState (..), DAPSpecificResponse (..), HasSpecificMessages (..),
  RIO, RioContext (..), pushMessage)
import Morley.Debugger.Protocol.DAP qualified as DAP
import Morley.Michelson.Parser qualified as P
import Morley.Michelson.Runtime (parseExpandContract)
import Morley.Michelson.Runtime.Dummy (dummyContractEnv)
import Morley.Michelson.TypeCheck (typeCheckContract, typeVerifyParameter, typeVerifyStorage)
import Morley.Michelson.Typed (Contract, Contract' (..), SomeContract (..))
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import System.Directory (doesFileExist)
import System.FilePath (takeFileName, (<.>), (</>))
import Text.Interpolation.Nyan

import Language.LIGO.Debugger.Handlers.Types
import Language.LIGO.Debugger.Michelson (dummyMapper)
import Util (groupByKey)

data LIGO

instance HasSpecificMessages LIGO where
  type Launch LIGO = LigoLaunchRequest
  type ExtraRequestExt LIGO = LigoSpecificRequest
  type ExtraEventExt LIGO = Void
  type ExtraResponseExt LIGO = LigoSpecificResponse
  type LanguageServerStateExt LIGO = Void

  handleLaunch LigoLaunchRequest {..} = do
    initRes <- lift . lift $
      runExceptT (initDummyDebuggerSession logMessage argumentsLigoLaunchRequest)
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
  handleRequestExt = \case
    InitializeLoggerRequest req -> handleInitializeLogger req

handleInitializeLogger :: LigoInitializeLoggerRequest -> RIO LIGO ()
handleInitializeLogger LigoInitializeLoggerRequest {..} = do
  let file = fileLigoInitializeLoggerRequestArguments argumentsLigoInitializeLoggerRequest
  let logFileMb = do
        dir <- logDirLigoInitializeLoggerRequestArguments argumentsLigoInitializeLoggerRequest
        Just $ dir </> takeFileName file <.> "log"
  whenJust logFileMb openLogHandle

  ch <- asks _rcOutputChannel

  program <- runExceptT $ ifM (liftIO $ doesFileExist file)
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

initDummyDebuggerSession
  :: (String -> RIO LIGO ())
     -- ^ Logger to file
  -> LigoLaunchRequestArguments
  -> ExceptT DAP.Message (RIO LIGO) DAPSessionState
initDummyDebuggerSession logger LigoLaunchRequestArguments {..} = do
  --program <- checkArgument "program" programLigoLaunchRequestArguments
  let program = "/home/heitor/Serokell/ligo/tools/debugger/src-mapper/noop.tz"
  storageT <- checkArgument "storage" storageLigoLaunchRequestArguments
  paramT <- checkArgument "parameter" parameterLigoLaunchRequestArguments

  unlessM (liftIO $ doesFileExist program) $
    throwError $ DAP.mkErrorMessage "Contract file not found" $ toText program

  let src = P.MSFile program

  uContract <- liftIO (Utf8.readFile program) <&> parseExpandContract src >>=
    either (throwError . DAP.mkErrorMessage "Could not parse contract" . pretty) pure

  forM_ (U.contractCode uContract) $ lift . logger . pretty

  uParam <- P.parseExpandValue src (toText paramT) &
    either (throwError . DAP.mkErrorMessage "Could not parse parameter" . pretty) pure

  uStorage <- P.parseExpandValue src (toText storageT) &
    either (throwError . DAP.mkErrorMessage "Could not parse storage" . pretty) pure

  entrypoint <-
    maybe (Right U.DefEpName) U.buildEpName (fromString <$> entrypointLigoLaunchRequestArguments)
    & either (throwError . DAP.mkErrorMessage "Could not parse entrypoint" . toText) pure

  -- This do is purely for scoping, otherwise GHC trips up:
  --     • Couldn't match type ‘a0’ with ‘()’
  --         ‘a0’ is untouchable
  do
    SomeContract (contract@Contract{} :: Contract cp st) <-
      typeCheckContract uContract
      & typeCheckingForDebugger
      & either (throwError . DAP.mkErrorMessage "Could not typecheck contract") pure

    -- TODO
    let indexedContract = T.mapContractCode (annotateInstrWith $ map InstrNo $ IntMap.keys $ _smLocs dummyMapper) contract

    lift $ logger $ pretty $ cCode indexedContract

    epcRes <- T.mkEntrypointCall entrypoint (cParamNotes contract) &
      maybe (throwError $ DAP.mkErrorMessage "Entrypoint not found" $ pretty entrypoint) pure

    case epcRes of
      T.MkEntrypointCallRes (_ :: T.Notes arg) epc -> do
        arg <- typeVerifyParameter @arg mempty uParam
          & typeCheckingForDebugger
          & either (throwError . DAP.mkErrorMessage "Parameter does not typecheck") pure
        storage <- typeVerifyStorage @st uStorage
          & typeCheckingForDebugger
          & either (throwError . DAP.mkErrorMessage "Storage does not typecheck") pure

        let his = collectInterpretSnapshots indexedContract epc arg storage dummyContractEnv
        pure $ DAPSessionState (mkDebuggerState (SourcePath program) his) mempty program

-- | Construct initial debugger state.
mkDebuggerState :: SourceType -> InterpretHistory -> DebuggerState
mkDebuggerState source (InterpretHistory snapshots) =
  let
    sortedLocs = groupByKey _slPath _slSrcPos $ IntMap.elems $ _smLocs dummyMapper
  in
  DebuggerState
    { _dsSnapshots    = mkTapeL snapshots
    , _dsSources      = Map.fromList $ DebugSource mempty . Set.fromList <<$>> sortedLocs
    , _dsIndexedLocs  = dummyMapper
    , _dsSourceOrigin = source
    }

checkArgument :: MonadError DAP.Message m => Text -> Maybe a -> m a
checkArgument _    (Just a) = pure a
checkArgument name Nothing  = throwError DAP.defaultMessage
  { DAP.formatMessage = toString $ "Required configuration option \"" <> name <> "\" not found in launch.json."
  }
