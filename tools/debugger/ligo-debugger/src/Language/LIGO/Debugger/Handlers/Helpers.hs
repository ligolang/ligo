-- | Helpers for implementing DAP handlers.
module Language.LIGO.Debugger.Handlers.Helpers
  ( module Language.LIGO.Debugger.Handlers.Helpers
  ) where

import Prelude hiding (try)

import Control.Monad.Except (liftEither, throwError, withExceptT)
import Data.Char qualified as C
import Data.HashMap.Strict qualified as HM
import Data.Singletons (SingI, demote)
import Data.Typeable (cast)
import Fmt.Buildable (Buildable, build, pretty)
import Fmt.Utils (Doc)
import Text.Interpolation.Nyan hiding (rmode')
import UnliftIO.Exception (fromEither, throwIO, try)
import Util

import Morley.Debugger.Core.Common (typeCheckingForDebugger)
import Morley.Debugger.Core.Navigate (SourceLocation)
import Morley.Debugger.DAP.LanguageServer qualified as MD
import Morley.Debugger.DAP.Types
  (HasSpecificMessages (LanguageServerStateExt), MonadRIO, RioContext (..))
import Morley.Michelson.Interpret (RemainingSteps)
import Morley.Michelson.Parser qualified as P
import Morley.Michelson.TypeCheck (typeVerifyTopLevelType)
import Morley.Michelson.Typed (Contract' (..))
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as U
import Morley.Util.Constrained (Constrained (..))
import Morley.Util.Lens (makeLensesWith, postfixLFields)

import Control.AbortingThreadPool qualified as AbortingThreadPool
import Control.DelayedValues qualified as DelayedValues

import Language.LIGO.AST (Info, LIGO, SomeLIGO (..))
import Language.LIGO.Debugger.CLI
import Language.LIGO.Debugger.Common
import Language.LIGO.Debugger.Error
import Language.LIGO.Debugger.Michelson
import Language.LIGO.Extension
import Language.LIGO.Range
import Language.LIGO.Scope

-- | Type which caches all things that we need for
-- launching the contract.
--
-- All these @Maybe@s inside this type are needed
-- to track whether this field is initialized or not.
-- If you try to unwrap @Nothing@ when @Just@ is expected
-- then a @PluginCommunicationException@ should be thrown.
data CollectedRunInfo where
  CollectedRunInfo
    :: forall cp st arg
     . (T.ParameterScope cp, T.StorageScope st)
    =>
    { criContract :: T.Contract cp st
    , criEpcMb :: Maybe (T.EntrypointCallT cp arg)
    , criParameterMb :: Maybe (T.Value arg)
    , criStorageMb :: Maybe (T.Value st)
    } -> CollectedRunInfo

onlyContractRunInfo
  :: forall cp st
   . (T.ParameterScope cp, T.StorageScope st)
  => T.Contract cp st
  -> CollectedRunInfo
onlyContractRunInfo contract = CollectedRunInfo
  { criContract = contract
  , criEpcMb = Nothing
  , criParameterMb = Nothing
  , criStorageMb = Nothing
  }

-- | The information for conversion of Michelson value to LIGO format.
data PreLigoConvertInfo = PreLigoConvertInfo
  { plciMichVal :: T.SomeValue
    -- ^ Michelson value.
  , plciLigoType :: LigoType
    -- ^ Known LIGO type of the value which the conversion will result in.
  } deriving stock (Show, Eq)

instance Hashable PreLigoConvertInfo where
  hashWithSalt s (PreLigoConvertInfo (Constrained michVal) ty) = s
    `hashWithSalt` pretty @(T.Value _) @Text michVal
    -- ↑ Pretty-printer for michelson values on itself is not a reversible
    -- function (e.g. address and contract are represented in the same way),
    -- but if we include type, the resulting values will be unique
    `hashWithSalt` ty

-- | LIGO-debugger-specific state that we initialize before debugger session
-- creation.
data LigoLanguageServerState = LigoLanguageServerState
  { lsProgram :: Maybe FilePath
  , lsCollectedRunInfo :: Maybe CollectedRunInfo
  , lsAllLocs :: Maybe (Set SourceLocation)
  , lsBinaryPath :: Maybe FilePath
  , lsParsedContracts :: Maybe (HashMap FilePath (LIGO Info))
  , lsLambdaLocs :: Maybe (HashSet Range)
  , lsLigoTypesVec :: Maybe LigoTypesVec
  , lsEntrypoints :: Maybe (Map U.EpName U.Ty)
    -- ^ A list of available @Michelson@ entrypoints.
  , lsPickedEntrypoint :: Maybe Text
  , lsVarsComputeThreadPool :: AbortingThreadPool.Pool
  , lsToLigoValueConverter :: DelayedValues.Manager PreLigoConvertInfo LigoOrMichValue
  , lsMoveId :: Word
    -- ^ The identifier of position, assigned a unique id after each step
    -- (visiting the same snapshot twice will also result in different ids).
  , lsMaxSteps :: Maybe RemainingSteps
    -- ^ Max amount of steps that the debugger will do.
    -- If it is @Nothing@ then max steps is infinite.
  , lsEntrypointType :: Maybe LigoType
    -- ^ the type of the @main@ method.
  , lsScopes :: Maybe (HashMap FilePath [Scope])
    -- ^ Scopes from @get-scope@ command for each contract file.
  , lsArgumentRanges :: Maybe (HashSet Range)
    -- ^ Ranges for arguments in partially applied functions.
  }

instance Buildable LigoLanguageServerState where
  build LigoLanguageServerState{..} =  [int||
    Debugging program: #{lsProgram}
    |]

withMichelsonEntrypoint
  :: (MonadIO m)
  => T.Contract param st
  -> Text
  -> (forall arg. SingI arg => T.Notes arg -> T.EntrypointCallT param arg -> m a)
  -> m a
withMichelsonEntrypoint contract@T.Contract{} entrypoint cont = do
  let noParseEntrypointErr = ConfigurationException .
        [int|m|Could not parse entrypoint: #{id}|]
  michelsonEntrypoint <- case entrypoint of
    ep -> U.buildEpName (toText $ firstLetterToLowerCase $ toString ep)
      & first noParseEntrypointErr
      & fromEither

  let noEntrypointErr = ConfigurationException $
        [int||Entrypoint `#{michelsonEntrypoint}` not found|]
  T.MkEntrypointCallRes notes call <-
    T.mkEntrypointCall michelsonEntrypoint (cParamNotes contract)
    & maybe (throwIO noEntrypointErr) pure

  cont notes call
  where
    -- LIGO has constructors starting from capital letters,
    -- however in Michelson they appear as field annotations starting from
    -- lower-case letter.
    -- This is a detail that we would like to hide from the end user.
    firstLetterToLowerCase = \case
      [] -> []
      c : rest -> C.toLower c : rest

-- | Try our best to parse and typecheck a value of a certain category.
parseValue
  :: forall t m.
     (SingI t, HasLigoClient m)
  => FilePath
  -> Text
  -> Text
  -> Text
  -> LigoType
  -> m (Either Text (T.Value t))
parseValue ctxContractPath category val valueLang ligoType = runExceptT do
  let src = P.MSName category
  uvalue <- case valueLang of
    "LIGO" -> do
      lift (try $ compileLigoExpression src ctxContractPath val) >>= \case
        Right x -> pure x
        Left (err :: LigoCallException) -> throwError [int||
            Error parsing #{category}:

            #{err}
          |]
    "Michelson" ->
      P.parseExpandValue src val
        & first (pretty . MD.prettyFirstError)
        & liftEither

    _ -> throwError [int||
        Expected "LIGO" or "Michelson" in field "#{category}Lang" \
        but got #{valueLang}
      |]

  ext <- withExceptT (toText . displayException) $ getExt ctxContractPath

  let typ :: Doc =
        case ligoType of
          LigoTypeResolved{} -> buildType ext ligoType
          _ ->  [int||#{demote @t} // n.b.: the expected type is \
               shown in Michelson format|]

  typeVerifyTopLevelType mempty uvalue
    & typeCheckingForDebugger
    & first do \_ -> [int||
        The value is not of type `#{typ}`
      |]
    & liftEither

getServerState :: MonadRIO ext m => m (LanguageServerStateExt ext)
getServerState =
  asks _rcLSState >>= readTVarIO >>=
    maybe (throwIO uninitLanguageServerExc) pure

uninitLanguageServerExc :: PluginCommunicationException
uninitLanguageServerExc =
  PluginCommunicationException "Language server state is not initialized"

expectInitialized :: (MonadIO m) => Text -> m (Maybe a) -> m a
expectInitialized errMsg maybeM = maybeM >>= \case
  Nothing -> throwIO $ PluginCommunicationException errMsg
  Just val -> pure val

getProgram
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m FilePath
getProgram = "Program is not initialized" `expectInitialized` (lsProgram <$> getServerState)

getCollectedRunInfo
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m CollectedRunInfo
getCollectedRunInfo =
  "Collected run info is not initialized" `expectInitialized` (lsCollectedRunInfo <$> getServerState)

getAllLocs
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (Set SourceLocation)
getAllLocs = "All locs are not initialized" `expectInitialized` (lsAllLocs <$> getServerState)

getParsedContracts
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (HashMap FilePath (LIGO Info))
getParsedContracts =
  "Parsed contracts are not initialized" `expectInitialized` (lsParsedContracts <$> getServerState)

getLambdaLocs
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (HashSet Range)
getLambdaLocs = "Lambda locs are not initialized" `expectInitialized` (lsLambdaLocs <$> getServerState)

getMaxStepsMb
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (Maybe RemainingSteps)
getMaxStepsMb = lsMaxSteps <$> getServerState

getEntrypointType
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m LigoType
getEntrypointType = "Entrypoint type is not initialized" `expectInitialized` (lsEntrypointType <$> getServerState)

getLigoTypesVec
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m LigoTypesVec
getLigoTypesVec = "Ligo types are not initialized" `expectInitialized` (lsLigoTypesVec <$> getServerState)

getEntrypoints
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (Map U.EpName U.Ty)
getEntrypoints = "Entrypoints are not initialized" `expectInitialized` (lsEntrypoints <$> getServerState)

getPickedEntrypoint
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m Text
getPickedEntrypoint = "Picked entrypoint is not initialized" `expectInitialized` (lsPickedEntrypoint <$> getServerState)

getLigoScopes
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (HashMap FilePath [Scope])
getLigoScopes = "Scopes are not initialized" `expectInitialized` (lsScopes <$> getServerState)

getArgumentRanges
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (HashSet Range)
getArgumentRanges = "Argument ranges are not initialized" `expectInitialized` (lsArgumentRanges <$> getServerState)

getParameterStorageAndOpsTypes :: LigoType -> (LigoType, LigoType, LigoType)
getParameterStorageAndOpsTypes (LigoTypeResolved typ) =
  fromMaybe (LigoType Nothing, LigoType Nothing, LigoType Nothing) do
    LTCArrow LigoTypeArrow{..} <- pure $ _lteTypeContent typ
    LTCRecord LigoTypeTable{..} <- pure $ _lteTypeContent _ltaType1

    param <- _lttFields HM.!? "0"
    st <- _lttFields HM.!? "1"
    pure (LigoTypeResolved param, LigoTypeResolved st, LigoTypeResolved _ltaType2)
getParameterStorageAndOpsTypes _ = (LigoType Nothing, LigoType Nothing, LigoType Nothing)

parseContracts :: (HasLigoClient m) => [FilePath] -> m (HashMap FilePath (LIGO Info))
parseContracts allFiles = do
  allFilesNE <-
    maybe
      (throwIO $ LigoCallException [int||File list is empty|])
      pure
      (nonEmpty allFiles)

  parsedFiles <- decodeCST allFilesNE <&> fmap \(SomeLIGO _ ast) -> ast
  pure $ HM.fromList $ zip allFiles parsedFiles

-- | Some exception in debugger logic.
data SomeDebuggerException where
  SomeDebuggerException :: DebuggerException e => e -> SomeDebuggerException

deriving stock instance Show SomeDebuggerException

instance Exception SomeDebuggerException where
  displayException (SomeDebuggerException e) = displayException e

  fromException e@(SomeException e') =
    asum
      [ SomeDebuggerException <$> fromException @LigoCallException e
      , SomeDebuggerException <$> fromException @LigoDecodeException e
      , SomeDebuggerException <$> fromException @MichelsonDecodeException e
      , SomeDebuggerException <$> fromException @ConfigurationException e
      , SomeDebuggerException <$> fromException @UnsupportedLigoVersionException e
      , SomeDebuggerException <$> fromException @UnsupportedExtension e
      , SomeDebuggerException <$> fromException @ReplacementException e
      , SomeDebuggerException <$> fromException @PluginCommunicationException e
      , SomeDebuggerException <$> fromException @ImpossibleHappened e
      , SomeDebuggerException <$> fromException @LigoIOException e
      , SomeDebuggerException <$> fromException @LigoResolveConfigException e
      , cast @_ @SomeDebuggerException e'
      ]

makeLensesWith postfixLFields ''LigoLanguageServerState
