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
      -- ^ Compiled contract.
    , criEpcMb :: Maybe (T.EntrypointCallT cp arg)
      -- ^ Entry point lifting sequence.
    , criParameterMb :: Maybe (T.Value arg)
      -- ^ Compiled parameter.
    , criStorageMb :: Maybe (T.Value st)
      -- ^ Compiled storage.
    } -> CollectedRunInfo

-- | Creates @CollectedRunInfo@ from compiled contract.
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
    -- ^ A path to the main contract's file.
  , lsCollectedRunInfo :: Maybe CollectedRunInfo
    -- ^ A collected information that is required
    -- to launch the contract.
  , lsAllLocs :: Maybe (Set SourceLocation)
    -- ^ Statement + interesting expression locations.
  , lsBinaryPath :: Maybe FilePath
    -- ^ A path to LIGO binary.
  , lsParsedContracts :: Maybe (HashMap FilePath (LIGO Info))
    -- ^ All parsed LIGO files.
  , lsLambdaLocs :: Maybe (HashSet Range)
    -- ^ Locations assigned to @LAMBDA@ instruction.
  , lsLigoTypesVec :: Maybe LigoTypesVec
    -- ^ A contract's type environment.
  , lsEntrypoints :: Maybe (Map U.EpName U.Ty)
    -- ^ A list of available @Michelson@ entrypoints.
  , lsPickedEntrypoint :: Maybe Text
    -- ^ An entry point name to run.
  , lsVarsComputeThreadPool :: Maybe AbortingThreadPool.Pool
    -- ^ A thread pool for variables decompilation.
  , lsToLigoValueConverter :: Maybe (DelayedValues.Manager PreLigoConvertInfo LigoOrMichValue)
    -- ^ A manager that converts @PreLigoConvertInfo@ into @LigoOrMichValue@.
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

-- | @withMichelsonEntrypoint contract entrypoint cont@ extracts
-- annotations @Notes arg@ and entry point lifting sequence @EntrypointCallT param arg@
-- and passes them into @cont@.
-- Throws @ConfigurationException@ if @entrypoint@ doesn't exist or it's malformed.
withMichelsonEntrypoint
  :: (MonadIO m)
  => T.Contract param st
  -> Text -- ^ Entry point name.
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

-- | Gets server state.
-- Throws @PluginCommunicationException@ if the state is not
-- initialized.
getServerState :: MonadRIO ext m => m (LanguageServerStateExt ext)
getServerState =
  asks _rcLSState >>= readTVarIO >>=
    maybe (throwIO uninitLanguageServerExc) pure

-- | An exception that is thrown when the server state is
-- uninitialized.
uninitLanguageServerExc :: PluginCommunicationException
uninitLanguageServerExc =
  PluginCommunicationException "Language server state is not initialized"

-- | @expectInitialized errMsg maybeM@ tries to unwrap @Maybe@
-- inside @maybeM@ computation.
-- Throws @PluginCommunicationException@ with @errMsg@ text
-- if underlying value is @Nothing@.
expectInitialized :: (MonadIO m) => Text -> m (Maybe a) -> m a
expectInitialized errMsg maybeM = maybeM >>= \case
  Nothing -> throwIO $ PluginCommunicationException errMsg
  Just val -> pure val

-- | Gets a program.
-- Throws a @PluginCommunicationException@ if @lsProgram@ field is @Nothing@.
getProgram
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m FilePath
getProgram = "Program is not initialized" `expectInitialized` (lsProgram <$> getServerState)

-- | Gets a collected run info.
-- Throws a @PluginCommunicationException@ if @lsCollectedRunInfo@ field is @Nothing@.
getCollectedRunInfo
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m CollectedRunInfo
getCollectedRunInfo =
  "Collected run info is not initialized" `expectInitialized` (lsCollectedRunInfo <$> getServerState)

-- | Gets all locations.
-- Throws @PluginCommunicationException@ if @lsAllLocs@ field is @Nothing@.
getAllLocs
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (Set SourceLocation)
getAllLocs = "All locs are not initialized" `expectInitialized` (lsAllLocs <$> getServerState)

-- | Gets all parsed contract files.
-- Throws @PluginCommunicationException@ if @lsParsedContracts@ field is @Nothing@.
getParsedContracts
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (HashMap FilePath (LIGO Info))
getParsedContracts =
  "Parsed contracts are not initialized" `expectInitialized` (lsParsedContracts <$> getServerState)

-- | Gets all lambda locations.
-- Throws @PluginCommunicationException@ if @lsLambdaLocs@ field is @Nothing@.
getLambdaLocs
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (HashSet Range)
getLambdaLocs = "Lambda locs are not initialized" `expectInitialized` (lsLambdaLocs <$> getServerState)

-- | Gets max steps.
getMaxStepsMb
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (Maybe RemainingSteps)
getMaxStepsMb = lsMaxSteps <$> getServerState

-- | Gets entry point LIGO type.
-- Throws @PluginCommunicationException@ if @lsEntrypointType@ field is @Nothing@.
getEntrypointType
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m LigoType
getEntrypointType = "Entrypoint type is not initialized" `expectInitialized` (lsEntrypointType <$> getServerState)

-- | Gets contract's type environment.
-- Throws @PluginCommunicationException@ if @lsLigoTypesVec@ field is @Nothing@.
getLigoTypesVec
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m LigoTypesVec
getLigoTypesVec = "Ligo types are not initialized" `expectInitialized` (lsLigoTypesVec <$> getServerState)

-- | Gets a map from entry point name to its type.
-- Throws @PluginCommunicationException@ if @lsEntrypoints@ field is @Nothing@.
getEntrypoints
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (Map U.EpName U.Ty)
getEntrypoints = "Entrypoints are not initialized" `expectInitialized` (lsEntrypoints <$> getServerState)

-- | Gets picked entry point name.
-- Throws @PluginCommunicationException@ if @lsPickedEntrypoint@ field is @Nothing@.
getPickedEntrypoint
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m Text
getPickedEntrypoint = "Picked entrypoint is not initialized" `expectInitialized` (lsPickedEntrypoint <$> getServerState)

-- | Gets scopes.
-- Throws @PluginCommunicationException@ if @lsScopes@ field is @Nothing@.
getLigoScopes
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (HashMap FilePath [Scope])
getLigoScopes = "Scopes are not initialized" `expectInitialized` (lsScopes <$> getServerState)

-- | Gets argument ranges.
-- Throws @PluginCommunicationException@ if @lsArgumentRanges@ field is @Nothing@.
getArgumentRanges
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (HashSet Range)
getArgumentRanges = "Argument ranges are not initialized" `expectInitialized` (lsArgumentRanges <$> getServerState)

-- | Gets a thread pool for variables decompilation.
-- Throws @PluginCommunicationException@ if @lsVarsComputeThreadPool@ field is @Nothing@.
getVarsComputeThreadPool
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m AbortingThreadPool.Pool
getVarsComputeThreadPool =
  "Thread pool is not initialized" `expectInitialized` (lsVarsComputeThreadPool <$> getServerState)

-- | Gets LIGO value's converting manager.
-- Throws @PluginCommunicationException@ if @lsToLigoValueConverter@ field is @Nothing@.
getToLigoValueConverter
  :: (LanguageServerStateExt ext ~ LigoLanguageServerState)
  => MonadRIO ext m => m (DelayedValues.Manager PreLigoConvertInfo LigoOrMichValue)
getToLigoValueConverter =
  "To LIGO value converter is not initialized" `expectInitialized` (lsToLigoValueConverter <$> getServerState)

-- | @getParameterStorageAndOpsTypes epType@ assumes that @epType@ has
-- the following format: @(param * st) -> operation list * st@
--
-- Returns if format matches:
-- 1. @param@ type.
-- 2. @st@ type.
-- 3. @operation list * st@ type.
--
-- Otherwise, return 3 @LigoTypeUnresolved@.
getParameterStorageAndOpsTypes :: LigoType -> (LigoType, LigoType, LigoType)
getParameterStorageAndOpsTypes (LigoTypeResolved typ) =
  fromMaybe (LigoType Nothing, LigoType Nothing, LigoType Nothing) do
    LTCArrow LigoTypeArrow{..} <- pure $ _lteTypeContent typ
    LTCRecord LigoTypeTable{..} <- pure $ _lteTypeContent _ltaType1

    param <- _lttFields HM.!? "0"
    st <- _lttFields HM.!? "1"
    pure (LigoTypeResolved param, LigoTypeResolved st, LigoTypeResolved _ltaType2)
getParameterStorageAndOpsTypes _ = (LigoTypeUnresolved, LigoTypeUnresolved, LigoTypeUnresolved)

-- | Parses all passed contracts.
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
      , SomeDebuggerException <$> fromException @UnsupportedExtension e
      , SomeDebuggerException <$> fromException @ReplacementException e
      , SomeDebuggerException <$> fromException @PluginCommunicationException e
      , SomeDebuggerException <$> fromException @ImpossibleHappened e
      , SomeDebuggerException <$> fromException @LigoIOException e
      , SomeDebuggerException <$> fromException @LigoResolveConfigException e
      , cast @_ @SomeDebuggerException e'
      ]

makeLensesWith postfixLFields ''LigoLanguageServerState
