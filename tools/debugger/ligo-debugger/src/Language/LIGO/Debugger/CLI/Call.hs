module Language.LIGO.Debugger.CLI.Call
  ( checkCompilation
  , compileLigoContractDebug
  , compileLigoExpression
  , getAvailableEntrypoints
  , decompileLigoValues
  , resolveConfig

    -- * Helpers
  , preprocess

    -- * Versions
  , Version (..)

  , getLigoVersion
  , parseLigoVersion
  , VersionSupport (..)
  , isSupportedVersion
  , minimalSupportedVersion
  , recommendedVersion
  , getVersionIssuesDetails

    -- * Exceptions
  , UnsupportedLigoVersionException (..)
  ) where

import Control.Arrow ((>>>))
import Data.Aeson
  (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), Value, eitherDecode',
  eitherDecodeStrict', object, withObject, (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (coerce)
import Data.Map qualified as M
import Data.MessagePack (errorMessages, unpackEither)
import Data.SemVer qualified as SemVer
import Data.Text qualified as T
import Data.Text qualified as Text
import Fmt.Buildable (Buildable, build, pretty)
import GHC.IO.Exception qualified as IOException
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (isPathSeparator, takeDirectory, (</>))
import System.Process (cwd, proc)
import System.Process.ByteString.Lazy qualified as PExtras
import Text.Interpolation.Nyan hiding (rmode')

import UnliftIO
  (Handler (Handler), MonadUnliftIO, catches, hFlush, handle, throwIO, try, withSystemTempFile)
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Exception (fromEither)
import UnliftIO.Process (readCreateProcessWithExitCode)

import Morley.Michelson.Parser qualified as MP
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as MU

import Language.LIGO.Debugger.CLI.Exception
import Language.LIGO.Debugger.CLI.Helpers
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.CLI.Types.LigoValue
import Language.LIGO.Debugger.CLI.Types.LigoValue.Codegen
import Language.LIGO.Debugger.Error
import Language.LIGO.Debugger.Handlers.Types
import Language.LIGO.ParseTree

import Util

----------------------------------------------------------------------------
-- Execution
----------------------------------------------------------------------------

-- | A command line argument to @ligo@.
--
-- This newtype serves to protect us from passing user input to LIGO
-- without the necessary preprocessing.
--
-- You can instantiate this with any concrete text without issues
-- (as long as you understand its semantics),
-- but to pass values that are known only at runtime use 'strArg' and family.
-- Do not use 'fromString' manually!
newtype LigoCliArg = LigoCliArg {unLigoCliArg :: String}
  deriving stock (Eq, Show)
  deriving newtype (IsString)

-- | Turn a string into CLI argument for LIGO.
strArg :: ToString s => s -> LigoCliArg
strArg = LigoCliArg . protect . toString
  where
    protect arg = case arg of
      ('-' : _) -> ' ' : arg
      _ -> arg

rewrapIOError :: MonadUnliftIO m => m a -> m a
rewrapIOError = UnliftIO.handle \exc@IOException.IOError{} ->
  UnliftIO.throwIO LigoIOException
    { lieType = IOException.ioe_type exc
    , lieDescription = toText $ IOException.ioe_description exc
    }

callLigoBS :: HasLigoClient m => Maybe FilePath -> [LigoCliArg] -> Maybe Source -> m LByteString
callLigoBS _rootDir args conM = do
  LigoClientEnv {..} <- getLigoClientEnv
  liftIO $ do
    let raw = maybe "" (BSL.fromStrict . encodeUtf8 . srcText) conM
    let fpM = srcPath <$> conM
    -- FIXME (LIGO-545): We should set the cwd to `rootDir`, but it seems there
    -- is a bug in the `process` library preventing us from doing it, as it
    -- causes non-determinism with handles receiving invalid arguments when
    -- running our tests with multiple threads.
    -- Additionally, using `withCurrentDirectory` from `directory` is no help
    -- either, as it doesn't seem thread-safe either.
    let process = proc _lceClientPath (coerce args)
    (ec, lo, le) <- PExtras.readCreateProcessWithExitCode process raw
      & rewrapIOError
    unless (ec == ExitSuccess && le == mempty) $ -- TODO: separate JSON errors and other ones
      UnliftIO.throwIO $ LigoClientFailureException (lazyBytesToText lo) (lazyBytesToText le) fpM (Just ec)
    pure lo

-- | Calls the LIGO binary (extracted from 'getLigoClientEnv') with the provided
-- root directory (ignored for now), arguments, and contract.
--
-- If '_lceLigoProcesses' is 'Nothing', this function will make a blocking call
-- to LIGO, otherwise it will use an unused process handle from the process
-- pool.
callLigo :: HasLigoClient m => Maybe FilePath -> [LigoCliArg] -> Maybe Source -> m Text
callLigo rootDir args conM = do
  LigoClientEnv{..} <- getLigoClientEnv
  let fpM = srcPath <$> conM
  -- FIXME (LIGO-545): We should set the cwd to `rootDir`, but it seems there is
  -- a bug in the `process` library preventing us from doing it, as it causes
  -- non-determinism with handles receiving invalid arguments when running our
  -- tests with multiple threads. Additionally, using `withCurrentDirectory`
  -- from `directory` is no help either, as it doesn't seem thread-safe either.
  let raw = maybe "" (toString . srcText) conM
  let process = (proc _lceClientPath $ coerce args){cwd = rootDir}
  (ec, lo, le) <- readCreateProcessWithExitCode process raw
    & rewrapIOError
  unless (ec == ExitSuccess && null le) $ -- TODO: separate JSON errors and other ones
    UnliftIO.throwIO $ LigoClientFailureException (toText lo) (toText le) fpM (Just ec)
  pure $ toText lo

newtype Version = Version
  { getVersion :: Text
  }

instance ToJSON Version where
  toJSON (Version ver) = object ["version" .= ver]

parseValue :: FromJSON a => Value -> Either String a
parseValue = parseEither parseJSON

-- | Abstracts boilerplate present in many of our LIGO handlers.
--
-- This function will simply ignore the contents of the
-- 'Source' and call LIGO with the file that is directly on the disk.
withLigo
  :: (HasLigoClient m)
  => Source  -- ^ The source file that will be given to LIGO.
  -> (FilePath -> [LigoCliArg])  -- ^ A function that should return the command line arguments for the provided file which will be used to call LIGO.
  -> (Source -> Value -> m a)  -- ^ Given the source file after calling LIGO and its decoded JSON value, what to do to return the final value.
  -> m a
withLigo src@(Source fp _) getArgs decode =
  UnliftIO.try (callLigoBS Nothing (getArgs fp) (Just src)) >>= \case
    Left LigoClientFailureException{cfeStderr} ->
      handleLigoMessages fp cfeStderr
    Right result -> case eitherDecode' result of
      Left e -> UnliftIO.throwIO $ LigoMalformedJSONException e (lazyBytesToText result) fp
      Right json -> decode (Source fp (lazyBytesToText result)) json

----------------------------------------------------------------------------
-- Execute ligo binary itself

-- | Get the current LIGO version.
--
-- ```
-- ligo --version
-- ```
--
-- TODO: rename this back to @getLigoVersion@.
getLigoVersionRaw :: (HasLigoClient m) => m Version
getLigoVersionRaw =
  Version . Text.strip <$> callLigo Nothing ["--version"] Nothing

-- | Get the current LIGO version, but in case of any failure return 'Nothing'.
--
-- ```
-- ligo --version
-- ```
getLigoVersionSafe :: (HasLigoClient m) => m (Maybe Version)
getLigoVersionSafe = do
  rightToMaybe <$> UnliftIO.try @_ @SomeException getLigoVersionRaw

findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot currentDirectory = do
  packageExists <- doesFileExist $ currentDirectory </> packageName
  if
    | packageExists -> pure $ Just currentDirectory
    | currentDirectory == "." || isRoot currentDirectory -> pure Nothing
    | otherwise -> findProjectRoot $ takeDirectory currentDirectory
  where
    packageName = "package.json"
    isRoot [c] = isPathSeparator c
    isRoot _   = False

projectRootToCliArg :: Maybe FilePath -> [LigoCliArg]
projectRootToCliArg = maybe [] (\projectRoot -> ["--project-root", strArg projectRoot])

-- | Call the preprocessor on some contract, handling all preprocessor directives.
--
-- This function will call the contract with a temporary file path, dumping the
-- contents of the given source so LIGO reads the contents. This allows us to
-- continue using the preprocessor even if it's an unsaved LSP buffer.
--
-- ```
-- ligo print preprocessed ${temp_file_name} --lib ${contract_dir} --format json
-- ```
preprocess
  :: (HasLigoClient m)
  => Source
  -> m Source
preprocess source = do
  maybeProjectRoot <- liftIO $ findProjectRoot dir
  withLigo source
    (\tempFp ->
      [ "print", "preprocessed", strArg tempFp
      , "--lib", strArg dir
      , "--format", "json"
      ] ++ projectRootToCliArg maybeProjectRoot
    )
    (\_ json ->
      case parseValue json of
        Left err -> do
          UnliftIO.throwIO $ LigoPreprocessFailedException (toText err) fp
        Right newContract ->
          pure $ Source fp newContract)
  where
    fp = srcPath source
    dir = takeDirectory fp

-- | A middleware for processing `ExpectedClientFailure` error needed to pass it
-- multiple levels up allowing us from restoring from expected ligo errors.
handleLigoErrors :: (HasLigoClient m) => FilePath -> Text -> m a
handleLigoErrors path encodedErr = do
  case eitherDecodeStrict' @(NonEmpty LigoError) $ encodeUtf8 encodedErr of
    Left err -> do
      UnliftIO.throwIO $ LigoErrorNodeParseErrorException (toText err) encodedErr path
    Right decodedErrors -> do
      UnliftIO.throwIO $
        LigoDecodedExpectedClientFailureException decodedErrors [] path

-- | Like 'handleLigoError', but used for the case when multiple LIGO errors may
-- happen. On a decode failure, attempts to decode as a single LIGO error
-- instead.
handleLigoMessages :: (HasLigoClient m) => FilePath -> Text -> m a
handleLigoMessages path encodedErr = do
  case eitherDecodeStrict' @LigoMessages $ encodeUtf8 encodedErr of
    Left _ ->
      -- It's possible LIGO has output a list of errors instead. Try to decode
      -- it:
      handleLigoErrors path encodedErr
    Right (LigoMessages decodedErrors decodedWarnings) -> do
      UnliftIO.throwIO $
        LigoDecodedExpectedClientFailureException decodedErrors decodedWarnings path

----------------------------------------------------------------------------
-- LIGO Debugger stuff
----------------------------------------------------------------------------

withMapLigoExc :: (HasLigoClient m) => m a -> m a
withMapLigoExc = flip catches
  [ Handler \(e :: LigoClientFailureException) ->
      throwIO ([int||#{cfeStderr e}|] :: LigoCallException)

  , Handler \(e :: LigoIOException) -> do
      LigoClientEnv ligoPath <- getLigoClientEnv
      let callException :: LigoCallException =
            [int||#{displayException e}
            Perhaps you specified a wrong path to LIGO executable: #{ligoPath}
            |]

      throwIO callException
  ]

{-
  Here and in the next calling @ligo@ binary functions
  we don't use '--format / --display-format json' flags.

  It's because we don't want to support @json@-schemas
  for @ligo@ errors. They look complex and it's
  not obvious how to extract useful info from them.
  Moreover, one day they can change this format
  and it would be painful to resolve it on our side.
-}

-- | When user picks an entrypoint we want to be sure that
-- the contract will compile with it.
checkCompilation :: (HasLigoClient m) => EntrypointName -> FilePath -> m ()
checkCompilation EntrypointName{..} file = void $ withMapLigoExc $
  callLigoBS Nothing
    do concat
        [ ["compile", "contract"]
        , ["--no-warn"]
        , guard (not $ T.null enModule) >> ["-m", strArg enModule]
        , ["--experimental-disable-optimizations-for-debugging"]
        , ["--disable-michelson-typechecking"]
        , [strArg file]
        ]
    Nothing

-- | Run ligo to compile the contract with all the necessary debug info.
compileLigoContractDebug :: forall m. (HasLigoClient m) => EntrypointName -> FilePath -> m (LigoMapper 'Unique)
compileLigoContractDebug EntrypointName{..} file = withMapLigoExc $
  callLigoBS Nothing
    do concat
        [ ["compile", "contract"]
        , ["--no-warn"]
        , ["--michelson-format", "msgpack"]
        , ["--michelson-comments", "location"]
        , ["--michelson-comments", "env"]
        , guard (not $ T.null enModule) >> ["-m", strArg enModule]
        , ["--experimental-disable-optimizations-for-debugging"]
        , ["--disable-michelson-typechecking"]
        , [strArg file]
        ]
    Nothing
    >>= either (throwIO . LigoDecodeException "decoding source mapper" . unlines . fmap toText . errorMessages) pure
      . unpackEither

-- | Run ligo to compile expression into Michelson in the context of the
-- given file.
compileLigoExpression :: forall m. (HasLigoClient m)
                      => MP.MichelsonSource -> FilePath -> Text -> m MU.Value
compileLigoExpression valueOrigin ctxFile expr = withMapLigoExc $
  callLigo Nothing
    [ "compile", "expression"
    , "--no-warn"
    , "--init-file", strArg ctxFile
    , "auto"  -- `syntax` argument, we can leave `auto` since context file is specified
    , strArg expr
    ] Nothing
    >>= decodeOutput
  where
    decodeOutput :: Text -> m MU.Value
    decodeOutput txt =
      MP.parseExpandValue valueOrigin txt
        & first (LigoDecodeException "parsing Michelson value" .  pretty)
        & fromEither

getAvailableEntrypoints :: forall m. (HasLigoClient m)
                        => FilePath -> m EntrypointsList
getAvailableEntrypoints file = withMapLigoExc $
  callLigo Nothing
    [ "info", "list-declarations"
    , "--only-ep"
    , strArg file
    ] Nothing
    >>= decodeOutput
  where
    decodeOutput :: Text -> m EntrypointsList
    decodeOutput txt =
      maybe
        do throwIO $ LigoDecodeException "decoding list declarations" txt
        pure
        do parseEntrypointsList txt

-- | Tries to decompile Michelson values with @LigoType@s in @LIGO@ ones.
decompileLigoValues :: forall m. (HasLigoClient m) => [(LigoType, T.SomeValue)] -> m [Maybe LigoValue]
decompileLigoValues typesAndValues = withMapLigoExc do
  withSystemTempFile "ligoValue.mligo" \path hndl -> do
    let contractCode = generateDecompilation typesAndValues

    hPutStrLn hndl (pretty @_ @Text contractCode)
    hFlush hndl

    callLigoBS Nothing
      [ "run", "test"
      , strArg path
      ] Nothing
    >>= decodeOutput
  where
    decodeOutput :: LByteString -> m [Maybe LigoValue]
    decodeOutput = either (throwIO . LigoDecodeException "decoding ligo decompile" . toText) pure
      . Aeson.eitherDecode

resolveConfig :: forall m. (HasLigoClient m) => FilePath -> m LigoLaunchRequest
resolveConfig configPath = withMapLigoExc do
  handleJSONException LigoResolveConfigException $
    callLigoBS Nothing
      [ "info", "resolve-config"
      , "--format", "json"
      , strArg configPath
      ] Nothing
      >>= decodeOutput
  where
    decodeOutput :: LByteString -> m LigoLaunchRequest
    decodeOutput bts = either (throwIO . LigoDecodeException "decoding config from ligo" . toText) pure do
      value <- Aeson.eitherDecode bts
      parseEither parseLaunchRequest value

    parseLaunchRequest :: Value -> Parser LigoLaunchRequest
    parseLaunchRequest = withObject "config" \o -> do
      let noDebug = Nothing
      logDir <- o .:? "log_dir"
      program <- o .:? "program"
      michelsonEntrypoint <- o .:? "michelson_entrypoint"
      storage <- o .:? "storage"
      entrypoint <- o .:? "entrypoint"
      parameter <- o .:? "parameter"
      contractEnv <- traverse parseContractEnv =<< o .:? "contract_env"
      pure LigoLaunchRequest{..}
      where
        parseContractEnv :: Value -> Parser LigoContractEnv
        parseContractEnv = replaceTextualNumbers >>> withObject "contract env" \o -> do
          now <- o .:? "now"
          balance <- o .:? "balance"
          amount <- o .:? "amount"
          self <- o .:? "self"
          source <- o .:? "source"
          sender <- o .:? "sender"
          chainId <- o .:? "chain_id"
          level <- o .:? "level"
          votingPowers <- o .:? "voting_powers"
          pure LigoContractEnv{..}

-- Versions
----------------------------------------------------------------------------

-- | The current LIGO version is completely unsupported.
data UnsupportedLigoVersionException = UnsupportedLigoVersionException SemVer.Version
  deriving stock (Show)

instance Buildable UnsupportedLigoVersionException where
  build (UnsupportedLigoVersionException verActual) =
    [int||Used `ligo` executable has #semv{verActual} version which is not supported|]

instance Exception UnsupportedLigoVersionException where
  displayException = pretty

instance DebuggerException UnsupportedLigoVersionException where
  type ExceptionTag UnsupportedLigoVersionException = "UnsupportedLigoVersion"
  debuggerExceptionType _ = UserException
  debuggerExceptionData (UnsupportedLigoVersionException verActual) = M.fromList
    [ ("actualVersion", [int||#semv{verActual}|])
    , ("recommendedVersion", [int||#semv{recommendedVersion}|])
    ]
  shouldInterruptDebuggingSession = False

-- | Run ligo to get the version of executable.
getLigoVersion :: (HasLigoClient m) => m Version
getLigoVersion = withMapLigoExc getLigoVersionRaw

parseLigoVersion :: Version -> Maybe SemVer.Version
parseLigoVersion =
  rightToMaybe . SemVer.fromText . T.strip . getVersion

-- | Whether a particular version of @ligo@ version we treat as supported.
data VersionSupport
  = VersionSupported
    -- ^ We fully support a version with high level of assurance.
    --
    -- If our tests pass for a particular version, we can treat it as supported.
  | VersionPartiallySupported
    -- ^ This version works for us with exception of some minor corner cases;
    -- or we don't know for sure if this version is supported.
  | VersionUnsupported
    -- ^ We do not provide an adequate support for this version.
  deriving stock (Eq, Show, Enum, Bounded)

-- | @x > y@ means that @x@ assumes better support than @y@.
instance Ord VersionSupport where
  -- writing down this instance manually to avoid invalid
  -- addition of new constructors

  compare = compare `on` \case
    VersionUnsupported -> 0 :: Int
    VersionPartiallySupported -> 1
    VersionSupported -> 2

instance Buildable VersionSupport where
  build = \case
    VersionSupported -> "supported"
    VersionPartiallySupported -> "partially supported"
    VersionUnsupported -> "unsupported"

-- | See how much do we support the provided version of @ligo@.
isSupportedVersion :: SemVer.Version -> VersionSupport
isSupportedVersion ver = fromMaybe fullSupport $ asum
  -- List of rules to detect an unsupported version.
  --
  -- Rules in `docs/ligo-versions.md` make sure that this function
  -- is kept up-to-date.
  --
  -- You can add custom rules, e.g. the following one excludes one version
  -- because it is buggy:
  --
  -- @
  -- ver == [Data.SemVer.QQ.version|0.1.2|]
  --   ?- noSupport
  -- @
  [ -- Debug information in the necessary format is not available in old versions
    ver < minimalSupportedVersion
      ?- noSupport

    -- Future versions that we didn't check yet
  , ver > recommendedVersion  -- don't hesitate to replace this with a higher constant
      ?- partialSupport
  ]
  where
    infix 0 ?-
    (?-) :: Bool -> a -> Maybe a
    cond ?- res = guard cond $> res

    noSupport = VersionUnsupported
    partialSupport = VersionPartiallySupported
    fullSupport = VersionSupported

  -- Implementation note: in case in the future we'll want to provide the users
  -- with the full list of supported versions, we can define rules in terms of
  -- 'Data.SemVer.Constraint.Constraint'.

-- | Minimal version which we at least partially support.
--
-- We extract this to a separate variable only because it is needed
-- in tests.
minimalSupportedVersion :: SemVer.Version
minimalSupportedVersion =
  $$(readSemVerQ $ resourcesFolder </> "versions" </> "minimal-supported")

-- | Version that we suggest the user to use with our debugger.
--
-- For now we assume that debugger may break more often and more badly than LSP,
-- so it has its own recommended version. By experience, it quite possible that
-- LIGO introduces breaking changes that we cannot workaround and that may take
-- a while to fix.
-- When the situation stabilizes, we can get rid of this and assume that we
-- are successful at supporting the latest version.
recommendedVersion :: SemVer.Version
recommendedVersion =
  $$(readSemVerQ $ resourcesFolder </> "versions" </> "recommended")

-- | A clarifying message that mentions issues with ligo version being
-- unsupported, in case any such issues take place.
getVersionIssuesDetails :: (HasLigoClient m) => m (Maybe Text)
getVersionIssuesDetails = do
  (parseLigoVersion <$> getLigoVersion) <&> \case
        Nothing -> Just [int|n|
          You seem to be using not a stable release of ligo,
          consider trying #semv{recommendedVersion}.
          |]
        Just ver -> case isSupportedVersion ver of
          -- We should never get this as @unsupported@ case is checked
          -- at startup, but putting some message here nevertheless just in case
          VersionUnsupported -> Just [int|n|
            Note that the current version of ligo #semv{ver} is not supported!
            |]
          VersionPartiallySupported -> Just [int|n|
            Note that the current ligo version #semv{ver} is not guaranteed to work
            correctly with debugger.

            If you need debugging capabilities, you might try ligo of
            #semv{recommendedVersion} version until the new version of
            the extension is released.
            |]
          VersionSupported -> Nothing
