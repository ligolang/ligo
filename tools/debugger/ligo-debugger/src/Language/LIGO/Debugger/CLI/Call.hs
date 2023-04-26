module Language.LIGO.Debugger.CLI.Call
  ( compileLigoContractDebug
  , compileLigoExpression
  , getAvailableEntrypoints
  , decompileLigoValues

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

import Data.Aeson
  (FromJSON (parseJSON), KeyValue ((.=)), ToJSON, Value, eitherDecode', eitherDecodeStrict', object)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (coerce)
import Data.Map qualified as M
import Data.SemVer qualified as SemVer
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Debug qualified
import Fmt (Buildable, build, pretty)
import GHC.IO.Exception qualified as IOException
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (isPathSeparator, takeDirectory, takeFileName, (</>))
import System.IO (hGetChar)
import System.Process (cwd, proc)
import System.Process.ByteString.Lazy qualified as PExtras
import Text.Interpolation.Nyan

import UnliftIO (MonadUnliftIO, hFlush, handle, throwIO, try, withSystemTempFile)
import UnliftIO.Directory (doesFileExist)
import UnliftIO.Exception (fromEither, mapExceptionM)
import UnliftIO.Pool (Pool)
import UnliftIO.Pool qualified as Pool
import UnliftIO.Process (getProcessExitCode, readCreateProcessWithExitCode)

import Morley.Michelson.Parser qualified as MP
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as MU

import Language.LIGO.Debugger.CLI.Exception
import Language.LIGO.Debugger.CLI.Helpers
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.CLI.Types.LigoValue
import Language.LIGO.Debugger.CLI.Types.LigoValue.Codegen
import Language.LIGO.Debugger.Error
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
  where
    rewrapIOError = UnliftIO.handle \exc@IOException.IOError{} ->
      UnliftIO.throwIO LigoIOException
        { lieType = IOException.ioe_type exc
        , lieDescription = toText $ IOException.ioe_description exc
        }

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
  case _lceLigoProcesses of
    Nothing -> do
      let raw = maybe "" (toString . srcText) conM
      let process = (proc _lceClientPath $ coerce args){cwd = rootDir}
      (ec, lo, le) <- readCreateProcessWithExitCode process raw
      unless (ec == ExitSuccess && null le) $ -- TODO: separate JSON errors and other ones
        UnliftIO.throwIO $ LigoClientFailureException (toText lo) (toText le) fpM (Just ec)
      pure $ toText lo
    Just ligoProcesses ->
      Pool.withResource ligoProcesses \LigoProcess{..} -> liftIO do
        Text.hPutStrLn _lpStdin $ unwords $ map (Debug.show . unLigoCliArg) args
        hFlush _lpStdin
        lo <- readUntilNull _lpStdout
        le <- readUntilNull _lpStderr
        getProcessExitCode _lpLigo >>= \case
          Nothing | Text.null le -> pure lo
          -- We assume a failure even in the case of 'ExitSuccess' because LIGO
          -- should not quit.
          ec -> UnliftIO.throwIO $ LigoClientFailureException lo le fpM ec
  where
    readLoop :: Handle -> IO String
    readLoop h =
      hGetChar h >>= \case
        '\0' -> pure []
        c    -> fmap (c :) (readLoop h)

    readUntilNull :: Handle -> IO Text
    readUntilNull = fmap toText . readLoop

newtype Version = Version
  { getVersion :: Text
  }

instance ToJSON Version where
  toJSON (Version ver) = object ["version" .= ver]

parseValue :: FromJSON a => Value -> Either String a
parseValue = parseEither parseJSON

-- | Abstracts boilerplate present in many of our LIGO handlers.
--
-- This function will write the file to the temporary directory in the disk
-- given the provided 'TempSettings' and call LIGO with its written contents.
-- But if the file is not dirty, it will simply ignore the contents of the
-- 'Source' and call LIGO with the file that is directly on the disk. It's up to
-- the caller of this function to ensure that the dirty field of the 'Source' is
-- accurate.
withLigo
  :: (HasLigoClient m)
  => Source  -- ^ The source file that will be given to LIGO.
  -> TempSettings  -- ^ The temporary directory that should be used in case the file is dirty.
  -> (FilePath -> [LigoCliArg])  -- ^ A function that should return the command line arguments for the provided file which will be used to call LIGO.
  -> (Source -> Value -> m a)  -- ^ Given the source file after calling LIGO and its decoded JSON value, what to do to return the final value.
  -> m a
withLigo src@(Source fp True contents) (TempSettings rootDir tempDirTemplate) getArgs decode =
  withTempDirTemplate \tempDir ->
    withTempFile tempDir (takeFileName fp) \tempFp hnd -> do
      -- Create temporary file and call LIGO with it.
      liftIO (Text.hPutStr hnd contents *> hClose hnd)
      UnliftIO.try (callLigoBS (Just rootDir) (getArgs tempFp) (Just src)) >>= \case
        -- LIGO produced output in stderr, or exited with non-zero exit code.
        Left LigoClientFailureException {cfeStderr} -> do
          let fixMarkers' = Text.replace (toText tempFp) (toText fp)
          handleLigoMessages fp (fixMarkers' cfeStderr)
        Right result -> case eitherDecode' result of
          -- Failed to decode as a Value, this indicates that the JSON output
          -- from LIGO is malformed. This is a bug in the LIGO binary and we
          -- should contact the LIGO team if it ever happens.
          Left e -> UnliftIO.throwIO $ LigoMalformedJSONException e (lazyBytesToText result) fp
          Right json -> decode (Source tempFp True (lazyBytesToText result)) json
  where
    withTempDirTemplate = case tempDirTemplate of
      GenerateDir template -> withTempDirectory rootDir template
      UseDir path -> ($ path)
withLigo src@(Source fp False _) _ getArgs decode =
  UnliftIO.try (callLigoBS Nothing (getArgs fp) (Just src)) >>= \case
    Left LigoClientFailureException{cfeStderr} ->
      handleLigoMessages fp cfeStderr
    Right result -> case eitherDecode' result of
      Left e -> UnliftIO.throwIO $ LigoMalformedJSONException e (lazyBytesToText result) fp
      Right json -> decode (Source fp False (lazyBytesToText result)) json

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
  => TempSettings
  -> Source
  -> m Source
preprocess tempSettings source = do
  maybeProjectRoot <- liftIO $ findProjectRoot dir
  withLigo source tempSettings
    (\tempFp ->
      [ "print", "preprocessed", strArg tempFp
      , "--lib", strArg dir
      , "--format", "json"
      ] ++ projectRootToCliArg maybeProjectRoot
    )
    (\(Source tempFp isDirty _) json ->
      case parseValue json of
        Left err -> do
          UnliftIO.throwIO $ LigoPreprocessFailedException (toText err) fp
        Right newContract ->
          bool pure (fixMarkers tempFp) isDirty $ Source fp isDirty newContract)
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
-- Debugging utilities
----------------------------------------------------------------------------

-- | A datatype that automatically manages a @LigoProcess@ lifecycle,
-- for debugging.
newtype WithLigoDebug a = WithLigoDebug
  { runWithLigoDebug :: Pool LigoProcess -> IO a
  } deriving stock (Functor)

instance Applicative WithLigoDebug where
  pure = WithLigoDebug . const . pure

  WithLigoDebug f <*> WithLigoDebug a = WithLigoDebug \ligo ->
    f ligo <*> a ligo

instance Monad WithLigoDebug where
  WithLigoDebug a >>= k = WithLigoDebug \ligo ->
    a ligo >>= \x -> runWithLigoDebug (k x) ligo

instance MonadIO WithLigoDebug where
  liftIO = WithLigoDebug . const

instance MonadUnliftIO WithLigoDebug where
  withRunInIO inner = WithLigoDebug \ligo ->
    withRunInIO \run -> inner (run . flip runWithLigoDebug ligo)

instance MonadFail WithLigoDebug where
  fail = WithLigoDebug . const . fail

instance HasLigoClient WithLigoDebug where
  getLigoClientEnv = WithLigoDebug \ligo ->
    LigoClientEnv <$> liftIO ligoBinaryPath <*> pure (Just ligo)

----------------------------------------------------------------------------
-- LIGO Debugger stuff
----------------------------------------------------------------------------

withMapLigoExc :: (MonadUnliftIO m) => m a -> m a
withMapLigoExc = mapExceptionM \(e :: LigoClientFailureException) ->
  [int||#{cfeStderr e}|] :: LigoCallException

{-
  Here and in the next calling @ligo@ binary functions
  we don't use '--format / --display-format json' flags.

  It's because we don't want to support @json@-schemas
  for @ligo@ errors. They look complex and it's
  not obvious how to extract useful info from them.
  Moreover, one day they can change this format
  and it would be painful to resolve it on our side.
-}

-- | Run ligo to compile the contract with all the necessary debug info.
compileLigoContractDebug :: forall m. (HasLigoClient m) => String -> FilePath -> m (LigoMapper 'Unique)
compileLigoContractDebug entrypoint file = withMapLigoExc $
  callLigoBS Nothing
    [ "compile", "contract"
    , "--no-warn"
    , "--michelson-format", "json"
    , "--michelson-comments", "location"
    , "--michelson-comments", "env"
    , "-e", strArg entrypoint
    , "--experimental-disable-optimizations-for-debugging"
    , "--disable-michelson-typechecking"
    , strArg file
    ] Nothing
    >>= either (throwIO . LigoDecodeException "decoding source mapper" . toText) pure
      . Aeson.eitherDecode

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
isSupportedVersion ver = fromMaybe VersionSupported $ asum
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
  [
    -- Debug information in the necessary format is not available in old versions
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
