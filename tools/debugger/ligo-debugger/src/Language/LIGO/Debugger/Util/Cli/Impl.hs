-- | Module that handles ligo binary execution.
module Language.LIGO.Debugger.Util.Cli.Impl
  ( -- * LIGO exception handling
    SomeLigoException (..)
  , LigoErrorNodeParseErrorException  (..)
  , LigoClientFailureException (..)
  , LigoDecodedExpectedClientFailureException (..)
  , LigoIOException (..)

    -- * Versioning
  , Version (..)

    -- * Calling LIGO
  , LigoCliArg (..)
  , strArg
  , callLigo
  , callLigoBS
  , getLigoVersionRaw
  , getLigoVersionSafe
  , preprocess
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Aeson
  (FromJSON (..), ToJSON (..), Value, eitherDecode', eitherDecodeStrict', object, (.=))
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (coerce)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Debug qualified
import GHC.IO.Exception qualified as IOException
import System.Exit (ExitCode (..))
import System.FilePath (isPathSeparator, takeDirectory, takeFileName, (</>))
import System.IO (hFlush, hGetChar)
import System.Process.ByteString.Lazy qualified as PExtras
import Text.Interpolation.Nyan
import UnliftIO.Directory (canonicalizePath, doesFileExist)
import UnliftIO.Exception qualified as UnliftIO (handle, throwIO, try)
import UnliftIO.Pool (Pool)
import UnliftIO.Pool qualified as Pool
import UnliftIO.Process
  (CreateProcess (..), getProcessExitCode, proc, readCreateProcessWithExitCode)
import UnliftIO.Temporary (withTempDirectory, withTempFile)

import Language.LIGO.Debugger.Util.AST.Includes (extractIncludes)
import Language.LIGO.Debugger.Util.Cli.Json
import Language.LIGO.Debugger.Util.Cli.Types
import Language.LIGO.Debugger.Util.ParseTree (Source (..))
import Language.LIGO.Debugger.Util.Parser (LineMarker (lmFile))
import Language.LIGO.Debugger.Util.Util (lazyBytesToText)

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

class Exception a => LigoException a where

-- | Catch ligo failure to be able to restore from it
data LigoClientFailureException = LigoClientFailureException
  { cfeStdout :: Text -- ^ stdout
  , cfeStderr :: Text -- ^ stderr
  , cfeFile   :: Maybe FilePath  -- ^ File that caused the error
  , cfeExit   :: Maybe ExitCode  -- ^ The exit code that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

-- | Expected ligo failure decoded from its JSON output
data LigoDecodedExpectedClientFailureException = LigoDecodedExpectedClientFailureException
  { decfeErrorsDecoded :: NonEmpty LigoError -- ^ Successfully decoded ligo errors
  , decfeWarningsDecoded :: [LigoError]
  , decfeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

data LigoPreprocessFailedException = LigoPreprocessFailedException
  { pfeMessage :: Text -- ^ Successfully decoded ligo error
  , pfeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

data LigoFormatFailedException = LigoFormatFailedException
  { ffeMessage :: Text -- ^ Successfully decoded ligo error
  , ffeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

-- | Starting @ligo@ executable failed - some system error.
--
-- Likely LIGO isn't installed or was not found.
data LigoIOException = LigoIOException
  { lieType :: IOException.IOErrorType
  , lieDescription :: Text
  } deriving anyclass (LigoException)
    deriving stock (Show)

----------------------------------------------------------------------------
-- Errors that may fail due to changes in ligo compiler

-- | Parse error occured during ligo output JSON decoding.
data LigoErrorNodeParseErrorException = LigoErrorNodeParseErrorException
  { lnpeError :: Text -- ^ Error description
  , lnpeOutput :: Text -- ^ The JSON output which we failed to decode
  , lnpeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

-- | Parse error occured during ligo stderr JSON decoding.
data LigoDefinitionParseErrorException = LigoDefinitionParseErrorException
  { ldpeError :: Text -- ^ Error description
  , ldpeOutput :: Text -- ^ The JSON output which we failed to decode
  , ldpeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

-- | LIGO produced a malformed JSON that can't be encoded into a Value. This is
-- a bug in the compiler that must be reported to the LIGO team.
data LigoMalformedJSONException = LigoMalformedJSONException
  { lmjeError :: String -- ^ Error description
  , lmjeOutput :: Text -- ^ The JSON output which we failed to decode
  , lmjeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

data SomeLigoException where
  SomeLigoException :: LigoException a => a -> SomeLigoException

deriving stock instance Show SomeLigoException

instance Exception SomeLigoException where
  displayException (SomeLigoException a) = [int||Error (LIGO): #{displayException a}|]

  fromException e =
    asum
      [ SomeLigoException <$> fromException @LigoClientFailureException                e
      , SomeLigoException <$> fromException @LigoDecodedExpectedClientFailureException e
      , SomeLigoException <$> fromException @LigoErrorNodeParseErrorException          e
      , SomeLigoException <$> fromException @LigoMalformedJSONException                e
      , SomeLigoException <$> fromException @LigoDefinitionParseErrorException         e
      , SomeLigoException <$> fromException @LigoPreprocessFailedException             e
      , SomeLigoException <$> fromException @LigoFormatFailedException                 e
      , SomeLigoException <$> fromException @LigoIOException                           e
      ]

instance Exception LigoClientFailureException where
  displayException LigoClientFailureException {..} =
    [int||LIGO binary failed with
    #{stdOut}
    #{stdErr}
    #{causedBy}|]
    where
      causedBy = maybe ("" :: String) (\file -> [int||Caused by: #{file}|]) cfeFile

      stdOut
        | Text.null cfeStdout = "" :: String
        | otherwise = [int||Stdout: #{cfeStdout}|]

      stdErr
        | Text.null cfeStderr = "" :: String
        | otherwise = [int||Stderr: #{cfeStderr}|]

instance Exception LigoDecodedExpectedClientFailureException where
  displayException LigoDecodedExpectedClientFailureException {..} =
    [int||LIGO binary produced expected error which we successfully decoded as:
    #{toList decfeErrorsDecoded}
    With warnings
    #{toList decfeWarningsDecoded}
    Caused by: #{decfeFile}|]

instance Exception LigoErrorNodeParseErrorException where
  displayException LigoErrorNodeParseErrorException {..} =
    [int||LIGO binary produced an error JSON which we were unable to decode:
    #{lnpeError}
    Caused by: #{lnpeFile}
    JSON output dumped:
    #{lnpeOutput}|]

instance Exception LigoMalformedJSONException where
  displayException LigoMalformedJSONException {..} =
    [int||LIGO binary produced a malformed JSON:
    #{lmjeError}
    Caused by: #{lmjeFile}
    JSON output dumped:
    #{lmjeOutput}|]

instance Exception LigoDefinitionParseErrorException where
  displayException LigoDefinitionParseErrorException {..} =
    [int||LIGO binary produced a definition output which we consider malformed:
    #{ldpeError}
    Caused by: #{ldpeFile}
    JSON output dumped:
    #{ldpeOutput}|]

instance Exception LigoPreprocessFailedException where
  displayException LigoPreprocessFailedException {..} =
    [int||LIGO failed to preprocess contract with: #{pfeMessage}
    Caused by: #{pfeFile}|]

instance Exception LigoFormatFailedException where
  displayException LigoFormatFailedException {..} =
    [int||LIGO failed to format contract with: #{ffeMessage}
    Caused by: #{ffeFile}|]

instance Exception LigoIOException where
  displayException LigoIOException {..} =
    [int||LIGO executable run failed: #{lieDescription}|]

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

fixMarkers
  :: MonadIO m
  => FilePath  -- ^ Name of the temporary file to be replaced.
  -> Source  -- ^ Source file which has its formatted/preprocessed contents.
  -> m Source
fixMarkers tempFp (Source fp dirty contents) = liftIO do  -- HACK: I use `liftIO` here to avoid a `MonadFail m` constraint.
  markers <- extractIncludes contents
  let files = hashNub $ map lmFile markers
  filesRelation <- filter (uncurry (/=)) <$> traverse (sequenceA . (id &&& canonicalizePath)) files
  let replace old new = Text.replace (toText old) (toText new)
  let go = mconcat $ map (\(old, new) -> Endo $ replace old new) ((tempFp, fp) : filesRelation)
  pure $ Source fp dirty $ appEndo go contents

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
