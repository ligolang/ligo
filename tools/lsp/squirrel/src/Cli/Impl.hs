{-# LANGUAGE DeriveGeneric #-}

-- | Module that handles ligo binary execution.
module Cli.Impl
  ( SomeLigoException (..)
  , LigoErrorNodeParseErrorException  (..)
  , LigoClientFailureException (..)
  , LigoDecodedExpectedClientFailureException (..)
  , LigoUnexpectedCrashException (..)

  , Version (..)

  , callLigo
  , callForFormat
  , getLigoVersion
  , preprocess
  , getLigoDefinitions
  ) where

import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Data.Aeson (ToJSON (..), eitherDecodeStrict', object, (.=))
import Data.Foldable (asum, toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as Text
import Duplo.Pretty (pp)
import Katip (LogItem (..), PayloadSelection (AllKeys), ToObject)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, hFlush)
import System.Process
import Text.Regex.TDFA ((=~), getAllTextSubmatches)
import UnliftIO.Exception (Exception (..), SomeException (..), throwIO, try)
import UnliftIO.Temporary (withTempFile)

import Cli.Json
import Cli.Types
import Log (Log, i)
import Log qualified
import ParseTree (Source (..))

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

class Exception a => LigoException a where

-- | Catch ligo failure to be able to restore from it
data LigoClientFailureException = LigoClientFailureException
  { cfeStdout :: Text -- ^ stdout
  , cfeStderr :: Text -- ^ stderr
  , cfeFile   :: Maybe FilePath  -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

-- | Expected ligo failure decoded from its JSON output
data LigoDecodedExpectedClientFailureException = LigoDecodedExpectedClientFailureException
  { decfeErrorsDecoded :: NonEmpty LigoError -- ^ Successfully decoded ligo errors
  , decfeWarningsDecoded :: [LigoError]
  , decfeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (LigoException)
    deriving stock (Show)

-- | Ligo has unexpectedly crashed.
data LigoUnexpectedCrashException = LigoUnexpectedCrashException
  { uceMessage :: Text -- ^ extracted failure message
  , uceFile :: FilePath -- ^ File that caused the error
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

data SomeLigoException where
  SomeLigoException :: LigoException a => a -> SomeLigoException

deriving stock instance Show SomeLigoException

instance Exception SomeLigoException where
  displayException (SomeLigoException a) = [i|Error (LIGO): #{displayException a}|]

  fromException e =
    asum
      [ SomeLigoException <$> fromException @LigoClientFailureException                e
      , SomeLigoException <$> fromException @LigoDecodedExpectedClientFailureException e
      , SomeLigoException <$> fromException @LigoErrorNodeParseErrorException          e
      , SomeLigoException <$> fromException @LigoDefinitionParseErrorException         e
      , SomeLigoException <$> fromException @LigoUnexpectedCrashException              e
      , SomeLigoException <$> fromException @LigoPreprocessFailedException             e
      , SomeLigoException <$> fromException @LigoFormatFailedException                 e
      ]

instance Exception LigoClientFailureException where
  displayException LigoClientFailureException {cfeStdout, cfeStderr, cfeFile} =
    "LIGO binary failed with\nStdout:\n" <> unpack cfeStdout
    <> "\nStderr:\n" <> unpack cfeStderr
    <> "\nCaused by: " <> fromMaybe "<unknown file>" cfeFile

instance Exception LigoDecodedExpectedClientFailureException where
  displayException LigoDecodedExpectedClientFailureException {decfeErrorsDecoded, decfeWarningsDecoded, decfeFile} =
    "LIGO binary produced expected error which we successfully decoded as:\n"
    <> show (pp $ toList decfeErrorsDecoded)
    <> "\nWith warnings:\n"
    <> show (pp decfeWarningsDecoded)
    <> "\nCaused by: " <> decfeFile

instance Exception LigoErrorNodeParseErrorException where
  displayException LigoErrorNodeParseErrorException {lnpeError, lnpeOutput, lnpeFile} =
    "LIGO binary produced an error JSON which we consider malformed:\n"
    <> unpack lnpeError
    <> "\nCaused by: " <> lnpeFile
    <> "\nJSON output dumped:\n"
    <> unpack lnpeOutput

instance Exception LigoDefinitionParseErrorException where
  displayException LigoDefinitionParseErrorException {ldpeError, ldpeOutput, ldpeFile} =
    "LIGO binary produced a definition output which we consider malformed:\n"
    <> unpack ldpeError
    <> "\nCaused by: " <> ldpeFile
    <> "\nJSON output dumped:\n"
    <> unpack ldpeOutput

instance Exception LigoUnexpectedCrashException where
  displayException LigoUnexpectedCrashException {uceMessage, uceFile} =
    "LIGO binary crashed with error: " <> unpack uceMessage
    <> "\nCaused by: " <> uceFile

instance Exception LigoPreprocessFailedException where
  displayException LigoPreprocessFailedException {pfeMessage, pfeFile} =
    "LIGO failed to preprocess contract with\n:" <> unpack pfeMessage
    <> "\nCaused by: " <> pfeFile

instance Exception LigoFormatFailedException where
  displayException LigoFormatFailedException {ffeMessage, ffeFile} =
    "LIGO failed to format contract with\n:" <> unpack ffeMessage
    <> "\nCaused by: " <> ffeFile

----------------------------------------------------------------------------
-- Execution
----------------------------------------------------------------------------

callLigoImpl :: HasLigoClient m => Maybe FilePath -> [String] -> Maybe Source -> m Text
callLigoImpl rootDir args conM = do
  LigoClientEnv {..} <- getLigoClientEnv
  liftIO $ do
    let raw = maybe "" (unpack . srcText) conM
    let fpM = srcPath <$> conM
    let
      process = (proc _lceClientPath args)
        { cwd = rootDir
        }
    (ec, lo, le) <- readCreateProcessWithExitCode process raw
    unless (ec == ExitSuccess && le == mempty) $ -- TODO: separate JSON errors and other ones
      throwIO $ LigoClientFailureException (pack lo) (pack le) fpM
    pure $ pack lo

-- | Call ligo binary and return stdin and stderr accordingly.
callLigo :: HasLigoClient m => Maybe FilePath -> [String] -> Source -> m Text
callLigo rootDir args = callLigoImpl rootDir args . Just

newtype Version = Version
  { getVersion :: Text
  }

instance ToJSON Version where
  toJSON (Version ver) = object ["version" .= ver]

deriving anyclass instance ToObject Version

instance LogItem Version where
  payloadKeys = const $ const AllKeys

-- | Run some action using a temporary file, given some source. The action will
-- be provided with the path to the temporary file, as well as its handle.
-- Returns the original action, along with a function that replaces occurrences
-- of the temporary file with the original one.
usingTemporaryDir
  :: MonadUnliftIO m
  => FilePath
  -> Source
  -> (FilePath -> Handle -> m a)
  -> m (a, Text -> Text)
usingTemporaryDir rootDir (Source fp contents) action =
  withTempFile rootDir (takeFileName fp) \tempFp handle -> do
    liftIO do
      Text.hPutStr handle contents
      hFlush handle
    let fixMarkers = Text.replace (pack tempFp) (pack fp)
    (, fixMarkers) <$> action tempFp handle

----------------------------------------------------------------------------
-- Execute ligo binary itself

-- | Get the current LIGO version.
--
-- ```
-- ligo --version
-- ```
getLigoVersion :: (HasLigoClient m, Log m) => m (Maybe Version)
getLigoVersion = Log.addNamespace "getLigoVersion" do
  mbOut <- try $ callLigoImpl Nothing ["--version"] Nothing
  case mbOut of
    -- We don't want to die if we failed to parse the version...
    Left (SomeException e) -> do
      $(Log.err) [i|Couldn't get LIGO version with: #{e}|]
      pure Nothing
    Right output ->
      pure $ Just $ Version $ Text.strip output

-- | Call LIGO's pretty printer on some contract.
--
-- This function will call the contract with a temporary file path, dumping the
-- contents of the given source so LIGO reads the contents. This allows us to
-- call the pretty printer even if it's an unsaved LSP buffer.
--
-- ```
-- ligo print pretty ${temp_file_name} --format json
-- ```
--
-- FIXME: LIGO expands preprocessor directives before pretty printing.
-- See: https://gitlab.com/ligolang/ligo/-/issues/1374
callForFormat :: (HasLigoClient m, Log m) => FilePath -> Source -> m Text
callForFormat projDir source = Log.addNamespace "callForFormat" $ Log.addContext source do
  (mbOut, fixMarkers) <- usingTemporaryDir projDir source \tempFp _ ->
    try $ callLigo (Just projDir)
      ["print", "pretty", tempFp, "--format", "json"]
      source
  case fixMarkers <$> mbOut of
    Right output ->
      case eitherDecodeStrict' @Text $ encodeUtf8 output of
        Left err -> do
          $(Log.err) [i|Could not format document with error: #{err}|]
          throwIO $ LigoFormatFailedException (pack err) fp
        Right formatted -> pure formatted
    Left LigoClientFailureException {cfeStderr} ->
      handleLigoError fp (fixMarkers cfeStderr)
  where
    fp :: FilePath
    fp = srcPath source

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
  :: (HasLigoClient m, Log m)
  => FilePath
  -> Source
  -> m Source
preprocess projDir contract = Log.addNamespace "preprocess" $ Log.addContext contract do
  $(Log.debug) [i|preprocessing the following contract:\n #{contract}|]
  (mbOut, fixMarkers) <- usingTemporaryDir projDir contract \tempFp _ -> do
    try $ callLigo (Just projDir)
      ["print", "preprocessed", tempFp, "--lib", dir, "--format", "json"]
      contract
  case fixMarkers <$> mbOut of
    Right output ->
      case eitherDecodeStrict' @Text . encodeUtf8 $ output of
        Left err -> do
          $(Log.err) [i|Unable to preprocess contract with: #{err}|]
          throwIO $ LigoPreprocessFailedException (pack err) fp
        Right newContract -> pure $ Source fp newContract
    Left LigoClientFailureException {cfeStderr} ->
      handleLigoError fp (fixMarkers cfeStderr)
  where
    fp, dir :: FilePath
    fp = srcPath contract
    dir = takeDirectory fp

-- | Get ligo definitions from raw contract.
getLigoDefinitions
  :: (HasLigoClient m, Log m)
  => FilePath
  -> Source
  -> m LigoDefinitions
getLigoDefinitions projDir contract = Log.addNamespace "getLigoDefinitions" $ Log.addContext contract do
  $(Log.debug) [i|parsing the following contract:\n#{contract}|]
  (mbOut, fixMarkers) <- usingTemporaryDir projDir contract \tempFp _ ->
    try $ callLigo (Just projDir)
      ["info", "get-scope", tempFp, "--format", "json", "--with-types", "--lib", dir]
      contract
  case fixMarkers <$> mbOut of
    Right output ->
      case eitherDecodeStrict' @LigoDefinitions . encodeUtf8 $ output of
        Left err -> do
          $(Log.err) [i|Unable to parse ligo definitions with: #{err}|]
          throwIO $ LigoDefinitionParseErrorException (pack err) output fp
        Right definitions -> pure definitions
    Left LigoClientFailureException {cfeStderr} ->
      handleLigoMessages fp (fixMarkers cfeStderr)
  where
    fp, dir :: FilePath
    fp = srcPath contract
    dir = takeDirectory fp

-- | A middleware for processing `ExpectedClientFailure` error needed to pass it
-- multiple levels up allowing us from restoring from expected ligo errors.
handleLigoError :: (HasLigoClient m, Log m) => FilePath -> Text -> m a
handleLigoError path stderr = Log.addNamespace "handleLigoError" do
  case eitherDecodeStrict' @LigoError . encodeUtf8 $ stderr of
    Left err -> do
      let failureRecovery = attemptToRecoverFromPossibleLigoCrash err $ unpack stderr
      case failureRecovery of
        Left failure -> do
          $(Log.err) [i|ligo error decoding failure: #{failure}|]
          throwIO $ LigoErrorNodeParseErrorException (pack failure) stderr path
        Right recovered -> do
          -- LIGO doesn't dump any information we can extract to figure out
          -- where this error occurred, so we just log it for now. E.g.: a
          -- type-checker error just crashes with "Update an expression which is not a record"
          -- in the old typer. In the new typer, the error is the less
          -- intuitive "type error : break_ctor propagator".
          $(Log.err) [i|ligo crashed: #{recovered}|]
          throwIO $ LigoUnexpectedCrashException (pack recovered) path
    Right decodedError -> do
      $(Log.err) [i|ligo error decoding successful with:\n#{decodedError}|]
      throwIO $ LigoDecodedExpectedClientFailureException (pure decodedError) [] path

-- | Like 'handleLigoError', but used for the case when multiple LIGO errors may
-- happen. On a decode failure, attempts to decode as a single LIGO error
-- instead.
handleLigoMessages :: (HasLigoClient m, Log m) => FilePath -> Text -> m a
handleLigoMessages path stderr = Log.addNamespace "handleLigoErrors" do
  case eitherDecodeStrict' @LigoMessages $ encodeUtf8 stderr of
    Left err -> do
      $(Log.err) [i|ligo errors decoding failure: #{err}|]
      -- It's possible it's the old format, with only one error. Try to decode
      -- it instead:
      handleLigoError path stderr
    Right (LigoMessages decodedErrors decodedWarnings) -> do
      $(Log.err) [i|ligo errors decoding successful with:\n#{toList decodedErrors <> decodedWarnings}|]
      throwIO $ LigoDecodedExpectedClientFailureException decodedErrors decodedWarnings path

-- | When LIGO fails to e.g. typecheck, it crashes. This function attempts to
-- extract the error message that was included with the crash.
-- Returns 'Left' if we failed to decode with the first parameter, otherwise
-- returns 'Right' with the recovered crash message.
attemptToRecoverFromPossibleLigoCrash :: String -> String -> Either String String
attemptToRecoverFromPossibleLigoCrash errDecoded stdErr = case getAllTextSubmatches (stdErr =~ regex) of
  [_, err] -> Right err
  _        -> Left errDecoded
  where
    regex :: String
    regex = "Fatal error: exception \\(Failure \"(.*)\"\\)"
