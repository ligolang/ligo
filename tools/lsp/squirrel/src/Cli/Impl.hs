{-# LANGUAGE DeriveGeneric, DerivingVia, RecordWildCards #-}

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
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader
import Data.Aeson (ToJSON (..), eitherDecodeStrict', object, (.=))
import Data.Bifunctor (bimap)
import Data.Foldable (asum)
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
import System.IO.Temp (withSystemTempFile)
import System.Process
import Text.Regex.TDFA ((=~), getAllTextSubmatches)
import UnliftIO.Exception (Exception (..), SomeException (..), catchAny, throwIO, try)

import Cli.Json
import Cli.Types
import Log (Log, i)
import Log qualified
import ParseTree (Source (..), srcToText)

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
  { decfeErrorDecoded :: LigoError -- ^ Successfully decoded ligo error
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
      ]

instance Exception LigoClientFailureException where
  displayException LigoClientFailureException {cfeStdout, cfeStderr, cfeFile} =
    "LIGO binary failed with\nStdout:\n" <> unpack cfeStdout
    <> "\nStderr:\n" <> unpack cfeStderr
    <> "\nCaused by: " <> fromMaybe "<unknown file>" cfeFile

instance Exception LigoDecodedExpectedClientFailureException where
  displayException LigoDecodedExpectedClientFailureException {decfeErrorDecoded, decfeFile} =
    "LIGO binary produced expected error which we successfully decoded as:\n"
    <> show (pp decfeErrorDecoded)
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

----------------------------------------------------------------------------
-- Execution
----------------------------------------------------------------------------

callLigoImpl :: HasLigoClient m => [String] -> Maybe Source -> m (Text, Text)
callLigoImpl args conM = do
  LigoClientEnv {..} <- getLigoClientEnv
  liftIO $ do
    raw <- maybe "" unpack <$> traverse srcToText conM
    let fpM = srcPath <$> conM
    (ec, lo, le) <- readProcessWithExitCode _lceClientPath args raw
    unless (ec == ExitSuccess && le == mempty) $ -- TODO: separate JSON errors and other ones
      throwIO $ LigoClientFailureException (pack lo) (pack le) fpM
    return (pack lo, pack le)

-- | Call ligo binary and return stdin and stderr accordingly.
callLigo :: HasLigoClient m => [String] -> Source -> m (Text, Text)
callLigo args = callLigoImpl args . Just

newtype Version = Version
  { getVersion :: Text
  }

instance ToJSON Version where
  toJSON (Version ver) = object ["version" .= ver]

deriving anyclass instance ToObject Version

instance LogItem Version where
  payloadKeys = const $ const AllKeys

usingTemporaryDir :: MonadUnliftIO m => Source -> (FilePath -> Handle -> m a) -> m a
usingTemporaryDir src action =
  withRunInIO \run -> withSystemTempFile (takeFileName $ srcPath src) \tempFp handle -> do
    contents <- srcToText src
    Text.hPutStr handle contents
    hFlush handle
    run $ action tempFp handle

fixMarkers :: FilePath -> FilePath -> Text -> Text
fixMarkers tempFp fp = Text.replace (pack tempFp) (pack fp)

----------------------------------------------------------------------------
-- Execute ligo binary itself

-- | Get the current LIGO version.
--
-- ```
-- ligo --version
-- ```
getLigoVersion :: (HasLigoClient m, Log m) => m (Maybe Version)
getLigoVersion = Log.addNamespace "getLigoVersion" do
  mbOut <- try $ callLigoImpl ["--version"] Nothing
  case mbOut of
    -- We don't want to die if we failed to parse the version...
    Left (SomeException e) -> do
      $(Log.err) [i|Couldn't get LIGO version with: #{e}|]
      pure Nothing
    Right (output, e) -> do
      unless (Text.null e) $
        $(Log.warning) [i|LIGO produced an error with the output: #{e}|]
      pure $ Just $ Version $ Text.strip output

-- | Call LIGO's pretty printer on some contract.
--
-- This function will call the contract with a temporary file path, dumping the
-- contents of the given source so LIGO reads the contents. This allows us to
-- call the pretty printer even if it's an unsaved LSP buffer.
--
-- ```
-- ligo print pretty ${temp_file_name}
-- ```
--
-- FIXME: LIGO expands preprocessor directives before pretty printing. We should
-- find a workaround for this or report to them.
callForFormat :: (HasLigoClient m, Log m) => Source -> m (Maybe Text)
callForFormat source = Log.addNamespace "callForFormat" $ Log.addContext source $
  usingTemporaryDir source \tempFp _ ->
    let
      getResult = callLigo
        ["print", "pretty", tempFp]
        source
    in
    (Just . fst <$> getResult) `catchAny` \err -> do
      $(Log.err) [i|Could not format document with error: #{err}|]
      pure Nothing

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
  => Source
  -> m (Source, Text)
preprocess contract = Log.addNamespace "preprocess" $ Log.addContext contract do
  $(Log.debug) [i|preprocessing the following contract:\n #{contract}|]
  (mbOut, tempFp) <- usingTemporaryDir contract \tempFp _ -> do
    mbOut <- try $ callLigo
      ["print", "preprocessed", tempFp, "--lib", dir, "--format", "json"]
      contract
    pure (mbOut, tempFp)
  case join bimap (fixMarkers tempFp fp) <$> mbOut of
    Right (output, errs) ->
      case eitherDecodeStrict' @Text . encodeUtf8 $ output of
        Left err -> do
          $(Log.err) [i|Unable to preprocess contract with: #{err}|]
          throwIO $ LigoPreprocessFailedException (pack err) fp
        Right newContract -> pure $ (, errs) case contract of
          Path       path   -> Text path newContract
          Text       path _ -> Text path newContract
          ByteString path _ -> ByteString path $ encodeUtf8 newContract
    Left LigoClientFailureException {cfeStderr} ->
      handleLigoError fp cfeStderr
  where
    fp, dir :: FilePath
    fp = srcPath contract
    dir = takeDirectory fp

-- | Get ligo definitions from raw contract.
getLigoDefinitions
  :: (HasLigoClient m, Log m)
  => Source
  -> m (LigoDefinitions, Text)
getLigoDefinitions contract = Log.addNamespace "getLigoDefinitions" $ Log.addContext contract do
  $(Log.debug) [i|parsing the following contract:\n#{contract}|]
  let path = srcPath contract
  (mbOut, tempFp) <- usingTemporaryDir contract \tempFp _ ->
    fmap (, tempFp) $ try $ callLigo
      ["info", "get-scope", tempFp, "--format", "json", "--with-types", "--lib", dir]
      contract
  case join bimap (fixMarkers tempFp fp) <$> mbOut of
    Right (output, errs) ->
      case eitherDecodeStrict' @LigoDefinitions . encodeUtf8 $ output of
        Left err -> do
          $(Log.err) [i|Unable to parse ligo definitions with: #{err}|]
          throwIO $ LigoDefinitionParseErrorException (pack err) output path
        Right definitions -> return (definitions, errs)
    Left LigoClientFailureException {cfeStderr} ->
      handleLigoError path cfeStderr
  where
    fp, dir :: FilePath
    fp = srcPath contract
    dir = takeDirectory fp

-- | A middleware for processing `ExpectedClientFailure` error needed to pass it
-- multiple levels up allowing us from restoring from expected ligo errors.
handleLigoError :: (HasLigoClient m, Log m) => FilePath -> Text -> m a
handleLigoError path stderr = Log.addNamespace "handleLigoError" do
  -- Call ligo with `compile contract` to extract more readable error message
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
      throwIO $ LigoDecodedExpectedClientFailureException decodedError path

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
