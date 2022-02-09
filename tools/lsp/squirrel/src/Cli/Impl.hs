{-# LANGUAGE DeriveGeneric, DerivingVia, RecordWildCards #-}

-- | Module that handles ligo binary execution.
module Cli.Impl
  ( SomeLigoException (..)
  , LigoErrorNodeParseErrorException  (..)
  , LigoExpectedClientFailureException (..)
  , LigoDecodedExpectedClientFailureException (..)
  , LigoUnexpectedCrashException (..)

  , Version (..)

  , callLigo
  , getLigoVersion
  , preprocess
  , getLigoDefinitions
  ) where

import Control.Exception.Safe (Exception (..), SomeException (..), try, tryAny)
import Control.Monad
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader
import Data.Aeson (ToJSON (..), eitherDecodeStrict', object, (.=))
import Data.Bifunctor (bimap)
import Data.Foldable (asum)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as Text
import Data.Typeable (cast)
import Duplo.Pretty (PP (PP), Pretty (..), text, (<+>), (<.>))
import Katip (LogItem (..), PayloadSelection (AllKeys), ToObject)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, hFlush)
import System.IO.Temp (withSystemTempFile)
import System.Process
import Text.Regex.TDFA ((=~), getAllTextSubmatches)

import Cli.Json
import Cli.Types
import Log (Log, i)
import Log qualified
import ParseTree (Source (..), srcToText)

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

class Exception a => LigoException a where

-- | ligo call unexpectedly failed (returned non-zero exit code)
data LigoUnexpectedClientFailureException = LigoUnexpectedClientFailureException
  { ucfeExitCode :: Int  -- ^ Exit code
  , ucfeStdout   :: Text -- ^ stdout
  , ucfeStderr   :: Text -- ^ stderr
  , ucfeFile     :: Maybe FilePath  -- ^ File that caused the error
  } deriving anyclass (Exception, LigoException)
    deriving Show via (PP LigoUnexpectedClientFailureException)

-- | Catch expected ligo failure to be able to restore from it
data LigoExpectedClientFailureException = LigoExpectedClientFailureException
  { ecfeStdout :: Text -- ^ stdout
  , ecfeStderr :: Text -- ^ stderr
  , ecfeFile   :: Maybe FilePath  -- ^ File that caused the error
  } deriving anyclass (Exception, LigoException)
    deriving Show via (PP LigoExpectedClientFailureException)

-- | Expected ligo failure decoded from its JSON output
data LigoDecodedExpectedClientFailureException = LigoDecodedExpectedClientFailureException
  { decfeErrorDecoded :: LigoError -- ^ Successfully decoded ligo error
  , decfeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (Exception, LigoException)
    deriving Show via (PP LigoDecodedExpectedClientFailureException)

-- | Ligo has unexpectedly crashed.
data LigoUnexpectedCrashException = LigoUnexpectedCrashException
  { uceMessage :: Text -- ^ extracted failure message
  , uceFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (Exception, LigoException)
    deriving Show via (PP LigoUnexpectedCrashException)

data LigoPreprocessFailedException = LigoPreprocessFailedException
  { pfeMessage :: Text -- ^ Successfully decoded ligo error
  , pfeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (Exception, LigoException)
    deriving Show via (PP LigoPreprocessFailedException)

----------------------------------------------------------------------------
-- Errors that may fail due to changes in ligo compiler

-- | Parse error occured during ligo output JSON decoding.
data LigoErrorNodeParseErrorException = LigoErrorNodeParseErrorException
  { lnpeError :: Text -- ^ Error description
  , lnpeOutput :: Text -- ^ The JSON output which we failed to decode
  , lnpeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (Exception, LigoException)
    deriving Show via (PP LigoErrorNodeParseErrorException)

-- | Parse error occured during ligo stderr JSON decoding.
data LigoDefinitionParseErrorException = LigoDefinitionParseErrorException
  { ldpeError :: Text -- ^ Error description
  , ldpeOutput :: Text -- ^ The JSON output which we failed to decode
  , ldpeFile :: FilePath -- ^ File that caused the error
  } deriving anyclass (Exception, LigoException)
    deriving Show via (PP LigoDefinitionParseErrorException)

data SomeLigoException
  = forall a . (LigoException a, Pretty a) => SomeLigoException a
  deriving Show via (PP SomeLigoException)

instance Exception SomeLigoException where
  displayException = show . pp
  fromException e =
    asum
      [ SomeLigoException <$> fromException @LigoUnexpectedClientFailureException      e
      , SomeLigoException <$> fromException @LigoExpectedClientFailureException        e
      , SomeLigoException <$> fromException @LigoDecodedExpectedClientFailureException e
      , SomeLigoException <$> fromException @LigoErrorNodeParseErrorException          e
      , SomeLigoException <$> fromException @LigoDefinitionParseErrorException         e
      , SomeLigoException <$> fromException @LigoPreprocessFailedException             e
      ]

----------------------------------------------------------------------------
-- Pretty
----------------------------------------------------------------------------

instance Pretty LigoUnexpectedClientFailureException where
  pp LigoUnexpectedClientFailureException {ucfeExitCode, ucfeStdout, ucfeStderr, ucfeFile} =
      "ligo binary unexpectedly failed with error code" <+> pp ucfeExitCode
        <+> ".\nStdout:\n" <.> pp ucfeStdout
        <.> "\nStderr:\n" <.> pp ucfeStderr
        <.> "\nCaused by:" <+> pp (pack <$> ucfeFile)

instance Pretty LigoExpectedClientFailureException where
  pp LigoExpectedClientFailureException {ecfeStdout, ecfeStderr, ecfeFile} =
      "ligo binary failed as expected with\nStdout:\n" <.> pp ecfeStdout
      <.> "\nStderr:\n" <.> pp ecfeStderr
      <.> "\nCaused by:" <+> pp (pack <$> ecfeFile)

instance Pretty LigoDecodedExpectedClientFailureException where
  pp LigoDecodedExpectedClientFailureException {decfeErrorDecoded, decfeFile} =
      "ligo binary produced expected error which we successfully decoded as:\n"
      <.> text (show decfeErrorDecoded)
      <.> "\nCaused by:" <+> pp (pack decfeFile)

instance Pretty LigoErrorNodeParseErrorException where
  pp LigoErrorNodeParseErrorException {lnpeError, lnpeOutput, lnpeFile} =
      "ligo binary produced error JSON which we consider malformed:\n"
      <.> pp lnpeError
      <.> "\nCaused by:" <+> pp (pack lnpeFile)
      <.> "\nJSON output dumped:\n"
      <.> pp lnpeOutput

instance Pretty LigoDefinitionParseErrorException where
  pp LigoDefinitionParseErrorException {ldpeError, ldpeOutput, ldpeFile} =
      "ligo binary produced a definition output which we consider malformed:\n"
      <.> pp ldpeError
      <.> "\nCaused by:" <+> pp (pack ldpeFile)
      <.> "\nJSON output dumped:\n"
      <.> pp ldpeOutput

instance Pretty LigoUnexpectedCrashException where
  pp LigoUnexpectedCrashException {uceMessage, uceFile} =
      "ligo binary crashed with error: " <+> pp uceMessage
      <.> "\nCaused by:" <+> pp (pack uceFile)

instance Pretty SomeLigoException where
  pp (SomeLigoException a) =
    "Error (ligo):" <+> pp a

instance Pretty LigoPreprocessFailedException where
  pp LigoPreprocessFailedException {pfeMessage, pfeFile} =
    "ligo failed to preprocess contract with\n:" <.> pp pfeMessage
    <.> "\nCaused by:" <+> pp (pack pfeFile)

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
      throwM $ LigoExpectedClientFailureException (pack lo) (pack le) fpM
    unless (le == mempty) $
      let ec' = case ec of { ExitSuccess -> 0; ExitFailure n -> n } in
      throwM $ LigoUnexpectedClientFailureException ec' (pack lo) (pack le) fpM
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

usingTemporaryDir :: MonadIO m => Source -> (FilePath -> Handle -> IO a) -> m a
usingTemporaryDir src action =
  liftIO $ withSystemTempFile (takeFileName $ srcPath src) \tempFp handle -> do
    contents <- srcToText src
    Text.hPutStr handle contents
    hFlush handle
    action tempFp handle

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
          throwM $ LigoPreprocessFailedException (pack err) fp
        Right newContract -> pure $ (, errs) case contract of
          Path       path   -> Text path newContract
          Text       path _ -> Text path newContract
          ByteString path _ -> ByteString path $ encodeUtf8 newContract
    Left LigoExpectedClientFailureException {ecfeStderr} ->
      handleLigoError fp ecfeStderr
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
    fmap (, tempFp) $ tryAny $ callLigo
      ["info", "get-scope", tempFp, "--format", "json", "--with-types", "--lib", dir]
      contract
  case join bimap (fixMarkers tempFp fp) <$> mbOut of
    Right (output, errs) ->
      case eitherDecodeStrict' @LigoDefinitions . encodeUtf8 $ output of
        Left err -> do
          $(Log.err) [i|Unable to parse ligo definitions with: #{err}|]
          throwM $ LigoDefinitionParseErrorException (pack err) output path
        Right definitions -> return (definitions, errs)
    Left (SomeException e) -> case cast e of
      Just LigoExpectedClientFailureException {ecfeStderr} ->
        handleLigoError path ecfeStderr
      Nothing -> case cast e of
        Just LigoUnexpectedClientFailureException {ucfeStderr} ->
          handleLigoError path ucfeStderr
        Nothing -> throwM e
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
      -- LIGO dumps its crash information on StdOut rather than StdErr.
      let failureRecovery = attemptToRecoverFromPossibleLigoCrash err $ unpack stderr
      case failureRecovery of
        Left failure -> do
          $(Log.err) [i|ligo error decoding failure: #{failure}|]
          throwM $ LigoErrorNodeParseErrorException (pack failure) stderr path
        Right recovered -> do
          -- LIGO doesn't dump any information we can extract to figure out
          -- where this error occurred, so we just log it for now. E.g.: a
          -- type-checker error just crashes with "Update an expression which is not a record"
          -- in the old typer. In the new typer, the error is the less
          -- intuitive "type error : break_ctor propagator".
          $(Log.err) [i|ligo crashed: #{recovered}|]
          throwM $ LigoUnexpectedCrashException (pack recovered) path
    Right decodedError -> do
      $(Log.err) [i|ligo error decoding successful with:\n#{decodedError}|]
      throwM $ LigoDecodedExpectedClientFailureException decodedError path

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
