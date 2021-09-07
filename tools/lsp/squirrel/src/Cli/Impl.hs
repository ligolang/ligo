{-# LANGUAGE DeriveGeneric, DerivingVia, RecordWildCards #-}

-- | Module that handles ligo binary execution.
module Cli.Impl
  ( SomeLigoException (..)
  , LigoExpectedClientFailureException (..)
  , LigoDecodedExpectedClientFailureException (..)
  , LigoUnexpectedCrashException (..)
  , callLigo
  , getLigoDefinitions
  , parseLigoDefinitions
  , parseLigoOutput
  , getLigoDefinitionsFrom
  ) where

import Control.Exception.Safe (Exception (..), try)
import Control.Monad
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Reader
import Data.Aeson (eitherDecodeStrict')
import Data.Aeson.Types (FromJSON)
import Data.ByteString.Lazy.Char8 qualified as S8L
import Data.Foldable (asum)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Duplo.Pretty (PP (PP), Pretty (..), text, (<+>), (<.>))
import System.Exit (ExitCode (..))
import System.Process
import Text.Regex.TDFA ((=~), getAllTextSubmatches)

import Cli.Json
import Cli.Types
import Extension (Lang (..), getExt)
import Log (i)
import Log qualified
import ParseTree (Source (..), srcToText)

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

class Exception a => LigoException a where

-- | ligo call unexpectedly failed (returned non-zero exit code)
data LigoUnexpectedClientFailureException
  = LigoUnexpectedClientFailureException
  { ucfeExitCode :: Int  -- ^ Exit code
  , ucfeStdout   :: Text -- ^ stdout
  , ucfeStderr   :: Text -- ^ stderr
  } deriving anyclass (Exception, LigoException)
    deriving Show via (PP LigoUnexpectedClientFailureException)

-- | Catch expected ligo failure to be able to restore from it
data LigoExpectedClientFailureException
  = LigoExpectedClientFailureException
  { ecfeStdout :: Text -- ^ stdout
  , ecfeStderr :: Text -- ^ stderr
  } deriving anyclass (Exception, LigoException)
    deriving Show via (PP LigoExpectedClientFailureException)

-- | Expected ligo failure decoded from its JSON output
newtype LigoDecodedExpectedClientFailureException
  = LigoDecodedExpectedClientFailureException
  { decfeErrorDecoded :: LigoError -- ^ Successfully decoded ligo error
  } deriving anyclass (Exception, LigoException)
    deriving Show via (PP LigoDecodedExpectedClientFailureException)

-- | Ligo has unexpectedly crashed.
newtype LigoUnexpectedCrashException
  = LigoUnexpectedCrashException
    { uceMessage :: Text -- ^ extracted failure message
  } deriving anyclass (Exception, LigoException)
    deriving Show via (PP LigoUnexpectedCrashException)

----------------------------------------------------------------------------
-- Errors that may fail due to changes in ligo compiler

-- | Parse error occured during ligo output JSON decoding.
newtype LigoErrorNodeParseErrorException
  = LigoErrorNodeParseErrorException
  { lnpeError :: Text -- ^ Error description
  } deriving anyclass (Exception, LigoException)
    deriving Show via (PP LigoErrorNodeParseErrorException)

-- | Parse error occured during ligo stderr JSON decoding.
newtype LigoDefinitionParseErrorException
  = LigoDefinitionParseErrorException
  { ldpeError :: Text -- ^ Error description
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
      ]

----------------------------------------------------------------------------
-- Pretty
----------------------------------------------------------------------------

instance Pretty LigoUnexpectedClientFailureException where
  pp LigoUnexpectedClientFailureException {ucfeExitCode, ucfeStdout, ucfeStderr} =
      "ligo binary unexpectedly failed with error code" <+> pp ucfeExitCode
        <+> ".\nStdout:\n" <.> pp ucfeStdout <.> "\nStderr:\n" <.> pp ucfeStderr

instance Pretty LigoExpectedClientFailureException where
  pp LigoExpectedClientFailureException {ecfeStdout, ecfeStderr} =
      "ligo binary failed as expected with\nStdout:\n" <.> pp ecfeStdout
      <.> "\nStderr:\n" <.> pp ecfeStderr

instance Pretty LigoDecodedExpectedClientFailureException where
  pp LigoDecodedExpectedClientFailureException {decfeErrorDecoded} =
      "ligo binary produced expected error which we successfully decoded as:\n"
      <.> text (show decfeErrorDecoded)

instance Pretty LigoErrorNodeParseErrorException where
  pp LigoErrorNodeParseErrorException {lnpeError} =
      "ligo binary produced error JSON which we consider malformed:\n"
      <.> pp lnpeError <.> "[end]"

instance Pretty LigoDefinitionParseErrorException where
  pp LigoDefinitionParseErrorException {ldpeError} =
      "ligo binary produced output which we consider malformed:\n"
      <.> pp ldpeError

instance Pretty LigoUnexpectedCrashException where
  pp LigoUnexpectedCrashException {uceMessage} =
      "ligo binary crashed with error: " <+> pp uceMessage

instance Pretty SomeLigoException where
  pp (SomeLigoException a) =
    "Error (ligo):" <+> pp a

----------------------------------------------------------------------------
-- Execution
----------------------------------------------------------------------------

-- | Call ligo binary and return stdin and stderr accordingly.
callLigo
  :: HasLigoClient m => [String] -> Source -> m (Text, Text)
callLigo args con = do
  LigoClientEnv {..} <- getLigoClientEnv
  liftIO $ do
    raw <- srcToText con
    (ec, lo, le) <- readProcessWithExitCode _lceClientPath args (unpack raw)
    unless (ec == ExitSuccess && le == mempty) $ -- TODO: separate JSON errors and other ones
      throwM $ LigoExpectedClientFailureException (pack lo) (pack le)
    unless (le == mempty) $
      throwM $ LigoUnexpectedClientFailureException 0 (pack lo) (pack le)
    return (pack lo, pack le)

----------------------------------------------------------------------------
-- Parse from output file

-- | Parse ligo definitions from ligo output file generated by
-- ```
-- ligo get-scope `cat ${contract_path}` --format=json --with-types
-- ```
-- and return a hashmap of scope name and its values.
parseLigoDefinitions
  :: HasLigoClient m
  => FilePath
  -> m LigoDefinitions
parseLigoDefinitions contractPath = do
  output <- liftIO $ S8L.readFile contractPath
  case eitherDecodeStrict' . encodeUtf8 . pack . S8L.unpack $ output of
    Left err -> throwM $ LigoDefinitionParseErrorException (pack err)
    Right definitions -> return definitions

-- | Helper function used for parsing parts of ligo JSON output.
parseLigoOutput
  :: forall a . FromJSON a => FilePath -> IO a
parseLigoOutput contractPath = do
  output <- S8L.readFile contractPath
  case eitherDecodeStrict' @a . encodeUtf8 . pack . S8L.unpack $ output of
    Left err -> throwM $ LigoDefinitionParseErrorException (pack err)
    Right definitions -> return definitions

----------------------------------------------------------------------------
-- Execute ligo binary itself

-- | Get ligo definitions from a contract by calling ligo binary.
getLigoDefinitionsFrom
  :: HasLigoClient m
  => FilePath
  -> m (LigoDefinitions, Text)
getLigoDefinitionsFrom contractPath = do
  contents <- liftIO $ S8L.readFile contractPath
  getLigoDefinitions $ ByteString contractPath (S8L.toStrict contents)

-- | Get ligo definitions from raw contract.
getLigoDefinitions
  :: HasLigoClient m
  => Source
  -> m (LigoDefinitions, Text)
getLigoDefinitions contract = do
  Log.debug "LIGO.PARSE" [i|parsing the following contract:\n #{contract}|]
  ext <- getExt (srcPath contract)
  let
    syntax = case ext of
      Reason -> "reasonligo"
      Pascal -> "pascaligo"
      Caml   -> "cameligo"
  mbOut <- try $
    -- TODO: Use --typer=new, but currently it displays a lot of logging
    -- information together with the JSON which makes it difficult to reason
    -- about. It displays more errors than --typer=old (default).
    callLigo ["get-scope", "--format=json", "--with-types", "--syntax=" <> syntax, srcPath contract] contract
  case mbOut of
    Right (output, errs) ->
      --Log.debug "LIGO.PARSE" [i|Successfully called ligo with #{output}|]
      case eitherDecodeStrict' @LigoDefinitions . encodeUtf8 $ output of
        Left err -> do
          Log.debug "LIGO.PARSE" [i|Unable to parse ligo definitions with: #{err}|]
          throwM $ LigoDefinitionParseErrorException (pack err)
        Right definitions -> return (definitions, errs)

    -- A middleware for processing `ExpectedClientFailure` error needed to pass it multiple levels up
    -- allowing us from restoring from expected ligo errors.
    Left LigoExpectedClientFailureException {ecfeStdout, ecfeStderr} -> do
      -- otherwise call ligo with `compile contract` to extract more readable error message
      Log.debug "LIGO.PARSE" [i|decoding ligo error|]
      case eitherDecodeStrict' @LigoError . encodeUtf8 $ ecfeStderr of
        Left err -> do
          -- LIGO dumps its crash information on StdOut rather than StdErr.
          let failureRecovery = attemptToRecoverFromPossibleLigoCrash err $ unpack ecfeStdout
          case failureRecovery of
            Left failure -> do
              Log.debug "LIGO.PARSE" [i|ligo error decoding failure: #{failure}|]
              throwM $ LigoErrorNodeParseErrorException $ pack failure
            Right recovered -> do
              -- LIGO doesn't dump any information we can extract to figure out
              -- where this error occurred, so we just log it for now. E.g.: a
              -- type-checker error just crashes with "Update an expression which is not a record"
              -- in the old typer. In the new typer, the error is the less
              -- intuitive "type error : break_ctor propagator".
              Log.debug "LIGO.PARSE" [i|ligo crashed with: #{recovered}|]
              throwM $ LigoUnexpectedCrashException $ pack recovered
        Right decodedError -> do
          Log.debug "LIGO.PARSE" [i|ligo error decoding successful with:\n#{decodedError}|]
          throwM $ LigoDecodedExpectedClientFailureException decodedError

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
    regex = "ligo: internal error, uncaught exception:\n      \\(Failure \"(.*)\"\\)"
