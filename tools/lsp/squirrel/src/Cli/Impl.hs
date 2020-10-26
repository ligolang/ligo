{-# LANGUAGE DeriveGeneric, DerivingVia, RecordWildCards #-}

-- | Module that handles ligo binary execution.
module Cli.Impl
  ( LigoBinaryCallError(..)
  , callLigo
  , getLigoDefinitions
  , parseLigoDefinitions
  , parseLigoOutput
  , getLigoDefinitionsFrom
  , runLigoClient
  ) where

import Control.Monad.Catch (Exception (..), MonadThrow (throwM))
import Control.Exception (throwIO, IOException, try, catch)
import Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString.Lazy.Char8 as S8L
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as S
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Duplo.Pretty (PP (PP), Pretty (..), text, (<+>), (<.>))
import System.Exit (ExitCode (..))
import System.Process 
import Data.Aeson.Types (FromJSON)
import Control.Monad
import Control.Monad.Reader

import Cli.Json
import Cli.Types
import qualified Log
import Product 
import Log (i)

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

data LigoBinaryCallError
  = -- | ligo call unexpectedly failed (returned non-zero exit code).
    -- The error contains the error code, stdout and stderr contents.
    UnexpectedClientFailure
      Int -- ^ Exit code
      Text -- ^ stdout
      Text -- ^ stderr

  | -- | Catch expected ligo failure to be able to restore from it.
    ExpectedClientFailure
      Text -- ^ stdout 
      Text -- ^ stderr

  | -- | Expected ligo failure decoded from its JSON output.
    DecodedExpectedClientFailure
      LigoError -- ^ decoded JSON stderr

  -- Below are the errors which may fail due to some changes in ligo compiller.

  --   -- | Ligo compiller produced a type which we consider is malformed
  --   MalformedType
  --     Text
  | -- | Parse error occured during ligo output JSON decoding.
    DefinitionParseError
      Text
  | -- | Parse error occured during ligo stderr JSON decoding.
    LigoErrorNodeParseError
      Text 
  deriving (Show) via PP LigoBinaryCallError

instance Exception LigoBinaryCallError where
  displayException = show . pp

instance Pretty LigoBinaryCallError where
  pp = \case
    UnexpectedClientFailure errCode output errOutput ->
      "ligo binary unexpectedly failed with error code" <+> pp errCode
        <+> ".\nStdout:\n" <.> pp output <.> "\nStderr:\n" <.> pp errOutput
    ExpectedClientFailure output errOutput ->
      "ligo binary failed as expected with\nStdout:\n" <.> pp output 
      <.> "\nStderr:\n" <.> pp errOutput 
    DecodedExpectedClientFailure err -> 
      "ligo binary produced expected error which we successfully decoded as:\n" <.> text (show err) 
    LigoErrorNodeParseError err -> 
      "ligo binary produced error JSON which we consider malformed:\n" <.> pp err
    DefinitionParseError err ->
      "ligo binary produced output which we consider malformed:\n" <.> pp err

----------------------------------------------------------------------------
-- Execution
----------------------------------------------------------------------------

runLigoClient :: r -> ReaderT r m a -> m a
runLigoClient = flip runReaderT

-- | Call ligo binary and return stdin and stderr accordingly.
callLigo 
  :: HasLigoClient m => [String] -> m (Text, Text)
callLigo args = do 
  LigoClientEnv {..} <- getLigoClientEnv
  liftIO $ readProcessWithExitCode' _lceClientPath args "" >>= \case
    (ExitSuccess, output, errOutput) -> pure (pack output, pack errOutput)
    (ExitFailure errCode, pack -> output, pack -> errOutput) ->
      throwM $ UnexpectedClientFailure errCode output errOutput

-- | Call ligo binary and pass raw contract to its stdin and return 
-- stdin and stderr accordingly.
callLigoWith
  :: HasLigoClient m => [String] -> RawContractCode -> m (Text, Text)
callLigoWith args (RawContractCode con) = do
  env@LigoClientEnv {..} <- getLigoClientEnv
  liftIO $ do 
    Log.debug "LIGO" [i|Running ligo on #{env} with #{args}|]
    (Just ligoIn, Just ligoOut, Just ligoErr, ligoProc) <- 
      createProcess (proc _lceClientPath args) 
        { std_out = CreatePipe
        , std_in  = CreatePipe 
        , std_err = CreatePipe
        }
    S.hPut ligoIn $ S8L.toStrict con
    res <- S.hGetContents ligoOut 
    le <- S.hGetContents ligoErr
    ec <- waitForProcess ligoProc
    unless (ec == ExitSuccess) $ do 
      throwM $ ExpectedClientFailure (decodeUtf8 res) (decodeUtf8 le)
    unless (le == mempty) $ do
      throwM $ UnexpectedClientFailure 0 (decodeUtf8 res) (decodeUtf8 le) 
    Log.debug "LIGO" [i|Successfully exited with stdout:\n#{S8.unpack res}\nand stderr:\n#{S8.unpack le}|]
    return (decodeUtf8 res, decodeUtf8 le)

-- | Variant of @readProcessWithExitCode@ that prints a better error in case of
-- an exception in the inner @readProcessWithExitCode@ call.
readProcessWithExitCode'
  :: FilePath
  -> [String]
  -> String
  -> IO (ExitCode, String, String)
readProcessWithExitCode' fp args inp =
    readProcessWithExitCode fp args inp `catchAny` handler
  where
    handler :: SomeException -> IO (ExitCode, String, String)
    handler e = do
      Log.err "CLI" errorMsg
      throwIO e

    errorMsg =
      mconcat
        [ "ERROR!! There was an error in executing `"
        , show fp
        , "` program. Is the executable available in PATH ?"
        ]

----------------------------------------------------------------------------
-- Execution
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- Parse from output file

-- | Parse ligo definitions from ligo output file generated by
-- ```
-- ligo get-scope ${contract_code} --format=json --with-types
-- ```
-- and return a hashmap of scope name and its values.
parseLigoDefinitions
  :: HasLigoClient m
  => FilePath
  -> m LigoDefinitions
parseLigoDefinitions contractPath = do
  output <- liftIO $ S8L.readFile contractPath
  case eitherDecodeStrict' . encodeUtf8 . pack . S8L.unpack $ output of
    Left err -> throwM $ DefinitionParseError (pack err)
    Right definitions -> return definitions

-- | Helper function used for parsing parts of ligo JSON output.
parseLigoOutput
  :: forall a . FromJSON a => FilePath -> IO a
parseLigoOutput contractPath = do
  output <- S8L.readFile contractPath
  case eitherDecodeStrict' @a . encodeUtf8 . pack . S8L.unpack $ output of
    Left err -> throwM $ DefinitionParseError (pack err)
    Right definitions -> return definitions

-- -- | Extract types from a ligo scope resolution file generated by
-- -- ```
-- -- ligo get-scope contract --format=json --with-types
-- -- ```
-- parseLigoTypesFor
--   :: FilePath -- ^ Ligo output file path
--   -> Text -- ^ Declaration name
--   -> IO [(Text, LigoTypeFull)]
-- parseLigoTypesFor contractPath name = do
--   output <- C8.readFile contractPath
--   case eitherDecodeStrict' @Value . encodeUtf8 . pack . C8.unpack $ output of
--     Left err -> throwM $ ScopeParseError (pack err)
--     Right scopes -> do
--       let variables = scopes ^? key "definitions" . key "variables"
--       case variables of
--         Nothing -> throwM $ VariableExtractError scopes
--         Just variables' -> return $ extractLigoTypesFrom name variables'

----------------------------------------------------------------------------
-- Execute ligo binary itself

-- | Get ligo definitions from a contract by calling ligo binary.
getLigoDefinitionsFrom 
  :: HasLigoClient m 
  => FilePath
  -> m (LigoDefinitions, Text)
getLigoDefinitionsFrom contractPath = do 
  contents <- liftIO $ S8L.readFile contractPath 
  getLigoDefinitions $ RawContractCode contents

-- | Get ligo definitions from raw contract.
getLigoDefinitions
  :: HasLigoClient m
  => RawContractCode
  -> m (LigoDefinitions, Text)
getLigoDefinitions contract = do
  env <- getLigoClientEnv
  Log.debug "LIGO.PARSE" [i|parsing the following contract:\n #{contract}|]
  mbOut <- liftIO . try @LigoBinaryCallError . runLigoClient (env :> Nil) $ 
    callLigoWith ["get-scope", "--format=json", "--with-types", "--syntax=pascaligo", "/dev/stdin"] contract
  case mbOut of 
    Right (output, errs) -> do
      Log.debug "LIGO.PARSE" [i|Successfully called ligo with #{output}|]
      case eitherDecodeStrict' @LigoDefinitions . encodeUtf8 $ output of
        Left err -> do 
          Log.debug "LIGO.PARSE" [i|Unable to parse ligo definitions with: #{err}|]
          throwM $ DefinitionParseError (pack err)
        Right definitions -> return (definitions, errs)

    -- A middleware for processing `ExpectedClientFailure` error needed to pass it multiple levels up 
    -- allowing us from restoring from expected ligo errors.
    Left (ExpectedClientFailure _ ligoStdErr) -> do
      -- otherwise call ligo with `compile-contract` to extract more readable error message
      Log.debug "LIGO.PARSE" [i|decoding ligo error|] 
      case eitherDecodeStrict' @LigoError . encodeUtf8 $ ligoStdErr of 
        Left err -> do 
          Log.debug "LIGO.PARSE" [i|ligo error decoding failure #{err}|]
          throwM $ LigoErrorNodeParseError (pack err)
        Right decodedError -> do 
          Log.debug "LIGO.PARSE" [i|ligo error decoding successfull with:\n#{decodedError}|]
          throwM $ DecodedExpectedClientFailure decodedError 

    -- All other errors remain untouched
    Left err -> throwM err 

-- | Extract a list of types in scopes from aeson @Value@ for some specific declaration under "name" field.
-- extractLigoTypesFrom :: Text -> Value -> [(Text, LigoTypeFull)]
-- extractLigoTypesFrom name context =
--   let current =
--         context
--           ^@.. members
--             <. filteredBy
--               (key "name"
--                  . _String
--                  . filtered (== name))
--               . key "t"
--               . (_JSON :: Prism' Value LigoTypeFull)
--    in -- TODO: needs research on nested scopes, currently we think that the list is
--       -- flat, but if it's not, you can simply uncomment code below
--       -- deeper =
--       --   context
--       --     ^. members
--       --       . members
--       --       . key "t"
--       --       . to (f name)
--       current -- <> deeper
