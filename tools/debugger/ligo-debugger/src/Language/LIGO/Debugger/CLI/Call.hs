module Language.LIGO.Debugger.CLI.Call
  ( Version (..)
  , checkCompilation
  , compileLigoContractDebug
  , compileLigoExpression
  , getAvailableModules
  , decompileLigoValues
  , resolveConfig
  , getScopes
  , getVersion
  , decodeCST
  ) where

import Control.Arrow ((>>>))
import Data.Aeson (Value, withObject, (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Coerce (coerce)
import Data.MessagePack (errorMessages, unpackEither)
import Data.Text qualified as T
import Fmt.Buildable (pretty)
import GHC.IO.Exception qualified as IOException
import System.Exit (ExitCode (ExitSuccess))
import System.Process (cwd, proc)
import System.Process.ByteString.Lazy qualified as PExtras
import Text.Interpolation.Nyan hiding (rmode')
import Util

import UnliftIO
  (Handler (Handler), MonadUnliftIO, catches, hFlush, handle, throwIO, withSystemTempFile)
import UnliftIO.Exception (fromEither)
import UnliftIO.Process (readCreateProcessWithExitCode)

import Morley.Michelson.Parser qualified as MP
import Morley.Michelson.Typed qualified as T
import Morley.Michelson.Untyped qualified as MU

import Language.LIGO.AST.Parser
import Language.LIGO.AST.Skeleton (Info, SomeLIGO)
import Language.LIGO.Debugger.CLI.Exception
import Language.LIGO.Debugger.CLI.Helpers
import Language.LIGO.Debugger.CLI.Types
import Language.LIGO.Debugger.CLI.Types.LigoValue
import Language.LIGO.Debugger.CLI.Types.LigoValue.Codegen
import Language.LIGO.Debugger.Error
import Language.LIGO.Debugger.Handlers.Types

----------------------------------------------------------------------------
-- Execution
----------------------------------------------------------------------------

-- | Output of @ligo --version@. Use @getVersion@ to obtain an instance of this data type.
newtype Version = Version {unVersion :: Text}
  deriving stock (Eq, Show)

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

-- | Wrap arbitrary @IOError@ into @LigoIOException@.
rewrapIOError :: MonadUnliftIO m => m a -> m a
rewrapIOError = UnliftIO.handle \exc@IOException.IOError{} ->
  UnliftIO.throwIO LigoIOException
    { lieType = IOException.ioe_type exc
    , lieDescription = toText $ IOException.ioe_description exc
    }

-- | Same as @callLigo@ but returns lazy bytes instead.
-- It's useful when the output could be large (e.g. @compileLigoContractDebug@).
callLigoBS :: HasLigoClient m => Maybe FilePath -> [LigoCliArg] -> m LByteString
callLigoBS _rootDir args = do
  LigoClientEnv {..} <- getLigoClientEnv
  liftIO $ do
    -- FIXME (LIGO-545): We should set the cwd to `rootDir`, but it seems there
    -- is a bug in the `process` library preventing us from doing it, as it
    -- causes non-determinism with handles receiving invalid arguments when
    -- running our tests with multiple threads.
    -- Additionally, using `withCurrentDirectory` from `directory` is no help
    -- either, as it doesn't seem thread-safe either.
    let process = proc _lceClientPath (coerce args)
    (ec, lo, le) <- PExtras.readCreateProcessWithExitCode process ""
      & rewrapIOError
    unless (ec == ExitSuccess && le == mempty) $ -- TODO: separate JSON errors and other ones
      UnliftIO.throwIO $ LigoClientFailureException (lazyBytesToText lo) (lazyBytesToText le) Nothing (Just ec)
    pure lo

-- | Calls the LIGO binary (extracted from 'getLigoClientEnv') with the provided
-- root directory (ignored for now), arguments, and contract.
--
-- If '_lceLigoProcesses' is 'Nothing', this function will make a blocking call
-- to LIGO, otherwise it will use an unused process handle from the process
-- pool.
callLigo :: HasLigoClient m => Maybe FilePath -> [LigoCliArg] -> m Text
callLigo rootDir args = do
  LigoClientEnv{..} <- getLigoClientEnv
  -- FIXME (LIGO-545): We should set the cwd to `rootDir`, but it seems there is
  -- a bug in the `process` library preventing us from doing it, as it causes
  -- non-determinism with handles receiving invalid arguments when running our
  -- tests with multiple threads. Additionally, using `withCurrentDirectory`
  -- from `directory` is no help either, as it doesn't seem thread-safe either.
  let process = (proc _lceClientPath $ coerce args){cwd = rootDir}
  (ec, lo, le) <- readCreateProcessWithExitCode process ""
    & rewrapIOError
  unless (ec == ExitSuccess && null le) $ -- TODO: separate JSON errors and other ones
    UnliftIO.throwIO $ LigoClientFailureException (toText lo) (toText le) Nothing (Just ec)
  pure $ toText lo

----------------------------------------------------------------------------
-- LIGO Debugger stuff
----------------------------------------------------------------------------

-- | Handles exceptions that may be produced when calling a LIGO executable.
-- Maps them to the debugger ones.
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
checkCompilation :: (HasLigoClient m) => ModuleName -> FilePath -> m ()
checkCompilation ModuleName{..} file = void $ withMapLigoExc $
  callLigoBS Nothing
    do concat
        [ ["compile", "contract"]
        , ["--no-warn"]
        , guard (not $ T.null enModule) >> ["-m", strArg enModule]
        , ["--experimental-disable-optimizations-for-debugging"]
        , ["--disable-michelson-typechecking"]
        , [strArg file]
        ]

-- | Run ligo to compile the contract with all the necessary debug info.
compileLigoContractDebug :: forall m. (HasLigoClient m) => ModuleName -> FilePath -> m (LigoMapper 'Unique)
compileLigoContractDebug ModuleName{..} file = withMapLigoExc $
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
    ]
    >>= decodeOutput
  where
    decodeOutput :: Text -> m MU.Value
    decodeOutput txt =
      MP.parseExpandValue valueOrigin txt
        & first (LigoDecodeException "parsing Michelson value" .  pretty)
        & fromEither

-- | Get all modules with entry points.
getAvailableModules :: forall m. (HasLigoClient m)
                        => FilePath -> m ModuleNamesList
getAvailableModules file = withMapLigoExc $
  callLigo Nothing
    [ "info", "list-declarations"
    , "--only-ep"
    , strArg file
    ]
    >>= decodeOutput
  where
    decodeOutput :: Text -> m ModuleNamesList
    decodeOutput txt =
      maybe
        do throwIO $ LigoDecodeException "decoding list declarations" txt
        pure
        do parseModuleNamesList txt

-- | Tries to decompile Michelson values with @LigoType@s in @LIGO@ ones.
decompileLigoValues :: forall m. (HasLigoClient m) => [(LigoType, T.SomeValue)] -> m [Maybe LigoValue]
decompileLigoValues typesAndValues = withMapLigoExc do
  withSystemTempFile "ligoValue.mligo" \path hndl -> do
    let contractCode = generateDecompilation typesAndValues

    hPutStrLn hndl (pretty @_ @Text contractCode)
    hFlush hndl

    callLigoBS Nothing
      [ "run", "test"
      , "--no-warn"
      , strArg path
      ]
    >>= decodeOutput
  where
    decodeOutput :: LByteString -> m [Maybe LigoValue]
    decodeOutput = either (throwIO . LigoDecodeException "decoding ligo decompile" . toText) pure
      . Aeson.eitherDecode

-- | Resolve a debugging configuration written in LIGO.
resolveConfig :: forall m. (HasLigoClient m) => FilePath -> m LigoLaunchRequest
resolveConfig configPath = withMapLigoExc do
  handleJSONException LigoResolveConfigException $
    callLigoBS Nothing
      [ "info", "resolve-config"
      , "--format", "json"
      , strArg configPath
      ]
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
      entrypoint <- o .:? "entrypoint"
      storage <- o .:? "storage"
      moduleName <- o .:? "module_name"
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

-- | @getScopes path@ gets all scopes for the contract that has a @path@ file path.
getScopes :: forall m. (HasLigoClient m) => FilePath -> m LigoDefinitions
getScopes contractPath = withMapLigoExc do
  callLigoBS Nothing
    [ "info", "get-scope"
    , "--format", "json"
    , "--no-stdlib"
    , strArg contractPath
    ]
    >>= decodeOutput
  where
    decodeOutput :: LByteString -> m LigoDefinitions
    decodeOutput = either (throwIO . LigoDecodeException "decoding ligo scopes" . toText) pure
      . Aeson.eitherDecode

-- | @getVersion path@ returns the current version of LIGO (@ligo --version@).
getVersion :: forall m. (HasLigoClient m) => m Version
getVersion = withMapLigoExc do
  callLigoBS Nothing ["--version"] >>= decodeOutput
  where
    decodeOutput :: LByteString -> m Version
    decodeOutput = pure . Version . T.strip . decodeUtf8

-- | @decodeCST files@ takes a batch of @files@ and produces parsed ASTs.
-- The order of produced ASTs corresponds to the order of @files@.
decodeCST :: forall m. (HasLigoClient m) => NonEmpty FilePath -> m [SomeLIGO Info]
decodeCST files = withMapLigoExc $
  callLigoBS
    Nothing
    do mconcat
        [ [ "info", "dump-cst"
          ]
        , toList (strArg <$> files)
        ]
    >>= decodeOutput
  where
    decodeOutput :: LByteString -> m [SomeLIGO Info]
    decodeOutput bts = do
      ASTs asts <-
        either (throwIO . LigoDecodeException "decoding ligo cst" . unlines . fmap toText . errorMessages) pure
          $ (unpackEither bts)

      pure asts
