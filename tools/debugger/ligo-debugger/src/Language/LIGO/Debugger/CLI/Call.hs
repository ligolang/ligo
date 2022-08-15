module Language.LIGO.Debugger.CLI.Call
  ( compileLigoContractDebug
  , compileLigoExpression
  , getAvailableEntrypoints

    -- * Utilities
  , runAndReadOutput
  ) where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Fmt (pretty)
import System.Exit (ExitCode (..))
import Text.Interpolation.Nyan
import UnliftIO.Exception (throwIO)
import UnliftIO.Process
  (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)

import Morley.Michelson.Parser qualified as MP
import Morley.Michelson.Untyped qualified as MU

import Language.LIGO.Debugger.CLI.Types
import Data.Aeson.Lens (key, AsPrimitive (_String))
import Data.Text.Encoding qualified as T

throwLeftWith :: (MonadError LigoException m) => (e -> LigoException) -> Either e a -> m a
throwLeftWith f = either (throwError . f) pure

-- | Runs a process and consumes its stdout.
runAndReadOutput
  :: (MonadIO m, MonadError LigoException m)
  => (LByteString -> r)
     -- ^ Parsing function.
     -- It is obliged to read all the data it wants to once @r@ is evaluated
     -- to WHNF, due to the common concern about lazy IO.
  -> FilePath
     -- ^ Command to execute.
  -> [String]
     -- ^ Command line arguments to pass.
  -> m r
runAndReadOutput readOutput cmd args = do
  liftIO . try $ withCreateProcess
    (proc cmd args)
    { std_out = CreatePipe
    , std_err = CreatePipe
    , create_group = True
      -- ↑ Make sure that spawned process does not automatically receive our SIGINT
    }
    \_ mOut mErr pHandler -> case (mOut, mErr) of
      (Just out, Just err) -> do
        !res <- readOutput <$> LBS.hGetContents out
        waitForProcess pHandler >>= \case
          ExitFailure code -> do
            !except <- Aeson.decode <$> LBS.hGetContents err :: IO (Maybe LigoException)
            whenJust except $ \e -> throwIO e
            let pkProcessName = toText cmd
            throwIO ([int||#{pkProcessName}: Unknown error (#{code})|] :: LigoException)
          ExitSuccess -> do
            return res
      (_, _) -> error "Unexpectedly process had no output handlers"
  >>= throwLeftWith id


-- | Run ligo to compile the contract with all the necessary debug info.
compileLigoContractDebug :: (MonadIO m, MonadError LigoException m)
                         => String -> FilePath -> m LigoMapper
compileLigoContractDebug entrypoint file = do
  runAndReadOutput decodeOutput
    "ligo"
    [ "compile", "contract"
    , "--no-warn"
    , "--display-format"
    , "json"
    , "--michelson-format", "json"
    , "--michelson-comments", "location"
    , "--michelson-comments", "env"
    , "-e", entrypoint
    , "--experimental-disable-optimizations-for-debugging"
    , "--disable-michelson-typechecking"
    , file
    ]
    >>= throwLeftWith [int|m|Unexpected output of `ligo` from decoding source mapper: #{toText}|]
  where
    decodeOutput :: LBS.ByteString -> Either String LigoMapper
    decodeOutput bs =
      maybe (Left "Ligo compile contract: json-output parse error")
            Aeson.eitherDecode
            (LBS.fromStrict . T.encodeUtf8 <$> (bs ^? key "json_code" . _String))

-- | Run ligo to compile expression into Michelson in the context of the
-- given file.
compileLigoExpression :: (MonadIO m, MonadError LigoException m)
                      => MP.MichelsonSource -> FilePath -> Text -> m MU.Value
compileLigoExpression valueOrigin ctxFile expr =
  runAndReadOutput
    decodeOutput
    "ligo"
    [ "compile", "expression"
    , "--no-warn"
    , "--display-format"
    , "json"
    , "--init-file", ctxFile
    , "auto"  -- `syntax` argument, we can leave `auto` since context file is specified
    , toString expr
    ]
    >>= throwLeftWith [int|m|Unexpected output of `ligo` from parsing Michelson value: #{id}|]
  where
    decodeOutput :: LBS.ByteString -> Either Text MU.Value
    decodeOutput bs =
      maybe (Left "Ligo compile expression: json-output parse error")
      (first prettyPrint . MP.parseExpandValue valueOrigin . decodeUtf8)
      (LBS.fromStrict . T.encodeUtf8 <$> (bs ^? key "text_code" . _String))
    prettyPrint :: MP.ParserException -> Text
    prettyPrint = pretty

-- TODO: use "get entrypoints" functionality when it is available in `ligo`.

getAvailableEntrypoints :: (MonadIO m, MonadError LigoException m)
                        => FilePath -> m EntrypointsList
getAvailableEntrypoints file = do
  runAndReadOutput
    Aeson.eitherDecode
    "ligo"
    [ "info", "list-declarations"
    , "--display-format", "json"
    , file
    ]
    >>= throwLeftWith [int|m|Unexpected output of `ligo` from decoding list declarations #{toText}|]

{- TODO: combine ligo calling with the one from LSP and use the shared code

Pros of our code here:
* Does not perform unnecessary textual conversions,
  decodes the output on the fly
  (this seems important, because debug output can be HUGE)
* Cares about posibility of ligo being killed

Pros of code in LSP:
* Simpler
* Does not depend on hardcoded `ligo` executable
* Accounts stderr properly
* Other features

-}
