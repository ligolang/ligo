module Language.LIGO.Debugger.CLI.Call
  ( compileLigoContractDebug
  , BadLigoOutput(..)

    -- * Utilities
  , runAndReadOutput
  , ProcessKilledException(..)
  ) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Fmt (Buildable (..), pretty)
import System.Exit (ExitCode (..))
import Text.Interpolation.Nyan
import UnliftIO.Process
  (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)

import Language.LIGO.Debugger.CLI.Types

data ProcessKilledException = ProcessKilledException
  { pkeProcessName :: Text
  , pkeCode        :: Int
  , pkeMessage     :: Text
  } deriving stock (Show)

instance Buildable ProcessKilledException where
  build ProcessKilledException{..} = [int||
    Running '#{pkeProcessName}' failed with #{pkeCode} code:
      #{pkeMessage}
   |]

instance Exception ProcessKilledException where
  displayException = pretty

-- | Runs a process and consumes its stdout.
runAndReadOutput
  :: MonadIO m
  => (LByteString -> r)
     -- ^ Parsing function.
     -- It is obliged to read all the data it wants to once @r@ is evaluated
     -- to WHNF, due to the common concern about lazy IO.
  -> FilePath
     -- ^ Command to execute.
  -> [String]
     -- ^ Command line arguments to pass.
  -> m r
runAndReadOutput readOutput cmd args =
  liftIO $ withCreateProcess
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
            !errMsg <- decodeUtf8 <$> LBS.hGetContents err
            throwIO ProcessKilledException
              { pkeProcessName = toText cmd
              , pkeCode = code
              , pkeMessage = errMsg
              }
          ExitSuccess ->
            pass
        return res
      (_, _) -> error "Unexpectedly process had no output handlers"


newtype BadLigoOutput = BadLigoOutput
  { bloMessage :: Text
  } deriving stock (Show)

instance Buildable BadLigoOutput where
  build BadLigoOutput{..} =
    [int||
      Unexpected output of `ligo`: #{bloMessage}.
      Perhaps using the wrong ligo version?
     |]

instance Exception BadLigoOutput where
  displayException = pretty

-- | Run ligo to compile the contract with all the necessary debug info.
compileLigoContractDebug :: (MonadIO m) => FilePath -> m LigoMapper
compileLigoContractDebug file =
  runAndReadOutput Aeson.eitherDecode
    "ligo"
    [ "compile", "contract"
    , "--michelson-format", "json"
    , "--michelson-comments", "location"
    , "--michelson-comments", "env"
    , file
    ]
    >>= either (throwIO . BadLigoOutput . toText) pure

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
