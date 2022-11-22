-- | All the types needed for cli to work.
module Cli.Types
  ( LigoProcess (..)
  , LigoClientEnv (..)
  , HasLigoClient(..)
  , TempDir (..)
  , TempSettings (..)
  , ligoBinaryPath
  , debugTempSettings
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Default (Default (..))
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (getCanonicalTemporaryDirectory)
import UnliftIO.Directory (getCurrentDirectory)
import UnliftIO.Exception as UnliftIO (catch, throwIO)
import UnliftIO.Pool (Pool)
import UnliftIO.Process (ProcessHandle)

import Log (LogT, NoLoggingT)

data LigoProcess = LigoProcess
  { -- | LIGO process handle
    _lpLigo :: ProcessHandle
  , -- | Write handle
    _lpStdin :: Handle
  , -- | Read handle
    _lpStdout :: Handle
  , -- | Error handle
    _lpStderr :: Handle
  }

-- | Environment passed throughout the ligo interaction
data LigoClientEnv = LigoClientEnv
  { -- | LIGO binary path
    _lceClientPath :: FilePath
  , -- | Information regarding LIGO processes
    _lceLigoProcesses :: Maybe (Pool LigoProcess)
  }

-- | Attempts to get the environment variable 'LIGO_BINARY_PATH'. If such
-- variable is not present, defaults to "ligo", assuming it is in PATH.
ligoBinaryPath :: IO FilePath
ligoBinaryPath = getEnv "LIGO_BINARY_PATH" `UnliftIO.catch` \e ->
  if isDoesNotExistError e
  then pure "ligo"
  else throwIO e

instance Default LigoClientEnv where
  def = LigoClientEnv{_lceClientPath = "ligo", _lceLigoProcesses = Nothing}

class MonadUnliftIO m => HasLigoClient m where
  getLigoClientEnv :: m LigoClientEnv

-- Mostly for debugging purposes
instance HasLigoClient IO where
  getLigoClientEnv = do
    _lceClientPath <- ligoBinaryPath
    pure def
      { _lceClientPath
      }

instance HasLigoClient m => HasLigoClient (LogT m) where
  getLigoClientEnv = lift getLigoClientEnv

instance HasLigoClient m => HasLigoClient (NoLoggingT m) where
  getLigoClientEnv = lift getLigoClientEnv

data TempDir
  = GenerateDir String
  -- ^ Generate a new temporary directory using the given name template.
  | UseDir FilePath
  -- ^ Use an existing directory.

data TempSettings = TempSettings
  { tsProjectPath :: FilePath
    -- ^ The absolute path to the root directory where indexing occurs.
  , tsTemporaryDir :: TempDir
    -- ^ The name template or path for the temporary directory.
  }

-- | Returns a @TempSettings@ using the current directory for the project path
-- and the canonical temporory directory for writing to the disk.
-- For debugging.
debugTempSettings :: IO TempSettings
debugTempSettings =
  TempSettings <$> getCurrentDirectory <*> fmap UseDir getCanonicalTemporaryDirectory
