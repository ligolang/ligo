-- | All the types needed for cli to work.
module Cli.Types
  ( LigoClientEnv (..)
  , HasLigoClient(..)
  , TempDir (..)
  , TempSettings (..)
  , ligoBinaryPath
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (lift)
import Data.Default (Default (..))
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (catch, throwIO)

import Log (LogT, NoLoggingT)

-- | Environment passed throughout the ligo interaction
newtype LigoClientEnv = LigoClientEnv
  { -- | Ligo binary path
    _lceClientPath :: FilePath
  }
  deriving stock (Show)

-- | Attempts to get the environment variable 'LIGO_BINARY_PATH'. If such
-- variable is not present, defaults to "ligo", assuming it is in PATH.
ligoBinaryPath :: IO FilePath
ligoBinaryPath = getEnv "LIGO_BINARY_PATH" `catch` \e ->
  if isDoesNotExistError e
  then pure "ligo"
  else throwIO e

instance Default LigoClientEnv where
  def = LigoClientEnv "ligo"

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
