-- | All the types needed for cli to work.
module Cli.Types
  ( LigoClient
  , LigoClientEnv (..)
  , HasLigoClientEnv (..)
  , HasLigoClient
  )
where

import Control.Lens (Lens')
import Control.Monad.Catch.Pure (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Reader (ReaderT)

-- | Type of the client itself.
type LigoClient = ReaderT LigoClientEnv IO

-- | Environment passed throughout the ligo interaction
data LigoClientEnv = LigoClientEnv
  { -- | Ligo binary path
    _lceClientPath :: FilePath
  , -- | Whether we need to print logs from ligo
    _lceVerbose :: Bool
  }
  deriving stock (Show)

class HasLigoClientEnv env where
  ligoClientEnvL :: Lens' env LigoClientEnv

type HasLigoClient env m = (HasLigoClientEnv env, MonadReader env m, MonadIO m, MonadThrow m)
