-- | All the types needed for cli to work.
module Cli.Types
  ( LigoClient
  , LigoClientEnv (..)
  , HasLigoClient(..)
  )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, asks)

import Data.Default (Default (..))

import Product

type LigoClient = ReaderT (Product '[LigoClientEnv]) IO

-- | Environment passed throughout the ligo interaction
data LigoClientEnv = LigoClientEnv
  { -- | Ligo binary path
    _lceClientPath :: FilePath
  , -- | Whether we need to print logs from ligo
    _lceVerbose :: Bool
  }
  deriving stock (Show)

class (Monad m, MonadIO m, MonadCatch m) => HasLigoClient m where
  getLigoClientEnv :: m LigoClientEnv

instance
  Contains LigoClientEnv env
    =>
  HasLigoClient (ReaderT (Product env) IO)
    where
      getLigoClientEnv = asks (getElem @LigoClientEnv @env)

instance HasLigoClient IO where
  getLigoClientEnv = pure def

instance Default LigoClientEnv where
  def = LigoClientEnv "ligo" False
