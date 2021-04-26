-- | All the types needed for cli to work.
module Cli.Types
  ( LigoClientEnv (..)
  , HasLigoClient(..)
  , RawContractCode(..)
  )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy.Char8 as S8L

import Data.Default (Default (..))


newtype RawContractCode = RawContractCode
  { unRawContract :: S8L.ByteString
  } deriving stock Show


-- | Environment passed throughout the ligo interaction
newtype LigoClientEnv = LigoClientEnv
  { -- | Ligo binary path
    _lceClientPath :: FilePath
  }
  deriving stock (Show)

instance Default LigoClientEnv where
  def = LigoClientEnv "ligo"


class (Monad m, MonadIO m, MonadCatch m) => HasLigoClient m where
  getLigoClientEnv :: m LigoClientEnv
