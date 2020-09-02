-- | All the types needed for cli to work.
module Cli.Types
  ( LigoClient
  , LigoClientEnv (..)
  )
where

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
