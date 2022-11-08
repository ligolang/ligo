module Common
  ( WebIDEM
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Katip (KatipT)
import Servant (ServerError)

import Config (Config)

type WebIDEM = KatipT (ReaderT Config (ExceptT ServerError IO))
