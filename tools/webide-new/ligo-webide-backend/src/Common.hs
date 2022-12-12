module Common
  ( WebIDEM
  ) where

import Katip (KatipT)

import Config (Config)
import Servant.Server.Internal.ServerError (ServerError)

type WebIDEM = KatipT (ReaderT Config (ExceptT ServerError IO))
