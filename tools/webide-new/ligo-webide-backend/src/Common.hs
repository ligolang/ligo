module Common
  ( WebIDEM
  ) where

import Katip (KatipT)
import Servant (ServerError)

import Config (Config)

type WebIDEM = KatipT (ReaderT Config (ExceptT ServerError IO))
