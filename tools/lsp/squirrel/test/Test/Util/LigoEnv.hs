{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provide a 'HasLigoClient' instance for simple 'IO'.
-- The path it returns is just `ligo`, so the binary will be looked up
-- in PATH.
module Test.Util.LigoEnv
  (
  ) where

import Data.Default (def)

import Cli.Types (HasLigoClient (..))


instance HasLigoClient IO where
  getLigoClientEnv = pure def
