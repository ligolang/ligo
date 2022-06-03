-- | Common types for debugger.
module Language.LIGO.Debugger.Types
  ( EmbeddedLigoMeta
  ) where

import Language.LIGO.Debugger.CLI.Types

-- Type of meta that we embed in Michelson contract to later use it
-- in debugging.
type EmbeddedLigoMeta = LigoIndexedInfo
