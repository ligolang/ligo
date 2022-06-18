-- | Common stuff for debugger.
module Language.LIGO.Debugger.Common
  ( EmbeddedLigoMeta
  ) where

import Language.LIGO.Debugger.CLI.Types

-- | Type of meta that we embed in Michelson contract to later use it
-- in debugging.
type EmbeddedLigoMeta = LigoIndexedInfo
