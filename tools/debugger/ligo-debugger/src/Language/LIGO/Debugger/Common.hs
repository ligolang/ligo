-- | Common stuff for debugger.
module Language.LIGO.Debugger.Common
  ( EmbeddedLigoMeta
  , ligoPositionToSrcPos
  , ligoRangeToSourceLocation
  ) where

import Unsafe qualified

import Morley.Debugger.Core.Navigate (SourceLocation (..))
import Morley.Michelson.ErrorPos (Pos (..), SrcPos (..))

import Language.LIGO.Debugger.CLI.Types
import Morley.Debugger.Core.Snapshots (SourceType(..))

-- | Type of meta that we embed in Michelson contract to later use it
-- in debugging.
type EmbeddedLigoMeta = LigoIndexedInfo

ligoPositionToSrcPos :: HasCallStack => LigoPosition -> SrcPos
ligoPositionToSrcPos (LigoPosition l c) =
  SrcPos
    (Pos $ Unsafe.fromIntegral @Integer @Word (toInteger l - 1))
    (Pos c)

ligoRangeToSourceLocation :: HasCallStack => LigoRange -> SourceLocation
ligoRangeToSourceLocation LigoRange{..} =
  SourceLocation (SourcePath lrFile) (ligoPositionToSrcPos lrStart)
