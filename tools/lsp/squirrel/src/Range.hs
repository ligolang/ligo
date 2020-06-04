
{- | Continious location inside the source and utilities.
-}

module Range
  ( Range(..)
  , HasRange(..)
  , diffRange
  , cutOut
  )
  where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding

import Pretty

-- | A continious location in text.
data Range = Range
  { rStart  :: (Int, Int, Int)  -- ^ [Start: line, col, byte-offset...
  , rFinish :: (Int, Int, Int)  -- ^ ... End: line, col, byte-offset).
  }
  deriving (Show) via PP Range

-- | TODO: Ugh. Purge it.
diffRange :: Range -> Range -> Range
diffRange (Range ws wf) (Range ps _) = Range (max ws ps) wf

instance Pretty Range where
  pp (Range (ll, lc, _) (rl, rc, _)) =
    brackets do
      int ll <> ":" <> int lc <> "-" <> int rl <> ":" <> int rc

-- | Ability to get range out of something.
class HasRange a where
  getRange :: a -> Range

-- | Extract textual representation of given range.
cutOut :: Range -> ByteString -> Text
cutOut (Range (_, _, s) (_, _, f)) bs =
  decodeUtf8
    $ BS.take (f - s)
    $ BS.drop  s
      bs

