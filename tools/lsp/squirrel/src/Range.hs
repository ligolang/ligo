
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
import Lattice
import Product

-- | A continious location in text.
data Range = Range
  { rStart  :: (Int, Int, Int)  -- ^ [Start: line, col, byte-offset...
  , rFinish :: (Int, Int, Int)  -- ^ ... End: line, col, byte-offset).
  }
  deriving (Show) via PP Range
  deriving stock (Ord)

-- | TODO: Ugh. Purge it.
diffRange :: Range -> Range -> Range
diffRange (Range ws wf) (Range ps _) = Range (max ws ps) wf

instance Pretty Range where
  pp (Range (ll, lc, _) (rl, rc, _)) =
    color 2 do
      brackets do
        int ll <> ":" <> int lc <> "-" <> int rl <> ":" <> int rc

-- | Ability to get range out of something.
class HasRange a where
  getRange :: a -> Range

instance HasRange Range where
  getRange = id

instance Contains Range xs => HasRange (Product xs) where
  getRange = getElem

-- | Extract textual representation of given range.
cutOut :: Range -> ByteString -> Text
cutOut (Range (_, _, s) (_, _, f)) bs =
  decodeUtf8
    $ BS.take (f - s)
    $ BS.drop  s
      bs

instance Lattice Range where
  Range (ll1, lc1, _) (ll2, lc2, _) <? Range (rl1, rc1, _) (rl2, rc2, _) =
    (rl1 < ll1 || rl1 == ll1 && rc1 <= lc1) &&
    (rl2 > ll2 || rl2 == ll2 && rc2 >= lc2)

instance Eq Range where
  Range (l, c, _) (r, d, _) == Range (l1, c1, _) (r1, d1, _) =
    (l, c, r, d) == (l1, c1, r1, d1)