
{- | Continious location inside the source and utilities.
-}

module Range
  ( Range(..)
  , HasRange(..)
  , diffRange
  , cutOut
  , point
  )
  where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding

import Pretty
import Lattice
import Product

point :: Int -> Int -> Range
point l c = Range (l, c, 0) (l, c, 0) ""

-- | A continious location in text.
data Range = Range
  { rStart  :: (Int, Int, Int)  -- ^ [Start: line, col, byte-offset...
  , rFinish :: (Int, Int, Int)  -- ^ ... End: line, col, byte-offset).
  , rFile   :: FilePath
  }
  deriving (Show) via PP Range
  deriving stock (Ord)

-- | TODO: Ugh. Purge it.
diffRange :: Range -> Range -> Range
diffRange (Range ws wf f) (Range ps _ _) = Range (max ws ps) wf f

instance Pretty Range where
  pp (Range (ll, lc, _) (rl, rc, _) f) =
    color 2 do
      brackets do
        text f <> ":"
          <> int ll <> ":"
          <> int lc <> "-"
          <> int rl <> ":"
          <> int rc

-- | Ability to get range out of something.
class HasRange a where
  getRange :: a -> Range

instance HasRange Range where
  getRange = id

instance Contains Range xs => HasRange (Product xs) where
  getRange = getElem

-- | Extract textual representation of given range.
cutOut :: Range -> ByteString -> Text
cutOut (Range (_, _, s) (_, _, f) _) bs =
  decodeUtf8
    $ BS.take (f - s)
    $ BS.drop  s
      bs

instance Lattice Range where
  Range (ll1, lc1, _) (ll2, lc2, _) _ <? Range (rl1, rc1, _) (rl2, rc2, _) _ =
    (rl1 < ll1 || rl1 == ll1 && rc1 <= lc1) &&
    (rl2 > ll2 || rl2 == ll2 && rc2 >= lc2)

instance Eq Range where
  Range (l, c, _) (r, d, _) f == Range (l1, c1, _) (r1, d1, _) f1 =
    (l, c, r, d, f) == (l1, c1, r1, d1, f1)