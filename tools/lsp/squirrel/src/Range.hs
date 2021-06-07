{- | Continuous location inside the source and utilities.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Range
  ( HasRange(..)
  , Range(..)
  , cutOut
  , excluding
  , intersects
  , interval
  , fromLspPosition
  , fromLspRange
  , merged
  , point
  , toLspRange
  )
  where

import qualified Language.LSP.Types as LSP (Position (..), Range (..))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding

import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree

import Product

point :: Int -> Int -> Range
point l c = Range (l, c, 0) (l, c, 0) ""

-- | Construct a range spanning a single line `line` from a column
-- `colSt` (inclusively) to `colFin` (exclusively).
interval :: Int -> Int -> Int -> Range
interval line colSt colFin = Range (line, colSt, 0) (line, colFin, 0) ""

-- | A continuous location in text.
data Range = Range
  { rStart  :: (Int, Int, Int)  -- ^ [Start: line, col, byte-offset...
  , rFinish :: (Int, Int, Int)  -- ^ ... End: line, col, byte-offset).
  , rFile   :: FilePath
  }
  deriving (Show) via PP Range

instance Pretty Range where
  pp (Range (ll, lc, _) (rl, rc, _) f) =
    text f <.> ":"
    <.> int ll <.> ":"
    <.> int lc <.> "-"
    <.> int rl <.> ":"
    <.> int rc

-- | Ability to get range out of something.
class HasRange a where
  getRange :: a -> Range

instance HasRange Range where
  getRange = id

instance Contains Range xs => HasRange (Product xs) where
  getRange = getElem

-- | Convert `squirrel` range to `haskell-lsp` range.
-- Note that we consider the first line to be at position 1.
toLspRange :: Range -> LSP.Range
toLspRange Range
  { rStart  = (rsl, rsc, _)
  , rFinish = (rfl, rfc, _)
  } = LSP.Range
  { LSP._start = LSP.Position{ LSP._line = rsl - 1, LSP._character = rsc - 1 }
  , LSP._end   = LSP.Position{ LSP._line = rfl - 1, LSP._character = rfc - 1 }
  }

fromLspPosition :: LSP.Position -> Range
fromLspPosition (LSP.Position l c) = point (l + 1) (c + 1)

fromLspRange :: LSP.Range -> Range
fromLspRange
  (LSP.Range
    (fromLspPosition -> s)
    (fromLspPosition -> e)) = merged s e

instance (Contains Range xs, Apply Functor fs) => HasRange (Tree fs (Product xs)) where
  getRange = getElem . extract

-- | Extract textual representation of given range.
cutOut :: Range -> ByteString -> Text
cutOut (Range (_, _, s) (_, _, f) _) bs =
  decodeUtf8
    $ BS.take (f - s)
    $ BS.drop  s
      bs

excluding :: Range -> Range -> Range
excluding (Range _ s _) (Range _ f t) = Range s f t

merged :: Range -> Range -> Range
merged (Range s _ _) (Range _ f t) = Range s f t

-- | Returns whether the two ranges have some common intersection. Ranges in
-- different files are always considered to not have intersections.
--
-- N.B.: If R1 = [(l1, c1) ... (l2, c2)] and R2 = [(l2, c2) ... (l3, c3)], this
-- function will return 'False'. That is, a single point of intersection is not
-- enough for both of them to be considered equal, since ranges are exclusive in
-- their end points.
intersects :: Range -> Range -> Bool
intersects (Range (ll1, lc1, _) (ll2, lc2, _) lf) (Range (rl1, rc1, _) (rl2, rc2, _) rf)
  -- Different files never intersect.
  | lf /= rf = False
  -- If l's start is before or at r's start, it intersects iff its end is after
  -- r's start (but not at, see function's N.B. part).
  | ll1 < rl1 || ll1 == rl1 && lc1 <= rc1 = ll2 > rl1 || ll2 == rl1 && lc2 > rc1
  -- Same as before, but with l and r swapped.
  | rl1 < ll1 || rl1 == ll1 && rc1 <= lc1 = rl2 > ll1 || rl2 == ll1 && rc2 > lc1
  -- Otherwise, the ranges are disjoint.
  | otherwise = False

instance Lattice Range where
  Range (ll1, lc1, _) (ll2, lc2, _) _
    `leq` Range (rl1, rc1, _) (rl2, rc2, _) _ =
    (rl1 < ll1 || rl1 == ll1 && rc1 <= lc1) &&
    (rl2 > ll2 || rl2 == ll2 && rc2 >= lc2)

instance Eq Range where
  Range (l, c, _) (r, d, _) f == Range (l1, c1, _) (r1, d1, _) f1 =
    (l, c, r, d, f) == (l1, c1, r1, d1, f1)

instance Ord Range where
  Range (l, c, _) (r, d, _) f `compare` Range (l1, c1, _) (r1, d1, _) f1 =
    compare l l1 <> compare c c1 <> compare r r1 <> compare d d1 <> compare f f1

instance (Contains Range xs, Eq (Product xs)) => Ord (Product xs) where (<=) = leq

instance (Contains Range xs, Eq (Product xs)) => Lattice (Product xs) where
  a `leq` b = getElem @Range a `leq` getElem @Range b
