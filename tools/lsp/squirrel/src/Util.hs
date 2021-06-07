module Util
  ( foldMapM
  , unconsFromEnd
  , safeIndex
  , nubOrd
  , nubOnOrd
  , zipWithRepeat
  ) where

import Data.Foldable (foldlM)
import qualified Data.Set as Set

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldlM folder mempty
  where
    folder !acc new = (acc <>) <$> f new

-- | Split into list into a pair of all but the last elements, and the last
-- element.
--
-- @unconsFromEnd [1, 2, 3] = Just ([1, 2], 3)@
unconsFromEnd :: [a] -> Maybe ([a], a)
unconsFromEnd [] = Nothing
unconsFromEnd xs = Just (init xs, last xs) -- doubt we should care about two passes

safeIndex :: (Eq t, Num t) => [a] -> t -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x : _) 0 = Just x
safeIndex (_ : xs) n = safeIndex xs (n - 1)

-- | Like 'nub', but with O(n log n) complexity. Requires 'Ord' constraint.
nubOrd :: Ord a => [a] -> [a]
nubOrd = nubOnOrd id

-- | Like 'nubOrd', but takes a projection function for custom comparison.
nubOnOrd :: Ord b => (a -> b) -> [a] -> [a]
nubOnOrd f = go Set.empty
  where
    go _  [] = []
    go ys (x : xs)
      | y `Set.member` ys = go ys xs
      | otherwise         = x : go (Set.insert y ys) xs
      where
        y = f x

-- | Like 'zipWith', but if one list is greater in length than the other, it
-- duplicates elements from the bigger list.
--
-- E.g.: For two lists 'as' and bs' with elements a1, a2, ..., a7
-- and b1, b2, ..., b9 it will perform
-- f a1 b1 : f a2 b2 : ... : f a7 b7 : f b8 b8 : f b9 b9 : [].
zipWithRepeat :: (a -> a -> b) -> [a] -> [a] -> [b]
zipWithRepeat f as       []       = zipWith f as as
zipWithRepeat f []       bs       = zipWith f bs bs
zipWithRepeat f (a : as) (b : bs) = f a b : zipWithRepeat f as bs
