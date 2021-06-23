module Util
  ( foldMapM
  , unconsFromEnd
  , safeIndex
  , nubOrd
  , nubOnOrd
  , toUri
  , toLocation
  , unionOrd
  , findKey
  , removeDots
  ) where

import Data.Foldable (foldlM)
import Data.Map.Internal qualified as MI
import Data.Set qualified as Set
import Language.LSP.Types qualified as J
import System.FilePath (joinPath, splitDirectories)

import Range

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

toUri :: Range -> J.Uri
toUri = J.filePathToUri . rFile

toLocation :: Range -> J.Location
toLocation = J.Location <$> toUri <*> toLspRange

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

-- | Takes the union of two lists, leaving no duplicates using the provided 'Ord'
-- instance.  O((m + n) log (m + n)) complexity.
unionOrd :: Ord a => [a] -> [a] -> [a]
unionOrd as bs = nubOrd (as <> bs)

-- | Find a key in a map by testing for equality using some projection.
-- O(log n) assuming the projection function is O(1).
--
-- N.B.: It is required that the projection function preserves the order of the
-- Map, that is, for all keys a < b < c < ... in the map, it is required that
-- f a < f b < f c < ....
findKey :: (Ord k, Ord k') => (k -> k') -> k' -> MI.Map k v -> Maybe (k, v)
findKey _ _ MI.Tip = Nothing
findKey f x (MI.Bin _ k v l r) = case compare x (f k) of
  LT -> findKey f x l
  EQ -> Just (k, v)
  GT -> findKey f x r

removeDots :: FilePath -> FilePath
removeDots = joinPath . fst . foldr removeDir ([], 0) . splitDirectories
  where
    removeDir :: FilePath -> ([FilePath], Int) -> ([FilePath], Int)
    removeDir name (acc, n)
      | name == "."  = (       acc, n)
      | name == ".." = (       acc, n + 1)
      | n > 0        = (       acc, n - 1)
      | otherwise    = (name : acc, 0)
