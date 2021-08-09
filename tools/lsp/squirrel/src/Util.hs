module Util
  ( foldMapM
  , unconsFromEnd
  , safeIndex
  , toUri
  , toLocation
  , unionOrd
  , findKey
  , removeDots
  , chunksOf
  , split
  , mapConcurrentlyBounded

  -- * Debugging utilities
  , validate
  ) where

import Data.Foldable (foldlM)
import Data.Map.Internal qualified as MI
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Language.LSP.Types qualified as J
import System.FilePath (joinPath, splitDirectories)
import UnliftIO.Async qualified as Async
import UnliftIO.Concurrent qualified as Concurrent
import Witherable (ordNub)

import Duplo.Lattice
import Duplo.Tree

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
toUri = J.filePathToUri . _rFile

toLocation :: Range -> J.Location
toLocation = J.Location <$> toUri <*> toLspRange

-- | Takes the union of two lists, leaving no duplicates using the provided 'Ord'
-- instance.  O((m + n) log (m + n)) complexity.
unionOrd :: Ord a => [a] -> [a] -> [a]
unionOrd as bs = ordNub (as <> bs)

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

-- | Divides the list into chunks of equally sized bins. The numeric parameter
-- defines the size of each bin.
--
-- If the list cannot be evenly divided, it will create one extra bin with less
-- elements.
--
-- A non-positive argument will return a bin containing the original list.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | n <= 0    = [xs]
  | otherwise = l : chunksOf n r
  where
    (l, r) = splitAt n xs

-- | Like 'chunksOf', but the numeric parameter chooses the number of bins
-- rather than the length of the bins.
--
-- If the list cannot be evenly divided, it will create extra bins as necessary
-- accordingly to ``length xs `quot` n``.
split :: Int -> [a] -> [[a]]
split n xs = chunksOf (length xs `quot` n) xs

-- | Maps a list concurrently, but first divides the input into bins accordingly
-- to the number of concurrent capabilities.
mapConcurrentlyBounded :: MonadUnliftIO m => (a -> m b) -> [a] -> m [b]
mapConcurrentlyBounded f xs = do
  caps <- Concurrent.getNumCapabilities
  concat <$> Async.mapConcurrently (traverse f) (split caps xs)

-- | Throws an error if the tree contains any subtrees such that the ranges are
-- not smaller than its parent nodes, or returns the tree unmodified, otherwise.
--
-- The error might be useful for debugging, as it will include the offending
-- ranges.
--
-- Warning: Use only for debugging.
validate :: (Functor f, Lattice a, Show a) => Cofree f a -> Cofree f a
validate (info :< tree) = info :< fmap (go info) tree
  where
    go info' (info'' :< tree')
      | info'' `leq` info' = info'' :< fmap (go info'') tree'
      | otherwise = error $ show info'' <> " ≰ " <> show info'
