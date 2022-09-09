module Util.Graph
  ( traverseAM
  , traverseAMConcurrently
  , forAM
  , forAMConcurrently
  , wcc
  , wccFor
  ) where

import Algebra.Graph.NonEmpty.AdjacencyMap qualified as NEG
import Algebra.Graph.AdjacencyMap.Algorithm (scc)
import Algebra.Graph.AdjacencyMap as G
import Control.Arrow ((&&&), second)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.List (find)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tuple (swap)
import UnliftIO.Async (pooledMapConcurrently)

traverseAMImpl
  :: (Monad m, Ord a, Ord b)
  => ((a -> m (a, b)) -> [a] -> m [(a, b)])
  -> (a -> m b)
  -> AdjacencyMap a
  -> m (AdjacencyMap b)
traverseAMImpl traverser f g = do
  keysList <- traverser (sequenceA . (id &&& f)) (G.vertexList g)
  let adj = G.adjacencyMap g
  let keys = Map.fromList keysList
  pure $ G.fromAdjacencySets $ map (second (Set.map (keys Map.!) . (adj Map.!)) . swap) keysList

-- | Traverse an adjacency map.
traverseAM :: (Monad m, Ord a, Ord b) => (a -> m b) -> AdjacencyMap a -> m (AdjacencyMap b)
traverseAM = traverseAMImpl traverse

-- | Traverse an adjacency map concurrently.
traverseAMConcurrently :: (MonadUnliftIO m, Ord a, Ord b) => (a -> m b) -> AdjacencyMap a -> m (AdjacencyMap b)
traverseAMConcurrently = traverseAMImpl pooledMapConcurrently

-- | Flipped version of 'traverseAM'.
forAM :: (Monad m, Ord a, Ord b) => AdjacencyMap a -> (a -> m b) -> m (AdjacencyMap b)
forAM = flip traverseAM

-- | Flipped version of 'traverseAMConcurrently'.
forAMConcurrently :: (MonadUnliftIO m, Ord a, Ord b) => AdjacencyMap a -> (a -> m b) -> m (AdjacencyMap b)
forAMConcurrently = flip traverseAMConcurrently

-- | Finds all weakly connected components of the graph.
wcc :: Ord a => AdjacencyMap a -> [AdjacencyMap a]
wcc graph =
  let components = fmap NEG.vertexSet $ vertexList $ scc $ overlay (transpose graph) graph
   in fmap (\x -> induce (`Set.member` x) graph) components

-- | Tries to find the WCC such that the given vertex is present.
wccFor :: Ord a => a -> AdjacencyMap a -> Maybe (AdjacencyMap a)
wccFor x = find (G.hasVertex x) . wcc
