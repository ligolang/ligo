module Util
  ( groupByKey
  ) where

import Data.List (groupBy)

groupByKey :: Ord k => (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
groupByKey f g =
  extractGroup f g
  . groupBy ((==) `on` f)
  . sortBy (comparing f)

extractGroup :: (a -> k) -> (a -> v) -> [[a]] -> [(k, [v])]
extractGroup _ _ [] = []
extractGroup f g ([] : xs) = extractGroup f g xs
extractGroup f g (ys@(y : _) : xs) = (f y, g <$> ys) : extractGroup f g xs
