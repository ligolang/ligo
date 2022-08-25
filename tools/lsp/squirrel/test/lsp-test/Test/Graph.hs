module Test.Graph
  ( test_WccAlgorithm
  , test_WccProperty
  ) where

import Algebra.Graph.AdjacencyMap
import Data.List (sort)
import Test.QuickCheck.Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck

import Util.Graph (wcc)

data WCCTest = WCCTest (AdjacencyMap Int) [AdjacencyMap Int]

inputGraphs :: [WCCTest]
inputGraphs =
  [ WCCTest
      ((1 * 2) + (1 * 3) + (3 * 4) + (2 * 4))
      [(1 * 2) + (1 * 3) + (3 * 4) + (2 * 4)]
  , WCCTest
      ((1 * 2 * 3) + (4 * 5 * 6))
      [1 * 2 * 3, 4 * 5 * 6]
  , WCCTest
      (1 * 2 * 3)
      [1 * 2 * 3]
  , WCCTest
      ((1 * (2 * 3)) + (4 * 5) + (6 * 7 + 7 * 6 + 8 * 6))
      [1 * 2 * 3, 4 * 5, 6 * 7 + 7 * 6 + 8 * 6]
  ]

test_WccAlgorithm :: TestTree
test_WccAlgorithm = testGroup "Graph WCC algorithm" (fmap runTest (zip [1 .. ] inputGraphs))
  where
    runTest :: (Int, WCCTest) -> TestTree
    runTest (num, WCCTest graph expected) =
      let w = sort $ wcc graph
          se = sort expected
        in testCase
             ("Running wcc test " ++ show num) $
               assertBool (formatException w se) (w == se)

    formatException :: [AdjacencyMap Int] -> [AdjacencyMap Int] -> String
    formatException got expected = mconcat
      [ "Expected graph to contain these components:\n"
      , show expected
      , "\n"
      , "But instead got: \n"
      , show got
      ]

test_WccProperty :: TestTree
test_WccProperty = testProperty "Graph WCC algorithm properties" runTest
  where
    runTest :: Gen Bool
    runTest = do
      graph <- genGraph 100
      let reconstructed = overlays $ wcc graph
      pure (graph == reconstructed)

genGraph :: Int -> Gen (AdjacencyMap Int)
genGraph graphSize = genGraph' graphSize graphSize empty

genGraph' :: Int -> Int -> AdjacencyMap Int -> Gen (AdjacencyMap Int)
genGraph' 0 _ m = return m
genGraph' iter size m = do
  v1 <- vertex <$> chooseInt (0, size)
  v2 <- vertex <$> chooseInt (0, size)
  option <- chooseInt (0, 3)
  case option of
    0 -> overlay (connect v1 v2) <$> genGraph' (iter - 1) size m
    1 -> connect (overlay v1 v2) <$> genGraph' (iter - 1) size m
    2 -> overlay (overlay v1 v2) <$> genGraph' (iter - 1) size m
    3 -> connect (connect v1 v2) <$> genGraph' (iter - 1) size m
    x -> error ("Impossible, got " ++ show x)
