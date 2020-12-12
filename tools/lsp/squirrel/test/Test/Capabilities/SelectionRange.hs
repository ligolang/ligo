module Test.Capabilities.SelectionRange
  ( unit_selectionRangeInsideCase
  ) where

import Data.Function ((&))
import System.FilePath ((</>))
import Test.HUnit (Assertion)

import AST.Capabilities.SelectionRange (findCoveringRanges)
import Range (Range (..), point)

import Test.Capabilities.Util (contractsDir)
import Test.FixedExpectations (shouldBe)
import Test.Util (readContract)

data SimpleRange = SimpleRange (Int, Int) (Int, Int)
  deriving stock (Eq, Show)

simplify :: Range -> SimpleRange
simplify (Range (l1, c1, _) (l2, c2, _) _) = SimpleRange (l1, c1) (l2, c2)

unit_selectionRangeInsideCase :: Assertion
unit_selectionRangeInsideCase = do
  tree <- readContract (contractsDir </> "heap.ligo")
  let position = point 16 8
      results = findCoveringRanges tree position
              & map simplify
  results `shouldBe` [ SimpleRange (16, 8) (16, 12)
                     , SimpleRange (16, 8) (16, 16)
                     , SimpleRange (16, 8) (16, 21)
                     , SimpleRange (15, 6) (18, 9)
                     , SimpleRange (14, 4) (18, 9)
                     , SimpleRange (11, 3) (21, 4)
                     , SimpleRange (11, 3) (21, 11)
                     , SimpleRange (10, 1) (21, 11)
                     , SimpleRange (1, 1) (107, 1)
                     ]
