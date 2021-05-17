module Test.Capabilities.SelectionRange
  ( unit_selectionRangeInsideCase
  ) where

import Control.Lens ((^.))
import Data.Function ((&))
import System.FilePath ((</>))
import Test.HUnit (Assertion)

import AST.Capabilities.SelectionRange (findCoveringRanges)
import AST.Skeleton (nestedLIGO)
import Range (Range (..), point)

import qualified Test.Common.Capabilities.Util as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.Util (readContract)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "selection-range"

data SimpleRange = SimpleRange (Int, Int) (Int, Int)
  deriving stock (Eq, Show)

simplify :: Range -> SimpleRange
simplify (Range (l1, c1, _) (l2, c2, _) _) = SimpleRange (l1, c1) (l2, c2)

unit_selectionRangeInsideCase :: Assertion
unit_selectionRangeInsideCase = do
  tree <- readContract (contractsDir </> "heap.ligo")
  let position = point 16 8
      results = findCoveringRanges (tree ^. nestedLIGO) position
              & map simplify
  results `shouldBe` [ SimpleRange (16, 8) (16, 12)
                     , SimpleRange (16, 8) (16, 16)
                     , SimpleRange (16, 8) (16, 21)
                     , SimpleRange (15, 6) (18, 9)
                     , SimpleRange (14, 4) (18, 9)
                     , SimpleRange (11, 3) (21, 4)
                     , SimpleRange (11, 3) (21, 11)
                     , SimpleRange (10, 1) (21, 11)
                     , SimpleRange (1, 1) (105, 1)
                     ]
