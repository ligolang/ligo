module Test.Capabilities.SelectionRange
  ( unit_selectionRangeInsideCase
  ) where

import Control.Lens ((^.))
import Data.Function ((&))
import Language.LSP.Types qualified as J
import System.FilePath ((</>))
import Test.HUnit (Assertion)

import AST.Capabilities.SelectionRange (findCoveringRanges)
import AST.Skeleton (nestedLIGO)
import Range (Range (..), point)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldMatchList)
import Test.Common.Util (readContract)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "selection-range"

data SimpleRange = SimpleRange (J.UInt, J.UInt) (J.UInt, J.UInt) FilePath
  deriving stock (Eq, Show)

simplify :: Range -> SimpleRange
simplify (Range (l1, c1, _) (l2, c2, _) f) = SimpleRange (l1, c1) (l2, c2) f

unit_selectionRangeInsideCase :: Assertion
unit_selectionRangeInsideCase = do
  let filepath = contractsDir </> "heap.ligo"
  tree <- readContract filepath
  let position = (point 15 8){_rFile = filepath}
      results = findCoveringRanges (tree ^. nestedLIGO) position
              & map simplify
  results `shouldMatchList`
    [ SimpleRange (15,  8) ( 15, 12) filepath
    , SimpleRange (15,  8) ( 15, 16) filepath
    , SimpleRange (15,  8) ( 15, 21) filepath
    , SimpleRange (14,  6) ( 17,  7) filepath
    , SimpleRange (13,  4) ( 17,  7) filepath
    , SimpleRange (10, 46) ( 20,  4) filepath
    , SimpleRange (10, 46) ( 20, 11) filepath
    , SimpleRange (10,  1) ( 20, 11) filepath
    , SimpleRange ( 1,  1) (101,  1) filepath
    ]
