module Test.Capabilities.SelectionRange
  ( unit_selectionRange
  ) where

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

unit_selectionRange :: Assertion
unit_selectionRange = do
  let filepath = contractsDir </> "simple.mligo"
  tree <- readContract filepath
  let position = (point 2 3){_rFile = filepath}
      results = findCoveringRanges (tree ^. nestedLIGO) position
              & map simplify
  results `shouldMatchList`
    [ SimpleRange (2, 3) (2, 4) filepath
    , SimpleRange (2, 3) (2, 9) filepath
    , SimpleRange (1, 1) (2, 9) filepath
    , SimpleRange (1, 1) (3, 1) filepath
    ]
