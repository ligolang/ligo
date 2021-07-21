module Test.Capabilities.References (unit_references) where

import Data.List (sort)
import Language.LSP.Test
import Language.LSP.Types (Location (..), Position (..), Range (..), filePathToUri, List (..))
import System.Directory (makeAbsolute)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.Util (openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "find"

unit_references :: Assertion
unit_references = do
  let filename = "heap.ligo"

  refs <- fmap (\(List x) -> x) $ runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    getReferences doc (Position 7 9) True

  filepath <- makeAbsolute (contractsDir </> filename)
  let uri = filePathToUri filepath
  sort refs `shouldBe` fmap (Location uri)
    [ Range (Position 7 9) (Position 7 16)
    , Range (Position 11 29) (Position 11 36)
    , Range (Position 24 30) (Position 24 37)
    , Range (Position 68 30) (Position 68 37)
    ]
