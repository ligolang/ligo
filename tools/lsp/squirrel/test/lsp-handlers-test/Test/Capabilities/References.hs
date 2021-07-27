module Test.Capabilities.References
  ( unit_references
  , unit_close_open_docs
  ) where

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

unit_close_open_docs :: Assertion
unit_close_open_docs = do
  let b1fp = "includes" </> "B1.ligo"
  let b2fp = "includes" </> "B2" </> "B2.ligo"
  let b3fp = "includes" </> "B3.ligo"

  (refs1, refs2, refs3) <- runHandlersTest contractsDir do
    doc1 <- openLigoDoc b2fp
    List refs1 <- getReferences doc1 (Position 2 11) True
    closeDoc doc1

    -- We still need to get the same references even after closing and opening
    -- the document. Checks whether `RIO.load` works as intended.
    doc2 <- openLigoDoc b2fp
    List refs2 <- getReferences doc2 (Position 2 11) True
    closeDoc doc2

    -- Should not vary even when extracting from another file.
    doc3 <- openLigoDoc b1fp
    List refs3 <- getReferences doc3 (Position 2 20) True
    closeDoc doc3

    pure (sort refs1, sort refs2, sort refs3)

  b1 <- filePathToUri <$> makeAbsolute (contractsDir </> b1fp)
  b2 <- filePathToUri <$> makeAbsolute (contractsDir </> b2fp)
  b3 <- filePathToUri <$> makeAbsolute (contractsDir </> b3fp)
  refs1 `shouldBe`
    [ Location b1 (Range (Position 2 20) (Position 2 22))
    , Location b2 (Range (Position 2 11) (Position 2 13))
    , Location b3 (Range (Position 0  6) (Position 0  8))
    ]

  refs2 `shouldBe` refs1
  refs3 `shouldBe` refs1
