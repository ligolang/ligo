module Test.Capabilities.Folding
  ( unit_folding_range
  , unit_folding_range_jsligo
  ) where

import Language.LSP.Test
import Language.LSP.Types
import Language.LSP.Types.Lens ()
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.LSP (getResponseResult, openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "folding-range"

-- lsp-test doesn't provide a function for testing folding range
getFoldingRanges :: TextDocumentIdentifier -> Session [FoldingRange]
getFoldingRanges doc =
  let params = FoldingRangeParams Nothing Nothing doc
  in (\(List x) -> x) . getResponseResult <$> request STextDocumentFoldingRange params

unit_folding_range :: Assertion
unit_folding_range = do
  let filename = "eq_bool.ligo"

  foldingRanges <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    getFoldingRanges doc
  foldingRanges `shouldBe`
    [ FoldingRange { _startLine = 0
                   , _startCharacter = Just 0
                   , _endLine = 4
                   , _endCharacter = Just 15
                   , _kind = Just FoldingRangeRegion
                   }
    , FoldingRange { _startLine = 1
                   , _startCharacter = Just 2
                   , _endLine = 4
                   , _endCharacter = Just 3
                   , _kind = Just FoldingRangeRegion
                   }
    , FoldingRange { _startLine = 3
                   , _startCharacter = Just 4
                   , _endLine = 3
                   , _endCharacter = Just 48
                   , _kind = Just FoldingRangeRegion
                   }
    ]

unit_folding_range_jsligo :: Assertion
unit_folding_range_jsligo = do
  let filename = "eq_bool.jsligo"

  foldingRanges <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    getFoldingRanges doc
  foldingRanges `shouldBe`
    [ FoldingRange { _startLine = 2
                   , _startCharacter = Just 11
                   , _endLine = 4
                   , _endCharacter = Just 1
                   , _kind = Just FoldingRangeRegion
                   }
    , FoldingRange { _startLine = 2
                   , _startCharacter = Just 44
                   , _endLine = 4
                   , _endCharacter = Just 1
                   , _kind = Just FoldingRangeRegion
                   }
    , FoldingRange { _startLine = 3
                   , _startCharacter = Just 2
                   , _endLine = 3
                   , _endCharacter = Just 48
                   , _kind = Just FoldingRangeRegion
                   }
    , FoldingRange { _startLine = 3
                   , _startCharacter = Just 14
                   , _endLine = 3
                   , _endCharacter = Just 29
                   , _kind = Just FoldingRangeRegion
                   }
    , FoldingRange { _startLine = 3
                   , _startCharacter = Just 35
                   , _endLine = 3
                   , _endCharacter = Just 48
                   , _kind = Just FoldingRangeRegion
                   }
    ]