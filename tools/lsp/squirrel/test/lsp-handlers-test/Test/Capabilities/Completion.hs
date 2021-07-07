module Test.Capabilities.Completion (unit_completion) where

import Control.Lens ((^.))
import Data.List (sort)
import Language.LSP.Test
import Language.LSP.Types (Position (..))
import Language.LSP.Types.Lens (label)
import System.FilePath ((</>))

import Test.HUnit (Assertion)

import Test.Common.Util (openLigoDoc, runHandlersTest)
import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "completion"

unit_completion :: Assertion
unit_completion = do
  let filename = "type-attribute.ligo"

  completions <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    getCompletions doc (Position 14 34)

  sort (fmap (^. label) completions) `shouldBe` ["id", "is_admin"]
