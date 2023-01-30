module Test.Capabilities.Definition
  ( unit_definition_jsligo
  ) where

import System.Directory (makeAbsolute)
import System.FilePath ((</>))

import Language.LSP.Test
import Language.LSP.Types (Location (..), Position (..), Range (..), filePathToUri, type (|?) (..))
import Test.HUnit (Assertion)

import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.LSP (openLigoDoc, runHandlersTest)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "definition"

unit_definition_jsligo :: Assertion
unit_definition_jsligo = do
  let filename = "increment.jsligo"

  eitherDefs <- runHandlersTest contractsDir $ do
    doc <- openLigoDoc filename
    getDefinitions doc (Position 2 2)

  filepath <- makeAbsolute (contractsDir </> filename)
  let uri = filePathToUri filepath
  eitherDefs `shouldBe` InL [Location uri (Range (Position 1 6) (Position 1 12))]
