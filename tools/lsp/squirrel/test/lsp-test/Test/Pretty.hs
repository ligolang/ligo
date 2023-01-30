module Test.Pretty
  ( unit_pretty_sum_type_jsligo
  , unit_pretty_sum_type_mligo
  ) where

import AST (SomeLIGO (..), contractTree, lppDialect, parse)
import Data.Text (strip)
import Log (NoLoggingT (runNoLoggingT), i)
import ParseTree (pathToSrc)
import System.FilePath ((</>))
import Test.Common.Capabilities.Util qualified as Common (contractsDir)
import Test.Common.Util (renderNoLineLengthLimit)
import Test.Hspec.Expectations (shouldBe)
import Test.HUnit (Assertion)

contractsDir :: FilePath
contractsDir = Common.contractsDir </> "pretty"

checkPretty :: FilePath -> Text -> Assertion
checkPretty fp expected = do
  contract <- runNoLoggingT $ parse =<< pathToSrc (contractsDir </> fp)
  let SomeLIGO lang code =  contractTree contract
  strip (renderNoLineLengthLimit $ lppDialect lang code) `shouldBe` expected

unit_pretty_sum_type_jsligo :: Assertion
unit_pretty_sum_type_jsligo = checkPretty "sum-type.jsligo"
  [i|type parameter = ["Increment", int] | ["Decrement", int] | ["Reset"]|]

unit_pretty_sum_type_mligo :: Assertion
unit_pretty_sum_type_mligo = checkPretty "sum-type.mligo"
  [i|type parameter = Increment of int | Decrement of int | Reset|]
