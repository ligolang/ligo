module Test.Capabilities.SignatureHelp
  ( test_simpleFunctionCall
  ) where

import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import AST (Fallback)
import AST.Capabilities.SignatureHelp (SignatureInformation (..), findSignatures)
import Range (point)

import Test.Capabilities.Util (contractsDir)
import Test.FixedExpectations (shouldBe)
import Test.Util (readContractWithScopes)

test_simpleFunctionCall :: TestTree
test_simpleFunctionCall
  = testGroup "Signature Help on a simple function call" testCases
  where
    testCases = map (uncurry makeTestCase) functionCallContracts
    functionCallContracts =
      [ ("all-okay.ligo", point 3 44)
      , ("no-params.ligo", point 3 44)
      , ("unclosed-paren.ligo", point 3 44)
      , ("no-semicolon-in-block-after-var-decl.ligo", point 5 24)
      , ("no-semicolon-in-block-after-const-decl.ligo", point 7 31)
      ]
    makeTestCase filename = testCase filename . makeTest filename

    makeTest filename position = do
      let filepath = contractsDir </> "function-call" </> filename
      tree <- readContractWithScopes @Fallback filepath
      let results = findSignatures tree position
      results `shouldBe` [ SignatureInformation
                           { _label = "bar"
                           , _documentation = Just ""
                           , _parameters = Nothing
                           }
                         ]
