module Test.Capabilities.SignatureHelp
  ( test_simpleFunctionCall
  ) where

import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

import AST (Fallback)
import AST.Capabilities.SignatureHelp
  (ParameterInformation (..), SignatureInformation (..), findSignatures, makeSignatureLabel)
import AST.Scope.Common (HasScopeForest)
import Extension (ElimExt (..), onExt)
import Range (Range, point)

import Test.Capabilities.Util (contractsDir)
import Test.FixedExpectations (shouldBe)
import Test.Util (readContractWithScopes)

test_simpleFunctionCall :: TestTree
test_simpleFunctionCall
  = testGroup "Signature Help on a simple function call"
    [fallbackGroup] -- TODO maybe add FromCompiler scopes testing
  where
    functionCallContracts =
      -- pascaligo
      [ ("all-okay.ligo", point 3 44)
      , ("no-params.ligo", point 3 44)
      , ("unclosed-paren.ligo", point 3 44)
      , ("no-semicolon-in-block-after-var-decl.ligo", point 5 24)
      , ("no-semicolon-in-block-after-const-decl.ligo", point 5 24)

      -- camligo
      , ("all-okay.mligo", point 3 32)
      , ("no-params.mligo", point 3 32)

      -- reasonligo
      , ("all-okay.religo", point 3 35)
      , ("no-params.religo", point 3 35)
      ]

    fallbackGroup = testGroup "Fallback scopes" (testCases @Fallback)

    testCases :: forall parser. HasScopeForest parser IO => [TestTree]
    testCases = map (uncurry (makeTestCase @parser)) functionCallContracts

    makeTestCase
      :: forall parser. HasScopeForest parser IO => String -> Range -> TestTree
    makeTestCase filename = testCase filename . makeTest @parser filename

    makeTest
      :: forall parser. HasScopeForest parser IO
      => FilePath -> Range -> Assertion
    makeTest filename position = do

      parameterLabel <- flip onExt filename ElimExt
        { eePascal = "(const i : int)"
        , eeCaml = "(i : int)"
        , eeReason = "(i : int)"
        }
      let parameter = ParameterInformation
            { _label = parameterLabel
            , _documentation = Nothing
            }

      let filepath = contractsDir </> "function-call" </> filename
      tree <- readContractWithScopes @parser filepath
      let results = findSignatures tree position
      results `shouldBe` [ SignatureInformation
                           { _label = makeSignatureLabel "bar" [parameterLabel]
                           , _documentation = Just ""
                           , _parameters = Just [parameter]
                           }
                         ]
