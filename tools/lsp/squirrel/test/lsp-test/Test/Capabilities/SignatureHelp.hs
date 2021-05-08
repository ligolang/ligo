{-# LANGUAGE RecordWildCards #-}

module Test.Capabilities.SignatureHelp
  ( test_simpleFunctionCall
  ) where

import Control.Lens ((^.))
import Data.Text (Text)
import qualified Language.LSP.Types as J
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

import AST (Fallback)
import AST.Capabilities.SignatureHelp
  (SignatureInformation (..), findSignature, makeSignatureLabel, toLspParameter)
import AST.Scope.Common (HasScopeForest)
import AST.Skeleton (nestedLIGO)
import Extension (getExt)
import Range (Range, point)

import Test.Capabilities.Util (contractsDir)
import Test.FixedExpectations (shouldBe)
import Test.Util (readContractWithScopes)
import Test.Util.LigoEnv ()

data TestInfo = TestInfo
  { tiContract :: String
  , tiCursor :: Range
  , tiFunction :: Text
  , tiParameters :: [Text]
  , tiActiveParamNo :: Int
  }

caseInfos :: [TestInfo]
caseInfos =
  [ TestInfo
    { tiContract = "all-okay.ligo"
    , tiCursor = point 3 44
    , tiFunction = "bar"
    , tiParameters = ["const i : int"]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "no-params.ligo"
    , tiCursor = point 3 44
    , tiFunction = "bar"
    , tiParameters = ["const i : int"]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "unclosed-paren.ligo"
    , tiCursor = point 3 44
    , tiFunction = "bar"
    , tiParameters = ["const i : int"]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "no-semicolon-in-block-after-var-decl.ligo"
    , tiCursor = point 5 24
    , tiFunction = "bar"
    , tiParameters = ["const i : int"]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "no-semicolon-in-block-after-const-decl.ligo"
    , tiCursor = point 5 24
    , tiFunction = "bar"
    , tiParameters = ["const i : int"]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "active-parameter-is-2nd.ligo"
    , tiCursor = point 3 47
    , tiFunction = "bar"
    , tiParameters = ["const a : int", "const b : int"]
    , tiActiveParamNo = 1
    }

  , TestInfo
    { tiContract = "all-okay.mligo"
    , tiCursor = point 3 32
    , tiFunction = "bar"
    , tiParameters = ["(i : int)"]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "no-params.mligo"
    , tiCursor = point 3 32
    , tiFunction = "bar"
    , tiParameters = ["(i : int)"]
    , tiActiveParamNo = 0
    }

  , TestInfo
    { tiContract = "all-okay.religo"
    , tiCursor = point 3 35
    , tiFunction = "bar"
    , tiParameters = ["(i : int)"]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "no-params.religo"
    , tiCursor = point 3 35
    , tiFunction = "bar"
    , tiParameters = ["(i : int)"]
    , tiActiveParamNo = 0
    }
  ]

test_simpleFunctionCall :: TestTree
test_simpleFunctionCall
  = testGroup "Signature Help on a simple function call"
    [fallbackGroup] -- TODO maybe add FromCompiler scopes testing
  where
    fallbackGroup = testGroup "Fallback scopes" (testCases @Fallback)

    testCases :: forall parser. HasScopeForest parser IO => [TestTree]
    testCases = map (makeTestCase @parser) caseInfos

    makeTestCase
      :: forall parser. HasScopeForest parser IO => TestInfo -> TestTree
    makeTestCase info = testCase (tiContract info) (makeTest @parser info)

    makeTest
      :: forall parser. HasScopeForest parser IO => TestInfo -> Assertion
    makeTest TestInfo{..} = do
      let filepath = contractsDir </> "signature-help" </> tiContract
      tree <- readContractWithScopes @parser filepath
      dialect <- getExt filepath
      let result = findSignature (tree ^. nestedLIGO) tiCursor
      result `shouldBe`
        Just ( SignatureInformation
               { _label = makeSignatureLabel dialect tiFunction tiParameters
               , _documentation = Just ""
               , _parameters = Just . J.List $ map toLspParameter tiParameters
               }
             , tiActiveParamNo
             )
