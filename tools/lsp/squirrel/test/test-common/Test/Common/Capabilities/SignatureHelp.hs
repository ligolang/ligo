{-# LANGUAGE RecordWildCards #-}

module Test.Common.Capabilities.SignatureHelp
  ( simpleFunctionCallDriver
  ) where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Language.LSP.Types qualified as J
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import AST.Capabilities.SignatureHelp
  ( SignatureInformation (..), findSignature, makeSignatureLabel, toLspParameters
  )
import AST.Parser (parseContractsWithDependenciesScopes, parsePreprocessed)
import AST.Pretty (ppToText)
import AST.Scope.Common (HasScopeForest, contractTree, lookupContract)
import AST.Scope.ScopedDecl
  ( Parameter (..), Pattern (..), Type (..), lppLigoLike
  )
import AST.Skeleton (nestedLIGO)
import Extension (getExt)
import Range (Range, point)

import Test.Common.Capabilities.Util (contractsDir)
import Test.Common.FixedExpectations (shouldBe)

data TestInfo = TestInfo
  { tiContract :: String
  , tiCursor :: Range
  , tiFunction :: Text
  , tiParameters :: [Parameter]
  , tiActiveParamNo :: Int
  }

caseInfos :: [TestInfo]
caseInfos =
  [ TestInfo
    { tiContract = "all-okay.ligo"
    , tiCursor = point 3 44
    , tiFunction = "bar"
    , tiParameters = [ParameterBinding (IsVar "i") (AliasType "int")]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "no-params.ligo"
    , tiCursor = point 3 44
    , tiFunction = "bar"
    , tiParameters = [ParameterBinding (IsVar "i") (AliasType "int")]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "unclosed-paren.ligo"
    , tiCursor = point 3 44
    , tiFunction = "bar"
    , tiParameters = [ParameterBinding (IsVar "i") (AliasType "int")]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "no-semicolon-in-block-after-var-decl.ligo"
    , tiCursor = point 5 24
    , tiFunction = "bar"
    , tiParameters = [ParameterBinding (IsVar "i") (AliasType "int")]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "no-semicolon-in-block-after-const-decl.ligo"
    , tiCursor = point 5 24
    , tiFunction = "bar"
    , tiParameters = [ParameterBinding (IsVar "i") (AliasType "int")]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "active-parameter-is-2nd.ligo"
    , tiCursor = point 3 47
    , tiFunction = "bar"
    , tiParameters = [ParameterBinding (IsVar "a") (AliasType "int"), ParameterBinding (IsVar "b") (AliasType "int")]
    , tiActiveParamNo = 1
    }

  , TestInfo
    { tiContract = "all-okay.mligo"
    , tiCursor = point 3 32
    , tiFunction = "bar"
    , tiParameters = [ParameterPattern (IsAnnot (IsVar "i") (AliasType "int"))]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "no-params.mligo"
    , tiCursor = point 3 32
    , tiFunction = "bar"
    , tiParameters = [ParameterPattern (IsAnnot (IsVar "i") (AliasType "int"))]
    , tiActiveParamNo = 0
    }

  , TestInfo
    { tiContract = "all-okay.religo"
    , tiCursor = point 3 35
    , tiFunction = "bar"
    , tiParameters = [ParameterPattern (IsAnnot (IsVar "i") (AliasType "int"))]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "no-params.religo"
    , tiCursor = point 3 35
    , tiFunction = "bar"
    , tiParameters = [ParameterPattern (IsAnnot (IsVar "i") (AliasType "int"))]
    , tiActiveParamNo = 0
    }
  , TestInfo
    { tiContract = "LIGO-271.mligo"
    , tiCursor = point 3 30
    , tiFunction = "foo"
    , tiParameters =
      [ ParameterPattern (IsAnnot (IsVar "a") (AliasType "nat"))
      , ParameterPattern (IsAnnot (IsVar "b") (AliasType "nat"))
      ]
    , tiActiveParamNo = 1
    }
  , TestInfo
    { tiContract = "LIGO-271-curried.mligo"
    , tiCursor = point 3 17
    , tiFunction = "foo"
    , tiParameters =
      [ ParameterPattern (IsAnnot (IsVar "a") (AliasType "int"))
      , ParameterPattern (IsAnnot (IsVar "b") (AliasType "nat"))
      ]
    , tiActiveParamNo = 1
    }
  ]

simpleFunctionCallDriver :: forall parser. HasScopeForest parser IO => IO TestTree
simpleFunctionCallDriver = do
  graph <- parseContractsWithDependenciesScopes @parser parsePreprocessed (contractsDir </> "signature-help")
  pure $ testGroup "Signature Help on a simple function call" $ map (makeTestCase graph) caseInfos
  where
    makeTestCase graph info = testCase (tiContract info) (makeTest graph info)

    makeTest graph TestInfo{..} = do
      let filepath = contractsDir </> "signature-help" </> tiContract
      let tree = contractTree $ fromJust $ lookupContract filepath graph
      dialect <- getExt filepath
      let result = findSignature (tree ^. nestedLIGO) tiCursor
      let label = makeSignatureLabel dialect tiFunction (map (ppToText . lppLigoLike dialect) tiParameters)
      result `shouldBe`
        Just ( SignatureInformation
               { _label = label
               , _documentation = Just $ J.SignatureHelpDocString ""
               , _parameters = Just . J.List $ toLspParameters dialect tiParameters
               , _activeParameter = Nothing
               }
             , tiActiveParamNo
             )
