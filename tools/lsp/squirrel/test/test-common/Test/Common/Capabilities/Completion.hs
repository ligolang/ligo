{-# LANGUAGE RecordWildCards #-}

module Test.Common.Capabilities.Completion
  ( completionDriver
  , caseInfos
  ) where

import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import AST.Capabilities.Completion (Completion (..), complete)
import AST.Scope (HasScopeForest)
import Range (point)

import qualified Test.Common.Capabilities.Util (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.Util (readContractWithScopes)

contractsDir :: FilePath
contractsDir = Test.Common.Capabilities.Util.contractsDir </> "completion"

data TestInfo = TestInfo
  { tiContract :: String
  , tiPosition :: (Int, Int)
  , tiExpected :: [Completion]
  }

caseInfos :: [TestInfo]
caseInfos =
  [ TestInfo
    { tiContract = "no-prefix.ligo"
    , tiPosition = (1, 46)
    , tiExpected = []
    }
  , TestInfo
    { tiContract = "yes-prefix.ligo"
    , tiPosition = (1, 48)
    , tiExpected = [Completion{ cName = "parameter", cType = "int", cDoc = "" }]
    }

  , TestInfo
    { tiContract = "type-attribute.ligo"
    , tiPosition = (15, 35)
    , tiExpected = [ Completion{ cName = "id", cType = "nat", cDoc = "" }
                   , Completion {cName = "is_admin", cType = "bool", cDoc = ""}
                   ]
    }
  , TestInfo
    { tiContract = "type-attribute.mligo"
    , tiPosition = (13, 33)
    , tiExpected = [ Completion{ cName = "id", cType = "nat", cDoc = "" }
                   , Completion {cName = "is_admin", cType = "bool", cDoc = ""}
                   ]
    }
  , TestInfo
    { tiContract = "type-attribute.religo"
    , tiPosition = (13, 33)
    , tiExpected = [ Completion{ cName = "id", cType = "nat", cDoc = "" }
                   , Completion {cName = "is_admin", cType = "bool", cDoc = ""}
                   ]
    }

  , TestInfo
    { tiContract = "type-constructor.ligo"
    , tiPosition = (5, 21)
    , tiExpected = [Completion{ cName = "Increment", cType = "action", cDoc = "" }]
    }
  , TestInfo
    { tiContract = "type-constructor.mligo"
    , tiPosition = (5, 19)
    , tiExpected = [Completion{ cName = "Increment", cType = "action", cDoc = "" }]
    }
  , TestInfo
    { tiContract = "type-constructor.religo"
    , tiPosition = (5, 19)
    , tiExpected = [Completion{ cName = "Increment", cType = "action", cDoc = "" }]
    }

  , TestInfo
    { tiContract = "unfinished-field-name.mligo"
    , tiPosition = (8, 27)
    , tiExpected = [Completion {cName = "sum", cType = "int", cDoc = ""}]
    }

  , TestInfo
    { tiContract = "nested-fields.ligo"
    , tiPosition = (21, 37)
    , tiExpected = [Completion {cName = "series", cType = "int", cDoc = ""}]
    }
  , TestInfo
    { tiContract = "nested-fields.mligo"
    , tiPosition = (18, 36)
    , tiExpected = [Completion {cName = "series", cType = "int", cDoc = ""}]
    }
  , TestInfo
    { tiContract = "nested-fields.religo"
    , tiPosition = (18, 36)
    , tiExpected = [Completion {cName = "series", cType = "int", cDoc = ""}]
    }
  ]

makeTest :: forall parser. HasScopeForest parser IO => TestInfo -> IO ()
makeTest TestInfo{..} = do
  tree <- readContractWithScopes @parser (contractsDir </> tiContract)
  let position = uncurry point tiPosition
      results = complete position tree
  results `shouldBe` Just tiExpected

completionDriver :: forall parser. HasScopeForest parser IO => [TestInfo] -> TestTree
completionDriver = testGroup "Completion" . map makeTestCase
  where
    makeTestCase info = testCase (tiContract info) (makeTest @parser info)
