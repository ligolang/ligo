{-# LANGUAGE RecordWildCards #-}

module Test.Capabilities.Completion
  ( test_completion
  ) where

import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import AST.Capabilities.Completion (Completion (..), complete)
import AST.Scope.Fallback (Fallback)
import Range (point)

import qualified Test.Capabilities.Util (contractsDir)
import Test.FixedExpectations (shouldBe)
import Test.Util (readContractWithScopes)
import Test.Util.LigoEnv ()

contractsDir :: FilePath
contractsDir = Test.Capabilities.Util.contractsDir </> "completion"

data TestInfo = TestInfo
  { tiContract :: String
  , tiPosition :: (Int, Int)
  , tiExpected :: [Completion]
  }

caseInfos :: [TestInfo]
caseInfos =
  [ TestInfo
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
  ]

test_completion :: TestTree
test_completion = testGroup "Completion" testCases
  where
    testCases = map makeTestCase caseInfos
    makeTestCase info = testCase (tiContract info) (makeTest info)

    makeTest TestInfo{..} = do
      tree <- readContractWithScopes @Fallback (contractsDir </> tiContract)
      let position = uncurry point tiPosition
          results = complete position tree
      results `shouldBe` Just tiExpected
