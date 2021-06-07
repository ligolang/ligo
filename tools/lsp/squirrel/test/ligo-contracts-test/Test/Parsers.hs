module Test.Parsers
  ( test_okayContracts
  , test_badContracts
  ) where

import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Test.Common.Util (contractsDir, getContractsWithExtension, supportedExtensions)
import Test.Common.Util.Parsers (checkFile)

okayIgnoreContracts :: [FilePath]
okayIgnoreContracts = (contractsDir </>) <$> ignore
  where
    ignore =
      [ "modules.religo"
      , "get_scope_tests/module.mligo"
      , "uncurry_contract.mligo"
      , "existential.mligo"
      , "protocol_dalphanet.mligo"
      , "modules.mligo"
      , "heap.ligo"
      , "modules.ligo"
      , "assert.religo"
      , "warning_duplicate.mligo"
      ]

okayContractsDirs :: [FilePath]
okayContractsDirs = contractsDir : map (contractsDir </>) compilerTests
  where
    compilerTests =
      [ "basic_multisig/"
      , "get_scope_tests/"
      , "positive"
      -- TODO: Figure out which negative tests are for parsing and which are not
      -- , "negative/"
      ]

badContractsDirs :: [FilePath]
badContractsDirs = []

getOkayContractsWithExtension :: String -> IO [FilePath]
getOkayContractsWithExtension ext =
  foldMap (getContractsWithExtension ext okayIgnoreContracts) okayContractsDirs

getBadContractsWithExtension :: String -> IO [FilePath]
getBadContractsWithExtension ext
  = foldMap (getContractsWithExtension ext []) badContractsDirs

getOkayContracts :: IO [FilePath]
getOkayContracts = foldMap getOkayContractsWithExtension supportedExtensions

getBadContracts :: IO [FilePath]
getBadContracts = foldMap getBadContractsWithExtension supportedExtensions

test_okayContracts :: IO TestTree
test_okayContracts
  = testGroup "Parsers should parse these contracts" <$> testCases
  where
    testCases = map makeTestCase <$> getOkayContracts
    makeTestCase contractPath = testCase contractPath (checkFile True contractPath)

test_badContracts :: IO TestTree
test_badContracts
  = testGroup "Parsers should not parse these contracts" <$> testCases
  where
    testCases = map makeTestCase <$> getBadContracts
    makeTestCase contractPath = testCase contractPath (checkFile False contractPath)
