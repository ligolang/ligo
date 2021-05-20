module Test.Parsers
  ( test_okayContracts
  , test_badContracts
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Test.Common.Util (getContractsWithExtension, supportedExtensions)
import Test.Common.Util.Parsers (checkFile)

okayIgnoreContracts :: [FilePath]
okayIgnoreContracts = []

okayContractsDirs :: [FilePath]
okayContractsDirs = ["test/contracts/bugs"]

badContractsDirs :: [FilePath]
badContractsDirs = ["test/contracts/bad"]

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
