module Test.Parsers
  ( test_okayContracts
  , test_badContracts
  ) where

import Control.Exception.Safe (try)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Duplo (HandlerFailed (..))
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Test.FixedExpectations (Expectation, HasCallStack, expectationFailure)
import Test.Util (readContract)

contractsDir :: FilePath
contractsDir = "../../../src/test/contracts"

okayContractsDirs :: [FilePath]
okayContractsDirs = contractsDir : map (contractsDir </>) rest
  where
    rest = [ "basic_multisig/"
           , "get_scope_tests/"
           , "negative/"
           ]

badContractsDirs :: [FilePath]
badContractsDirs = []

getContractsWithExtension :: String -> FilePath -> IO [FilePath]
getContractsWithExtension ext dir = listDirectory dir
                                <&> filter (ext `isSuffixOf`)
                                <&> map (dir </>)

getOkayContractsWithExtension :: String -> IO [FilePath]
getOkayContractsWithExtension ext =
  foldMap (getContractsWithExtension ext) okayContractsDirs

getBadContractsWithExtension :: String -> IO [FilePath]
getBadContractsWithExtension ext
  = foldMap (getContractsWithExtension ext) badContractsDirs

getOkayContracts :: IO [FilePath]
getOkayContracts =
  foldMap getOkayContractsWithExtension [".ligo", ".mligo", "religo"]

getBadContracts :: IO [FilePath]
getBadContracts =
  foldMap getBadContractsWithExtension [".ligo", ".mligo", "religo"]

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

checkFile :: HasCallStack => Bool -> FilePath -> Expectation
checkFile shouldBeOkay path = do
  res <- try (readContract path)
  case (shouldBeOkay, res) of
    (True, Left (err :: HandlerFailed)) -> expectationFailure $
      "Parsing failed, but it shouldn't have." <>
      "File: " <> path <> ". Error: " <> show err <> "."
    (False, Right _) -> expectationFailure $
      "Parsing succeeded, but it shouldn't have." <>
      "File: " <> path <> "."
    _ -> pure ()
