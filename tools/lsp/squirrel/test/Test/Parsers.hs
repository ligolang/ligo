module Test.Parsers
  ( test_okayContracts
  , test_badContracts
  ) where

import Control.Exception.Safe (catch, throwIO, try)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Duplo (HandlerFailed (..))
import Language.Haskell.TH.Syntax (liftString)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Test.FixedExpectations (Expectation, HasCallStack, expectationFailure)
import Test.Util (readContractWithMessages)

contractsDir :: FilePath
contractsDir =
  $(
    let
      getDir :: IO FilePath
      getDir = getEnv "CONTRACTS_DIR" `catch` \e ->
        if isDoesNotExistError e
        then pure "../../../src/test/contracts"
        else throwIO e
    in liftIO getDir >>= liftString
  )

okayContractsDirs :: [FilePath]
-- FIXME: need to fix all parser issues
okayContractsDirs = "test/contracts/bugs" : [] {- contractsDir : map (contractsDir </>) rest
  where
    rest = [ "basic_multisig/"
           , "get_scope_tests/"
           -- TODO: Figure out which negative tests are for parsing and which are not
           -- , "negative/"
           ]
-}

badContractsDirs :: [FilePath]
badContractsDirs = "test/contracts/bad" : map (contractsDir </>) rest
  where
    rest = []

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
  foldMap getOkayContractsWithExtension [".ligo", ".mligo", ".religo"]

getBadContracts :: IO [FilePath]
getBadContracts =
  foldMap getBadContractsWithExtension [".ligo", ".mligo", ".religo"]

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
checkFile True path = do
  res <- try (readContractWithMessages path)
  case res of
    Left (err :: HandlerFailed) -> expectationFailure $
      "Parsing failed, but it shouldn't have. " <>
      "Error: " <> show err <> "."
    Right (_tree, msgs) -> case msgs of
      (_ : _) -> expectationFailure $
        "Parsing failed, but it shouldn't have. " <>
        "Messages: " <> show msgs <> "."
      [] -> pure ()
checkFile False path = do
  res <- try @_ @HandlerFailed (readContractWithMessages path)
  case res of
    Right (_tree, []) -> expectationFailure $
      "Parsing succeeded, but it shouldn't have."
    _ -> pure ()
