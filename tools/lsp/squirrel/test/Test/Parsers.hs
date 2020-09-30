module Test.Parsers
  ( unit_okayContracts
  , unit_badContracts
  ) where

import Control.Exception (try)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Duplo (HandlerFailed (..))
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec.Expectations (Expectation, HasCallStack, expectationFailure)
import Test.HUnit (Assertion)

import AST (parse)
import ParseTree (Source (Path))

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

unit_okayContracts :: Assertion
unit_okayContracts = getOkayContracts >>= mapM_ (checkFile True)

unit_badContracts :: Assertion
unit_badContracts = getBadContracts >>= mapM_ (checkFile False)

checkFile :: HasCallStack => Bool -> FilePath -> Expectation
checkFile shouldBeOkay path = do
  res <- try @HandlerFailed (readContract path)
  case (shouldBeOkay, res) of
    (True, Left err) -> expectationFailure $
      "Parsing failed, but it shouldn't have." <>
      "File: " <> path <> ". Error: " <> show err <> "."
    (False, Right _) -> expectationFailure $
      "Parsing succeeded, but it shouldn't have." <>
      "File: " <> path <> "."
    _ -> pure ()
