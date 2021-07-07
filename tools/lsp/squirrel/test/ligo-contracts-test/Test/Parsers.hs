module Test.Parsers
  ( test_okayContracts
  , test_badContracts
  ) where

import AST (parseContracts, srcPath)

import Data.List (isPrefixOf)
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
      , "heap-instance.ligo"
      , "modules.ligo"
      , "assert.religo"
      , "warning_duplicate.mligo"
      , "deep_pattern_matching/pm_test.religo"
      , "vars_consts/multiple_vars.ligo"
      , "tutorials/inter-contract-calls/PausableToken.ligo"
      , "tutorials/inter-contract-calls/CreateAndCall.religo"
      , "build/B.mligo"
      , "build/C.mligo"
      , "build/D.mligo"
      , "build/E.mligo"
      , "build/cycle_A.mligo"
      , "build/cycle_B.mligo"
      , "build/cycle_C.mligo"
      , "build/type_B.mligo"
      , "interpreter_tests/test_subst_with_storage_from_file.mligo"
      , "interpreter_tests/test_now_from_file.mligo"
      , "interpreter_tests/test_fail_from_file.mligo"
      , "interpreter_tests/test_example_from_file.mligo"
      , "interpreter_tests/compile_expr_from_file.mligo"
      ]

okayIgnoreDirs :: [FilePath]
okayIgnoreDirs = map (contractsDir </>) compilerTests
  where
    compilerTests =
      [ -- TODO: Figure out which negative tests are for parsing and which are not
        "negative"
      , "tutorials"
      ]

badContractsDirs :: [FilePath]
badContractsDirs = []

getBadContractsWithExtension :: String -> IO [FilePath]
getBadContractsWithExtension ext
  = foldMap (getContractsWithExtension ext []) badContractsDirs

getOkayContracts :: IO [FilePath]
getOkayContracts = do
  allContracts <- parseContracts (pure . srcPath) contractsDir
  pure $ filter (\x -> not $ any (`isPrefixOf` x) okayIgnoreDirs)
       $ filter (`notElem` okayIgnoreContracts)
         allContracts

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
