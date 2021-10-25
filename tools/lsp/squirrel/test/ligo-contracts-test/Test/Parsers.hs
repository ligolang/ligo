module Test.Parsers
  ( test_okayContracts
  , test_badContracts
  ) where

import AST (Fallback, parseContracts, srcPath)

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
      [ -- LIGO-157
        "modules.religo"
      , "get_scope_tests/module.mligo"
      , "uncurry_contract.mligo"
      , "modules.mligo"
      , "modules.ligo"
      , "warning_duplicate.mligo"
      , "interpreter_tests/test_imported.mligo"
      , "interpreter_tests/test_importer.mligo" -- Also needs LIGO-204
      , "interpreter_tests/test_module.mligo"
      , "modules_and_free_vars/nested_modules.mligo"
      , "modules_and_free_vars/simple.mligo"
      , "remove_unused_module.mligo"

        -- LIGO-309
      , "parametric_types.ligo"

        -- LIGO-204
      , "build/C_test.mligo"
      , "build/b.mligo"
      , "build/B.mligo"
      , "build/C.mligo"
      , "build/D.mligo"
      , "build/E.mligo"
      , "build/cycle_A.mligo"
      , "build/cycle_B.mligo"
      , "build/cycle_C.mligo"
      , "build/type_B.mligo"

        -- LIGO-309
      , "vars_consts/multiple_vars.ligo"

        -- LIGO-323
      , "heap-instance.ligo"

        -- LIGO-324
      , "interpreter_tests/compile_expr_from_file.mligo"

        -- Bad
      , "heap.ligo"  -- There is an accidental extra closing parenthesis.
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
    makeTestCase contractPath = testCase contractPath (checkFile @Fallback True contractPath)

test_badContracts :: IO TestTree
test_badContracts
  = testGroup "Parsers should not parse these contracts" <$> testCases
  where
    testCases = map makeTestCase <$> getBadContracts
    makeTestCase contractPath = testCase contractPath (checkFile @Fallback False contractPath)
