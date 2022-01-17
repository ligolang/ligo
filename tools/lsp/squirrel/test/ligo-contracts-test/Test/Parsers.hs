module Test.Parsers
  ( test_okayContracts
  , test_badContracts
  ) where

import AST (Fallback, parseContracts, srcPath)
import Progress (noProgress)

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
      , "modules_env.mligo"
      , "module_contract_simple.mligo"
      , "module_contract_complex.mligo"
      , "warning_duplicate.mligo"
      , "interpreter_tests/nesting_modules.mligo"
      , "interpreter_tests/test_imported.mligo"
      , "interpreter_tests/test_importer.mligo" -- Also needs LIGO-204
      , "interpreter_tests/test_module.mligo"
      , "modules_and_free_vars/nested_modules.mligo"
      , "modules_and_free_vars/simple.mligo"
      , "polymorphism/modules.mligo"
      , "polymorphism/modules.religo"
      , "remove_unused_module.mligo"
      , "build/module_scoping_bug.mligo"
      , "aggregation/bug_alias.mligo"
      , "aggregation/bug_alias2.mligo"
      , "aggregation/bug_alias3.mligo"
      , "aggregation/bug_alias4.mligo"
      , "aggregation/bug_alias5.mligo"
      , "aggregation/bug_alias6.mligo"
      , "aggregation/bug_alias7.mligo"
      , "aggregation/bug_alias8.mligo"
      , "aggregation/bug_alias9.mligo"
      , "aggregation/bug_alias10.mligo"
      , "aggregation/bug_alias11.mligo"
      , "aggregation/bug_alias12.mligo"
      , "aggregation/bug_alias13.mligo"

        -- LIGO-204
      , "build/C_test.mligo"
      , "build/b.mligo"
      , "build/B.mligo"
      , "build/B1.mligo"
      , "build/C.mligo"
      , "build/D.mligo"
      , "build/E.mligo"
      , "build/cycle_A.mligo"
      , "build/cycle_B.mligo"
      , "build/cycle_C.mligo"
      , "build/type_B.mligo"
      , "build/Xfoo.mligo"
      , "build/Xmain.mligo"
      , "interpreter_tests/A.mligo"
      , "interpreter_tests/C.mligo"
      , "interpreter_tests/imported_modules/a.mligo"
      , "interpreter_tests/imported_modules/b.mligo"
      , "interpreter_tests/imported_modules/main.mligo"
      , "interpreter_tests/imported_modules/test.mligo"
      , "interpreter_tests/test_many_imports.mligo"
      , "polymorphism/use_error.mligo" -- polymorphism/* tests also depend on
      , "polymorphism/use_monad.mligo" --   LIGO-331
      , "polymorphism/use_monad_set.mligo"
      , "polymorphism/use_nelist.mligo"

        -- LIGO-331
      , "polymorphism/cases_annotation1.mligo"
      , "polymorphism/cases_annotation2.mligo"
      , "polymorphism/comb.mligo"
      , "polymorphism/ctrct.mligo"
      , "polymorphism/error_monad.mligo"
      , "polymorphism/lambda.mligo"
      , "polymorphism/list_monad.mligo"
      , "polymorphism/map.mligo"
      , "polymorphism/module_k.mligo"
      , "polymorphism/nelist.mligo"
      , "polymorphism/set_monad.mligo"
      , "polymorphism/test.mligo"

        -- LIGO-404
      , "deep_pattern_matching/pm_test.religo"
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
  allContracts <- parseContracts (pure . srcPath) noProgress contractsDir
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
