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
      [ -- LIGO-204
        "build/C_test.mligo"
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
      , "interpreter_tests/test_importer.mligo"
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
      , "polymorphism/modules.mligo"
      , "polymorphism/modules.religo"

        -- LIGO-446
      , "match.ligo"
      , "loop.ligo"
      , "failwith.ligo"
      , "type_puning.ligo"
      , "heap.ligo"
      , "tutorials/optimisation/LargeEntrypointV2.ligo"
      , "tutorials/optimisation/EffectfulBindingV1.ligo"
      , "tutorials/optimisation/EffectfulBindingV2.ligo"
      , "tutorials/optimisation/LargeEntrypointV1.ligo"
      , "tutorials/optimisation/EffectfulBindingV3.ligo"
      , "multisig-v2.ligo"
      , "polymorphism/comb.ligo" -- LIGO-331
      , "pascaligo_long_remove.ligo"
      , "patch_long_path.ligo"
      , "long_remove.ligo"
      , "record.ligo"
      , "basic_multisig/multisig.ligo"
      , "loop14.ligo"
      , "loop17.ligo"
      , "multisig.ligo"
      , "hashlock.ligo"
      , "id.ligo"
      , "long_assign.ligo"

        -- LIGO-432
      , "tutorials/inter-contract-calls/CreateAndCall.religo"
      ]

okayIgnoreDirs :: [FilePath]
okayIgnoreDirs = map (contractsDir </>) compilerTests
  where
    compilerTests =
      [ -- TODO: Figure out which negative tests are for parsing and which are not
        "negative"
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
