module Test.Parsers
  ( test_okayContracts
  , test_badContracts
  ) where

import AST (Fallback, scanContracts)

import Data.List (isPrefixOf)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Test.Common.Util (contractsDir, testDir)
import Test.Common.Util.Parsers (checkFile)

data TestContracts
  = TestDir
    { tdRoot :: FilePath
    , tdIgnoreFiles :: [FilePath]
    , tdIgnoreDirs :: [FilePath]
    }
  | TestContract FilePath

okayTests :: [TestContracts]
okayTests =
  [ TestDir
    { tdRoot = contractsDir
    , tdIgnoreFiles =
      [ -- LIGO-204
        "build" </> "C_test.mligo"
      , "build" </> "b.mligo"
      , "build" </> "B.mligo"
      , "build" </> "B1.mligo"
      , "build" </> "C.mligo"
      , "build" </> "D.mligo"
      , "build" </> "E.mligo"
      , "build" </> "cycle_A.mligo"
      , "build" </> "cycle_B.mligo"
      , "build" </> "cycle_C.mligo"
      , "build" </> "type_B.mligo"
      , "build" </> "Xfoo.mligo"
      , "build" </> "Xmain.mligo"
      , "interpreter_tests" </> "A.mligo"
      , "interpreter_tests" </> "C.mligo"
      , "interpreter_tests" </> "imported_modules" </> "a.mligo"
      , "interpreter_tests" </> "imported_modules" </> "b.mligo"
      , "interpreter_tests" </> "imported_modules" </> "main.mligo"
      , "interpreter_tests" </> "imported_modules" </> "test.mligo"
      , "interpreter_tests" </> "test_importer.mligo"
      , "interpreter_tests" </> "test_many_imports.mligo"
      , "polymorphism" </> "use_error.mligo" -- polymorphism/* tests also depend on
      , "polymorphism" </> "use_monad.mligo" --   LIGO-331
      , "polymorphism" </> "use_monad_set.mligo"
      , "polymorphism" </> "use_nelist.mligo"
      , "negative" </> "polymorphism" </> "use_error.mligo"
      , "view_import.mligo"

        -- LIGO-331
      , "polymorphism" </> "cases_annotation1.mligo"
      , "polymorphism" </> "cases_annotation2.mligo"
      , "polymorphism" </> "comb.mligo"
      , "polymorphism" </> "ctrct.mligo"
      , "polymorphism" </> "error_monad.mligo"
      , "polymorphism" </> "lambda.mligo"
      , "polymorphism" </> "list_monad.mligo"
      , "polymorphism" </> "map.mligo"
      , "polymorphism" </> "module_k.mligo"
      , "polymorphism" </> "nelist.mligo"
      , "polymorphism" </> "set_monad.mligo"
      , "polymorphism" </> "test.mligo"
      , "polymorphism" </> "modules.mligo"
      , "polymorphism" </> "modules.religo"
      , "polymorphism" </> "annotate.mligo"
      , "polymorphism" </> "same_vars.mligo"
      , "negative" </> "polymorphism" </> "cases_annotation.mligo"
      , "negative" </> "polymorphism" </> "constants.mligo"
      , "negative" </> "polymorphism" </> "error_monad.mligo"

        -- LIGO-446
      , "match.ligo"
      , "loop.ligo"
      , "failwith.ligo"
      , "type_puning.ligo"
      , "heap.ligo"
      , "tutorials" </> "optimisation" </> "LargeEntrypointV2.ligo"
      , "tutorials" </> "optimisation" </> "EffectfulBindingV1.ligo"
      , "tutorials" </> "optimisation" </> "EffectfulBindingV2.ligo"
      , "tutorials" </> "optimisation" </> "LargeEntrypointV1.ligo"
      , "tutorials" </> "optimisation" </> "EffectfulBindingV3.ligo"
      , "multisig-v2.ligo"
      , "polymorphism" </> "comb.ligo" -- LIGO-331
      , "pascaligo_long_remove.ligo"
      , "patch_long_path.ligo"
      , "long_remove.ligo"
      , "record.ligo"
      , "basic_multisig" </> "multisig.ligo"
      , "loop14.ligo"
      , "loop17.ligo"
      , "multisig.ligo"
      , "hashlock.ligo"
      , "id.ligo"
      , "long_assign.ligo"

        -- LIGO-481
      , "aggregation" </> "bug_module_record.ligo"

        -- LIGO-477
      , "negative" </> "missing_funarg_annotation.religo"

        -- LIGO-478
      , "negative" </> "bad_michelson_insertion_3.ligo"

        -- LIGO fails to parse these:
      , "negative" </> "vars_consts" </> "capture_var_params.mligo"
      , "negative" </> "error_function_arguments.religo"
      , "negative" </> "error_syntax.ligo"
      ]
    , tdIgnoreDirs = []
    }
  , TestDir
    { tdRoot = testDir </> "error-recovery" </> "simple" </> "pascaligo" </> "original"
    , tdIgnoreFiles =
      [ -- LIGO-446
        "missing_semicolon_between_stmts.ligo"
      , "unfinished_code00.ligo"
      , "unfinished_code01.ligo"
      , "unfinished_code02.ligo"
      , "unfinished_code03.ligo"
      , "unfinished_code04.ligo"
      , "unfinished_code05.ligo"
      , "unfinished_code06.ligo"
      , "unfinished_code07.ligo"
      , "unfinished_code08.ligo"
      , "unfinished_code10.ligo"
      , "unfinished_code11.ligo"
      ]
    , tdIgnoreDirs = []
    }
  , TestDir
    { tdRoot = testDir </> "error-recovery" </> "simple" </> "reasonligo" </> "original"
    , tdIgnoreFiles =
      [ -- LIGO-479
        "two_extra_parenthesis_in_function_application.religo"
      , "extra_vbar_in_type_declaration.religo"
      , "missing_vbar_in_type_declaration.religo"
      ]
    , tdIgnoreDirs = []
    }
  , TestDir
    { tdRoot = testDir </> "error-recovery" </> "simple" </> "cameligo" </> "original"
    , tdIgnoreFiles = []
    , tdIgnoreDirs = []
    }
  , TestDir
    { tdRoot = testDir </> "error-recovery" </> "fuzzing" </> "pascaligo" </> "original"
    , tdIgnoreFiles =
      [ -- LIGO-446
        "1loop.ligo"
      , "2hashlock.ligo"
      , "2long_assign.ligo"
      , "2long_remove.ligo"
      , "4failwith.ligo"
      ]
    , tdIgnoreDirs = []
    }
  , TestDir
    { tdRoot = testDir </> "error-recovery" </> "fuzzing" </> "reasonligo" </> "original"
    , tdIgnoreFiles = []
    , tdIgnoreDirs = []
    }
  , TestDir
    { tdRoot = testDir </> "error-recovery" </> "fuzzing" </> "cameligo" </> "original"
    , tdIgnoreFiles = []
    , tdIgnoreDirs = []
    }
  ]

badTests :: [TestContracts]
badTests =
  [ TestDir
    { tdRoot = testDir </> "error-recovery"
    , tdIgnoreFiles =
      [ -- LIGO-474
        "simple" </> "pascaligo" </> "missing_name_of_argument.ligo"
      , "simple" </> "reasonligo" </> "missing_ident_in_type_decl.religo"
      , "simple" </> "cameligo" </> "missing_ident_in_type_decl.mligo"
      , "fuzzing" </> "pascaligo" </> "2transpiler_nested.ligo"
      , "fuzzing" </> "reasonligo" </> "4match_bis.religo"
      , "fuzzing" </> "reasonligo" </> "2self_address.religo"
      , "fuzzing" </> "reasonligo" </> "1bytes_arithmetic.religo"
      , "fuzzing" </> "reasonligo" </> "1loop.religo"
      , "fuzzing" </> "reasonligo" </> "16is_nat.religo"
      , "fuzzing" </> "reasonligo" </> "2record.religo"

        -- LIGO-476
      , "simple" </> "reasonligo" </> "unfinished_code06.religo"
      , "simple" </> "reasonligo" </> "unfinished_code10.religo"
      , "simple" </> "reasonligo" </> "unfinished_code12.religo"
      , "simple" </> "reasonligo" </> "unfinished_code05.religo"
      , "simple" </> "cameligo" </> "extra_then_kw.mligo"
      , "simple" </> "cameligo" </> "unfinished_code10.mligo"
      , "simple" </> "cameligo" </> "unfinished_code05.mligo"
      , "fuzzing" </> "reasonligo" </> "4tuples_no_annotation.religo"
      , "fuzzing" </> "cameligo" </> "1match_bis.mligo"
      , "fuzzing" </> "cameligo" </> "4michelson_or_tree.mligo"
      , "fuzzing" </> "cameligo" </> "1incr_decr.mligo"
      , "fuzzing" </> "cameligo" </> "2local_type_decl.mligo"
      , "fuzzing" </> "cameligo" </> "2address.mligo"
      ]
    , tdIgnoreDirs =
      [ "fuzzing" </> "cameligo" </> "original"
      , "fuzzing" </> "pascaligo" </> "original"
      , "fuzzing" </> "reasonligo" </> "original"
      , "simple" </> "cameligo" </> "original"
      , "simple" </> "pascaligo" </> "original"
      , "simple" </> "reasonligo" </> "original"
      ]
    }
  , TestDir
    { tdRoot = testDir </> "lexer"
    , tdIgnoreFiles =
      [ --LIGO-475
        "broken_string.ligo"
      , "negative_byte_sequence.religo"
      , "invalid_character_in_string.religo"
      , "negative_byte_sequence.ligo"
      , "reserved_name.religo"
      , "negative_byte_sequence.mligo"
      , "broken_string.religo"
      , "invalid_character_in_string.ligo"
      ]
    , tdIgnoreDirs = []
    }
  , TestDir
    { tdRoot = testDir </> "preprocessor"
    , tdIgnoreFiles =
      [ --LIGO-475
        "directive_inside_line.ligo"
      ]
    , tdIgnoreDirs = []
    }
  , TestContract $ contractsDir </> "negative" </> "vars_consts" </> "capture_var_params.mligo"
  , TestContract $ contractsDir </> "negative" </> "error_function_arguments.religo"
  , TestContract $ contractsDir </> "negative" </> "error_syntax.ligo"
  ]

getContracts :: [TestContracts] -> IO [FilePath]
getContracts = fmap concat . traverse go
  where
    go (TestDir dir ignoreContracts ignoreDirs) = do
      allContracts <- scanContracts dir
      let ignoreContracts' = map (dir </>) ignoreContracts
      let ignoreDirs' = map (dir </>) ignoreDirs
      pure
        $ filter (\x -> not (any (`isPrefixOf` x) ignoreDirs') && x `notElem` ignoreContracts')
          allContracts
    go (TestContract file) = pure [file]

test_okayContracts :: IO TestTree
test_okayContracts
  = testGroup "Parsers should parse these contracts" <$> testCases
  where
    testCases = map makeTestCase <$> getContracts okayTests
    makeTestCase contractPath = testCase contractPath (checkFile @Fallback True contractPath)

test_badContracts :: IO TestTree
test_badContracts
  = testGroup "Parsers should not parse these contracts" <$> testCases
  where
    testCases = map makeTestCase <$> getContracts badTests
    makeTestCase contractPath = testCase contractPath (checkFile @Fallback False contractPath)
