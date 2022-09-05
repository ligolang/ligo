module Test.Parsers
  ( test_okayContracts
  , test_badContracts
  ) where

import AST (Fallback, scanContracts)

import Data.HashSet qualified as HashSet
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
      , "negative" </> "regression_import_scope_B.mligo"
      , "view_import.mligo"
      , "view_import_and_alias.mligo"

        -- LIGO-331
      , "polymorphism" </> "cases_annotation1.mligo"
      , "polymorphism" </> "cases_annotation2.mligo"
      , "polymorphism" </> "comb.ligo"
      , "polymorphism" </> "comb.mligo"
      , "polymorphism" </> "comb.religo"
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
      , "negative" </> "interpreter_tests" </> "test_random.mligo"
      , "negative" </> "polymorphism" </> "cases_annotation.mligo"
      , "negative" </> "polymorphism" </> "constants.mligo"
      , "negative" </> "polymorphism" </> "error_monad.mligo"
      , "negative" </> "polymorphism" </> "unresolved" </> "contract2.mligo"

        -- LIGO-477
      , "negative" </> "missing_funarg_annotation.religo"

        -- LIGO-757
      , "negative" </> "error_reverse_app.mligo"
      , "negative" </> "error_reverse_app_2.mligo"
      , "reverse_app.mligo"
      , "build" </> "common" </> "storage.mligo"
      , "build" </> "instance" </> "main.mligo"

        -- LIGO fails to parse these:
      , "match.ligo"
      , "negative" </> "vars_consts" </> "capture_var_params.mligo"
      , "negative" </> "error_function_arguments.religo"
      , "negative" </> "error_syntax.ligo"
      , "negative" </> "modules_access_not_open1.ligo"
      , "negative" </> "modules_access_not_open1.mligo"
      , "negative" </> "modules_access_not_open1.jsligo"
      , "negative" </> "modules_access_not_open2.ligo"
      , "negative" </> "modules_access_not_open2.mligo"
      , "negative" </> "modules_access_not_open2.jsligo"
      , "negative" </> "switch_jsligo" </> "break_outside_case2.jsligo"
      , "negative" </> "switch_jsligo" </> "empty_switch.jsligo"
      , "negative" </> "switch_jsligo" </> "default_in_between.jsligo"
      , "negative" </> "switch_jsligo" </> "more_than_one_default.jsligo"
      ]
    , tdIgnoreDirs = []
    }
  , TestDir
    { tdRoot = testDir </> "error-recovery" </> "simple" </> "pascaligo" </> "original"
    , tdIgnoreFiles = []
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
    { tdRoot = testDir </> "error-recovery" </> "simple" </> "jsligo" </> "original"
    , tdIgnoreFiles = []
    , tdIgnoreDirs = []
    }
  , TestDir
    { tdRoot = testDir </> "error-recovery" </> "fuzzing" </> "pascaligo" </> "original"
    , tdIgnoreFiles = []
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
  , TestDir
    { tdRoot = testDir </> "error-recovery" </> "fuzzing" </> "jsligo" </> "original"
    , tdIgnoreFiles = []
    , tdIgnoreDirs = []
    }
  ]

badTests :: [TestContracts]
badTests =
  [ TestDir
    { tdRoot = testDir </> "error-recovery"
    , tdIgnoreFiles =
      [ -- LIGO-476
        "simple" </> "reasonligo" </> "unfinished_code06.religo"
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

        -- Accepted by LIGO's parser:
      , "simple" </> "pascaligo" </> "unfinished_code00.ligo"
      , "simple" </> "pascaligo" </> "unfinished_code04.ligo"
      , "simple" </> "jsligo"    </> "missing_semicolon_in_top_level.jsligo"

        -- Will be fixed when (MISSING) nodes will be handled
      , "simple" </> "jsligo" </> "missing_curly_bracket_in_record_decl.jsligo"
      , "simple" </> "jsligo" </> "lambda_with_missing_arguments.jsligo"
      , "simple" </> "jsligo" </> "missing_ident_in_type_decl.jsligo"
      , "simple" </> "jsligo" </> "extra_gt_zwsp.jsligo"
      , "simple" </> "jsligo" </> "unfinished_code13.jsligo"
      , "simple" </> "jsligo" </> "unfinished_code12.jsligo"
      , "simple" </> "jsligo" </> "unfinished_code09.jsligo"
      , "simple" </> "jsligo" </> "lambda_with_missing_arguments.jsligo"
      , "simple" </> "jsligo" </> "missing_expr_parenthesesR.jsligo"
      , "simple" </> "jsligo" </> "switch_with_missing_case_value.jsligo"

        -- Fix this later
      , "simple" </> "jsligo" </> "missing_semicolon_before_return_on_same_line.jsligo"
      ]
    , tdIgnoreDirs =
      [ "fuzzing" </> "cameligo" </> "original"
      , "fuzzing" </> "pascaligo" </> "original"
      , "fuzzing" </> "reasonligo" </> "original"
      , "fuzzing" </> "jsligo" </> "original"
      , "simple" </> "cameligo" </> "original"
      , "simple" </> "pascaligo" </> "original"
      , "simple" </> "reasonligo" </> "original"
      , "simple" </> "jsligo" </> "original"
      ]
    }
  , TestDir
    { tdRoot = testDir </> "lexer"
    , tdIgnoreFiles =
      [ --LIGO-475
        "negative_byte_sequence.religo"
      , "negative_byte_sequence.ligo"
      , "reserved_name.religo"
      , "negative_byte_sequence.mligo"
       -- Lexer cases
      , "LexerLib" </> "invalid_character_in_string.ligo"
      , "LexerLib" </> "invalid_character_in_string.jsligo"
      , "LexerLib" </> "invalid_character_in_string.religo"
      , "Style"    </> "odd_lengthed_bytes.ligo"
      , "Lexing"   </> "underflow_mutez.ligo"
      , "Lexing"   </> "overflow_mutez.ligo"
      ]
    , tdIgnoreDirs = []
    }
  ]

getContracts :: [TestContracts] -> IO [FilePath]
getContracts = fmap concat . traverse go
  where
    go (TestDir dir ignoreContracts ignoreDirs) = do
      let ignore = HashSet.fromList $ map (dir </>) (ignoreContracts <> ignoreDirs)
      scanContracts (not . (`HashSet.member` ignore)) dir
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
