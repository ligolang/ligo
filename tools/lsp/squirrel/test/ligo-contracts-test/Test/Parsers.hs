module Test.Parsers
  ( test_okayContracts
  , test_badContracts
  , test_contractsWithMissingNodes
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
      , "build" </> "common" </> "storage.mligo"
      , "build" </> "instance" </> "main.mligo"
      , "interpreter_tests" </> "A.mligo"
      , "interpreter_tests" </> "C.mligo"
      , "interpreter_tests" </> "imported_modules" </> "a.mligo"
      , "interpreter_tests" </> "imported_modules" </> "b.mligo"
      , "interpreter_tests" </> "imported_modules" </> "main.mligo"
      , "interpreter_tests" </> "imported_modules" </> "test.mligo"
      , "interpreter_tests" </> "test_importer.mligo"
      , "interpreter_tests" </> "test_many_imports.mligo"
      , "polymorphism" </> "use_error.mligo"
      , "polymorphism" </> "use_monad.mligo"
      , "polymorphism" </> "use_monad_set.mligo"
      , "polymorphism" </> "use_nelist.mligo"
      , "negative" </> "polymorphism" </> "use_error.mligo"
      , "negative" </> "regression_import_scope_B.mligo"
      , "view_import.mligo"
      , "view_import_and_alias.mligo"

        -- LIGO-808
      , "interpreter_tests" </> "originate_from_relative_path" </> "test" </> "a" </> "b" </> "test.mligo"
      , "interpreter_tests" </> "originate_from_relative_path" </> "test" </> "c" </> "d" </> "foo.mligo"
      , "infer_fun_application.mligo"

        -- https://gitlab.com/ligolang/ligo/-/issues/1545
      , "ticket_wallet.mligo", "ticket_builder.mligo", "negative" </> "layout.mligo"
      , "michelson_typed_opt.mligo", "annotated_michelson_record_tree.mligo"
      , "annotation_cases.mligo", "annotated_michelson_record_comb.mligo"
      , "build" </> "F.mligo", "edo_combs.mligo", "layout.pligo"
      , "interpreter_tests" </> "compile_expr.mligo", "interpreter_tests" </> "test_compare.mligo"
      , "interpreter_tests" </> "compile_expr_from_file.mligo"
      , "interpreter_tests" </> "test_record.ligo", "warning_layout.mligo"
      , "self_annotations.mligo", "annotated_michelson_variant_tree.mligo"
      , "annotated_michelson_variant_comb.mligo", "FA1.2.mligo"

        -- LIGO fails to parse these:
      , "match.ligo"
      , "negative" </> "vars_consts" </> "capture_var_params.mligo"
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
      , "top_level_patterns" </> "contracts" </> "jsligo" </> "nested_record.jsligo"
      , "top_level_patterns" </> "contracts" </> "jsligo" </> "record.jsligo"
      ]
    , tdIgnoreDirs = []
    }
  , TestDir
    { tdRoot = testDir </> "error-recovery" </> "simple" </> "pascaligo" </> "original"
    , tdIgnoreFiles = []
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
    { tdRoot = testDir </> "error-recovery" </> "fuzzing" </> "cameligo" </> "original"
    , tdIgnoreFiles =
      [ -- https://gitlab.com/ligolang/ligo/-/issues/1545
        "4annotated_michelson_record_comb.mligo"
      , "2annotated_michelson_record_comb.mligo"
      , "1annotated_michelson_record_tree.mligo"
      , "4ticket_wallet.mligo"
      , "1ticket_wallet.mligo"
      , "1ticket_builder.mligo"
      ]
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
        "simple" </> "cameligo" </> "extra_then_kw.mligo"
      , "simple" </> "cameligo" </> "unfinished_code10.mligo"
      , "simple" </> "cameligo" </> "unfinished_code05.mligo"
      , "fuzzing" </> "cameligo" </> "1match_bis.mligo"
      , "fuzzing" </> "cameligo" </> "4michelson_or_tree.mligo"
      , "fuzzing" </> "cameligo" </> "1incr_decr.mligo"
      , "fuzzing" </> "cameligo" </> "2local_type_decl.mligo"
      , "fuzzing" </> "cameligo" </> "2address.mligo"

        -- Accepted by LIGO's parser:
      , "simple" </> "pascaligo" </> "unfinished_code00.ligo"
      , "simple" </> "pascaligo" </> "unfinished_code04.ligo"
      , "simple" </> "jsligo"    </> "missing_semicolon_in_top_level.jsligo"
      , "simple" </> "jsligo"    </> "missing_type_annotation_in_lambda_in_match.jsligo"

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
      , "fuzzing" </> "jsligo" </> "original"
      , "simple" </> "cameligo" </> "original"
      , "simple" </> "pascaligo" </> "original"
      , "simple" </> "jsligo" </> "original"
      ]
    }
  , TestDir
    { tdRoot = testDir </> "lexer"
    , tdIgnoreFiles =
      [ --LIGO-475
        "negative_byte_sequence.ligo"
      , "negative_byte_sequence.mligo"
       -- Lexer cases
      , "LexerLib" </> "invalid_character_in_string.ligo"
      , "LexerLib" </> "invalid_character_in_string.jsligo"
      , "Style"    </> "odd_lengthed_bytes.ligo"
      , "Lexing"   </> "underflow_mutez.ligo"
      , "Lexing"   </> "overflow_mutez.ligo"
      ]
    , tdIgnoreDirs = []
    }
  ]

contractsWithMissingNodes :: [TestContracts]
contractsWithMissingNodes =
  fmap (TestContract . (</>) (testDir </> "error-recovery"))
    [ "simple" </> "jsligo" </> "missing_curly_bracket_in_record_decl.jsligo"
    , "simple" </> "jsligo" </> "lambda_with_missing_arguments.jsligo"
    , "simple" </> "jsligo" </> "missing_ident_in_type_decl.jsligo"
    , "simple" </> "jsligo" </> "extra_gt_zwsp.jsligo"
    , "simple" </> "jsligo" </> "unfinished_code13.jsligo"
    , "simple" </> "jsligo" </> "unfinished_code12.jsligo"
    , "simple" </> "jsligo" </> "unfinished_code09.jsligo"
    , "simple" </> "jsligo" </> "lambda_with_missing_arguments.jsligo"
    , "simple" </> "jsligo" </> "missing_expr_parenthesesR.jsligo"
    , "simple" </> "jsligo" </> "switch_with_missing_case_value.jsligo"
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

test_contractsWithMissingNodes :: IO TestTree
test_contractsWithMissingNodes
  = testGroup "Trying to parse contracts with missing nodes" <$> testCases
  where
    testCases = map makeTestCase <$> getContracts contractsWithMissingNodes
    makeTestCase contractPath = testCase contractPath (checkFile @Fallback False contractPath)
