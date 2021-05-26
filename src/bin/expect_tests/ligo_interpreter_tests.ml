open Cli_expect

let test basename = "./" ^ basename
let pwd = Sys.getcwd ()
let () = Sys.chdir "../../test/contracts/interpreter_tests/"

let%expect_test _ =
  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "lambda_call" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "higher_order1" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "higher_order2" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "higher_order3" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "higher_order4" ] ;
  [%expect {|
    Test passed with true |}] ;

    run_ligo_good [ "test" ; test "interpret_test.mligo" ; "concats" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "record_concat" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "record_patch" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "record_lambda" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "variant_match" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "bool_match" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "list_match" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "tuple_proj" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "list_const" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "options_match_some" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "options_match_none" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "is_nat_yes" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "is_nat_no" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "abs_int" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "nat_int" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_list" ] ;
  [%expect {|
    Test passed with true |}] ;

  (* run_ligo_good [ "test" ; test "interpret_test.mligo" ; "iter_list_fail" ] ;
  [%expect {|
    Test was successful |}] ; *)

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "fold_list" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "comparison_int" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "comparison_string" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "divs_int" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "divs_nat" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "var_neg" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "sizes" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "modi" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "fold_while" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "assertion_pass" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_finds" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_fold" ] ;
  [%expect {|
    Test passed with true |}] ;

  (* run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_iter" ] ;
  [%expect {|
    Test was successful |}] ; *)

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_map" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_mem" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_remove" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_update" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "set_add" ] ;
  [%expect {|
    Test passed with true |}] ;

  (* 
  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "set_iter_fail" ] ;
  [%expect {|
    Test was successful |}] ;
  *)

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "set_mem" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "recursion_let_rec_in" ] ;
  [%expect {|
    Test passed with true |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "top_level_recursion" ] ;
  [%expect {|
    Test passed with true |}] 

let%expect_test _ =
  run_ligo_good [ "test" ; test "interpret_test_log.mligo" ; "log" ] ;
  [%expect {|
    {a = 1 ; b = 2n ; c = "aaa"}
    One (())
    Test passed with true |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_now.mligo" ; "test" ] ;
  [%expect {|
  "storage at origination"
  "2000-01-01T10:10:10Z"
  "setting now at:"
  "storage after calling"
  "2010-01-01T10:10:11Z"
  Test passed with true |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_fail.mligo" ; "test" ] ;
  [%expect {|
  Test passed with "my contract always fail" |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "compile_expr.mligo" ; "test1" ] ;
  [%expect {|
  Test passed with () |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_example.mligo" ; "test" ] ;
  [%expect {|
  Test passed with 111 |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "test_subst_with_storage.mligo" ; "test" ] ;
  [%expect {|
  Test passed with () |}]

(* do not remove that :) *)
let () = Sys.chdir pwd

let bad_test n = bad_test ("/interpreter_tests/"^n)

let%expect_test _ =
  run_ligo_bad [ "test" ; bad_test "test_failure1.mligo" ; "test" ] ;
  [%expect {|
    File "../../test/contracts/negative//interpreter_tests/test_failure1.mligo", line 2, characters 2-25:
      1 | let test =
      2 |   failwith "I am failing"

    Test failed with "I am failing" |}]

let%expect_test _ =
  run_ligo_bad [ "test" ; bad_test "test_failure2.mligo" ; "test" ] ;
  [%expect {|
    File "../../test/contracts/negative//interpreter_tests/test_failure2.mligo", line 2, characters 4-16:
      1 | let test =
      2 |     assert false

    Failed assertion |}]

let%expect_test _ =
  run_ligo_bad [ "test" ; bad_test "test_failure3.mligo" ; "test" ] ;
  [%expect {|
    File "../../test/contracts/negative//interpreter_tests/test_failure3.mligo", line 2, characters 11-38:
      1 | let test =
      2 |   let ut = Test.reset_state 2n [1n;1n] in
      3 |   let x = Test.compile_value 1 in

    An uncaught error occured in the object language:
    Insufficient tokens in initial accounts to create one roll |}]