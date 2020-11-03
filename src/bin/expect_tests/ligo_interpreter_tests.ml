open Cli_expect

let test basename =
  "../../test/contracts/" ^ basename

let bad_test basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "lambda_call" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "higher_order1" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "higher_order2" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "higher_order3" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "higher_order4" ] ;
  [%expect {|
    Test was successful |}] ;

    run_ligo_good [ "test" ; test "interpret_test.mligo" ; "concats" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "record_concat" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "record_patch" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "record_lambda" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "variant_match" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "bool_match" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "list_match" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "tuple_proj" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "list_const" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "options_match_some" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "options_match_none" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "is_nat_yes" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "is_nat_no" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "abs_int" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "nat_int" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_list" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "iter_list_fail" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "fold_list" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "comparison_int" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "comparison_string" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "divs_int" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "divs_nat" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "divs_tez" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "var_neg" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "sizes" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "modi" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "fold_while" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "assertion_pass" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "assertion_fail" ] ;
  [%expect {|
    Test was successful |}] ;


  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_finds" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_finds_fail" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_fold" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_iter" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_map" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_mem" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_remove" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "map_update" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "set_add" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "set_iter_fail" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "set_mem" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "recursion_let_rec_in" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "top_level_recursion" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test.mligo" ; "pack_unpack" ] ;
  [%expect {|
    Test was successful |}] 

let%expect_test _ =
  run_ligo_good [ "test" ; test "interpret_test_log.mligo" ; "log" ] ;
  [%expect {|
    {a = 1 ; b = 2n ; c = "aaa"}
    One (())
    Test was successful |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "interpret_test2.mligo" ; "create_contract" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test2.mligo" ; "self" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test2.mligo" ; "test1" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test2.mligo" ; "assert_failure_internal" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test2.mligo" ; "assert_failure" ] ;
  [%expect {|
    Test was successful |}] ;

  run_ligo_good [ "test" ; test "interpret_test2.mligo" ; "diff_address" ] ;
  [%expect {|
    Test was successful |}]

let%expect_test _ =
  run_ligo_bad [ "test" ; bad_test "interpret_test_error1.mligo" ; "error1"] ;
  [%expect {|
    in file "../../test/contracts/negative/interpret_test_error1.mligo", line 3, characters 10-46
    Contract not found in the current context |}]

let%expect_test _ =
  run_ligo_good [ "test" ; test "interpret_test_apply.mligo" ; "test"] ;
  [%expect {|
    Test was successful |}]

let%expect_test _ =
  run_ligo_bad [ "test" ; bad_test "interpret_test_failwith1.mligo" ; "test"] ;
  [%expect {|
    Evaluation failed with:
    "wat" |}]

let%expect_test _ =
  run_ligo_bad [ "test" ; bad_test "interpret_test_failwith2.mligo" ; "test"] ;
  [%expect {|
    Evaluation failed with:
    "wat" |}]