open Cli_expect

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "t1"; "--init-file"; test "parametric_types.mligo" ];
  [%expect {|
    ( 1 , "one" ) |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "t2"; "--init-file"; test "parametric_types.mligo" ];
  [%expect {|
    CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))) |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "t3"; "--init-file"; test "parametric_types.mligo" ];
  [%expect {|
    ( +1 , 1 ) |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "t4"; "--init-file"; test "parametric_types.mligo" ];
  [%expect {|
    CONS(1 , CONS(2 , CONS(3 , LIST_EMPTY()))) |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "t5"; "--init-file"; test "parametric_types.mligo" ];
  [%expect {|
    1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t2"; "--init-file"; test "parametric_types.jsligo" ];
  [%expect {|
    Bar(42) |}];
  run_ligo_good [ "run"; "interpret"; "t6"; "--init-file"; test "parametric_types.mligo" ];
  [%expect {|
    Bar(42) |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t3"; "--init-file"; test "parametric_types.jsligo" ];
  [%expect {|
    Bar(42) |}];
  run_ligo_good [ "run"; "interpret"; "t7"; "--init-file"; test "parametric_types.mligo" ];
  [%expect {|
    Bar(42) |}]

(*
let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "t1"; "--init-file"; test "parametric_types.ligo" ];
  [%expect {|
    ( 1 , "one" ) |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "t2"; "--init-file"; test "parametric_types.ligo" ];
  [%expect {|
    CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))) |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "t3"; "--init-file"; test "parametric_types.ligo" ];
  [%expect {|
    ( +1 , 1 ) |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "t4"; "--init-file"; test "parametric_types.ligo" ];
  [%expect {|
    CONS(1 , CONS(2 , CONS(3 , LIST_EMPTY()))) |}]
*)

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "parametric_types1.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/parametric_types1.mligo", line 1, characters 20-24:
      1 | type fail_big_map = bool map
                              ^^^^

    Type map is applied to a wrong number of arguments, expected: 2 got: 1 |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "parametric_types2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/parametric_types2.mligo", line 2, characters 11-23:
      1 | type 'a foo = 'a * 'a
      2 | type bar = (int,string) foo
                     ^^^^^^^^^^^^

    Type foo is applied to a wrong number of arguments, expected: 1 got: 2 |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "parametric_types3.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/parametric_types3.mligo", line 2, characters 11-14:
      1 | type ('a,'b,'c) foo = 'a * 'b * 'c
      2 | type bar = int foo
                     ^^^

    Type foo is applied to a wrong number of arguments, expected: 3 got: 1 |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "parametric_types4.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/parametric_types4.mligo", line 1, characters 9-15:
      1 | type x = option list
                   ^^^^^^

    Ill formed type "option". Hint: you might be missing some type arguments. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "parametric_types5.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/parametric_types5.mligo", line 1, characters 0-26:
      1 | type ('a,'a) foo = 'a * 'a
          ^^^^^^^^^^^^^^^^^^^^^^^^^^

    Repeated type variable in type.
    Hint: Change the name. |}]

(*
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "parametric_types1.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/parametric_types1.ligo", line 1, characters 21-30:
      1 | type fail_big_map is map(bool)

    Type map is applied to a wrong number of arguments, expected: 2 got: 1 |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "parametric_types2.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/parametric_types2.ligo", line 2, characters 12-27:
      1 | type foo(a) is a * a
      2 | type bar is foo(int,string)

    Type foo is applied to a wrong number of arguments, expected: 1 got: 2 |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "parametric_types3.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/parametric_types3.ligo", line 2, characters 12-20:
      1 | type foo(a,b,c) is a * b * c
      2 | type bar is foo(int)

    Type foo is applied to a wrong number of arguments, expected: 3 got: 1 |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "parametric_types4.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/parametric_types4.ligo", line 1, characters 10-22:
      1 | type x is list(option)

    Invalid type
    Ill formed type "option".Hint: you might be missing some type arguments. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "parametric_types5.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/parametric_types5.ligo", line 1, characters 0-22:
      1 | type foo(a,a) is a * a

    Repeated type variable in type.
    Hint: Change the name. |}]
*)
