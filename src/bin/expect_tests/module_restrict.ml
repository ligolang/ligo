open Cli_expect

(* AT THE TIME THOSE TESTS WERE WRITTEN: WE DO NOT SUPPORT MODULE OPENS *)
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-core"; bad_test "modules_access_not_open1.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/modules_access_not_open1.mligo", line 1, characters 13-16:
      1 | let y = A.B.(x.z)
                       ^^^

    Invalid access. A variable is expected |}];
  run_ligo_bad [ "print"; "ast-core"; bad_test "modules_access_not_open2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/modules_access_not_open2.mligo", line 1, characters 13-18:
      1 | let x = A.B.(x + y)
                       ^^^^^

    Invalid access. A variable is expected |}];
  run_ligo_bad [ "print"; "ast-core"; bad_test "modules_access_not_open1.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/modules_access_not_open1.jsligo", line 1, characters 12-13:
      1 | let y = A.B.(x.z)
                      ^
    Ill-formed namespace selection.
    At this point, one of the following is expected:
      * a namespace name;
      * a string denoting a constructor;
      * a record or array. |}];
  run_ligo_bad [ "print"; "ast-core"; bad_test "modules_access_not_open2.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/modules_access_not_open2.jsligo", line 1, characters 12-13:
      1 | let x = A.B.(x + y)
                      ^
    Ill-formed namespace selection.
    At this point, one of the following is expected:
      * a namespace name;
      * a string denoting a constructor;
      * a record or array. |}]

(*
  run_ligo_bad
    [ "print"; "ast-core"; bad_test "module_parametric_type_access.ligo" ];
  [%expect
    {|
  File "../../test/contracts/negative/module_parametric_type_access.ligo", line 5, characters 12-25:
    4 |
    5 | type fii is Foo.foo (int)

  Expected a declaration name |}]
*)
