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
    Syntax error. |}]
