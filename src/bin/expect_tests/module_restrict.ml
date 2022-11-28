open Cli_expect

(* AT THE TIME THOSE TESTS WERE WRITTEN: WE DO NOT SUPPORT MODULE OPENS *)
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-imperative"; bad_test "modules_access_not_open1.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/modules_access_not_open1.ligo", line 1, characters 14-19:
      1 | const y = A.B.(x.z)

    Expected a declaration name |}];
  run_ligo_bad [ "print"; "ast-imperative"; bad_test "modules_access_not_open2.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/modules_access_not_open2.ligo", line 1, characters 14-21:
      1 | const x = A.B.(x + y)

    Expected a declaration name |}];
  run_ligo_bad [ "print"; "ast-imperative"; bad_test "modules_access_not_open1.mligo" ];
  [%expect
    {|
  File "../../test/contracts/negative/modules_access_not_open1.mligo", line 1, characters 12-13:
    1 | let y = A.B.(x.z)
  Ill-formed selection of a value from a module.
  At this point, the qualified name of a value is expected. |}];
  run_ligo_bad [ "print"; "ast-imperative"; bad_test "modules_access_not_open2.mligo" ];
  [%expect
    {|
  File "../../test/contracts/negative/modules_access_not_open2.mligo", line 1, characters 12-13:
    1 | let x = A.B.(x + y)
  Ill-formed selection of a value from a module.
  At this point, the qualified name of a value is expected. |}];
  run_ligo_bad [ "print"; "ast-imperative"; bad_test "modules_access_not_open1.jsligo" ];
  [%expect
    {|
  File "../../test/contracts/negative/modules_access_not_open1.jsligo", line 1, characters 12-13:
    1 | let y = A.B.(x.z)
  Ill-formed selection of a value in a module.
  At this point, the qualified name of a value is expected. |}];
  run_ligo_bad [ "print"; "ast-imperative"; bad_test "modules_access_not_open2.jsligo" ];
  [%expect
    {|
  File "../../test/contracts/negative/modules_access_not_open2.jsligo", line 1, characters 12-13:
    1 | let x = A.B.(x + y)
  Ill-formed selection of a value in a module.
  At this point, the qualified name of a value is expected. |}];
  run_ligo_bad
    [ "print"; "ast-imperative"; bad_test "module_parametric_type_access.ligo" ];
  [%expect
    {|
  File "../../test/contracts/negative/module_parametric_type_access.ligo", line 5, characters 16-19:
    4 |
    5 | type fii is Foo.foo (int)

  Expected a declaration name |}]
