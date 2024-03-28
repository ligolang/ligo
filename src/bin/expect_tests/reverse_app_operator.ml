open Cli_expect

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "reverse_app.mligo" ];
  [%expect
    {|
    File "../../test/contracts/reverse_app.mligo", line 29, characters 2-8:
     28 |   in
     29 |   assert (a = b)
            ^^^^^^
     30 |
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/reverse_app.mligo", line 37, characters 2-8:
     36 |   in
     37 |   assert (a = b)
            ^^^^^^
     38 |
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/reverse_app.mligo", line 45, characters 2-8:
     44 |   in
     45 |   assert (a = b)
            ^^^^^^
     46 |
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    File "../../test/contracts/reverse_app.mligo", line 53, characters 2-8:
     52 |   in
     53 |   assert (a = b)
            ^^^^^^
     54 |
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    Everything at the top-level was executed.
    - test exited with value ().
    - test exited with value ().
    - test exited with value ().
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_test "error_reverse_app.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/error_reverse_app.mligo", line 4, characters 19-26:
      3 |
      4 | let typing_error = "Hello" |> f
                             ^^^^^^^

    Invalid type(s).
    Expected "int", but got: "string". |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_test "error_reverse_app_2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/error_reverse_app_2.mligo", line 6, characters 19-29:
      5 |
      6 | let typing_error = f 42 |> gg |> h
                             ^^^^^^^^^^

    This expression has type "[y]int -> int", but an expression was expected of type
    "int".
    Type "[y]int -> int" is not compatible with type "int". |}]
