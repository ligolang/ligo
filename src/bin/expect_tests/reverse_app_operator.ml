open Cli_expect

let%expect_test _ =
  run_ligo_good [ "run"; "test"; test "reverse_app.mligo" ];
  [%expect
    {|
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

    Invalid type(s)
    Cannot unify "[y]int -> int" with "int". |}]
