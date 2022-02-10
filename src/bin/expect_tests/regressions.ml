open Cli_expect
let negative basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; negative "regression_typecheking_recursive_function.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/regression_typecheking_recursive_function.mligo", line 2, characters 15-26:
      1 |
      2 | let rec toto : unit -> int = fun () -> ()

    Invalid type(s).
    Expected: "unit -> int", but got: "unit -> unit". |}]
