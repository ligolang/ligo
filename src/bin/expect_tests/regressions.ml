open Cli_expect
let negative basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; negative "regression_typecheking_recursive_function.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/regression_typecheking_recursive_function.mligo", line 2, characters 39-41:
      1 |
      2 | let rec toto : unit -> int = fun () -> ()

    Invalid type(s).
    Expected: "unit", but got: "int". |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; negative "regression_import_scope_B.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/regression_import_scope_B.mligo", line 2, characters 8-11:
      1 |
      2 | let b = A.a
      3 |

    Module "A" not found. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; "../../test/contracts/aggregation/nested_modules.mligo"];
  [%expect{|
    File "../../test/contracts/aggregation/nested_modules.mligo", line 59, characters 10-16:
     58 |
     59 | let main (action, store : int * int) : operation list * int =
     60 |   [], A.Bx.nested + A.toto + A2.Cx.toto + A3.toto + A4.toto + End.Top.toto + M.titi
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/aggregation/nested_modules.mligo", line 59, characters 18-23:
     58 |
     59 | let main (action, store : int * int) : operation list * int =
     60 |   [], A.Bx.nested + A.toto + A2.Cx.toto + A3.toto + A4.toto + End.Top.toto + M.titi
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    { parameter int ;
      storage int ;
      code { DROP ;
             PUSH int 0 ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 0 ;
             DUP 3 ;
             DUP 4 ;
             DIG 4 ;
             DUP 6 ;
             DIG 6 ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]
