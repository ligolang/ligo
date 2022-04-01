open Cli_expect

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (test "option_map.mligo") ; "--protocol" ; "ithaca" ] ;
  [%expect{|
    { parameter unit ;
      storage (option int) ;
      code { CDR ;
             MAP { PUSH string "foo" ; PAIR } ;
             MAP { CDR } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; (test "option_map.mligo") ; "--protocol" ; "ithaca" ] ;
  [%expect{|
    Everything at the top-level was executed.
    - test exited with value (). |}]


let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; (test "option_map.mligo") ] ;
  [%expect{|
    File "../../test/contracts/option_map.mligo", line 6, characters 44-67:
      5 | let main (_, store : unit * int option) : operation list * (int option) =
      6 |   ([] : operation list), Option.map to_int (Option.map to_tup store)
      7 |

    Option.map is supported in protocol Ithaca onwards.
    Hint: pass the compiler option `--protocol ithaca`. |}]

let%expect_test _ =
  run_ligo_bad [ "run" ; "test" ; (test "option_map.mligo") ] ;
  [%expect{|
    File "../../test/contracts/option_map.mligo", line 6, characters 44-67:
      5 | let main (_, store : unit * int option) : operation list * (int option) =
      6 |   ([] : operation list), Option.map to_int (Option.map to_tup store)
      7 |

    Option.map is supported in protocol Ithaca onwards.
    Hint: pass the compiler option `--protocol ithaca`. |}]