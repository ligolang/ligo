open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

(* avoid pretty printing *)
let () = Unix.putenv "TERM" "dumb"

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "bad_michelson_insertion_1.ligo" ; "main" ] ;
  [%expect{|
    Error(s) occurred while type checking the contract:
    Ill typed contract:
      1: { parameter nat ;
      2:   storage nat ;
      3:   code { LAMBDA (pair nat nat) nat ADD ; SWAP ; EXEC ; NIL operation ; PAIR } }
    At line 3 characters 35 to 38, unexpected primitive, only a sequence
    can be used here. |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "bad_michelson_insertion_2.ligo" ; "main" ] ;
  [%expect{xxx|
    in file "../../test/contracts/negative/bad_michelson_insertion_2.ligo", line 5, characters 34-40
      4 |   const f : (nat -> nat -> nat)= [%Michelson ({| ADD |} : nat -> nat -> nat)];
      5 | } with ((nil: list(operation)), f (p, s))

    Invalid type(s).
    Expected: "nat", but got: "( nat * nat )". |xxx}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; bad_contract "bad_michelson_insertion_3.ligo" ; "main" ] ;
  [%expect{|
    { parameter nat ;
      storage nat ;
      code { UNPAIR ; ADD ; NIL operation ; PAIR } } |}]
