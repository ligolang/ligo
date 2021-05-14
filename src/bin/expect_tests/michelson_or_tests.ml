open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

(* avoid pretty printing *)
let () = Unix.putenv "TERM" "dumb"

let%expect_test _ =
  run_ligo_good [ "dry-run" ; contract "double_michelson_or.mligo" ; "main" ; "unit" ; "(M_left (1) : storage)" ] ;
  [%expect {|
    File "../../test/contracts/double_michelson_or.mligo", line 6, characters 18-23:
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.
    File "../../test/contracts/double_michelson_or.mligo", line 6, characters 10-16:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.
    File "../../test/contracts/double_michelson_or.mligo", line 8, characters 6-9:
    Warning: unused variable "bar".
    Hint: replace it by "_bar" to prevent this warning.

    ( LIST_EMPTY() , M_right("one") ) |}];

  run_ligo_good [ "dry-run" ; contract "double_michelson_or.ligo" ; "main" ; "unit" ; "(M_left (1) : storage)" ] ;
  [%expect {|
    File "../../test/contracts/double_michelson_or.ligo", line 6, characters 42-47:
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.
    File "../../test/contracts/double_michelson_or.ligo", line 6, characters 21-27:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.
    File "../../test/contracts/double_michelson_or.ligo", line 9, characters 8-11:
    Warning: unused variable "bar".
    Hint: replace it by "_bar" to prevent this warning.

    ( LIST_EMPTY() , M_right("one") ) |}]


let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_or_tree.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/michelson_or_tree.mligo", line 6, characters 18-23:
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.
    File "../../test/contracts/michelson_or_tree.mligo", line 6, characters 10-16:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter unit ;
      storage (or (int %three) (or %four (int %one) (nat %two))) ;
      code { DROP ; PUSH int 1 ; LEFT nat ; RIGHT int ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "bad_michelson_or.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/negative/bad_michelson_or.mligo", line 6, characters 12-27:
      5 | let main (action, store : unit * storage) : return =
      6 |   let foo = M_right ("one") in
      7 |   (([] : operation list), (foo: storage))

    Incorrect usage of type "michelson_or".
    The contructor "M_right" must be annotated with a variant type. |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_or_tree_intermediary.ligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/michelson_or_tree_intermediary.ligo", line 6, characters 42-47:
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.
    File "../../test/contracts/michelson_or_tree_intermediary.ligo", line 6, characters 21-27:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter unit ;
      storage (or (int %three) (or (int %one) (nat %two))) ;
      code { DROP ; PUSH int 1 ; LEFT nat ; RIGHT int ; NIL operation ; PAIR } } |}]

