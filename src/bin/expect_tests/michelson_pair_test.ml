open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_pair_tree.ligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/michelson_pair_tree.ligo", line 6, characters 42-47:
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.
    File "../../test/contracts/michelson_pair_tree.ligo", line 6, characters 21-27:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter unit ;
      storage (pair (string %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH string "foo" ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_pair_tree.mligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/michelson_pair_tree.mligo", line 6, characters 18-23:
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.
    File "../../test/contracts/michelson_pair_tree.mligo", line 6, characters 10-16:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter unit ;
      storage (pair (int %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH int 3 ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_pair_tree.religo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/michelson_pair_tree.religo", line 6, characters 21-26:
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.
    File "../../test/contracts/michelson_pair_tree.religo", line 6, characters 13-19:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter unit ;
      storage (pair (int %three) (pair %four (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH int 3 ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_pair_tree.jsligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/michelson_pair_tree.jsligo", line 8, characters 13-19:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.
    File "../../test/contracts/michelson_pair_tree.jsligo", line 8, characters 21-26:
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    { parameter unit ;
      storage (pair (int %three) (pair %four (int %one) (nat %two))) ;
      code { LEFT (pair (list operation) (pair int (pair int nat))) ;
             LOOP_LEFT
               { DROP ;
                 PUSH nat 2 ;
                 PUSH int 1 ;
                 PAIR ;
                 PUSH int 3 ;
                 PAIR ;
                 NIL operation ;
                 PAIR ;
                 RIGHT (pair unit (pair int (pair int nat))) } } } |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "michelson_pair_tree_intermediary.ligo" ; "main" ] ;
  [%expect {|
    File "../../test/contracts/michelson_pair_tree_intermediary.ligo", line 6, characters 42-47:
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.
    File "../../test/contracts/michelson_pair_tree_intermediary.ligo", line 6, characters 21-27:
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter unit ;
      storage (pair (string %three) (pair (int %one) (nat %two))) ;
      code { DROP ;
             PUSH nat 2 ;
             PUSH int 1 ;
             PAIR ;
             PUSH string "foo" ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]
