open Cli_expect

let contract = test
let contract_resource name = test ("res/" ^ name)
let bad_contract = bad_test

(* warning unused variables example *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "warning_unused.mligo" ] ;
  [%expect {|
    File "../../test/contracts/warning_unused.mligo", line 11, characters 6-7:
     10 |   let x = s.x + 3 in
     11 |   let x = foo x in
     12 |   let x = bar s.x in
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    { parameter int ;
      storage (pair (int %x) (int %y)) ;
      code { CDR ;
             PUSH int 3 ;
             DUP 2 ;
             CAR ;
             ADD ;
             DROP ;
             PUSH int 3 ;
             PUSH int 9 ;
             DUP 3 ;
             CAR ;
             MUL ;
             ADD ;
             SWAP ;
             CDR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |}]

 (* warning non-duplicable variable used examples *)
let%expect_test _ =
run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "warning_duplicate.mligo" ] ;
[%expect{|
  File "../../test/contracts/warning_duplicate.mligo", line 2, characters 2-50:
    1 | module Foo = struct
    2 |   let x : nat ticket = Tezos.create_ticket 42n 42n
    3 | end
  :
  Warning: variable "Foo.x" cannot be used more than once.

  Error(s) occurred while checking the contract:
  At (unshown) location 8, type ticket nat cannot be used here because it is not duplicable. Only duplicable types can be used with the DUP instruction and as view inputs and outputs.
  At (unshown) location 8, Ticket in unauthorized position (type error). |}]


let%expect_test _ =
run_ligo_bad [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "warning_duplicate2.mligo" ] ;
[%expect{|
  File "../../test/contracts/warning_duplicate2.mligo", line 1, characters 4-5:
    1 | let x = Tezos.create_ticket 42n 42n
    2 | let x = (x, x)
  :
  Warning: variable "x" cannot be used more than once.

  Error(s) occurred while checking the contract:
  At (unshown) location 8, type ticket nat cannot be used here because it is not duplicable. Only duplicable types can be used with the DUP instruction and as view inputs and outputs.
  At (unshown) location 8, Ticket in unauthorized position (type error). |}]

  (* some check about the warnings of the E_constructor cases *)
let%expect_test _ =
run_ligo_good [ "compile" ; "contract" ; contract "warning_ambiguous_ctor.mligo" ] ;
[%expect{|
File "../../test/contracts/warning_ambiguous_ctor.mligo", line 9, characters 61-64:
  8 | (* here we expect a warning because both A constructor have the same parameter type *)
  9 | let main = fun (() , (_: union_b)) -> ([]: operation list) , A 1

Warning: The type of this value is ambiguous: Inferred type is union_b but could be of type union_a.
Hint: You might want to add a type annotation.

{ parameter unit ;
  storage (or (int %a) (nat %b)) ;
  code { DROP ; PUSH int 1 ; LEFT nat ; NIL operation ; PAIR } } |}];

run_ligo_good [ "compile" ; "contract" ; contract "not_ambiguous_ctor.mligo" ] ;
[%expect{|
{ parameter unit ;
  storage (or (nat %a) (nat %b)) ;
  code { DROP ; PUSH nat 1 ; LEFT nat ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "warning_sum_types.mligo" ] ;
  [%expect {|
    File "../../test/contracts/warning_sum_types.mligo", line 65, characters 14-23:
     64 |
     65 | let warn_me = TopTop 42
     66 | let warn_me = TopA 42

    Warning: The type of this value is ambiguous: Inferred type is ttop2 but could be of type ttop.
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 66, characters 14-21:
     65 | let warn_me = TopTop 42
     66 | let warn_me = TopA 42
     67 | let warn_me = TopB 42

    Warning: The type of this value is ambiguous: Inferred type is ta but could be of type ttop.
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 67, characters 14-21:
     66 | let warn_me = TopA 42
     67 | let warn_me = TopB 42
     68 |

    Warning: The type of this value is ambiguous: Inferred type is tb but could be of type ttop.
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 69, characters 14-19:
     68 |
     69 | let warn_me = BA 42
     70 | let warn_me = BB 42

    Warning: The type of this value is ambiguous: Inferred type is ta but could be of type tb.
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 70, characters 14-19:
     69 | let warn_me = BA 42
     70 | let warn_me = BB 42
     71 | let warn_me = AA 42

    Warning: The type of this value is ambiguous: Inferred type is tb but could be of type tb2.
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 71, characters 14-19:
     70 | let warn_me = BB 42
     71 | let warn_me = AA 42
     72 |

    Warning: The type of this value is ambiguous: Inferred type is ta but could be of type ta2.
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 73, characters 14-19:
     72 |
     73 | let warn_me = BN 42
     74 | let warn_me = AN 42 (* TODO : It should infer ta and warn about tn and not the contrary *)

    Warning: The type of this value is ambiguous: Inferred type is tn but could be of type tb.
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 74, characters 14-19:
     73 | let warn_me = BN 42
     74 | let warn_me = AN 42 (* TODO : It should infer ta and warn about tn and not the contrary *)
     75 | let warn_me = NN 42

    Warning: The type of this value is ambiguous: Inferred type is tn but could be of type ta.
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 75, characters 14-19:
     74 | let warn_me = AN 42 (* TODO : It should infer ta and warn about tn and not the contrary *)
     75 | let warn_me = NN 42
     76 |

    Warning: The type of this value is ambiguous: Inferred type is tn but could be of type tn2.
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 77, characters 14-22:
     76 |
     77 | let warn_me = TopS1 42
     78 | let warn_me = TopS2 42

    Warning: The type of this value is ambiguous: Inferred type is ttop but could be of type ts1.
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 78, characters 14-22:
     77 | let warn_me = TopS1 42
     78 | let warn_me = TopS2 42
     79 | let dont_warn_me = TopS3 42

    Warning: The type of this value is ambiguous: Inferred type is ttop but could be of type ts2.
    Hint: You might want to add a type annotation.

    { parameter int ;
      storage int ;
      code { DROP ; PUSH int 42 ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "warning_sum_types_shadowed.mligo" ] ;
  [%expect {|
    { parameter int ;
      storage int ;
      code { DROP ; PUSH int 42 ; NIL operation ; PAIR } } |}]
