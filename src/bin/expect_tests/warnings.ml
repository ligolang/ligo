open Cli_expect

let contract = test

(* warning unused variables example *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_unused.mligo" ];
  [%expect
    {|
    File "../../test/contracts/warning_unused.mligo", line 14, characters 6-7:
     13 |   let x = s.x + 3 in
     14 |   let x = foo x in
                ^
     15 |   let x = bar s.x in
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
             UPDATE 1 ;
             NIL operation ;
             PAIR } } |}]

(* warning non-duplicable variable used examples *)
let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; contract "warning_duplicate.mligo"
    ];
  [%expect
    {|
  File "../../test/contracts/warning_duplicate.mligo", line 2, characters 6-7:
    1 | module Foo = struct
    2 |   let x : nat ticket = Option.unopt (Tezos.create_ticket 42n 42n)
              ^
    3 | end
  :
  Warning: variable cannot be used more than once.

  Error(s) occurred while checking the contract:
  At (unshown) location 15, type ticket nat cannot be used here because it is not duplicable. Only duplicable types can be used with the DUP instruction and as view inputs and outputs.
  At (unshown) location 15, Ticket in unauthorized position (type error). |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; contract "warning_duplicate2.mligo"
    ];
  [%expect
    {|
  File "../../test/contracts/warning_duplicate2.mligo", line 1, characters 4-5:
    1 | let x = Tezos.create_ticket 42n 42n
            ^
    2 | let x = (x, x)
  :
  Warning: variable cannot be used more than once.

  Error(s) occurred while checking the contract:
  At (unshown) location 8, type option (ticket nat) cannot be used here because it is not duplicable. Only duplicable types can be used with the DUP instruction and as view inputs and outputs.
  At (unshown) location 8, Ticket in unauthorized position (type error). |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "duplicate_ticket_local_module.mligo" ];
  [%expect
    {|
  File "../../test/contracts/duplicate_ticket_local_module.mligo", line 10, characters 8-9:
    9 |
   10 |     let y = ticket, ticket
                ^
   11 |
  :
  Warning: variable cannot be used more than once.

  File "../../test/contracts/duplicate_ticket_local_module.mligo", line 8, characters 8-14:
    7 |   module B = struct
    8 |     let ticket = Option.unopt (Tezos.create_ticket 10n 10n)
                ^^^^^^
    9 |
  :
  Warning: variable cannot be used more than once.

  File "../../test/contracts/duplicate_ticket_local_module.mligo", line 8, characters 17-29:
    7 |   module B = struct
    8 |     let ticket = Option.unopt (Tezos.create_ticket 10n 10n)
                         ^^^^^^^^^^^^
    9 |
  :
  Warning: deprecated value.
  Use `Option.value_with_error` instead.

  File "../../test/contracts/duplicate_ticket_local_module.mligo", line 13, characters 6-18:
   12 |   end in
   13 |   [], Option.unopt (Tezos.join_tickets (fst B.y, snd B.y))
              ^^^^^^^^^^^^
  :
  Warning: deprecated value.
  Use `Option.value_with_error` instead.

  Error(s) occurred while type checking the contract:
  Ill typed contract:
    01: { parameter unit ;
    02:   storage (ticket nat) ;
    03:   code { DROP
    04:          /* [] */ ;
    05:          PUSH nat 10
    06:          /* [ nat ] */ ;
    07:          PUSH nat 10
    08:          /* [ nat : nat ] */ ;
    09:          TICKET
    10:          /* [ option (ticket nat) ] */ ;
    11:          IF_NONE
    12:            { PUSH string "option is None" /* [ string ] */ ; FAILWITH /* [] */ }
    13:            { /* [ ticket nat ] */ } ;
    14:          DUP ;
    15:          PAIR ;
    16:          JOIN_TICKETS ;
    17:          IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
    18:          NIL operation ;
    19:          PAIR } }
  At line 14 characters 9 to 12,
  type ticket nat cannot be used here because it is not duplicable. Only duplicable types can be used with the DUP instruction and as view inputs and outputs.
  At line 14 characters 9 to 12,
  Ticket in unauthorized position (type error). |}]

(* some check about the warnings of the E_constructor cases *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_ambiguous_ctor.mligo" ];
  [%expect
    {|
    File "../../test/contracts/warning_ambiguous_ctor.mligo", line 12, characters 67-70:
     11 | [@entry]
     12 | let main = fun (() : unit) (_ : union_b) -> ([] : operation list), A 1
                                                                             ^^^

    Warning: The type of "A(1)" is ambiguous: Inferred type is "union_b" but could be of type "union_a".
    Hint: You might want to add a type annotation.

    { parameter unit ;
      storage (or (int %a) (nat %b)) ;
      code { DROP ; PUSH int 1 ; LEFT nat ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile"; "contract"; contract "not_ambiguous_ctor.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (or (nat %a) (nat %b)) ;
      code { DROP ; PUSH nat 1 ; LEFT nat ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_sum_types.mligo" ];
  [%expect
    {|
    File "../../test/contracts/warning_sum_types.mligo", line 86, characters 14-23:
     85 |
     86 | let warn_me = TopTop 42
                        ^^^^^^^^^
     87 |

    Warning: The type of "TopTop(42)" is ambiguous: Inferred type is "ttop2" but could be of type "ttop".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 88, characters 14-21:
     87 |
     88 | let warn_me = TopA 42
                        ^^^^^^^
     89 |

    Warning: The type of "TopA(42)" is ambiguous: Inferred type is "ttop" but could be of type "ta".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 90, characters 14-21:
     89 |
     90 | let warn_me = TopB 42
                        ^^^^^^^
     91 |

    Warning: The type of "TopB(42)" is ambiguous: Inferred type is "ttop" but could be of type "tb".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 92, characters 14-19:
     91 |
     92 | let warn_me = BA 42
                        ^^^^^
     93 |

    Warning: The type of "BA(42)" is ambiguous: Inferred type is "tb" but could be of type "ta".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 94, characters 14-19:
     93 |
     94 | let warn_me = BB 42
                        ^^^^^
     95 |

    Warning: The type of "BB(42)" is ambiguous: Inferred type is "tb" but could be of type "tb2".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 96, characters 14-19:
     95 |
     96 | let warn_me = AA 42
                        ^^^^^
     97 |

    Warning: The type of "AA(42)" is ambiguous: Inferred type is "ta" but could be of type "ta2".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 98, characters 14-19:
     97 |
     98 | let warn_me = BN 42
                        ^^^^^
     99 |

    Warning: The type of "BN(42)" is ambiguous: Inferred type is "tb" but could be of type "tn".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 100, characters 14-19:
     99 |
    100 | let warn_me = AN 42
                        ^^^^^
    101 |

    Warning: The type of "AN(42)" is ambiguous: Inferred type is "tn" but could be of type "ta".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 104, characters 14-19:
    103 |
    104 | let warn_me = NN 42
                        ^^^^^
    105 |

    Warning: The type of "NN(42)" is ambiguous: Inferred type is "tn" but could be of type "tn2".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 106, characters 14-22:
    105 |
    106 | let warn_me = TopS1 42
                        ^^^^^^^^
    107 |

    Warning: The type of "TopS1(42)" is ambiguous: Inferred type is "ttop" but could be of type "ts1".
    Hint: You might want to add a type annotation.

    File "../../test/contracts/warning_sum_types.mligo", line 108, characters 14-22:
    107 |
    108 | let warn_me = TopS2 42
                        ^^^^^^^^
    109 |

    Warning: The type of "TopS2(42)" is ambiguous: Inferred type is "ttop" but could be of type "ts2".
    Hint: You might want to add a type annotation.

    { parameter int ;
      storage int ;
      code { DROP ; PUSH int 42 ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_sum_types_shadowed.mligo" ];
  [%expect
    {|
    { parameter int ;
      storage int ;
      code { DROP ; PUSH int 42 ; NIL operation ; PAIR } } |}]
