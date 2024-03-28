open Cli_expect

let contract basename = "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "bug_alias13.mligo" ];
  [%expect
    {|
    File "../../test/contracts/aggregation/bug_alias13.mligo", line 6, characters 6-12:
      5 |     fun (n : nat) -> let current_turn = current_turn 1n in
      6 |       assert (n = current_turn)
                ^^^^^^
      7 |
    :
    Warning: deprecated value.
    In a future version, this function will be deprecated, and using `Assert.assert` is encouraged for a smoother migration.

    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH nat 1 ;
             PUSH nat 1 ;
             ADD ;
             PUSH nat 2 ;
             COMPARE ;
             EQ ;
             IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "effects.mligo" ];
  [%expect
    {|
    { parameter int ;
      storage int ;
      code { CDR ; PUSH string "foo" ; FAILWITH } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "test"
    ; "--init-file"
    ; contract "bug_locally_bound_vars.mligo"
    ];
  [%expect {|
           42 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "test2"
    ; "--init-file"
    ; contract "bug_locally_bound_vars.mligo"
    ];
  [%expect {|
    "hehe" |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "test"
    ; "--init-file"
    ; contract "bug_locally_bound_vars2.mligo"
    ];
  [%expect {|
    43 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "test"
    ; "--init-file"
    ; contract "bug_locally_bound_vars3.mligo"
    ];
  [%expect {|
    2 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "test"
    ; "--init-file"
    ; contract "bug_locally_bound_vars4.mligo"
    ];
  [%expect {|
    42 |}]

(* These tests should be about performance, rather than functionality *)
let%expect_test _ =
  run_ligo_good [ "run"; "test"; contract "modules_include.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test exited with value 0. |}];
  run_ligo_good [ "compile"; "contract"; contract "modules_include2.mligo"; "-m"; "A11" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]
