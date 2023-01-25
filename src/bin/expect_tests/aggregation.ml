open Cli_expect

let contract basename = "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "bug_alias13.mligo" ];
  [%expect
    {|
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
