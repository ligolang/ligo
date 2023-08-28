open Cli_expect

let contract = test

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } ;
      view "v2" int int { CDR ; PUSH int 2 ; ADD } } |}]

(* view + #import : no view expected *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view_import.mligo" ];
  [%expect {| { parameter unit ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]

(* view inside module : no view expected *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view_inside_module.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

(* view + #import + alias : view expected *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view_import_and_alias.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } } |}]

(* view restrictions on primitives *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_test "view_restrictions1.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/view_restrictions1.mligo", line 1, character 0:
    ../../test/contracts/negative/view_restrictions1.mligo: No such file or directory. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_test "view_restrictions2.mligo" ];
  [%expect
    {|
    View rule violated:
          - Tezos.create_contract ; Tezos.set_delegate and Tezos.transaction cannot be used because they are stateful (expect in lambdas)
          - Tezos.self can't be used because the entry-point does not make sense in a view |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view_restrictions.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "ok_view"
           unit
           (lambda int (pair operation address))
           { DROP ;
             LAMBDA
               int
               (pair operation address)
               { PUSH mutez 0 ;
                 NONE key_hash ;
                 CREATE_CONTRACT
                   { parameter unit ; storage int ; code { CDR ; NIL operation ; PAIR } } ;
                 PAIR } } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view_tuple_storage.mligo" ];
  [%expect
    {|
    { parameter int ;
      storage (pair (pair (pair string nat) string nat) string) ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v" int mutez { DROP ; PUSH mutez 1000000 } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "view_shadow_ann.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } } |}]


let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "views_using_view.jsligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { DROP ; PUSH int 0 ; NIL operation ; PAIR } ;
      view "basic" address int { CDR ; PUSH int 0 ; ADD } ;
      view "not_funny" unit int { PUSH int 0 ; SWAP ; CDR ; DUP 2 ; ADD ; ADD } ;
      view "get_storage" unit int { CDR ; PUSH int 0 ; ADD } ;
      view "get_address" unit address { DROP ; SENDER } ;
      view "super_not_funny"
           unit
           int
           { PUSH int 0 ;
             SWAP ;
             CDR ;
             DUP ;
             DUP 3 ;
             ADD ;
             SWAP ;
             DUP 3 ;
             ADD ;
             DIG 2 ;
             ADD ;
             ADD } } |}]

let%expect_test _ =
  run_ligo_good [ "run"; "test"; contract "views_using_view.test.mligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_basic exited with value true.
    - test_not_funny exited with value true.
    - test_get_storage exited with value true.
    - test_get_address exited with value true.
    - test_super_not_funny exited with value true. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "call_view_tuple.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (pair (pair (int %a) (nat %b)) (mutez %c) (address %d)) ;
      code { CDR ;
             PUSH int 1 ;
             SOME ;
             IF_NONE
               {}
               { DROP ;
                 DUP ;
                 CDR ;
                 CDR ;
                 DUP 2 ;
                 CAR ;
                 CAR ;
                 SENDER ;
                 PAIR ;
                 VIEW "foo" unit ;
                 DROP ;
                 DUP ;
                 CDR ;
                 CDR ;
                 DUP 2 ;
                 CAR ;
                 CDR ;
                 VIEW "bar" unit ;
                 IF_NONE {} { DROP } } ;
             NIL operation ;
             PAIR } } |}]
