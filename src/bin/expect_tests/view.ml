open Cli_expect

let contract = test

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } } |}]

(* not warning is expected because the annotated view is still being included in the contract *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view.mligo" ; "--views" ; "v1,v2" ] ;
  [%expect {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } ;
      view "v2" int int { CDR ; PUSH int 2 ; ADD } } |}]

(* the following should trigger a warning because an annotated view is being ignored *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view.mligo" ; "--views" ; "v2" ] ;
  [%expect {|
    File "../../test/contracts/view.mligo", line 3, characters 12-14:
      2 |
      3 | [@view] let v1 (n,s: int * int) : int = s + n + 1
      4 | let v2 (_,s: int * int) : int = s + 2

    Warning: This view will be ignored, command line option override [
    view] annotation

    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v2" int int { CDR ; PUSH int 2 ; ADD } } |}]

(* bad view type *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "view.mligo" ; "--views" ; "v1,bad_view" ] ;
  [%expect {|
    File "../../test/contracts/view.mligo", line 5, characters 4-12:
      4 | let v2 (_,s: int * int) : int = s + 2
      5 | let bad_view (_,_: int * nat ) : nat = 1n
      6 |

    Invalid view argument.
    View 'bad_view' has storage type 'nat' and contract 'main' has storage type 'int'. |}]

(* view + #import : no view expected *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view_import.mligo" ] ;
  [%expect {| 
    { parameter unit ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]

(* view inside module : no view expected *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view_inside_module.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

(* view + #import + alias : view expected *)
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view_import_and_alias.mligo" ] ;
  [%expect {| 
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } } |}]

(* view restrictions on primitives *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "view_restrictions.mligo" ; "--views" ; "bad_view1" ; "--protocol" ; "jakarta" ] ;
  [%expect {| 
    File "../../test/contracts/view_restrictions.mligo", line 7, characters 10-70:
      6 | let bad_view1 (n,s: int * int) : int =
      7 |   let _ = Tezos.create_contract main (None : key_hash option) 0mutez 2 in
      8 |   s + n + 1

    View rule violated:
          - Tezos.create_contract ; Tezos.set_delegate and Tezos.transaction cannot be used because they are stateful (expect in lambdas)
          - Tezos.self can't be used because the entry-point does not make sense in a view |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; contract "view_restrictions.mligo" ; "--views" ; "bad_view2" ; "--protocol" ; "jakarta"  ] ;
  [%expect {| 
    File "../../test/contracts/view_restrictions.mligo", line 17, character 2 to line 18, character 3:
     16 | let bad_view2 ((),_: unit * int) : unit contract =
     17 |   let x : unit contract = Tezos.self "%default" in
     18 |   x

    View rule violated:
          - Tezos.create_contract ; Tezos.set_delegate and Tezos.transaction cannot be used because they are stateful (expect in lambdas)
          - Tezos.self can't be used because the entry-point does not make sense in a view |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view_restrictions.mligo" ; "--views" ; "ok_view" ] ;
  [%expect {| 
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "ok_view"
           unit
           bytes
           { DROP ;
             LAMBDA
               int
               (pair operation address)
               { PUSH mutez 0 ;
                 NONE key_hash ;
                 CREATE_CONTRACT
                   { parameter unit ; storage int ; code { CDR ; NIL operation ; PAIR } } ;
                 PAIR } ;
             PACK } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view_tuple_storage.mligo" ] ;
  [%expect {|
    { parameter int ;
      storage (pair (pair (pair string nat) string nat) string) ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v" int mutez { DROP ; PUSH mutez 1000000 } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "view_shadow_ann.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage int ;
      code { CDR ; NIL operation ; PAIR } ;
      view "v1" int int { UNPAIR ; PUSH int 1 ; SWAP ; DIG 2 ; ADD ; ADD } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; bad_test "views_shadow.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/views_shadow.mligo", line 3, characters 12-14:
      2 |
      3 | [@view] let v1 (n,s: int * int) : int = s + n + 1
      4 | let v1 (n,s: int * int) : int = s + n + 111111

    This declaration holds an annotation and is later shadowed. |}]

    