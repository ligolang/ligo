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

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "views_using_view.jsligo" ] ;
  [%expect {|
    File "../../test/contracts/views_using_view.jsligo", line 23, characters 0-98:
     22 | // @view
     23 | let super_not_funny = ([_,s]:[unit, storage]): storage => not_funny(unit, s) + get_storage(unit,s);
     24 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 20, characters 0-73:
     19 | // @view
     20 | let get_address = ([_,_s]:[unit, storage]): address => Tezos.get_sender();
     21 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 17, characters 0-59:
     16 | // @view
     17 | let get_storage = ([_,s]:[unit, storage]): storage => x + s;
     18 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 14, characters 0-83:
     13 | // @view
     14 | let not_funny = ([_,s]:[unit, storage]): storage => basic(Tezos.get_sender(),x + s);
     15 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 11, characters 0-57:
     10 | // @view
     11 | let basic = ([_a,s]:[address, storage]): storage => x + s;
     12 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 6, characters 0-9:
      5 |
      6 | let x = 0
      7 |

    Toplevel let declaration are silently change to const declaration.

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
  run_ligo_good [ "run" ; "test" ; contract "views_using_view.test.mligo" ] ;
  [%expect {|
    File "../../test/contracts/views_using_view.jsligo", line 23, characters 0-98:
     22 | // @view
     23 | let super_not_funny = ([_,s]:[unit, storage]): storage => not_funny(unit, s) + get_storage(unit,s);
     24 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 20, characters 0-73:
     19 | // @view
     20 | let get_address = ([_,_s]:[unit, storage]): address => Tezos.get_sender();
     21 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 17, characters 0-59:
     16 | // @view
     17 | let get_storage = ([_,s]:[unit, storage]): storage => x + s;
     18 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 14, characters 0-83:
     13 | // @view
     14 | let not_funny = ([_,s]:[unit, storage]): storage => basic(Tezos.get_sender(),x + s);
     15 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 11, characters 0-57:
     10 | // @view
     11 | let basic = ([_a,s]:[address, storage]): storage => x + s;
     12 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 6, characters 0-9:
      5 |
      6 | let x = 0
      7 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 23, characters 0-98:
     22 | // @view
     23 | let super_not_funny = ([_,s]:[unit, storage]): storage => not_funny(unit, s) + get_storage(unit,s);
     24 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 20, characters 0-73:
     19 | // @view
     20 | let get_address = ([_,_s]:[unit, storage]): address => Tezos.get_sender();
     21 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 17, characters 0-59:
     16 | // @view
     17 | let get_storage = ([_,s]:[unit, storage]): storage => x + s;
     18 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 14, characters 0-83:
     13 | // @view
     14 | let not_funny = ([_,s]:[unit, storage]): storage => basic(Tezos.get_sender(),x + s);
     15 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 11, characters 0-57:
     10 | // @view
     11 | let basic = ([_a,s]:[address, storage]): storage => x + s;
     12 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 6, characters 0-9:
      5 |
      6 | let x = 0
      7 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 23, characters 0-98:
     22 | // @view
     23 | let super_not_funny = ([_,s]:[unit, storage]): storage => not_funny(unit, s) + get_storage(unit,s);
     24 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 20, characters 0-73:
     19 | // @view
     20 | let get_address = ([_,_s]:[unit, storage]): address => Tezos.get_sender();
     21 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 17, characters 0-59:
     16 | // @view
     17 | let get_storage = ([_,s]:[unit, storage]): storage => x + s;
     18 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 14, characters 0-83:
     13 | // @view
     14 | let not_funny = ([_,s]:[unit, storage]): storage => basic(Tezos.get_sender(),x + s);
     15 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 11, characters 0-57:
     10 | // @view
     11 | let basic = ([_a,s]:[address, storage]): storage => x + s;
     12 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 6, characters 0-9:
      5 |
      6 | let x = 0
      7 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 23, characters 0-98:
     22 | // @view
     23 | let super_not_funny = ([_,s]:[unit, storage]): storage => not_funny(unit, s) + get_storage(unit,s);
     24 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 20, characters 0-73:
     19 | // @view
     20 | let get_address = ([_,_s]:[unit, storage]): address => Tezos.get_sender();
     21 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 17, characters 0-59:
     16 | // @view
     17 | let get_storage = ([_,s]:[unit, storage]): storage => x + s;
     18 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 14, characters 0-83:
     13 | // @view
     14 | let not_funny = ([_,s]:[unit, storage]): storage => basic(Tezos.get_sender(),x + s);
     15 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 11, characters 0-57:
     10 | // @view
     11 | let basic = ([_a,s]:[address, storage]): storage => x + s;
     12 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 6, characters 0-9:
      5 |
      6 | let x = 0
      7 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 23, characters 0-98:
     22 | // @view
     23 | let super_not_funny = ([_,s]:[unit, storage]): storage => not_funny(unit, s) + get_storage(unit,s);
     24 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 20, characters 0-73:
     19 | // @view
     20 | let get_address = ([_,_s]:[unit, storage]): address => Tezos.get_sender();
     21 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 17, characters 0-59:
     16 | // @view
     17 | let get_storage = ([_,s]:[unit, storage]): storage => x + s;
     18 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 14, characters 0-83:
     13 | // @view
     14 | let not_funny = ([_,s]:[unit, storage]): storage => basic(Tezos.get_sender(),x + s);
     15 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 11, characters 0-57:
     10 | // @view
     11 | let basic = ([_a,s]:[address, storage]): storage => x + s;
     12 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/views_using_view.jsligo", line 6, characters 0-9:
      5 |
      6 | let x = 0
      7 |

    Toplevel let declaration are silently change to const declaration.

    Everything at the top-level was executed.
    - test_basic exited with value true.
    - test_not_funny exited with value true.
    - test_get_storage exited with value true.
    - test_get_address exited with value true.
    - test_super_not_funny exited with value true. |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "call_view_tuple.mligo" ] ;
  [%expect {|
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