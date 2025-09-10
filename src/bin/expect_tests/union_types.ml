open Cli_expect

let contract name = String.concat [ "../../test/contracts/union_types/"; name ]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "union_formation.jsligo" ];
  [%expect
    "\n\
    \    { parameter nat ;\n\
    \      storage int ;\n\
    \      code { DROP ; PUSH int 1 ; NIL operation ; PAIR } }"]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "union_injection.jsligo" ];
  [%expect
    "\n\
    \    { parameter nat ;\n\
    \      storage int ;\n\
    \      code { DROP ; PUSH int 1 ; NIL operation ; PAIR } }"]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "union_subtype_all_same_type.jsligo" ];
  [%expect
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH bool True ;\n\
    \             SWAP ;\n\
    \             PAIR ;\n\
    \             LEFT (pair int bool) ;\n\
    \             IF_LEFT {} {} ;\n\
    \             LEFT (or (pair int bool) (pair int bool)) ;\n\
    \             IF_LEFT {} { IF_LEFT {} {} } ;\n\
    \             CAR ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "union_subtype_commutative.jsligo" ];
  [%expect
    "\n\
    \    { parameter nat ;\n\
    \      storage int ;\n\
    \      code { DROP ; PUSH int 1 ; NIL operation ; PAIR } }"]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "union_injection.jsligo" ];
  [%expect
    "\n\
    \    { parameter nat ;\n\
    \      storage int ;\n\
    \      code { DROP ; PUSH int 1 ; NIL operation ; PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "union_project_same_type_different_fields.jsligo" ];
  [%expect
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH bool True ;\n\
    \             SWAP ;\n\
    \             PAIR ;\n\
    \             LEFT (pair int string) ;\n\
    \             IF_LEFT { CAR } { CAR } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "union_project_same_type_different_fields.jsligo" ];
  [%expect
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH bool True ;\n\
    \             SWAP ;\n\
    \             PAIR ;\n\
    \             LEFT (pair int string) ;\n\
    \             IF_LEFT { CAR } { CAR } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "union_project_same_type_same_fields_same_order.jsligo"
    ];
  [%expect
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH bool True ;\n\
    \             SWAP ;\n\
    \             PAIR ;\n\
    \             LEFT (pair int string) ;\n\
    \             IF_LEFT { CAR } { CAR } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "union_project_same_type_same_fields_different_order.jsligo"
    ];
  [%expect
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH bool True ;\n\
    \             SWAP ;\n\
    \             PAIR ;\n\
    \             LEFT (pair string int) ;\n\
    \             IF_LEFT { CAR } { CDR } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "union_conditional_non_dependent.jsligo" ];
  [%expect
    "
    { parameter unit ;
      storage int ;
      code { CDR ;
             PUSH bool True ;
             SWAP ;
             PAIR ;
             LEFT (pair int string) ;
             IF_LEFT { CAR } { CAR } ;
             PUSH int 1 ;
             COMPARE ;
             EQ ;
             IF { PUSH int 3 } { PUSH int 4 } ;
             NIL operation ;
             PAIR } }"]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "union_conditional_dependent.jsligo" ];
  [%expect
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH string \"INT\" ;\n\
    \             PAIR ;\n\
    \             LEFT (pair string string) ;\n\
    \             IF_LEFT { DUP ; CDR ; SWAP ; CDR ; ADD } { CDR ; SIZE ; INT } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "union_conditional_dependent_nested.jsligo" ];
  [%expect
    "
    { parameter unit ;
      storage int ;
      code { CDR ;
             PUSH string \"INT\" ;
             PUSH string \"NUMBER\" ;
             PAIR 3 ;
             LEFT (or (pair string (pair string nat)) (pair string string)) ;
             LAMBDA
               (or (pair string (pair string int)) (pair string (pair string nat)))
               int
               { IF_LEFT { GET 4 ; PUSH int 1 ; ADD } { GET 4 ; INT } } ;
             SWAP ;
             IF_LEFT
               { LEFT (pair string (pair string nat)) ; EXEC }
               { IF_LEFT
                   { RIGHT (pair string (pair string int)) ; EXEC }
                   { SWAP ; DROP ; CDR ; SIZE ; INT } } ;
             NIL operation ;
             PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "union_conditional_dependent_redundant.jsligo" ];
  [%expect
    "
    { parameter unit ;
      storage int ;
      code { CDR ;
             PUSH string \"INT\" ;
             PAIR ;
             LEFT (pair string string) ;
             IF_LEFT { CDR ; PUSH int 1 ; ADD } { CDR ; SIZE ; INT } ;
             NIL operation ;
             PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "union_switch_break_non_dependent.jsligo" ];
  [%expect
    "
    { parameter unit ;
      storage int ;
      code { CDR ;
             PUSH bool True ;
             SWAP ;
             PAIR ;
             LEFT (pair int string) ;
             PUSH int -100 ;
             PUSH bool False ;
             DUP ;
             PUSH int 1 ;
             DUP 5 ;
             IF_LEFT { CAR } { CAR } ;
             COMPARE ;
             EQ ;
             OR ;
             IF { DROP 2 ; PUSH int 3 ; PUSH bool False } {} ;
             PUSH bool False ;
             PUSH int 1 ;
             DIG 4 ;
             IF_LEFT { CAR } { CAR } ;
             COMPARE ;
             EQ ;
             OR ;
             NOT ;
             OR ;
             IF { DROP ; PUSH int 4 } {} ;
             NIL operation ;
             PAIR } }"]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "union_switch_break_dependent.jsligo" ];
  [%expect
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH string \"INT\" ;\n\
    \             PAIR ;\n\
    \             LEFT (pair string string) ;\n\
    \             PUSH int -100 ;\n\
    \             DUP 2 ;\n\
    \             IF_LEFT { SWAP ; DROP ; DUP ; CDR ; SWAP ; CDR ; ADD } { DROP } ;\n\
    \             SWAP ;\n\
    \             IF_LEFT { DROP } { SWAP ; DROP ; CDR ; SIZE ; INT } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "union_switch_break_dependent_nested.jsligo" ];
  [%expect
    "
    { parameter unit ;
      storage int ;
      code { CDR ;
             PUSH string \"INT\" ;
             PUSH string \"NUMBER\" ;
             PAIR 3 ;
             LEFT (or (pair string (pair string nat)) (pair string string)) ;
             PUSH int -100 ;
             LAMBDA
               (pair int (or (pair string (pair string int)) (pair string (pair string nat))))
               unit
               { CDR ; IF_LEFT { GET 4 ; PUSH int 1 ; ADD ; DROP } { DROP } ; UNIT } ;
             DUP 2 ;
             APPLY ;
             DUP 3 ;
             IF_LEFT
               { LEFT (pair string (pair string nat)) ; EXEC ; DROP }
               { IF_LEFT { RIGHT (pair string (pair string int)) ; EXEC ; DROP } { DROP 2 } } ;
             LAMBDA
               (or (pair string (pair string int)) (pair string (pair string nat)))
               unit
               { DROP ; UNIT } ;
             DIG 2 ;
             IF_LEFT
               { LEFT (pair string (pair string nat)) ; EXEC ; DROP }
               { IF_LEFT
                   { RIGHT (pair string (pair string int)) ; EXEC ; DROP }
                   { DUG 2 ; DROP 2 ; CDR ; SIZE ; INT } } ;
             NIL operation ;
             PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "union_switch_return_non_dependent.jsligo" ];
  [%expect
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH bool True ;\n\
    \             SWAP ;\n\
    \             PAIR ;\n\
    \             LEFT (pair int string) ;\n\
    \             PUSH bool False ;\n\
    \             PUSH int 1 ;\n\
    \             DUP 3 ;\n\
    \             IF_LEFT { CAR } { CAR } ;\n\
    \             COMPARE ;\n\
    \             EQ ;\n\
    \             OR ;\n\
    \             IF { DROP ; PUSH int 3 }\n\
    \                { PUSH bool False ;\n\
    \                  PUSH int 1 ;\n\
    \                  DIG 2 ;\n\
    \                  IF_LEFT { CAR } { CAR } ;\n\
    \                  COMPARE ;\n\
    \                  EQ ;\n\
    \                  OR ;\n\
    \                  NOT ;\n\
    \                  PUSH bool False ;\n\
    \                  OR ;\n\
    \                  IF { PUSH int 4 } { PUSH int -1 } } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "union_switch_return_dependent.jsligo" ];
  [%expect
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH string \"INT\" ;\n\
    \             PAIR ;\n\
    \             LEFT (pair string string) ;\n\
    \             IF_LEFT { DUP ; CDR ; SWAP ; CDR ; ADD } { CDR ; SIZE ; INT } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "union_switch_return_dependent_nested.jsligo" ];
  [%expect
    "
    { parameter unit ;
      storage int ;
      code { CDR ;
             PUSH string \"INT\" ;
             PUSH string \"NUMBER\" ;
             PAIR 3 ;
             LEFT (or (pair string (pair string nat)) (pair string string)) ;
             LAMBDA
               (or (pair string (pair string int)) (pair string (pair string nat)))
               int
               { IF_LEFT { GET 4 ; PUSH int 1 ; ADD } { GET 4 ; INT } } ;
             SWAP ;
             IF_LEFT
               { LEFT (pair string (pair string nat)) ; EXEC }
               { IF_LEFT
                   { RIGHT (pair string (pair string int)) ; EXEC }
                   { SWAP ; DROP ; CDR ; SIZE ; INT } } ;
             NIL operation ;
             PAIR } }"]
