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
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH bool True ;\n\
    \             SWAP ;\n\
    \             PAIR ;\n\
    \             LEFT (pair int string) ;\n\
    \             PUSH int 1 ;\n\
    \             SWAP ;\n\
    \             IF_LEFT { CAR } { CAR } ;\n\
    \             COMPARE ;\n\
    \             EQ ;\n\
    \             IF { PUSH int 3 } { PUSH int 4 } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

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
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH string \"INT\" ;\n\
    \             PUSH string \"NUMBER\" ;\n\
    \             PAIR 3 ;\n\
    \             LEFT (or (pair string string nat) (pair string string)) ;\n\
    \             LAMBDA\n\
    \               (or (pair string string int) (pair string string nat))\n\
    \               int\n\
    \               { IF_LEFT { PUSH int 1 ; SWAP ; GET 4 ; ADD } { GET 4 ; INT } } ;\n\
    \             SWAP ;\n\
    \             IF_LEFT\n\
    \               { LEFT (pair string string nat) ; EXEC }\n\
    \               { IF_LEFT\n\
    \                   { RIGHT (pair string string int) ; EXEC }\n\
    \                   { SWAP ; DROP ; CDR ; SIZE ; INT } } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "union_conditional_dependent_redundant.jsligo" ];
  [%expect
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH string \"INT\" ;\n\
    \             PAIR ;\n\
    \             LEFT (pair string string) ;\n\
    \             IF_LEFT { PUSH int 1 ; SWAP ; CDR ; ADD } { CDR ; SIZE ; INT } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "union_switch_break_non_dependent.jsligo" ];
  [%expect
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH bool True ;\n\
    \             SWAP ;\n\
    \             PAIR ;\n\
    \             LEFT (pair int string) ;\n\
    \             PUSH int -100 ;\n\
    \             PUSH bool False ;\n\
    \             PUSH bool False ;\n\
    \             PUSH int 1 ;\n\
    \             DUP 5 ;\n\
    \             IF_LEFT { CAR } { CAR } ;\n\
    \             COMPARE ;\n\
    \             EQ ;\n\
    \             OR ;\n\
    \             IF { DROP 2 ; PUSH int 3 ; PUSH bool False } {} ;\n\
    \             PUSH bool False ;\n\
    \             PUSH int 1 ;\n\
    \             DIG 4 ;\n\
    \             IF_LEFT { CAR } { CAR } ;\n\
    \             COMPARE ;\n\
    \             EQ ;\n\
    \             OR ;\n\
    \             NOT ;\n\
    \             OR ;\n\
    \             IF { DROP ; PUSH int 4 } {} ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

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
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH string \"INT\" ;\n\
    \             PUSH string \"NUMBER\" ;\n\
    \             PAIR 3 ;\n\
    \             LEFT (or (pair string string nat) (pair string string)) ;\n\
    \             PUSH int -100 ;\n\
    \             LAMBDA\n\
    \               (or (pair string string int) (pair string string nat))\n\
    \               unit\n\
    \               { DUP ;\n\
    \                 IF_LEFT { DROP } { DROP } ;\n\
    \                 IF_LEFT { PUSH int 1 ; SWAP ; GET 4 ; ADD ; DROP } { DROP } ;\n\
    \                 UNIT } ;\n\
    \             DUP 3 ;\n\
    \             IF_LEFT\n\
    \               { LEFT (pair string string nat) ; EXEC ; DROP }\n\
    \               { IF_LEFT { RIGHT (pair string string int) ; EXEC ; DROP } { DROP 2 \
     } } ;\n\
    \             LAMBDA (or (pair string string int) (pair string string nat)) unit { \
     DROP ; UNIT } ;\n\
    \             DIG 2 ;\n\
    \             IF_LEFT\n\
    \               { LEFT (pair string string nat) ; EXEC ; DROP }\n\
    \               { IF_LEFT\n\
    \                   { RIGHT (pair string string int) ; EXEC ; DROP }\n\
    \                   { SWAP ; DIG 2 ; DROP 2 ; CDR ; SIZE ; INT } } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]

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
    "\n\
    \    { parameter unit ;\n\
    \      storage int ;\n\
    \      code { CDR ;\n\
    \             PUSH string \"INT\" ;\n\
    \             PUSH string \"NUMBER\" ;\n\
    \             PAIR 3 ;\n\
    \             LEFT (or (pair string string nat) (pair string string)) ;\n\
    \             LAMBDA\n\
    \               (or (pair string string int) (pair string string nat))\n\
    \               int\n\
    \               { IF_LEFT { PUSH int 1 ; SWAP ; GET 4 ; ADD } { GET 4 ; INT } } ;\n\
    \             SWAP ;\n\
    \             IF_LEFT\n\
    \               { LEFT (pair string string nat) ; EXEC }\n\
    \               { IF_LEFT\n\
    \                   { RIGHT (pair string string int) ; EXEC }\n\
    \                   { SWAP ; DROP ; CDR ; SIZE ; INT } } ;\n\
    \             NIL operation ;\n\
    \             PAIR } }"]
