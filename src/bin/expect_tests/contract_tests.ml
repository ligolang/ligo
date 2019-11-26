open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

let%expect_test _ =
  run_ligo_good [ "measure-contract" ; contract "coase.ligo" ; "main" ] ;
  [%expect {| 2066 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig.ligo" ; "main" ] ;
  [%expect {| 1093 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig-v2.ligo" ; "main" ] ;
  [%expect {| 627 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "vote.mligo" ; "main" ] ;
  [%expect {| 628 bytes |}] ;

  ()

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "coase.ligo" ; "main" ] ;
  [%expect {|
    { parameter
        (or (or (nat %buy_single) (nat %sell_single))
            (pair %transfer_single (nat %card_to_transfer) (address %destination))) ;
      storage
        (pair (pair (map %card_patterns nat (pair (mutez %coefficient) (nat %quantity)))
                    (map %cards nat (pair (address %card_owner) (nat %card_pattern))))
              (nat %next_id)) ;
      code { DUP ;
             CDR ;
             DIP { DUP } ;
             SWAP ;
             CAR ;
             IF_LEFT
               { DUP ;
                 IF_LEFT
                   { DUP ;
                     DIP { DIP 2 { DUP } ; DIG 2 } ;
                     PAIR ;
                     DUP ;
                     CAR ;
                     DIP { DUP } ;
                     SWAP ;
                     CDR ;
                     DIP { DUP } ;
                     SWAP ;
                     DIP { DUP ; CAR ; CAR } ;
                     GET ;
                     IF_NONE { PUSH string "GET_FORCE" ; FAILWITH } {} ;
                     DUP ;
                     CAR ;
                     DIP { DUP ; CDR ; PUSH nat 1 ; ADD } ;
                     MUL ;
                     DUP ;
                     AMOUNT ;
                     SWAP ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Not enough money" ; FAILWITH } { PUSH unit Unit } ;
                     DROP ;
                     NIL operation ;
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     CDR ;
                     PUSH nat 1 ;
                     ADD ;
                     DIP { DIP 2 { DUP } ; DIG 2 ; CAR } ;
                     SWAP ;
                     PAIR ;
                     DIP 3 { DROP } ;
                     DUG 2 ;
                     DIP 3 { DUP } ;
                     DIG 3 ;
                     CAR ;
                     CAR ;
                     DIP 5 { DUP } ;
                     DIG 5 ;
                     DIP { DIP 3 { DUP } ; DIG 3 ; SOME ; DIP { DUP } } ;
                     UPDATE ;
                     DIP { DROP } ;
                     DUP ;
                     DIP { DIP 4 { DUP } ; DIG 4 ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                     PAIR ;
                     PAIR ;
                     DIP 5 { DROP } ;
                     DUG 4 ;
                     DIP 4 { DUP } ;
                     DIG 4 ;
                     CAR ;
                     CDR ;
                     DIP 5 { DUP } ;
                     DIG 5 ;
                     CDR ;
                     DIP { DIP 6 { DUP } ; DIG 6 ; SOURCE ; PAIR ; SOME ; DIP { DUP } } ;
                     UPDATE ;
                     DIP { DROP } ;
                     DUP ;
                     DIP { DIP 5 { DUP } ; DIG 5 ; DUP ; CDR ; SWAP ; CAR ; CAR } ;
                     SWAP ;
                     PAIR ;
                     PAIR ;
                     DIP 6 { DROP } ;
                     DUG 5 ;
                     DIP 5 { DUP } ;
                     DIG 5 ;
                     CDR ;
                     PUSH nat 1 ;
                     ADD ;
                     DIP { DIP 5 { DUP } ; DIG 5 ; CAR } ;
                     SWAP ;
                     PAIR ;
                     DIP 6 { DROP } ;
                     DUG 5 ;
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     DIP { DIP 5 { DUP } ; DIG 5 } ;
                     PAIR ;
                     DIP { DROP 9 } }
                   { DUP ;
                     DIP { DIP 2 { DUP } ; DIG 2 } ;
                     PAIR ;
                     DUP ;
                     CAR ;
                     DIP { DUP } ;
                     SWAP ;
                     CDR ;
                     DIP { DUP } ;
                     SWAP ;
                     DIP { DUP ; CAR ; CDR } ;
                     GET ;
                     IF_NONE { PUSH string "GET_FORCE" ; FAILWITH } {} ;
                     DUP ;
                     CAR ;
                     SOURCE ;
                     SWAP ;
                     COMPARE ;
                     NEQ ;
                     IF { PUSH string "This card doesn't belong to you" ; FAILWITH }
                        { PUSH unit Unit } ;
                     DROP ;
                     DUP ;
                     CDR ;
                     DIP { DIP { DUP } ; SWAP ; CAR ; CAR } ;
                     GET ;
                     IF_NONE { PUSH string "GET_FORCE" ; FAILWITH } {} ;
                     DUP ;
                     CDR ;
                     PUSH nat 1 ;
                     SWAP ;
                     SUB ;
                     ABS ;
                     DIP { DUP ; CAR } ;
                     SWAP ;
                     PAIR ;
                     DIP { DROP } ;
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     CAR ;
                     CAR ;
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     CDR ;
                     DIP { DIP { DUP } ; SWAP ; SOME ; DIP { DUP } } ;
                     UPDATE ;
                     DIP { DROP } ;
                     DUP ;
                     DIP { DIP 3 { DUP } ; DIG 3 ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                     PAIR ;
                     PAIR ;
                     DIP 4 { DROP } ;
                     DUG 3 ;
                     DIP 3 { DUP } ;
                     DIG 3 ;
                     CAR ;
                     CDR ;
                     DIP 5 { DUP } ;
                     DIG 5 ;
                     DIP { DUP ; NONE (pair (address %card_owner) (nat %card_pattern)) } ;
                     UPDATE ;
                     DIP { DROP } ;
                     DUP ;
                     DIP { DIP 4 { DUP } ; DIG 4 ; DUP ; CDR ; SWAP ; CAR ; CAR } ;
                     SWAP ;
                     PAIR ;
                     PAIR ;
                     DIP 5 { DROP } ;
                     DUG 4 ;
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     CAR ;
                     DIP { DIP 2 { DUP } ; DIG 2 ; CDR } ;
                     MUL ;
                     SOURCE ;
                     CONTRACT unit ;
                     IF_NONE { PUSH string "bad address for get_contract" ; FAILWITH } {} ;
                     DIP { DUP } ;
                     SWAP ;
                     DIP { DUP } ;
                     UNIT ;
                     TRANSFER_TOKENS ;
                     DUP ;
                     NIL operation ;
                     SWAP ;
                     CONS ;
                     DIP { DIP 7 { DUP } ; DIG 7 } ;
                     PAIR ;
                     DIP { DROP 11 } } ;
                 DIP { DROP } }
               { DUP ;
                 DIP { DIP { DUP } ; SWAP } ;
                 PAIR ;
                 DUP ;
                 CAR ;
                 DIP { DUP } ;
                 SWAP ;
                 CDR ;
                 DUP ;
                 CAR ;
                 CDR ;
                 DIP 2 { DUP } ;
                 DIG 2 ;
                 CAR ;
                 DIP { DUP } ;
                 GET ;
                 IF_NONE { PUSH string "GET_FORCE" ; FAILWITH } {} ;
                 DUP ;
                 CAR ;
                 SOURCE ;
                 SWAP ;
                 COMPARE ;
                 NEQ ;
                 IF { PUSH string "This card doesn't belong to you" ; FAILWITH }
                    { PUSH unit Unit } ;
                 DROP ;
                 DIP 3 { DUP } ;
                 DIG 3 ;
                 CDR ;
                 DIP { DUP ; CDR } ;
                 PAIR ;
                 DIP { DROP } ;
                 DIP 3 { DUP } ;
                 DIG 3 ;
                 CAR ;
                 DIP { DUP ; SOME ; DIP { DIP { DUP } ; SWAP } } ;
                 UPDATE ;
                 DIP { DIP { DUP } ; SWAP ; DROP } ;
                 SWAP ;
                 DIP { DIP { DROP } ; DUP } ;
                 SWAP ;
                 DIP { DIP 2 { DUP } ; DIG 2 ; DUP ; CDR ; SWAP ; CAR ; CAR } ;
                 SWAP ;
                 PAIR ;
                 PAIR ;
                 DIP 3 { DROP } ;
                 DUG 2 ;
                 DIP 2 { DUP } ;
                 DIG 2 ;
                 NIL operation ;
                 PAIR ;
                 DIP { DROP 6 } } ;
             DIP { DROP 2 } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "multisig.ligo" ; "main" ] ;
  [%expect {|
    { parameter
        (pair (pair (nat %counter) (lambda %message unit (list operation)))
              (list %signatures (pair key_hash signature))) ;
      storage
        (pair (pair (list %auth key) (nat %counter)) (pair (string %id) (nat %threshold))) ;
      code { DUP ;
             CAR ;
             DIP { DUP ; CDR } ;
             PAIR ;
             DUP ;
             CAR ;
             DIP { DUP } ;
             SWAP ;
             CDR ;
             DIP { DUP } ;
             SWAP ;
             CAR ;
             CDR ;
             DIP 2 { DUP } ;
             DIG 2 ;
             CAR ;
             CAR ;
             DIP { DIP { DUP } ; SWAP ; CAR ; CDR } ;
             COMPARE ;
             NEQ ;
             IF { PUSH string "Counters does not match" ; FAILWITH }
                { DUP ;
                  DIP { DIP 2 { DUP } ; DIG 2 ; CAR ; CAR } ;
                  PAIR ;
                  DIP { DIP { DUP } ; SWAP ; CDR ; CAR ; CHAIN_ID ; SWAP ; PAIR } ;
                  PAIR ;
                  PACK ;
                  PUSH nat 0 ;
                  DIP 3 { DUP } ;
                  DIG 3 ;
                  CAR ;
                  CAR ;
                  DIP 5 { DUP } ;
                  DIG 5 ;
                  CDR ;
                  DIP { DUP ; DIP { DIP { DUP } ; SWAP } ; PAIR } ;
                  ITER { SWAP ;
                         PAIR ;
                         DUP ;
                         CAR ;
                         DIP { DUP } ;
                         SWAP ;
                         CDR ;
                         DIP { DUP } ;
                         SWAP ;
                         CAR ;
                         IF_CONS
                           { DIP { DUP } ;
                             SWAP ;
                             DIP { DIP 3 { DUP } ; DIG 3 ; CDR } ;
                             PAIR ;
                             DIP 4 { DROP } ;
                             DUG 3 ;
                             DIP 2 { DUP } ;
                             DIG 2 ;
                             CAR ;
                             DIP { DUP ; HASH_KEY } ;
                             COMPARE ;
                             EQ ;
                             IF { DUP ;
                                  DIP { DIP 2 { DUP } ; DIG 2 ; CDR ; DIP { DIP 7 { DUP } ; DIG 7 } } ;
                                  CHECK_SIGNATURE ;
                                  IF { DIP 3 { DUP } ;
                                       DIG 3 ;
                                       CDR ;
                                       PUSH nat 1 ;
                                       ADD ;
                                       DIP { DIP 3 { DUP } ; DIG 3 ; CAR } ;
                                       SWAP ;
                                       PAIR ;
                                       DIP 4 { DROP } ;
                                       DUG 3 ;
                                       PUSH unit Unit }
                                     { PUSH string "Invalid signature" ; FAILWITH } }
                                { PUSH unit Unit } ;
                             DIP { DROP 2 } }
                           { PUSH unit Unit } ;
                         DROP ;
                         DIP { DUP } ;
                         SWAP ;
                         DIP { DROP 3 } } ;
                  DUP ;
                  CAR ;
                  DIP { DIP { DUP } ; SWAP ; DROP } ;
                  SWAP ;
                  DIP { DIP { DROP } } ;
                  DUP ;
                  CDR ;
                  DIP { DIP 2 { DUP } ; DIG 2 ; DROP } ;
                  DIP 3 { DROP } ;
                  DUG 2 ;
                  DROP ;
                  DIP { DUP } ;
                  SWAP ;
                  DIP { DIP 4 { DUP } ; DIG 4 ; CDR ; CDR } ;
                  COMPARE ;
                  LT ;
                  IF { PUSH string "Not enough signatures passed the check" ; FAILWITH }
                     { DIP 4 { DUP } ;
                       DIG 4 ;
                       CAR ;
                       CDR ;
                       PUSH nat 1 ;
                       ADD ;
                       DIP { DIP 4 { DUP } ; DIG 4 ; DUP ; CDR ; SWAP ; CAR ; CAR } ;
                       SWAP ;
                       PAIR ;
                       PAIR ;
                       DIP 5 { DROP } ;
                       DUG 4 ;
                       PUSH unit Unit } ;
                  DIP { DROP 3 } } ;
             DROP ;
             DUP ;
             UNIT ;
             EXEC ;
             DIP { DIP { DUP } ; SWAP } ;
             PAIR ;
             DIP { DROP 5 } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "multisig-v2.ligo" ; "main" ] ;
  [%expect {|
    { parameter (lambda unit (list operation)) ;
      storage
        (pair (pair (set %auth address) (big_map %message_store bytes (set address)))
              (nat %threshold)) ;
      code { DUP ;
             CAR ;
             DIP { DUP ; CDR } ;
             PAIR ;
             DUP ;
             CDR ;
             DUP ;
             CAR ;
             CAR ;
             SENDER ;
             MEM ;
             NOT ;
             IF { PUSH string "Unauthorized address" ; FAILWITH } { PUSH unit Unit } ;
             DROP ;
             DIP { DUP } ;
             SWAP ;
             CAR ;
             DUP ;
             PACK ;
             DUP ;
             NIL operation ;
             SWAP ;
             DIP { DIP 3 { DUP } ; DIG 3 ; CAR ; CDR } ;
             GET ;
             IF_NONE
               { EMPTY_SET address }
               { DUP ; PUSH bool True ; SENDER ; UPDATE ; DIP { DROP } } ;
             DUP ;
             SIZE ;
             DIP { DIP 4 { DUP } ; DIG 4 ; CDR } ;
             COMPARE ;
             GE ;
             IF { DIP 2 { DUP } ;
                  DIG 2 ;
                  DIP { DIP 4 { DUP } ; DIG 4 ; CAR ; CDR ; NONE (set address) } ;
                  UPDATE ;
                  DIP { DIP 4 { DUP } ; DIG 4 ; DUP ; CDR ; SWAP ; CAR ; CAR } ;
                  SWAP ;
                  PAIR ;
                  PAIR ;
                  DIP 5 { DROP } ;
                  DUG 4 ;
                  DIP 3 { DUP } ;
                  DIG 3 ;
                  UNIT ;
                  EXEC ;
                  DIP { DIP { DUP } ; SWAP ; DROP } ;
                  SWAP ;
                  DIP { DIP { DROP } } ;
                  PUSH unit Unit }
                { DIP 2 { DUP } ;
                  DIG 2 ;
                  DIP { DUP ; SOME ; DIP { DIP 4 { DUP } ; DIG 4 ; CAR ; CDR } } ;
                  UPDATE ;
                  DIP { DIP 4 { DUP } ; DIG 4 ; DUP ; CDR ; SWAP ; CAR ; CAR } ;
                  SWAP ;
                  PAIR ;
                  PAIR ;
                  DIP 5 { DROP } ;
                  DUG 4 ;
                  PUSH unit Unit } ;
             DROP ;
             DIP { DUP } ;
             SWAP ;
             DIP { DIP 4 { DUP } ; DIG 4 } ;
             PAIR ;
             DIP { DROP 7 } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "vote.mligo" ; "main" ] ;
  [%expect {|
    { parameter
        (or (pair %init
               (pair (timestamp %beginning_time) (timestamp %finish_time))
               (string %title))
            (string %vote)) ;
      storage
        (pair (pair (pair (timestamp %beginning_time) (map %candidates string int))
                    (pair (timestamp %finish_time) (string %title)))
              (set %voters address)) ;
      code { DUP ;
             CAR ;
             IF_LEFT
               { DUP ;
                 DUP ;
                 CAR ;
                 CAR ;
                 DIP { PUSH int 0 ;
                       SOME ;
                       DIP { PUSH int 0 ;
                             SOME ;
                             EMPTY_MAP string int ;
                             SWAP ;
                             PUSH string "Yes" ;
                             UPDATE } ;
                       PUSH string "No" ;
                       UPDATE } ;
                 PAIR ;
                 DIP { DUP ; CAR ; CDR ; DIP { DUP ; CDR } ; PAIR } ;
                 PAIR ;
                 EMPTY_SET address ;
                 SWAP ;
                 PAIR ;
                 NIL operation ;
                 PAIR ;
                 DIP { DROP 2 } }
               { DUP ;
                 DIP { DIP { DUP } ; SWAP ; CDR } ;
                 PAIR ;
                 DUP ;
                 CAR ;
                 DIP { DUP } ;
                 SWAP ;
                 CDR ;
                 DIP { DUP } ;
                 SWAP ;
                 DIP { DUP ; CAR ; CAR ; CDR } ;
                 GET ;
                 IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                 DIP { DUP } ;
                 SWAP ;
                 CAR ;
                 CAR ;
                 CAR ;
                 DIP { DIP 2 { DUP } ;
                       DIG 2 ;
                       DIP { DUP ;
                             PUSH int 1 ;
                             ADD ;
                             SOME ;
                             DIP { DIP { DUP } ; SWAP ; CAR ; CAR ; CDR } } ;
                       UPDATE } ;
                 PAIR ;
                 DIP { DIP { DUP } ;
                       SWAP ;
                       CAR ;
                       CDR ;
                       CAR ;
                       DIP { DIP { DUP } ; SWAP ; CAR ; CDR ; CDR } ;
                       PAIR } ;
                 PAIR ;
                 DIP { DIP { DUP } ; SWAP ; CDR ; PUSH bool True ; SOURCE ; UPDATE } ;
                 PAIR ;
                 NIL operation ;
                 PAIR ;
                 DIP { DROP 5 } } ;
             DIP { DROP } } } |}]
