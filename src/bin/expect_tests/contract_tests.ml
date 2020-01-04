open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

let%expect_test _ =
  run_ligo_good [ "measure-contract" ; contract "coase.ligo" ; "main" ] ;
  [%expect {| 2066 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig.ligo" ; "main" ] ;
  [%expect {| 1093 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig-v2.ligo" ; "main" ] ;
  [%expect {| 2717 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "vote.mligo" ; "main" ] ;
  [%expect {| 628 bytes |}] ;

  run_ligo_good [ "compile-parameter" ; contract "coase.ligo" ; "main" ; "Buy_single (record card_to_buy = 1n end)" ] ;
  [%expect {| (Left (Left 1)) |}] ;

  run_ligo_good [ "compile-storage" ; contract "coase.ligo" ; "main" ; "record cards = (map end : cards) ; card_patterns = (map end : card_patterns) ; next_id = 3n ; end" ] ;
  [%expect {| (Pair (Pair {} {}) 3) |}] ;

  run_ligo_bad [ "compile-storage" ; contract "coase.ligo" ; "main" ; "Buy_single (record card_to_buy = 1n end)" ] ;
  [%expect {| ligo: different kinds:  {"a":"record[next_id -> nat , cards -> (TO_Map (nat,record[card_pattern -> nat , card_owner -> address])) , card_patterns -> (TO_Map (nat,record[quantity -> nat , coefficient -> mutez]))]","b":"sum[Transfer_single -> record[destination -> address , card_to_transfer -> nat] , Sell_single -> record[card_to_sell -> nat] , Buy_single -> record[card_to_buy -> nat]]"} |}] ;

  run_ligo_bad [ "compile-parameter" ; contract "coase.ligo" ; "main" ; "record cards = (map end : cards) ; card_patterns = (map end : card_patterns) ; next_id = 3n ; end" ] ;
  [%expect {| ligo: different kinds:  {"a":"sum[Transfer_single -> record[destination -> address , card_to_transfer -> nat] , Sell_single -> record[card_to_sell -> nat] , Buy_single -> record[card_to_buy -> nat]]","b":"record[next_id -> nat , cards -> (TO_Map (nat,record[card_pattern -> nat , card_owner -> address])) , card_patterns -> (TO_Map (nat,record[quantity -> nat , coefficient -> mutez]))]"} |}] ;

  ()

let%expect_test _  =
  run_ligo_good [ "compile-storage" ; contract "timestamp.ligo" ; "main" ; "now" ; "--predecessor-timestamp" ; "2042-01-01T00:00:00Z" ] ;
  [%expect {| "2042-01-01T00:00:01Z" |}]

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
    { parameter
        (or (or (unit %default) (lambda %send bytes (list operation)))
            (lambda %withdraw bytes (list operation))) ;
      storage
        (pair (pair (pair (set %authorized_addresses address) (nat %max_message_size))
                    (pair (nat %max_proposal) (map %message_store bytes (set address))))
              (pair (pair (map %proposal_counters address nat) (bytes %state_hash))
                    (nat %threshold))) ;
      code { DUP ;
             CDR ;
             DIP { DUP } ;
             SWAP ;
             CAR ;
             IF_LEFT
               { DUP ;
                 IF_LEFT
                   { DIP 2 { DUP } ; DIG 2 ; NIL operation ; PAIR ; DIP { DROP } }
                   { DUP ;
                     DIP { DIP 2 { DUP } ; DIG 2 } ;
                     PAIR ;
                     DUP ;
                     CDR ;
                     DUP ;
                     CAR ;
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
                     SIZE ;
                     DIP { DIP 2 { DUP } ; DIG 2 ; CAR ; CAR ; CDR } ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Message size exceed maximum limit" ; FAILWITH }
                        { PUSH unit Unit } ;
                     DROP ;
                     DUP ;
                     EMPTY_SET address ;
                     SWAP ;
                     DIP { DIP 3 { DUP } ; DIG 3 ; CAR ; CDR ; CDR } ;
                     GET ;
                     IF_NONE
                       { DIP 3 { DUP } ;
                         DIG 3 ;
                         CDR ;
                         CAR ;
                         CAR ;
                         SENDER ;
                         GET ;
                         IF_NONE { PUSH string "GET_FORCE" ; FAILWITH } {} ;
                         PUSH nat 1 ;
                         ADD ;
                         SOME ;
                         DIP { DIP 3 { DUP } ; DIG 3 ; CDR ; CAR ; CAR } ;
                         SENDER ;
                         UPDATE ;
                         DIP { DIP 3 { DUP } ;
                               DIG 3 ;
                               DUP ;
                               CAR ;
                               SWAP ;
                               CDR ;
                               DUP ;
                               CDR ;
                               SWAP ;
                               CAR ;
                               CDR } ;
                         PAIR ;
                         PAIR ;
                         SWAP ;
                         PAIR ;
                         DIP 4 { DROP } ;
                         DUG 3 ;
                         EMPTY_SET address ;
                         PUSH bool True ;
                         SENDER ;
                         UPDATE ;
                         DIP { DROP } ;
                         PUSH unit Unit }
                       { DUP ;
                         SENDER ;
                         MEM ;
                         IF { PUSH unit Unit }
                            { DIP 4 { DUP } ;
                              DIG 4 ;
                              CDR ;
                              CAR ;
                              CAR ;
                              SENDER ;
                              GET ;
                              IF_NONE { PUSH string "GET_FORCE" ; FAILWITH } {} ;
                              PUSH nat 1 ;
                              ADD ;
                              SOME ;
                              DIP { DIP 4 { DUP } ; DIG 4 ; CDR ; CAR ; CAR } ;
                              SENDER ;
                              UPDATE ;
                              DIP { DIP 4 { DUP } ;
                                    DIG 4 ;
                                    DUP ;
                                    CAR ;
                                    SWAP ;
                                    CDR ;
                                    DUP ;
                                    CDR ;
                                    SWAP ;
                                    CAR ;
                                    CDR } ;
                              PAIR ;
                              PAIR ;
                              SWAP ;
                              PAIR ;
                              DIP 5 { DROP } ;
                              DUG 4 ;
                              PUSH unit Unit } ;
                         DROP ;
                         DUP ;
                         PUSH bool True ;
                         SENDER ;
                         UPDATE ;
                         DIP { DIP { DUP } ; SWAP ; DROP } ;
                         SWAP ;
                         DROP ;
                         DIP { DROP } ;
                         PUSH unit Unit } ;
                     DROP ;
                     DIP 3 { DUP } ;
                     DIG 3 ;
                     CDR ;
                     CAR ;
                     CAR ;
                     SENDER ;
                     GET ;
                     IF_NONE { PUSH string "GET_FORCE" ; FAILWITH } {} ;
                     DUP ;
                     DIP { DIP 4 { DUP } ; DIG 4 ; CAR ; CDR ; CAR } ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Maximum number of proposal reached" ; FAILWITH }
                        { PUSH unit Unit } ;
                     DROP ;
                     NIL operation ;
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     SIZE ;
                     DIP { DIP 5 { DUP } ; DIG 5 ; CDR ; CDR } ;
                     COMPARE ;
                     GE ;
                     IF { DIP 3 { DUP } ;
                          DIG 3 ;
                          DIP { DIP 5 { DUP } ; DIG 5 ; CAR ; CDR ; CDR ; NONE (set address) } ;
                          UPDATE ;
                          DIP { DIP 5 { DUP } ;
                                DIG 5 ;
                                DUP ;
                                CDR ;
                                SWAP ;
                                CAR ;
                                DUP ;
                                CAR ;
                                SWAP ;
                                CDR ;
                                CAR } ;
                          SWAP ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          DIP 6 { DROP } ;
                          DUG 5 ;
                          DIP 5 { DUP } ;
                          DIG 5 ;
                          CDR ;
                          CAR ;
                          CDR ;
                          DIP { DIP 4 { DUP } ; DIG 4 } ;
                          EXEC ;
                          DIP { DROP } ;
                          DIP 5 { DUP } ;
                          DIG 5 ;
                          CDR ;
                          CAR ;
                          CDR ;
                          DIP { DIP 3 { DUP } ; DIG 3 } ;
                          CONCAT ;
                          SHA256 ;
                          DIP { DIP 5 { DUP } ;
                                DIG 5 ;
                                DUP ;
                                CAR ;
                                SWAP ;
                                CDR ;
                                DUP ;
                                CDR ;
                                SWAP ;
                                CAR ;
                                CAR } ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          DIP 6 { DROP } ;
                          DUG 5 ;
                          DIP 5 { DUP } ;
                          DIG 5 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          DIP { DIP 5 { DUP } ; DIG 5 } ;
                          ITER { SWAP ;
                                 PAIR ;
                                 DUP ;
                                 CAR ;
                                 DIP { DUP } ;
                                 SWAP ;
                                 CDR ;
                                 CAR ;
                                 DIP 2 { DUP } ;
                                 DIG 2 ;
                                 CDR ;
                                 CDR ;
                                 DIP { DUP } ;
                                 SWAP ;
                                 DIP { DIP 6 { DUP } ; DIG 6 } ;
                                 MEM ;
                                 IF { DIP { DUP } ;
                                      SWAP ;
                                      DIP { DUP ;
                                            PUSH nat 1 ;
                                            SWAP ;
                                            SUB ;
                                            ABS ;
                                            SOME ;
                                            DIP { DIP 2 { DUP } ; DIG 2 ; CDR ; CAR ; CAR } } ;
                                      UPDATE ;
                                      DIP { DIP 2 { DUP } ;
                                            DIG 2 ;
                                            DUP ;
                                            CAR ;
                                            SWAP ;
                                            CDR ;
                                            DUP ;
                                            CDR ;
                                            SWAP ;
                                            CAR ;
                                            CDR } ;
                                      PAIR ;
                                      PAIR ;
                                      SWAP ;
                                      PAIR ;
                                      DIP 3 { DROP } ;
                                      DUG 2 ;
                                      PUSH unit Unit }
                                    { PUSH unit Unit } ;
                                 DROP ;
                                 DIP 2 { DUP } ;
                                 DIG 2 ;
                                 DIP { DROP 4 } } ;
                          DUP ;
                          DIP { DIP 6 { DUP } ; DIG 6 ; DROP } ;
                          DIP 7 { DROP } ;
                          DUG 6 ;
                          DROP ;
                          PUSH unit Unit }
                        { DIP 3 { DUP } ;
                          DIG 3 ;
                          DIP { DIP 2 { DUP } ;
                                DIG 2 ;
                                SOME ;
                                DIP { DIP 5 { DUP } ; DIG 5 ; CAR ; CDR ; CDR } } ;
                          UPDATE ;
                          DIP { DIP 5 { DUP } ;
                                DIG 5 ;
                                DUP ;
                                CDR ;
                                SWAP ;
                                CAR ;
                                DUP ;
                                CAR ;
                                SWAP ;
                                CDR ;
                                CAR } ;
                          SWAP ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          DIP 6 { DROP } ;
                          DUG 5 ;
                          PUSH unit Unit } ;
                     DROP ;
                     DUP ;
                     DIP { DIP 5 { DUP } ; DIG 5 } ;
                     PAIR ;
                     DIP { DROP 8 } } ;
                 DIP { DROP } }
               { DUP ;
                 DIP { DIP { DUP } ; SWAP } ;
                 PAIR ;
                 DUP ;
                 CDR ;
                 DIP { DUP } ;
                 SWAP ;
                 CAR ;
                 DUP ;
                 PACK ;
                 DUP ;
                 DIP { DIP 2 { DUP } ; DIG 2 ; CAR ; CDR ; CDR } ;
                 GET ;
                 IF_NONE
                   { PUSH unit Unit }
                   { DUP ;
                     PUSH bool False ;
                     SENDER ;
                     UPDATE ;
                     DIP { DUP } ;
                     SWAP ;
                     SIZE ;
                     DIP { DUP ; SIZE } ;
                     COMPARE ;
                     NEQ ;
                     IF { DIP 4 { DUP } ;
                          DIG 4 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          SENDER ;
                          GET ;
                          IF_NONE { PUSH string "GET_FORCE" ; FAILWITH } {} ;
                          PUSH nat 1 ;
                          SWAP ;
                          SUB ;
                          ABS ;
                          SOME ;
                          DIP { DIP 4 { DUP } ; DIG 4 ; CDR ; CAR ; CAR } ;
                          SENDER ;
                          UPDATE ;
                          DIP { DIP 4 { DUP } ;
                                DIG 4 ;
                                DUP ;
                                CAR ;
                                SWAP ;
                                CDR ;
                                DUP ;
                                CDR ;
                                SWAP ;
                                CAR ;
                                CDR } ;
                          PAIR ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          DIP 5 { DROP } ;
                          DUG 4 ;
                          PUSH unit Unit }
                        { PUSH unit Unit } ;
                     DROP ;
                     DUP ;
                     SIZE ;
                     PUSH nat 0 ;
                     SWAP ;
                     COMPARE ;
                     EQ ;
                     IF { DIP 2 { DUP } ;
                          DIG 2 ;
                          DIP { DIP 4 { DUP } ; DIG 4 ; CAR ; CDR ; CDR ; NONE (set address) } ;
                          UPDATE ;
                          DIP { DIP 4 { DUP } ;
                                DIG 4 ;
                                DUP ;
                                CDR ;
                                SWAP ;
                                CAR ;
                                DUP ;
                                CAR ;
                                SWAP ;
                                CDR ;
                                CAR } ;
                          SWAP ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          DIP 5 { DROP } ;
                          DUG 4 ;
                          PUSH unit Unit }
                        { DIP 2 { DUP } ;
                          DIG 2 ;
                          DIP { DUP ; SOME ; DIP { DIP 4 { DUP } ; DIG 4 ; CAR ; CDR ; CDR } } ;
                          UPDATE ;
                          DIP { DIP 4 { DUP } ;
                                DIG 4 ;
                                DUP ;
                                CDR ;
                                SWAP ;
                                CAR ;
                                DUP ;
                                CAR ;
                                SWAP ;
                                CDR ;
                                CAR } ;
                          SWAP ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          DIP 5 { DROP } ;
                          DUG 4 ;
                          PUSH unit Unit } ;
                     DIP { DROP 2 } } ;
                 DROP ;
                 DIP 2 { DUP } ;
                 DIG 2 ;
                 NIL operation ;
                 PAIR ;
                 DIP { DROP 5 } } ;
             DIP { DROP 2 } } } |} ]

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

let%expect_test _ =
    run_ligo_good [ "compile-contract" ; contract "implicit.mligo" ; "main" ] ;
    [%expect {|
      { parameter key_hash ;
        storage unit ;
        code { DUP ;
               CAR ;
               IMPLICIT_ACCOUNT ;
               UNIT ;
               NIL operation ;
               PAIR ;
               DIP { DROP 2 } } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_type_operator.ligo" ; "main" ] ;
  [%expect {| ligo: bad type operator (TO_Map (unit,unit)): |}]

let%expect_test _ =
  run_ligo_bad [ "run-function" ; contract "failwith.ligo" ; "failer" ; "1" ] ;
  [%expect {| ligo: Execution failed:  {"value":"some_string","type":"string"} |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_address_format.religo" ; "main" ] ;
  [%expect {| ligo: in file "bad_address_format.religo", line 2, characters 25-47. Badly formatted address "KT1badaddr":  {"location":"in file \"bad_address_format.religo\", line 2, characters 25-47"} |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_timestamp.ligo" ; "main" ] ;
  [%expect {| ligo: in file "bad_timestamp.ligo", line 5, characters 29-43. Badly formatted timestamp "badtimestamp":  {"location":"in file \"bad_timestamp.ligo\", line 5, characters 29-43"} |}]