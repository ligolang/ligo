open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_good [ "measure-contract" ; contract "coase.ligo" ; "main" ] ;
  [%expect {| 1747 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig.ligo" ; "main" ] ;
  [%expect {| 1358 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig-v2.ligo" ; "main" ] ;
  [%expect {| 3294 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "vote.mligo" ; "main" ] ;
  [%expect {| 642 bytes |}] ;

  run_ligo_good [ "compile-parameter" ; contract "coase.ligo" ; "main" ; "Buy_single (record card_to_buy = 1n end)" ] ;
  [%expect {| (Left (Left 1)) |}] ;

  run_ligo_good [ "compile-storage" ; contract "coase.ligo" ; "main" ; "record cards = (map end : cards) ; card_patterns = (map end : card_patterns) ; next_id = 3n ; end" ] ;
  [%expect {| (Pair (Pair {} {}) 3) |}] ;

  run_ligo_bad [ "compile-storage" ; contract "coase.ligo" ; "main" ; "Buy_single (record card_to_buy = 1n end)" ] ;
  [%expect {|
    ligo: different kinds:  {"a":"record[next_id -> nat , cards -> (TO_Map (nat,record[card_pattern -> nat , card_owner -> address])) , card_patterns -> (TO_Map (nat,record[quantity -> nat , coefficient -> mutez]))]","b":"sum[Transfer_single -> record[destination -> address , card_to_transfer -> nat] , Sell_single -> record[card_to_sell -> nat] , Buy_single -> record[card_to_buy -> nat]]"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_bad [ "compile-parameter" ; contract "coase.ligo" ; "main" ; "record cards = (map end : cards) ; card_patterns = (map end : card_patterns) ; next_id = 3n ; end" ] ;
  [%expect {|
    ligo: different kinds:  {"a":"sum[Transfer_single -> record[destination -> address , card_to_transfer -> nat] , Sell_single -> record[card_to_sell -> nat] , Buy_single -> record[card_to_buy -> nat]]","b":"record[next_id -> nat , cards -> (TO_Map (nat,record[card_pattern -> nat , card_owner -> address])) , card_patterns -> (TO_Map (nat,record[quantity -> nat , coefficient -> mutez]))]"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}] ;

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
                     IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
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
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     DIP 3 { DUP } ;
                     DIG 3 ;
                     CDR ;
                     PUSH nat 1 ;
                     ADD ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     DIP 4 { DUP } ;
                     DIG 4 ;
                     DIP 6 { DUP } ;
                     DIG 6 ;
                     DIP { DIP { DUP } ;
                           SWAP ;
                           SOME ;
                           DIP { DIP 5 { DUP } ; DIG 5 ; CAR ; CAR } } ;
                     UPDATE ;
                     DIP { DUP ; CDR ; SWAP ; CAR ; CDR } ;
                     PAIR ;
                     PAIR ;
                     DUP ;
                     DIP { DUP } ;
                     SWAP ;
                     CDR ;
                     DIP { DIP 7 { DUP } ;
                           DIG 7 ;
                           SOURCE ;
                           PAIR ;
                           SOME ;
                           DIP { DIP { DUP } ; SWAP ; CAR ; CDR } } ;
                     UPDATE ;
                     DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
                     SWAP ;
                     PAIR ;
                     PAIR ;
                     DUP ;
                     DIP { DUP } ;
                     SWAP ;
                     CDR ;
                     PUSH nat 1 ;
                     ADD ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     DUP ;
                     NIL operation ;
                     PAIR ;
                     DIP { DROP 11 } }
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
                     IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                     DUP ;
                     CAR ;
                     SOURCE ;
                     SWAP ;
                     COMPARE ;
                     NEQ ;
                     IF { PUSH string "This card doesn't belong to you" ; FAILWITH }
                        { PUSH unit Unit } ;
                     DIP { DUP } ;
                     SWAP ;
                     CDR ;
                     DIP { DIP 2 { DUP } ; DIG 2 ; CAR ; CAR } ;
                     GET ;
                     IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                     DUP ;
                     DIP { DUP } ;
                     SWAP ;
                     CDR ;
                     PUSH nat 1 ;
                     SWAP ;
                     SUB ;
                     ABS ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     DIP 4 { DUP } ;
                     DIG 4 ;
                     DIP 4 { DUP } ;
                     DIG 4 ;
                     CDR ;
                     DIP { DIP { DUP } ;
                           SWAP ;
                           SOME ;
                           DIP { DIP 5 { DUP } ; DIG 5 ; CAR ; CAR } } ;
                     UPDATE ;
                     DIP { DUP ; CDR ; SWAP ; CAR ; CDR } ;
                     PAIR ;
                     PAIR ;
                     DIP 6 { DUP } ;
                     DIG 6 ;
                     DIP { DUP ; CAR ; CDR ; NONE (pair (address %card_owner) (nat %card_pattern)) } ;
                     UPDATE ;
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
                     DIP { DIP 4 { DUP } ;
                           DIG 4 ;
                           DIP 4 { DUP } ;
                           DIG 4 ;
                           DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
                           SWAP ;
                           PAIR ;
                           PAIR } ;
                     PAIR ;
                     DIP { DROP 13 } } ;
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
                 IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                 DUP ;
                 CAR ;
                 SOURCE ;
                 SWAP ;
                 COMPARE ;
                 NEQ ;
                 IF { PUSH string "This card doesn't belong to you" ; FAILWITH }
                    { PUSH unit Unit } ;
                 DIP 3 { DUP } ;
                 DIG 3 ;
                 DIP 5 { DUP } ;
                 DIG 5 ;
                 CAR ;
                 DIP { DIP 2 { DUP } ;
                       DIG 2 ;
                       DIP 6 { DUP } ;
                       DIG 6 ;
                       CDR ;
                       SWAP ;
                       CDR ;
                       SWAP ;
                       PAIR ;
                       SOME ;
                       DIP { DIP 3 { DUP } ; DIG 3 } } ;
                 UPDATE ;
                 DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
                 SWAP ;
                 PAIR ;
                 PAIR ;
                 NIL operation ;
                 PAIR ;
                 DIP { DROP 7 } } ;
             DIP { DROP 2 } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "multisig.ligo" ; "main" ] ;
  [%expect {|
    { parameter
        (pair (pair (nat %counter) (lambda %message unit (list operation)))
              (list %signatures (pair (key_hash %0) (signature %1)))) ;
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
             DUP ;
             DIP { DIP 2 { DUP } ; DIG 2 } ;
             PAIR ;
             DIP { DIP { DUP } ; SWAP } ;
             PAIR ;
             DIP 3 { DUP } ;
             DIG 3 ;
             CAR ;
             CAR ;
             DIP { DIP 2 { DUP } ; DIG 2 ; CAR ; CDR } ;
             COMPARE ;
             NEQ ;
             IF { PUSH string "Counters does not match" ; FAILWITH }
                { DIP 3 { DUP } ;
                  DIG 3 ;
                  CDR ;
                  DIP { DIP 2 { DUP } ; DIG 2 ; CAR ; CAR ; PUSH nat 0 ; SWAP ; PAIR } ;
                  ITER { SWAP ;
                         PAIR ;
                         DUP ;
                         CAR ;
                         CAR ;
                         DIP { DUP } ;
                         SWAP ;
                         CAR ;
                         CDR ;
                         DIP 2 { DUP } ;
                         DIG 2 ;
                         CDR ;
                         DIP 2 { DUP } ;
                         DIG 2 ;
                         DIP { DIP { DUP } ; SWAP } ;
                         PAIR ;
                         DIP 3 { DUP } ;
                         DIG 3 ;
                         IF_CONS
                           { DIP 4 { DUP } ;
                             DIG 4 ;
                             DIP 4 { DUP } ;
                             DIG 4 ;
                             CAR ;
                             DIP { DIP { DUP } ; SWAP ; HASH_KEY } ;
                             COMPARE ;
                             EQ ;
                             IF { DIP 5 { DUP } ;
                                  DIG 5 ;
                                  DIP 2 { DUP } ;
                                  DIG 2 ;
                                  DIP { DIP 5 { DUP } ;
                                        DIG 5 ;
                                        CDR ;
                                        DIP { DIP 10 { DUP } ;
                                              DIG 10 ;
                                              DIP { DIP 12 { DUP } ; DIG 12 ; CAR ; CAR } ;
                                              PAIR ;
                                              DIP { DIP 11 { DUP } ; DIG 11 ; CDR ; CAR ; CHAIN_ID ; SWAP ; PAIR } ;
                                              PAIR ;
                                              PACK } } ;
                                  CHECK_SIGNATURE ;
                                  IF { DIP 6 { DUP } ;
                                       DIG 6 ;
                                       PUSH nat 1 ;
                                       ADD ;
                                       DIP { DUP } ;
                                       SWAP ;
                                       DIP { DUP } ;
                                       SWAP ;
                                       DIP { DROP 2 } }
                                     { PUSH string "Invalid signature" ; FAILWITH } ;
                                  DIP { DROP ; DUP } ;
                                  SWAP ;
                                  DIP { DUP } ;
                                  SWAP ;
                                  DIP { DROP 2 } }
                                { DUP } ;
                             DIP { DROP } ;
                             DIP 3 { DUP } ;
                             DIG 3 ;
                             DIP 3 { DUP } ;
                             DIG 3 ;
                             SWAP ;
                             CDR ;
                             SWAP ;
                             PAIR ;
                             CAR ;
                             DIP { DUP } ;
                             PAIR ;
                             DIP { DROP 3 } }
                           { DUP } ;
                         DIP { DROP } ;
                         DIP 4 { DUP } ;
                         DIG 4 ;
                         DIP 5 { DUP } ;
                         DIG 5 ;
                         CAR ;
                         DIP 2 { DUP } ;
                         DIG 2 ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         PAIR ;
                         SWAP ;
                         CDR ;
                         SWAP ;
                         PAIR ;
                         DUP ;
                         DIP { DUP } ;
                         SWAP ;
                         CAR ;
                         DIP 3 { DUP } ;
                         DIG 3 ;
                         CAR ;
                         SWAP ;
                         CDR ;
                         SWAP ;
                         PAIR ;
                         SWAP ;
                         CDR ;
                         SWAP ;
                         PAIR ;
                         CAR ;
                         DIP { DROP 6 } } ;
                  DIP 3 { DUP } ;
                  DIG 3 ;
                  DIP { DUP } ;
                  SWAP ;
                  CDR ;
                  DIP { DIP 4 { DUP } ; DIG 4 ; CDR ; CDR } ;
                  COMPARE ;
                  LT ;
                  IF { PUSH string "Not enough signatures passed the check" ; FAILWITH }
                     { DIP 4 { DUP } ;
                       DIG 4 ;
                       DIP 5 { DUP } ;
                       DIG 5 ;
                       CAR ;
                       CDR ;
                       PUSH nat 1 ;
                       ADD ;
                       DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
                       SWAP ;
                       PAIR ;
                       PAIR ;
                       DIP { DUP } ;
                       SWAP ;
                       DIP { DUP } ;
                       SWAP ;
                       DIP { DROP 2 } } ;
                  DIP { DROP } ;
                  DIP 2 { DUP } ;
                  DIG 2 ;
                  CAR ;
                  DIP { DUP } ;
                  PAIR ;
                  DIP { DROP 2 } } ;
             DIP { DROP } ;
             DUP ;
             CAR ;
             CAR ;
             UNIT ;
             EXEC ;
             DIP { DUP ; CDR } ;
             PAIR ;
             DIP { DROP 6 } } } |} ]

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
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     CAR ;
                     DUP ;
                     PACK ;
                     DUP ;
                     SIZE ;
                     DIP { DIP 3 { DUP } ; DIG 3 ; CAR ; CAR ; CDR } ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Message size exceed maximum limit" ; FAILWITH }
                        { PUSH unit Unit } ;
                     DIP 4 { DUP } ;
                     DIG 4 ;
                     EMPTY_SET address ;
                     PAIR ;
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     DIP { DIP 5 { DUP } ; DIG 5 ; CAR ; CDR ; CDR } ;
                     GET ;
                     IF_NONE
                       { DIP 5 { DUP } ;
                         DIG 5 ;
                         DIP 6 { DUP } ;
                         DIG 6 ;
                         CDR ;
                         CAR ;
                         CAR ;
                         SENDER ;
                         GET ;
                         IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                         PUSH nat 1 ;
                         ADD ;
                         SOME ;
                         DIP { DIP 6 { DUP } ; DIG 6 ; CDR ; CAR ; CAR } ;
                         SENDER ;
                         UPDATE ;
                         DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                         PAIR ;
                         PAIR ;
                         SWAP ;
                         PAIR ;
                         DIP { DUP } ;
                         SWAP ;
                         CAR ;
                         DIP { DUP } ;
                         PAIR ;
                         EMPTY_SET address ;
                         PUSH bool True ;
                         SENDER ;
                         UPDATE ;
                         SWAP ;
                         CDR ;
                         SWAP ;
                         PAIR ;
                         DIP { DROP } }
                       { DIP 6 { DUP } ;
                         DIG 6 ;
                         DIP { DUP } ;
                         SWAP ;
                         SENDER ;
                         MEM ;
                         IF { DUP }
                            { DIP 7 { DUP } ;
                              DIG 7 ;
                              DIP 8 { DUP } ;
                              DIG 8 ;
                              CDR ;
                              CAR ;
                              CAR ;
                              SENDER ;
                              GET ;
                              IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                              PUSH nat 1 ;
                              ADD ;
                              SOME ;
                              DIP { DIP 8 { DUP } ; DIG 8 ; CDR ; CAR ; CAR } ;
                              SENDER ;
                              UPDATE ;
                              DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                              PAIR ;
                              PAIR ;
                              SWAP ;
                              PAIR ;
                              DIP { DUP } ;
                              SWAP ;
                              DIP { DUP } ;
                              SWAP ;
                              DIP { DROP 2 } } ;
                         DIP { DROP } ;
                         DIP 2 { DUP } ;
                         DIG 2 ;
                         CAR ;
                         DIP { DUP } ;
                         PAIR ;
                         DIP 2 { DUP } ;
                         DIG 2 ;
                         PUSH bool True ;
                         SENDER ;
                         UPDATE ;
                         SWAP ;
                         CDR ;
                         SWAP ;
                         PAIR ;
                         DIP { DROP 2 } } ;
                     DIP { DROP } ;
                     DUP ;
                     CAR ;
                     DIP { DUP } ;
                     SWAP ;
                     CDR ;
                     DUP ;
                     CDR ;
                     CAR ;
                     CAR ;
                     SENDER ;
                     GET ;
                     IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                     DUP ;
                     DIP { DIP { DUP } ; SWAP ; CAR ; CDR ; CAR } ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Maximum number of proposal reached" ; FAILWITH }
                        { PUSH unit Unit } ;
                     DIP 7 { DUP } ;
                     DIG 7 ;
                     DIP { DIP 3 { DUP } ; DIG 3 } ;
                     PAIR ;
                     DIP { DIP 6 { DUP } ; DIG 6 ; NIL operation ; SWAP ; PAIR } ;
                     PAIR ;
                     DIP { DIP 2 { DUP } ; DIG 2 } ;
                     PAIR ;
                     DIP 4 { DUP } ;
                     DIG 4 ;
                     SIZE ;
                     DIP { DIP 3 { DUP } ; DIG 3 ; CDR ; CDR } ;
                     COMPARE ;
                     GE ;
                     IF { DIP 3 { DUP } ;
                          DIG 3 ;
                          DIP 8 { DUP } ;
                          DIG 8 ;
                          DIP { DIP 4 { DUP } ; DIG 4 ; CAR ; CDR ; CDR ; NONE (set address) } ;
                          UPDATE ;
                          DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CAR } ;
                          SWAP ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          DUP ;
                          CDR ;
                          CAR ;
                          CDR ;
                          DIP { DIP 9 { DUP } ; DIG 9 } ;
                          EXEC ;
                          DIP { DUP } ;
                          SWAP ;
                          DIP 2 { DUP } ;
                          DIG 2 ;
                          CDR ;
                          CAR ;
                          CDR ;
                          DIP { DIP 10 { DUP } ; DIG 10 } ;
                          CONCAT ;
                          SHA256 ;
                          DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CAR } ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          DUP ;
                          CDR ;
                          CAR ;
                          CAR ;
                          DIP { DUP } ;
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
                                 DIP { DUP } ;
                                 PAIR ;
                                 DIP { DIP 2 { DUP } ; DIG 2 } ;
                                 PAIR ;
                                 DIP 2 { DUP } ;
                                 DIG 2 ;
                                 DIP { DIP 12 { DUP } ; DIG 12 } ;
                                 MEM ;
                                 IF { DIP 3 { DUP } ;
                                      DIG 3 ;
                                      DIP 3 { DUP } ;
                                      DIG 3 ;
                                      DIP { DIP 2 { DUP } ;
                                            DIG 2 ;
                                            PUSH nat 1 ;
                                            SWAP ;
                                            SUB ;
                                            ABS ;
                                            SOME ;
                                            DIP { DIP 4 { DUP } ; DIG 4 ; CDR ; CAR ; CAR } } ;
                                      UPDATE ;
                                      DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                                      PAIR ;
                                      PAIR ;
                                      SWAP ;
                                      PAIR ;
                                      DIP { DUP } ;
                                      SWAP ;
                                      CAR ;
                                      DIP { DUP } ;
                                      PAIR ;
                                      DIP { DROP } }
                                    { DUP } ;
                                 DIP { DROP } ;
                                 DIP 4 { DUP } ;
                                 DIG 4 ;
                                 DIP 5 { DUP } ;
                                 DIG 5 ;
                                 CAR ;
                                 DIP 2 { DUP } ;
                                 DIG 2 ;
                                 CDR ;
                                 DIP { DROP ; CDR } ;
                                 PAIR ;
                                 CAR ;
                                 DIP { DROP 5 } } ;
                          DIP 4 { DUP } ;
                          DIG 4 ;
                          DIP 4 { DUP } ;
                          DIG 4 ;
                          SWAP ;
                          CAR ;
                          PAIR ;
                          DIP 3 { DUP } ;
                          DIG 3 ;
                          DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CAR } ;
                          SWAP ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          DIP 2 { DUP } ;
                          DIG 2 ;
                          SWAP ;
                          CAR ;
                          PAIR ;
                          CAR ;
                          DIP { DUP } ;
                          PAIR ;
                          DIP { DROP 4 } }
                        { DUP ;
                          DIP 4 { DUP } ;
                          DIG 4 ;
                          DIP 9 { DUP } ;
                          DIG 9 ;
                          DIP { DIP 6 { DUP } ;
                                DIG 6 ;
                                SOME ;
                                DIP { DIP 5 { DUP } ; DIG 5 ; CAR ; CDR ; CDR } } ;
                          UPDATE ;
                          DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CAR } ;
                          SWAP ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          SWAP ;
                          CAR ;
                          PAIR } ;
                     DIP { DROP } ;
                     DUP ;
                     CAR ;
                     CDR ;
                     CDR ;
                     DIP { DUP ; CDR } ;
                     PAIR ;
                     DIP { DROP 13 } } ;
                 DIP { DROP } }
               { DUP ;
                 DIP { DIP { DUP } ; SWAP } ;
                 PAIR ;
                 DUP ;
                 CDR ;
                 DIP { DUP } ;
                 SWAP ;
                 CAR ;
                 PACK ;
                 DUP ;
                 DIP { DIP { DUP } ; SWAP } ;
                 PAIR ;
                 DIP { DUP } ;
                 SWAP ;
                 DIP { DIP 2 { DUP } ; DIG 2 ; CAR ; CDR ; CDR } ;
                 GET ;
                 IF_NONE
                   { DUP }
                   { DUP ;
                     PUSH bool False ;
                     SENDER ;
                     UPDATE ;
                     DIP 4 { DUP } ;
                     DIG 4 ;
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     SIZE ;
                     DIP { DIP { DUP } ; SWAP ; SIZE } ;
                     COMPARE ;
                     NEQ ;
                     IF { DIP 5 { DUP } ;
                          DIG 5 ;
                          DIP 6 { DUP } ;
                          DIG 6 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          SENDER ;
                          GET ;
                          IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                          PUSH nat 1 ;
                          SWAP ;
                          SUB ;
                          ABS ;
                          SOME ;
                          DIP { DIP 6 { DUP } ; DIG 6 ; CDR ; CAR ; CAR } ;
                          SENDER ;
                          UPDATE ;
                          DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                          PAIR ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          DIP { DUP } ;
                          SWAP ;
                          DIP { DUP } ;
                          SWAP ;
                          DIP { DROP 2 } }
                        { DUP } ;
                     DIP { DROP } ;
                     DUP ;
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     DIP { DIP 5 { DUP } ; DIG 5 } ;
                     PAIR ;
                     DIP { DUP } ;
                     PAIR ;
                     DIP 3 { DUP } ;
                     DIG 3 ;
                     SIZE ;
                     PUSH nat 0 ;
                     SWAP ;
                     COMPARE ;
                     EQ ;
                     IF { DIP { DUP } ;
                          SWAP ;
                          DIP 7 { DUP } ;
                          DIG 7 ;
                          DIP { DIP 2 { DUP } ; DIG 2 ; CAR ; CDR ; CDR ; NONE (set address) } ;
                          UPDATE ;
                          DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CAR } ;
                          SWAP ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          DIP { DUP } ;
                          SWAP ;
                          CAR ;
                          DIP { DUP } ;
                          PAIR ;
                          DIP { DROP } }
                        { DUP ;
                          DIP 2 { DUP } ;
                          DIG 2 ;
                          DIP 8 { DUP } ;
                          DIG 8 ;
                          DIP { DIP 5 { DUP } ;
                                DIG 5 ;
                                SOME ;
                                DIP { DIP 3 { DUP } ; DIG 3 ; CAR ; CDR ; CDR } } ;
                          UPDATE ;
                          DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CAR } ;
                          SWAP ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          SWAP ;
                          CAR ;
                          PAIR } ;
                     DIP { DROP } ;
                     DIP 5 { DUP } ;
                     DIG 5 ;
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     DIP { DUP } ;
                     SWAP ;
                     CAR ;
                     CDR ;
                     SWAP ;
                     CDR ;
                     SWAP ;
                     PAIR ;
                     DIP { DUP } ;
                     SWAP ;
                     CDR ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     DIP { DUP } ;
                     SWAP ;
                     CAR ;
                     CDR ;
                     SWAP ;
                     CDR ;
                     SWAP ;
                     PAIR ;
                     DIP { DUP } ;
                     SWAP ;
                     CDR ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     DIP { DROP 5 } } ;
                 DIP { DROP } ;
                 DUP ;
                 CDR ;
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
                 DIP { DIP { DUP } ; SWAP ; CDR } ;
                 PAIR ;
                 DUP ;
                 CAR ;
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
                 DIP { DUP ; CAR ; CAR ; CDR ; DIP { DUP ; CAR ; CDR } ; PAIR } ;
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
                 DIP { DUP ; CDR ; CAR ; CAR ; CDR } ;
                 GET ;
                 IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                 DIP { DUP } ;
                 SWAP ;
                 CDR ;
                 CAR ;
                 CAR ;
                 CAR ;
                 DIP { DIP { DUP } ;
                       SWAP ;
                       CAR ;
                       DIP { DUP ;
                             PUSH int 1 ;
                             ADD ;
                             SOME ;
                             DIP { DIP { DUP } ; SWAP ; CDR ; CAR ; CAR ; CDR } } ;
                       UPDATE } ;
                 PAIR ;
                 DIP { DIP { DUP } ;
                       SWAP ;
                       CDR ;
                       CAR ;
                       CDR ;
                       CAR ;
                       DIP { DIP { DUP } ; SWAP ; CDR ; CAR ; CDR ; CDR } ;
                       PAIR } ;
                 PAIR ;
                 DIP { DIP { DUP } ; SWAP ; CDR ; CDR ; PUSH bool True ; SOURCE ; UPDATE } ;
                 PAIR ;
                 NIL operation ;
                 PAIR ;
                 DIP { DROP 3 } } ;
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
  [%expect {|
    ligo: bad type operator (TO_Map (unit,unit)):

     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_good [ "run-function" ; contract "failwith.ligo" ; "failer" ; "1" ] ;
  [%expect {|
    failwith("some_string") |}]

let%expect_test _ =
  run_ligo_good [ "run-function" ; contract "failwith.ligo" ; "failer" ; "1" ; "--format=json" ] ;
  [%expect {|
    {"status":"ok","content":"failwith(\"some_string\")"} |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_address_format.religo" ; "main" ] ;
  [%expect {|
    ligo: in file "bad_address_format.religo", line 2, characters 25-47. Badly formatted literal: @"KT1badaddr" {"location":"in file \"bad_address_format.religo\", line 2, characters 25-47"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_timestamp.ligo" ; "main" ] ;
  [%expect {|
    ligo: in file "bad_timestamp.ligo", line 5, characters 29-43. Badly formatted timestamp "badtimestamp":  {"location":"in file \"bad_timestamp.ligo\", line 5, characters 29-43"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
    run_ligo_good [ "dry-run" ; contract "redeclaration.ligo" ; "main" ; "unit" ; "0" ] ;
    [%expect {|record[1 -> 0 , 0 -> list[]] |}]

let%expect_test _ =
    run_ligo_good [ "dry-run" ; contract "double_main.ligo" ; "main" ; "unit" ; "0" ] ;
    [%expect {|record[1 -> 2 , 0 -> list[]] |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "subtle_nontail_fail.mligo" ; "main" ] ;
  [%expect {|
    { parameter unit ;
      storage unit ;
      code { PUSH bool True ;
             IF { PUSH string "This contract always fails" ; FAILWITH }
                { PUSH string "This contract still always fails" ; FAILWITH } } } |}]

let%expect_test _ =
  (* TODO should not be bad? *)
  run_ligo_good [ "dry-run" ; contract "subtle_nontail_fail.mligo" ; "main" ; "()" ; "()" ] ;
  [%expect {|
    failwith("This contract always fails") |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "self_in_lambda.mligo" ; "main" ] ;
  [%expect {|
    ligo: Wrong SELF_ADDRESS location: SELF_ADDRESS is only allowed at top-level

     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_good [ "compile-storage" ; contract "big_map.ligo" ; "main" ; "(big_map1,unit)" ] ;
  [%expect {|
    (Pair { Elt 23 0 ; Elt 42 0 } Unit) |}]