open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_good [ "measure-contract" ; contract "coase.ligo" ; "main" ] ;
  [%expect {| 1874 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig.ligo" ; "main" ] ;
  [%expect {| 1163 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig-v2.ligo" ; "main" ] ;
  [%expect {| 2867 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "vote.mligo" ; "main" ] ;
  [%expect {| 617 bytes |}] ;

  run_ligo_good [ "compile-parameter" ; contract "coase.ligo" ; "main" ; "Buy_single (record card_to_buy = 1n end)" ] ;
  [%expect {| (Left (Left 1)) |}] ;

  run_ligo_good [ "compile-storage" ; contract "coase.ligo" ; "main" ; "record cards = (map end : cards) ; card_patterns = (map end : card_patterns) ; next_id = 3n ; end" ] ;
  [%expect {| (Pair (Pair {} {}) 3) |}] ;

  run_ligo_bad [ "compile-storage" ; contract "coase.ligo" ; "main" ; "Buy_single (record card_to_buy = 1n end)" ] ;
  [%expect {|
    ligo: different kinds:  {"a":"record[card_patterns -> (type_operator: Map (nat,record[coefficient -> mutez , quantity -> nat])) ,\n       cards -> (type_operator: Map (nat,record[card_owner -> address , card_pattern -> nat])) ,\n       next_id -> nat]","b":"sum[Buy_single -> record[card_to_buy -> nat] ,\n    Sell_single -> record[card_to_sell -> nat] ,\n    Transfer_single -> record[card_to_transfer -> nat ,\n                              destination -> address]]"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_bad [ "compile-parameter" ; contract "coase.ligo" ; "main" ; "record cards = (map end : cards) ; card_patterns = (map end : card_patterns) ; next_id = 3n ; end" ] ;
  [%expect {|
    ligo: different kinds:  {"a":"sum[Buy_single -> record[card_to_buy -> nat] ,\n    Sell_single -> record[card_to_sell -> nat] ,\n    Transfer_single -> record[card_to_transfer -> nat ,\n                              destination -> address]]","b":"record[card_patterns -> (type_operator: Map (nat,record[coefficient -> mutez , quantity -> nat])) ,\n       cards -> (type_operator: Map (nat,record[card_owner -> address , card_pattern -> nat])) ,\n       next_id -> nat]"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
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
                     IF_NONE
                       { PUSH string "buy_single: No card pattern." ; FAILWITH }
                       { DUP ; DIP { DROP } } ;
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
                     CDR ;
                     DIP { DIP 6 { DUP } ;
                           DIG 6 ;
                           SENDER ;
                           PAIR ;
                           SOME ;
                           DIP { DUP ; CAR ; CDR } } ;
                     UPDATE ;
                     DIP { DUP } ;
                     SWAP ;
                     DIP { DUP } ;
                     SWAP ;
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
                     DIP { DROP 12 } }
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
                     IF_NONE
                       { PUSH string "sell_single: No card." ; FAILWITH }
                       { DUP ; DIP { DROP } } ;
                     DUP ;
                     CAR ;
                     SENDER ;
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
                     IF_NONE
                       { PUSH string "sell_single: No card pattern." ; FAILWITH }
                       { DUP ; DIP { DROP } } ;
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
                     SENDER ;
                     CONTRACT unit ;
                     IF_NONE
                       { PUSH string "sell_single: No contract." ; FAILWITH }
                       { DUP ; DIP { DROP } } ;
                     DIP { DUP } ;
                     SWAP ;
                     DIP { DUP } ;
                     UNIT ;
                     TRANSFER_TOKENS ;
                     DUP ;
                     NIL operation ;
                     SWAP ;
                     CONS ;
                     DUP ;
                     DIP { DIP 5 { DUP } ;
                           DIG 5 ;
                           DIP 5 { DUP } ;
                           DIG 5 ;
                           DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
                           SWAP ;
                           PAIR ;
                           PAIR } ;
                     PAIR ;
                     DIP { DROP 14 } } ;
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
                 IF_NONE
                   { PUSH string "transfer_single: No card." ; FAILWITH }
                   { DUP ; DIP { DROP } } ;
                 DUP ;
                 CAR ;
                 SENDER ;
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
              (list %signatures (pair key_hash signature))) ;
      storage
        (pair (pair (list %auth key) (nat %counter)) (pair (string %id) (nat %threshold))) ;
      code { DUP ;
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
                  DIP 3 { DUP } ;
                  DIG 3 ;
                  CDR ;
                  DIP { DIP 2 { DUP } ; DIG 2 ; CAR ; CAR ; PUSH nat 0 ; SWAP ; PAIR } ;
                  ITER { SWAP ;
                         PAIR ;
                         DUP ;
                         CAR ;
                         CDR ;
                         DIP { DUP } ;
                         SWAP ;
                         CAR ;
                         CAR ;
                         DIP 2 { DUP } ;
                         DIG 2 ;
                         CDR ;
                         DIP { DUP } ;
                         SWAP ;
                         DIP { DIP 2 { DUP } ; DIG 2 } ;
                         PAIR ;
                         DIP 2 { DUP } ;
                         DIG 2 ;
                         IF_CONS
                           { DIP 3 { DUP } ;
                             DIG 3 ;
                             CAR ;
                             DIP { DUP ; HASH_KEY } ;
                             COMPARE ;
                             EQ ;
                             IF { DUP ;
                                  DIP { DIP 3 { DUP } ; DIG 3 ; CDR ; DIP { DIP 7 { DUP } ; DIG 7 } } ;
                                  CHECK_SIGNATURE ;
                                  IF { DIP 5 { DUP } ;
                                       DIG 5 ;
                                       PUSH nat 1 ;
                                       ADD ;
                                       DIP 6 { DUP } ;
                                       DIG 6 ;
                                       DIP { DUP } ;
                                       SWAP ;
                                       DIP { DROP 2 } }
                                     { PUSH string "Invalid signature" ; FAILWITH } ;
                                  DIP 6 { DUP } ;
                                  DIG 6 ;
                                  DIP { DUP } ;
                                  SWAP ;
                                  DIP { DROP 2 } }
                                { DIP 5 { DUP } ; DIG 5 } ;
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
                         DIP 5 { DUP } ;
                         DIG 5 ;
                         DIP { DUP } ;
                         SWAP ;
                         CAR ;
                         DIP { DUP ; CDR ; SWAP ; CAR ; CDR } ;
                         PAIR ;
                         PAIR ;
                         DIP { DUP } ;
                         SWAP ;
                         CDR ;
                         DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
                         SWAP ;
                         PAIR ;
                         PAIR ;
                         CAR ;
                         DIP { DROP 6 } } ;
                  DUP ;
                  CDR ;
                  DIP { DIP 3 { DUP } ; DIG 3 ; CDR ; CDR } ;
                  COMPARE ;
                  LT ;
                  IF { PUSH string "Not enough signatures passed the check" ; FAILWITH }
                     { DIP 3 { DUP } ;
                       DIG 3 ;
                       DIP 4 { DUP } ;
                       DIG 4 ;
                       CAR ;
                       CDR ;
                       PUSH nat 1 ;
                       ADD ;
                       DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
                       SWAP ;
                       PAIR ;
                       PAIR ;
                       DIP 4 { DUP } ;
                       DIG 4 ;
                       DIP { DUP } ;
                       SWAP ;
                       DIP { DROP 2 } } ;
                  DIP 4 { DUP } ;
                  DIG 4 ;
                  DIP { DUP } ;
                  SWAP ;
                  DIP { DROP 4 } } ;
             DIP { DUP } ;
             SWAP ;
             UNIT ;
             EXEC ;
             DIP { DUP } ;
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
                     EMPTY_SET address ;
                     DUP ;
                     DIP { DIP 5 { DUP } ; DIG 5 } ;
                     PAIR ;
                     DIP 3 { DUP } ;
                     DIG 3 ;
                     DIP { DIP 6 { DUP } ; DIG 6 ; CAR ; CDR ; CDR } ;
                     GET ;
                     IF_NONE
                       { DIP 6 { DUP } ;
                         DIG 6 ;
                         DIP 7 { DUP } ;
                         DIG 7 ;
                         CDR ;
                         CAR ;
                         CAR ;
                         SENDER ;
                         GET ;
                         IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                         PUSH nat 1 ;
                         ADD ;
                         SOME ;
                         DIP { DIP 7 { DUP } ; DIG 7 ; CDR ; CAR ; CAR } ;
                         SENDER ;
                         UPDATE ;
                         DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                         PAIR ;
                         PAIR ;
                         SWAP ;
                         PAIR ;
                         EMPTY_SET address ;
                         PUSH bool True ;
                         SENDER ;
                         UPDATE ;
                         DIP 2 { DUP } ;
                         DIG 2 ;
                         DIP 2 { DUP } ;
                         DIG 2 ;
                         SWAP ;
                         CAR ;
                         PAIR ;
                         CDR ;
                         DIP { DUP } ;
                         SWAP ;
                         PAIR ;
                         DIP { DROP 2 } }
                       { DUP ;
                         SENDER ;
                         MEM ;
                         IF { DIP 7 { DUP } ; DIG 7 }
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
                              DIP 8 { DUP } ;
                              DIG 8 ;
                              DIP { DUP } ;
                              SWAP ;
                              DIP { DROP 2 } } ;
                         DIP { DUP } ;
                         SWAP ;
                         PUSH bool True ;
                         SENDER ;
                         UPDATE ;
                         DIP 3 { DUP } ;
                         DIG 3 ;
                         DIP 2 { DUP } ;
                         DIG 2 ;
                         SWAP ;
                         CAR ;
                         PAIR ;
                         CDR ;
                         DIP { DUP } ;
                         SWAP ;
                         PAIR ;
                         DIP { DROP 3 } } ;
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
                     NIL operation ;
                     DUP ;
                     DIP { DIP 3 { DUP } ; DIG 3 } ;
                     PAIR ;
                     DIP 5 { DUP } ;
                     DIG 5 ;
                     SIZE ;
                     DIP { DIP 4 { DUP } ; DIG 4 ; CDR ; CDR } ;
                     COMPARE ;
                     GE ;
                     IF { DIP 4 { DUP } ;
                          DIG 4 ;
                          DIP 11 { DUP } ;
                          DIG 11 ;
                          DIP { DIP 5 { DUP } ; DIG 5 ; CAR ; CDR ; CDR ; NONE (set address) } ;
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
                          DIP { DIP 12 { DUP } ; DIG 12 } ;
                          EXEC ;
                          DIP { DUP } ;
                          SWAP ;
                          DIP 2 { DUP } ;
                          DIG 2 ;
                          CDR ;
                          CAR ;
                          CDR ;
                          DIP { DIP 13 { DUP } ; DIG 13 } ;
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
                                 DUP ;
                                 DIP { DIP 11 { DUP } ; DIG 11 } ;
                                 MEM ;
                                 IF { DIP { DUP } ;
                                      SWAP ;
                                      DIP { DUP } ;
                                      SWAP ;
                                      DIP { DIP 3 { DUP } ;
                                            DIG 3 ;
                                            CDR ;
                                            CDR ;
                                            PUSH nat 1 ;
                                            SWAP ;
                                            SUB ;
                                            ABS ;
                                            SOME ;
                                            DIP { DIP 2 { DUP } ; DIG 2 ; CDR ; CAR ; CAR } } ;
                                      UPDATE ;
                                      DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                                      PAIR ;
                                      PAIR ;
                                      SWAP ;
                                      PAIR ;
                                      DIP 2 { DUP } ;
                                      DIG 2 ;
                                      DIP { DUP } ;
                                      SWAP ;
                                      DIP { DROP 2 } }
                                    { DIP { DUP } ; SWAP } ;
                                 DIP 3 { DUP } ;
                                 DIG 3 ;
                                 CDR ;
                                 DIP { DUP } ;
                                 SWAP ;
                                 PAIR ;
                                 CAR ;
                                 DIP { DROP 4 } } ;
                          DIP 4 { DUP } ;
                          DIG 4 ;
                          DIP 4 { DUP } ;
                          DIG 4 ;
                          SWAP ;
                          CAR ;
                          PAIR ;
                          DIP 3 { DUP } ;
                          DIG 3 ;
                          SWAP ;
                          CDR ;
                          SWAP ;
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
                          DIP 5 { DUP } ;
                          DIG 5 ;
                          DIP 12 { DUP } ;
                          DIG 12 ;
                          DIP { DIP 7 { DUP } ;
                                DIG 7 ;
                                SOME ;
                                DIP { DIP 6 { DUP } ; DIG 6 ; CAR ; CDR ; CDR } } ;
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
                     DUP ;
                     DIP { DROP 17 } } ;
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
                 DIP { DIP { DUP } ; SWAP ; CAR ; CDR ; CDR } ;
                 GET ;
                 IF_NONE
                   { DIP { DUP } ; SWAP }
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
                     IF { DIP 3 { DUP } ;
                          DIG 3 ;
                          DIP 4 { DUP } ;
                          DIG 4 ;
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
                          DIP { DIP 4 { DUP } ; DIG 4 ; CDR ; CAR ; CAR } ;
                          SENDER ;
                          UPDATE ;
                          DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                          PAIR ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          DIP 4 { DUP } ;
                          DIG 4 ;
                          DIP { DUP } ;
                          SWAP ;
                          DIP { DROP 2 } }
                        { DIP 3 { DUP } ; DIG 3 } ;
                     DIP { DUP } ;
                     SWAP ;
                     SIZE ;
                     PUSH nat 0 ;
                     SWAP ;
                     COMPARE ;
                     EQ ;
                     IF { DUP ;
                          DIP 4 { DUP } ;
                          DIG 4 ;
                          DIP { DIP { DUP } ; SWAP ; CAR ; CDR ; CDR ; NONE (set address) } ;
                          UPDATE ;
                          DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CAR } ;
                          SWAP ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          DIP { DUP } ;
                          SWAP ;
                          DIP { DUP } ;
                          SWAP ;
                          DIP { DROP 2 } }
                        { DUP ;
                          DIP { DUP } ;
                          SWAP ;
                          DIP 5 { DUP } ;
                          DIG 5 ;
                          DIP { DIP 3 { DUP } ;
                                DIG 3 ;
                                SOME ;
                                DIP { DIP 2 { DUP } ; DIG 2 ; CAR ; CDR ; CDR } } ;
                          UPDATE ;
                          DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CAR } ;
                          SWAP ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          PAIR ;
                          DIP { DROP } } ;
                     DIP 5 { DUP } ;
                     DIG 5 ;
                     DIP 2 { DUP } ;
                     DIG 2 ;
                     DIP { DROP ; DUP } ;
                     SWAP ;
                     DIP { DROP 5 } } ;
                 DUP ;
                 NIL operation ;
                 PAIR ;
                 DIP { DROP 5 } } ;
             DIP { DROP 2 } } } |} ]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "vote.mligo" ; "main" ] ;
  [%expect {|
    { parameter
        (or (pair %reset (pair (timestamp %finish_time) (timestamp %start_time)) (string %title))
            (or %vote (unit %nay) (unit %yea))) ;
      storage
        (pair (pair (pair (timestamp %finish_time) (nat %nay))
                    (pair (timestamp %start_time) (string %title)))
              (pair (set %voters address) (nat %yea))) ;
      code { DUP ;
             CAR ;
             IF_LEFT
               { DUP ;
                 CAR ;
                 CAR ;
                 PUSH nat 0 ;
                 SWAP ;
                 PAIR ;
                 DIP { DUP ; CAR ; CDR ; DIP { DUP ; CDR } ; PAIR } ;
                 PAIR ;
                 DIP { PUSH nat 0 ; EMPTY_SET address ; PAIR } ;
                 PAIR ;
                 NIL operation ;
                 PAIR ;
                 DIP { DROP } }
               { DUP ;
                 DIP { DIP { DUP } ; SWAP ; CDR } ;
                 PAIR ;
                 DUP ;
                 CDR ;
                 NOW ;
                 SENDER ;
                 DIP 3 { DUP } ;
                 DIG 3 ;
                 CAR ;
                 IF_LEFT
                   { DIP 3 { DUP } ;
                     DIG 3 ;
                     DIP 4 { DUP } ;
                     DIG 4 ;
                     CAR ;
                     CAR ;
                     CDR ;
                     PUSH nat 1 ;
                     ADD ;
                     DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CDR ; SWAP ; CAR ; CAR } ;
                     SWAP ;
                     PAIR ;
                     PAIR ;
                     PAIR ;
                     DIP { DROP } }
                   { DIP 3 { DUP } ;
                     DIG 3 ;
                     DIP 4 { DUP } ;
                     DIG 4 ;
                     CDR ;
                     CDR ;
                     PUSH nat 1 ;
                     ADD ;
                     DIP { DUP ; CAR ; SWAP ; CDR ; CAR } ;
                     SWAP ;
                     PAIR ;
                     SWAP ;
                     PAIR ;
                     DIP { DROP } } ;
                 DUP ;
                 DIP 2 { DUP } ;
                 DIG 2 ;
                 DIP { DIP { DUP } ; SWAP ; CDR ; CAR ; PUSH bool True } ;
                 UPDATE ;
                 DIP { DUP ; CAR ; SWAP ; CDR ; CDR } ;
                 PAIR ;
                 SWAP ;
                 PAIR ;
                 NIL operation ;
                 PAIR ;
                 DIP { DROP 6 } } ;
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
  run_ligo_good [ "compile-contract" ; contract "amount_lambda.mligo" ; "main" ] ;
  (* AMOUNT should occur inside the second lambda, but not the first lambda *)
  [%expect {|
    { parameter bool ;
      storage (lambda unit mutez) ;
      code { DUP ;
             CAR ;
             IF { AMOUNT ;
                  DUP ;
                  LAMBDA
                    (pair mutez unit)
                    mutez
                    { DUP ; CAR ; SWAP ; CDR ; DIP { DUP } ; SWAP ; DIP { DROP 2 } } ;
                  SWAP ;
                  APPLY ;
                  DIP { DROP } }
                { LAMBDA unit mutez { DROP ; AMOUNT } } ;
             NIL operation ;
             PAIR ;
             DIP { DROP } } } |}]

let%expect_test _ =
  run_ligo_good [ "print-ast-typed" ; contract "sequence.mligo" ; ];
  [%expect {| const y = lambda (_) return let x = +1 in let _ = let x = +2 in UNIT() in let _ = let x = +23 in UNIT() in let _ = let x = +42 in UNIT() in x |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_type_operator.ligo" ; "main" ] ;
  [%expect {|
    ligo: bad type operator (TO_Map (unit,unit)):

     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_address_format.religo" ; "main" ] ;
  [%expect {|
    ligo: in file "bad_address_format.religo", line 2, characters 26-48. Badly formatted literal: @"KT1badaddr" {"location":"in file \"bad_address_format.religo\", line 2, characters 26-48"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; contract "bad_timestamp.ligo" ; "main" ] ;
  [%expect {|
    ligo: in file "bad_timestamp.ligo", line 7, characters 30-44. Badly formatted timestamp "badtimestamp":  {"location":"in file \"bad_timestamp.ligo\", line 7, characters 30-44"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
    run_ligo_good [ "dry-run" ; contract "redeclaration.ligo" ; "main" ; "unit" ; "0" ] ;
    [%expect {|( LIST_EMPTY() , 0 ) |}]

let%expect_test _ =
    run_ligo_good [ "dry-run" ; contract "double_main.ligo" ; "main" ; "unit" ; "0" ] ;
    [%expect {|( LIST_EMPTY() , 2 ) |}]

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

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_good [ "compile-storage" ; contract "big_map.ligo" ; "main" ; "(big_map1,unit)" ] ;
  [%expect {|
    (Pair { Elt 23 0 ; Elt 42 0 } Unit) |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "key_hash_comparable.ligo" ; "main" ] ;
  [%expect {|
    { parameter int ;
      storage (pair (map %one key_hash nat) (big_map %two key_hash bool)) ;
      code { DUP ; CDR ; NIL operation ; PAIR ; DIP { DROP } } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "long_sum_type_names.ligo" ; "main" ] ;
  [%expect {|
    ligo: Too long constructor 'Incrementttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt': names length is limited to 32 (tezos limitation)

     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_good [ "dry-run" ; contract "super-counter.mligo" ; "main" ; "test_param" ; "test_storage" ] ;
  [%expect {|
    ( LIST_EMPTY() , 3 ) |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "redundant_constructors.mligo" ; "main" ] ;
  [%expect {|
    ligo: redundant constructor:  {"constructor":"Add","environment":"- E[]\tT[union_a -> sum[Add -> int , Remove -> int]] ]"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "create_contract_toplevel.mligo" ; "main" ] ;
  [%expect {|
ligo: in file "create_contract_toplevel.mligo", line 4, character 35 to line 8, character 8. No free variable allowed in this lambda: variable 'store' {"expression":"CREATE_CONTRACT(lambda (#P:Some(( nat * string ))) : None return\n                let rhs#2 = #P in\n                let p = rhs#2.0 in\n                let s = rhs#2.1 in\n                ( LIST_EMPTY() : (type_operator: list(operation)) , store ) ,\n                NONE() : (type_operator: option(key_hash)) ,\n                300000000mutez ,\n                \"un\")","location":"in file \"create_contract_toplevel.mligo\", line 4, character 35 to line 8, character 8"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/introduction
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "create_contract_var.mligo" ; "main" ] ;
  [%expect {|
ligo: in file "create_contract_var.mligo", line 6, character 35 to line 10, character 5. No free variable allowed in this lambda: variable 'a' {"expression":"CREATE_CONTRACT(lambda (#P:Some(( nat * int ))) : None return\n                let rhs#2 = #P in\n                let p = rhs#2.0 in\n                let s = rhs#2.1 in\n                ( LIST_EMPTY() : (type_operator: list(operation)) , a ) ,\n                NONE() : (type_operator: option(key_hash)) ,\n                300000000mutez ,\n                1)","location":"in file \"create_contract_var.mligo\", line 6, character 35 to line 10, character 5"}


 If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/introduction
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "create_contract_no_inline.mligo" ; "main" ] ;
  [%expect {|
    ligo: in file "create_contract_no_inline.mligo", line 3, characters 40-46. unbound type variable:  {"variable":"return","location":"in file \"create_contract_no_inline.mligo\", line 3, characters 40-46","in":"- E[foo -> int]\tT[] ]","did_you_mean":"no suggestion"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_good [ "compile-contract" ; contract "create_contract.mligo" ; "main" ] ;
  [%expect {|
    { parameter string ;
      storage string ;
      code { PUSH string "un" ;
             PUSH mutez 300000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter nat ;
                 storage string ;
                 code { PUSH string "one" ; NIL operation ; PAIR ; DIP { DROP } } } ;
             PAIR ;
             DUP ;
             CAR ;
             NIL operation ;
             SWAP ;
             CONS ;
             DIP { DIP { DUP } ; SWAP ; CDR } ;
             PAIR ;
             DIP { DROP 2 } } } |}];

  run_ligo_good [ "compile-contract" ; contract "tuples_no_annotation.religo" ; "main" ] ;
  [%expect {|
    { parameter int ;        
      storage (pair (pair int string) (pair nat bool)) ;
      code { PUSH string "2" ;
             PUSH int 2 ;
             PAIR ;
             DIP { PUSH bool False ; PUSH nat 2 ; PAIR } ;
             PAIR ;
             NIL operation ;
             PAIR ;
             DIP { DROP } } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "self_type_annotation.ligo" ; "main" ] ;
  [%expect {|
    ligo: in file "self_type_annotation.ligo", line 8, characters 41-64. bad self type: expected (type_operator: Contract (int)) but got (type_operator: Contract (nat)) {"location":"in file \"self_type_annotation.ligo\", line 8, characters 41-64"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_good [ "compile-contract" ; contract "self_type_annotation.ligo" ; "main" ] ;
  [%expect {|
    { parameter nat ;
      storage int ;
      code { DUP ;
             SELF %default ;
             SWAP ;
             CDR ;
             NIL operation ;
             PAIR ;
             DIP { DROP 2 } } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; bad_contract "bad_contract.mligo" ; "main" ] ;
  [%expect {|
    ligo: in file "", line 0, characters 0-0. badly typed contract: unexpected entrypoint type {"location":"in file \"\", line 0, characters 0-0","entrypoint":"main","entrypoint_type":"( nat * int ) -> int"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "bad_contract2.mligo" ; "main" ] ;
  [%expect {|
    ligo: in file "", line 0, characters 0-0. bad return type: expected (type_operator: list(operation)), got string {"location":"in file \"\", line 0, characters 0-0","entrypoint":"main"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "bad_contract3.mligo" ; "main" ] ;
  [%expect {|
    ligo: in file "", line 0, characters 0-0. badly typed contract: expected {int} and {string} to be the same in the entrypoint type {"location":"in file \"\", line 0, characters 0-0","entrypoint":"main","entrypoint_type":"( nat * int ) -> ( (type_operator: list(operation)) * string )"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "self_with_entrypoint.ligo" ; "main" ] ;
  [%expect {|
    { parameter (or (unit %default) (int %toto)) ;
      storage nat ;
      code { SELF %toto ;
             DUP ;
             PUSH mutez 300000000 ;
             PUSH int 2 ;
             TRANSFER_TOKENS ;
             DUP ;
             NIL operation ;
             SWAP ;
             CONS ;
             DIP { DIP 2 { DUP } ; DIG 2 ; CDR } ;
             PAIR ;
             DIP { DROP 3 } } } |}] ;

  run_ligo_good [ "compile-contract" ; contract "self_without_entrypoint.ligo" ; "main" ] ;
  [%expect {|
    { parameter int ;
      storage nat ;
      code { SELF %default ;
             DUP ;
             PUSH mutez 300000000 ;
             PUSH int 2 ;
             TRANSFER_TOKENS ;
             DUP ;
             NIL operation ;
             SWAP ;
             CONS ;
             DIP { DIP 2 { DUP } ; DIG 2 ; CDR } ;
             PAIR ;
             DIP { DROP 3 } } } |}] ;

  run_ligo_bad [ "compile-contract" ; bad_contract "self_bad_entrypoint_format.ligo" ; "main" ] ;
  [%expect {|
    ligo: in file "self_bad_entrypoint_format.ligo", line 8, characters 52-58. bad entrypoint format: entrypoint "Toto" is badly formatted. We expect "%bar" for entrypoint Bar and "%default" when no entrypoint used {"location":"in file \"self_bad_entrypoint_format.ligo\", line 8, characters 52-58"}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad ["compile-contract"; bad_contract "nested_bigmap_1.religo"; "main"];
  [%expect {|
    ligo: It looks like you have nested a big map inside another big map. This is not supported. :  {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad ["compile-contract"; bad_contract "nested_bigmap_2.religo"; "main"];
  [%expect {|
    ligo: It looks like you have nested a big map inside another big map. This is not supported. :  {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}];
  
  run_ligo_bad ["compile-contract"; bad_contract "nested_bigmap_3.religo"; "main"];
  [%expect {|
    ligo: It looks like you have nested a big map inside another big map. This is not supported. :  {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}];

  run_ligo_bad ["compile-contract"; bad_contract "nested_bigmap_4.religo"; "main"];
  [%expect {|
    ligo: It looks like you have nested a big map inside another big map. This is not supported. :  {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]
