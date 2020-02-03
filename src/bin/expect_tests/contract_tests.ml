open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename
let bad_contract basename =
  "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_good [ "measure-contract" ; contract "coase.ligo" ; "main" ] ;
  [%expect {| 1682 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig.ligo" ; "main" ] ;
  [%expect {| 1003 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "multisig-v2.ligo" ; "main" ] ;
  [%expect {| 2522 bytes |}] ;

  run_ligo_good [ "measure-contract" ; contract "vote.mligo" ; "main" ] ;
  [%expect {| 586 bytes |}] ;

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
               { DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 DIP { DUP } ;
                 SWAP ;
                 PAIR ;
                 DUP ;
                 CAR ;
                 DIP { DUP } ;
                 SWAP ;
                 CDR ;
                 DUP ;
                 CAR ;
                 CAR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 GET ;
                 IF_NONE
                   { PUSH string "buy_single: No card pattern." ; FAILWITH }
                   { DUP ; DIP { DROP } } ;
                 DUP ;
                 PUSH nat 1 ;
                 SWAP ;
                 CDR ;
                 ADD ;
                 DIP { DUP } ;
                 SWAP ;
                 CAR ;
                 MUL ;
                 DUP ;
                 AMOUNT ;
                 SWAP ;
                 COMPARE ;
                 GT ;
                 IF { PUSH string "Not enough money" ; FAILWITH } { PUSH unit Unit } ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 PUSH nat 1 ;
                 DIG 4 ;
                 DUP ;
                 DUG 5 ;
                 CDR ;
                 ADD ;
                 SWAP ;
                 CAR ;
                 PAIR ;
                 DIG 4 ;
                 DUP ;
                 DUG 5 ;
                 DIG 5 ;
                 DUP ;
                 DUG 6 ;
                 CAR ;
                 CAR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 DIG 8 ;
                 DUP ;
                 DUG 9 ;
                 SWAP ;
                 SOME ;
                 SWAP ;
                 UPDATE ;
                 DIP { DUP ; CDR ; SWAP ; CAR ; CDR } ;
                 PAIR ;
                 PAIR ;
                 DUP ;
                 CAR ;
                 CDR ;
                 DIG 7 ;
                 DUP ;
                 DUG 8 ;
                 SENDER ;
                 PAIR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CDR ;
                 SWAP ;
                 SOME ;
                 SWAP ;
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
                 PUSH nat 1 ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CDR ;
                 ADD ;
                 SWAP ;
                 CAR ;
                 PAIR ;
                 DUP ;
                 NIL operation ;
                 PAIR ;
                 DIP { DROP 12 } }
               { DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 DIP { DUP } ;
                 SWAP ;
                 PAIR ;
                 DUP ;
                 CAR ;
                 DIP { DUP } ;
                 SWAP ;
                 CDR ;
                 DUP ;
                 CAR ;
                 CDR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 GET ;
                 IF_NONE
                   { PUSH string "sell_single: No card." ; FAILWITH }
                   { DUP ; DIP { DROP } } ;
                 DUP ;
                 SENDER ;
                 SWAP ;
                 CAR ;
                 COMPARE ;
                 NEQ ;
                 IF { PUSH string "This card doesn't belong to you" ; FAILWITH }
                    { PUSH unit Unit } ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CAR ;
                 CAR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CDR ;
                 GET ;
                 IF_NONE
                   { PUSH string "sell_single: No card pattern." ; FAILWITH }
                   { DUP ; DIP { DROP } } ;
                 DUP ;
                 PUSH nat 1 ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CDR ;
                 SUB ;
                 ABS ;
                 SWAP ;
                 CAR ;
                 PAIR ;
                 DIG 4 ;
                 DUP ;
                 DUG 5 ;
                 DIG 5 ;
                 DUP ;
                 DUG 6 ;
                 CAR ;
                 CAR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 DIG 6 ;
                 DUP ;
                 DUG 7 ;
                 CDR ;
                 SWAP ;
                 SOME ;
                 SWAP ;
                 UPDATE ;
                 DIP { DUP ; CDR ; SWAP ; CAR ; CDR } ;
                 PAIR ;
                 PAIR ;
                 DUP ;
                 CAR ;
                 CDR ;
                 DIG 7 ;
                 DUP ;
                 DUG 8 ;
                 NONE (pair (address %card_owner) (nat %card_pattern)) ;
                 SWAP ;
                 UPDATE ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CDR ;
                 DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 CAR ;
                 MUL ;
                 SENDER ;
                 CONTRACT unit ;
                 IF_NONE
                   { PUSH string "sell_single: No contract." ; FAILWITH }
                   { DUP ; DIP { DROP } } ;
                 DUP ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 UNIT ;
                 TRANSFER_TOKENS ;
                 DUP ;
                 NIL operation ;
                 SWAP ;
                 CONS ;
                 DIG 5 ;
                 DUP ;
                 DUG 6 ;
                 DIG 5 ;
                 DUP ;
                 DUG 6 ;
                 DIP { DUP ; CDR ; SWAP ; CAR ; CAR } ;
                 SWAP ;
                 PAIR ;
                 PAIR ;
                 DIP { DUP } ;
                 SWAP ;
                 PAIR ;
                 DIP { DROP 14 } } ;
             DIP { DROP } }
           { DIP { DUP } ;
             SWAP ;
             DIP { DUP } ;
             SWAP ;
             PAIR ;
             DUP ;
             CAR ;
             DIP { DUP } ;
             SWAP ;
             CDR ;
             DUP ;
             CAR ;
             CDR ;
             DUP ;
             DIG 3 ;
             DUP ;
             DUG 4 ;
             CAR ;
             GET ;
             IF_NONE
               { PUSH string "transfer_single: No card." ; FAILWITH }
               { DUP ; DIP { DROP } } ;
             DUP ;
             SENDER ;
             SWAP ;
             CAR ;
             COMPARE ;
             NEQ ;
             IF { PUSH string "This card doesn't belong to you" ; FAILWITH }
                { PUSH unit Unit } ;
             DIG 3 ;
             DUP ;
             DUG 4 ;
             DIG 3 ;
             DUP ;
             DUG 4 ;
             DIG 3 ;
             DUP ;
             DUG 4 ;
             DIG 7 ;
             DUP ;
             DUG 8 ;
             CDR ;
             SWAP ;
             CDR ;
             SWAP ;
             PAIR ;
             DIG 7 ;
             DUP ;
             DUG 8 ;
             CAR ;
             SWAP ;
             SOME ;
             SWAP ;
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
               { DIG 2 ; DUP ; DUG 3 ; NIL operation ; PAIR ; DIP { DROP } }
               { DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 DIP { DUP } ;
                 SWAP ;
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
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CAR ;
                 DUP ;
                 PACK ;
                 DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 CAR ;
                 CAR ;
                 CDR ;
                 DIP { DUP } ;
                 SWAP ;
                 SIZE ;
                 COMPARE ;
                 GT ;
                 IF { PUSH string "Message size exceed maximum limit" ; FAILWITH }
                    { PUSH unit Unit } ;
                 EMPTY_SET address ;
                 DIG 5 ;
                 DUP ;
                 DUG 6 ;
                 DIP { DUP } ;
                 SWAP ;
                 PAIR ;
                 DIG 6 ;
                 DUP ;
                 DUG 7 ;
                 CAR ;
                 CDR ;
                 CDR ;
                 DIG 4 ;
                 DUP ;
                 DUG 5 ;
                 GET ;
                 IF_NONE
                   { DIG 6 ;
                     DUP ;
                     DUG 7 ;
                     DIG 7 ;
                     DUP ;
                     DUG 8 ;
                     CDR ;
                     CAR ;
                     CAR ;
                     PUSH nat 1 ;
                     DIG 9 ;
                     DUP ;
                     DUG 10 ;
                     CDR ;
                     CAR ;
                     CAR ;
                     SENDER ;
                     GET ;
                     IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                     ADD ;
                     SOME ;
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
                     DIG 2 ;
                     DUP ;
                     DUG 3 ;
                     DIG 2 ;
                     DUP ;
                     DUG 3 ;
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
                     IF { DIG 7 ; DUP ; DUG 8 }
                        { DIG 7 ;
                          DUP ;
                          DUG 8 ;
                          DIG 8 ;
                          DUP ;
                          DUG 9 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          PUSH nat 1 ;
                          DIG 10 ;
                          DUP ;
                          DUG 11 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          SENDER ;
                          GET ;
                          IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                          ADD ;
                          SOME ;
                          SENDER ;
                          UPDATE ;
                          DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                          PAIR ;
                          PAIR ;
                          SWAP ;
                          PAIR ;
                          DIG 8 ;
                          DUP ;
                          DUG 9 ;
                          DIP { DUP } ;
                          SWAP ;
                          DIP { DROP 2 } } ;
                     DIP { DUP } ;
                     SWAP ;
                     PUSH bool True ;
                     SENDER ;
                     UPDATE ;
                     DIG 3 ;
                     DUP ;
                     DUG 4 ;
                     DIG 2 ;
                     DUP ;
                     DUG 3 ;
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
                 DIP { DUP } ;
                 SWAP ;
                 CAR ;
                 CDR ;
                 CAR ;
                 DIP { DUP } ;
                 SWAP ;
                 COMPARE ;
                 GT ;
                 IF { PUSH string "Maximum number of proposal reached" ; FAILWITH }
                    { PUSH unit Unit } ;
                 NIL operation ;
                 DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 DIP { DUP } ;
                 SWAP ;
                 PAIR ;
                 DIG 4 ;
                 DUP ;
                 DUG 5 ;
                 CDR ;
                 CDR ;
                 DIG 6 ;
                 DUP ;
                 DUG 7 ;
                 SIZE ;
                 COMPARE ;
                 GE ;
                 IF { DIG 4 ;
                      DUP ;
                      DUG 5 ;
                      DIG 5 ;
                      DUP ;
                      DUG 6 ;
                      CAR ;
                      CDR ;
                      CDR ;
                      DIG 12 ;
                      DUP ;
                      DUG 13 ;
                      NONE (set address) ;
                      SWAP ;
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
                      DIG 13 ;
                      DUP ;
                      DUG 14 ;
                      SWAP ;
                      EXEC ;
                      DIP { DUP } ;
                      SWAP ;
                      DIG 13 ;
                      DUP ;
                      DUG 14 ;
                      DIG 3 ;
                      DUP ;
                      DUG 4 ;
                      CDR ;
                      CAR ;
                      CDR ;
                      CONCAT ;
                      SHA256 ;
                      DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CAR } ;
                      SWAP ;
                      PAIR ;
                      PAIR ;
                      SWAP ;
                      PAIR ;
                      DUP ;
                      DIP { DUP } ;
                      SWAP ;
                      CDR ;
                      CAR ;
                      CAR ;
                      ITER { SWAP ;
                             PAIR ;
                             DUP ;
                             CAR ;
                             DIP { DUP } ;
                             SWAP ;
                             CDR ;
                             CAR ;
                             DIG 11 ;
                             DUP ;
                             DUG 12 ;
                             DIP { DUP } ;
                             SWAP ;
                             MEM ;
                             IF { DIP { DUP } ;
                                  SWAP ;
                                  DIG 2 ;
                                  DUP ;
                                  DUG 3 ;
                                  CDR ;
                                  CAR ;
                                  CAR ;
                                  PUSH nat 1 ;
                                  DIG 5 ;
                                  DUP ;
                                  DUG 6 ;
                                  CDR ;
                                  CDR ;
                                  SUB ;
                                  ABS ;
                                  DIG 3 ;
                                  DUP ;
                                  DUG 4 ;
                                  SWAP ;
                                  SOME ;
                                  SWAP ;
                                  UPDATE ;
                                  DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                                  PAIR ;
                                  PAIR ;
                                  SWAP ;
                                  PAIR ;
                                  DIG 2 ;
                                  DUP ;
                                  DUG 3 ;
                                  DIP { DUP } ;
                                  SWAP ;
                                  DIP { DROP 2 } }
                                { DIP { DUP } ; SWAP } ;
                             DIG 3 ;
                             DUP ;
                             DUG 4 ;
                             CDR ;
                             DIP { DUP } ;
                             SWAP ;
                             PAIR ;
                             CAR ;
                             DIP { DROP 4 } } ;
                      DIG 4 ;
                      DUP ;
                      DUG 5 ;
                      DIG 4 ;
                      DUP ;
                      DUG 5 ;
                      SWAP ;
                      CAR ;
                      PAIR ;
                      DIG 3 ;
                      DUP ;
                      DUG 4 ;
                      SWAP ;
                      CDR ;
                      SWAP ;
                      PAIR ;
                      DIG 2 ;
                      DUP ;
                      DUG 3 ;
                      SWAP ;
                      CAR ;
                      PAIR ;
                      CAR ;
                      DIP { DUP } ;
                      PAIR ;
                      DIP { DROP 4 } }
                    { DUP ;
                      DIG 5 ;
                      DUP ;
                      DUG 6 ;
                      DIG 6 ;
                      DUP ;
                      DUG 7 ;
                      CAR ;
                      CDR ;
                      CDR ;
                      DIG 8 ;
                      DUP ;
                      DUG 9 ;
                      DIG 14 ;
                      DUP ;
                      DUG 15 ;
                      SWAP ;
                      SOME ;
                      SWAP ;
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
           { DIP { DUP } ;
             SWAP ;
             DIP { DUP } ;
             SWAP ;
             PAIR ;
             DUP ;
             CDR ;
             DIP { DUP } ;
             SWAP ;
             CAR ;
             PACK ;
             DIP { DUP } ;
             SWAP ;
             CAR ;
             CDR ;
             CDR ;
             DIP { DUP } ;
             SWAP ;
             GET ;
             IF_NONE
               { DIP { DUP } ; SWAP }
               { DUP ;
                 PUSH bool False ;
                 SENDER ;
                 UPDATE ;
                 DUP ;
                 SIZE ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 SIZE ;
                 COMPARE ;
                 NEQ ;
                 IF { DIG 3 ;
                      DUP ;
                      DUG 4 ;
                      DIG 4 ;
                      DUP ;
                      DUG 5 ;
                      CDR ;
                      CAR ;
                      CAR ;
                      PUSH nat 1 ;
                      DIG 6 ;
                      DUP ;
                      DUG 7 ;
                      CDR ;
                      CAR ;
                      CAR ;
                      SENDER ;
                      GET ;
                      IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                      SUB ;
                      ABS ;
                      SOME ;
                      SENDER ;
                      UPDATE ;
                      DIP { DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; CDR } ;
                      PAIR ;
                      PAIR ;
                      SWAP ;
                      PAIR ;
                      DIG 4 ;
                      DUP ;
                      DUG 5 ;
                      DIP { DUP } ;
                      SWAP ;
                      DIP { DROP 2 } }
                    { DIG 3 ; DUP ; DUG 4 } ;
                 PUSH nat 0 ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 SIZE ;
                 COMPARE ;
                 EQ ;
                 IF { DUP ;
                      DIP { DUP } ;
                      SWAP ;
                      CAR ;
                      CDR ;
                      CDR ;
                      DIG 5 ;
                      DUP ;
                      DUG 6 ;
                      NONE (set address) ;
                      SWAP ;
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
                      DIG 2 ;
                      DUP ;
                      DUG 3 ;
                      CAR ;
                      CDR ;
                      CDR ;
                      DIG 4 ;
                      DUP ;
                      DUG 5 ;
                      DIG 7 ;
                      DUP ;
                      DUG 8 ;
                      SWAP ;
                      SOME ;
                      SWAP ;
                      UPDATE ;
                      DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CAR } ;
                      SWAP ;
                      PAIR ;
                      SWAP ;
                      PAIR ;
                      PAIR ;
                      DIP { DROP } } ;
                 DIG 5 ;
                 DUP ;
                 DUG 6 ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
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
           { PUSH nat 0 ;
             EMPTY_SET address ;
             PAIR ;
             DIP { DUP } ;
             SWAP ;
             CDR ;
             DIG 2 ;
             DUP ;
             DUG 3 ;
             CAR ;
             CDR ;
             PAIR ;
             PUSH nat 0 ;
             DIG 3 ;
             DUP ;
             DUG 4 ;
             CAR ;
             CAR ;
             PAIR ;
             PAIR ;
             PAIR ;
             NIL operation ;
             PAIR ;
             DIP { DROP } }
           { DIP { DUP } ;
             SWAP ;
             CDR ;
             DIP { DUP } ;
             SWAP ;
             PAIR ;
             DUP ;
             CDR ;
             NOW ;
             SENDER ;
             DIG 3 ;
             DUP ;
             DUG 4 ;
             CAR ;
             IF_LEFT
               { DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 PUSH nat 1 ;
                 DIG 5 ;
                 DUP ;
                 DUG 6 ;
                 CAR ;
                 CAR ;
                 CDR ;
                 ADD ;
                 DIP { DUP ; CDR ; SWAP ; CAR ; DUP ; CDR ; SWAP ; CAR ; CAR } ;
                 SWAP ;
                 PAIR ;
                 PAIR ;
                 PAIR ;
                 DIP { DROP } }
               { DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 PUSH nat 1 ;
                 DIG 5 ;
                 DUP ;
                 DUG 6 ;
                 CDR ;
                 CDR ;
                 ADD ;
                 DIP { DUP ; CAR ; SWAP ; CDR ; CAR } ;
                 SWAP ;
                 PAIR ;
                 SWAP ;
                 PAIR ;
                 DIP { DROP } } ;
             DUP ;
             DIP { DUP } ;
             SWAP ;
             CDR ;
             CAR ;
             DIG 3 ;
             DUP ;
             DUG 4 ;
             PUSH bool True ;
             SWAP ;
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
             DIP { DUP } ;
             SWAP ;
             CDR ;
             NIL operation ;
             DIG 2 ;
             DUP ;
             DUG 3 ;
             CAR ;
             CONS ;
             PAIR ;
             DIP { DROP 2 } } } |}];

  run_ligo_good [ "compile-contract" ; contract "tuples_no_annotation.religo" ; "main" ] ;
  [%expect {|
    { parameter int ;        
      storage (pair (pair int string) (pair nat bool)) ;
      code { PUSH bool False ;
             PUSH nat 2 ;
             PAIR ;
             PUSH string "2" ;
             PUSH int 2 ;
             PAIR ;
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
             DIG 2 ;
             DUP ;
             DUG 3 ;
             CDR ;
             NIL operation ;
             DIG 2 ;
             DUP ;
             DUG 3 ;
             CONS ;
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
             DIG 2 ;
             DUP ;
             DUG 3 ;
             CDR ;
             NIL operation ;
             DIG 2 ;
             DUP ;
             DUG 3 ;
             CONS ;
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
