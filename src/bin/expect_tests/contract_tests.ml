open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

let %expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "coase.ligo" ; "main" ] ;
  [%expect {|
    { parameter
        (or (or (nat %buy_single) (nat %sell_single))
            (pair %transfer_single (nat %card_to_transfer) (address %destination))) ;
      storage
        (pair (pair (map %card_patterns nat (pair (mutez %coefficient) (nat %quantity)))
                    (map %cards nat (pair (address %card_owner) (nat %card_pattern))))
              (nat %next_id)) ;
      code { LAMBDA
               (pair (pair (nat %card_to_transfer) (address %destination))
                     (pair (pair (map %card_patterns nat (pair (mutez %coefficient) (nat %quantity)))
                                 (map %cards nat (pair (address %card_owner) (nat %card_pattern))))
                           (nat %next_id)))
               (pair (list operation)
                     (pair (pair (map %card_patterns nat (pair (mutez %coefficient) (nat %quantity)))
                                 (map %cards nat (pair (address %card_owner) (nat %card_pattern))))
                           (nat %next_id)))
               { DUP ;
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
                 NIL operation ;
                 DUP ;
                 DIP { DIP 3 { DUP } ; DIG 3 } ;
                 PAIR ;
                 DIP { DROP 6 } } ;
             LAMBDA
               (pair nat
                     (pair (pair (map %card_patterns nat (pair (mutez %coefficient) (nat %quantity)))
                                 (map %cards nat (pair (address %card_owner) (nat %card_pattern))))
                           (nat %next_id)))
               (pair (list operation)
                     (pair (pair (map %card_patterns nat (pair (mutez %coefficient) (nat %quantity)))
                                 (map %cards nat (pair (address %card_owner) (nat %card_pattern))))
                           (nat %next_id)))
               { DUP ;
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
                 DUP ;
                 DIP { DIP 8 { DUP } ; DIG 8 } ;
                 PAIR ;
                 DIP { DROP 11 } } ;
             LAMBDA
               (pair nat
                     (pair (pair (map %card_patterns nat (pair (mutez %coefficient) (nat %quantity)))
                                 (map %cards nat (pair (address %card_owner) (nat %card_pattern))))
                           (nat %next_id)))
               (pair (list operation)
                     (pair (pair (map %card_patterns nat (pair (mutez %coefficient) (nat %quantity)))
                                 (map %cards nat (pair (address %card_owner) (nat %card_pattern))))
                           (nat %next_id)))
               { DUP ;
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
                 DIP { DROP 8 } } ;
             DIP 3 { DUP } ;
             DIG 3 ;
             CAR ;
             DIP 4 { DUP } ;
             DIG 4 ;
             CDR ;
             DIP { DUP } ;
             SWAP ;
             IF_LEFT
               { DUP ;
                 IF_LEFT
                   { DUP ;
                     DUP ;
                     DIP { DIP 3 { DUP } ; DIG 3 } ;
                     PAIR ;
                     DIP { DIP 5 { DUP } ; DIG 5 } ;
                     EXEC ;
                     DIP { DROP 2 } }
                   { DUP ;
                     DUP ;
                     DIP { DIP 3 { DUP } ; DIG 3 } ;
                     PAIR ;
                     DIP { DIP 6 { DUP } ; DIG 6 } ;
                     EXEC ;
                     DIP { DROP 2 } } ;
                 DIP { DROP } }
               { DUP ;
                 DUP ;
                 DIP { DIP 2 { DUP } ; DIG 2 } ;
                 PAIR ;
                 DIP { DIP 6 { DUP } ; DIG 6 } ;
                 EXEC ;
                 DIP { DROP 2 } } ;
             DIP { DROP 6 } } } |} ] ;
