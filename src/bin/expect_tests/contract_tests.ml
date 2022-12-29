open Cli_expect

let contract = test
let contract_resource name = test ("res/" ^ name)
let bad_contract = bad_test

(* avoid pretty printing *)
let () = Ligo_unix.putenv ~key:"TERM" ~data:"dumb"

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; contract "coase.ligo" ];
  [%expect {|
    1097 bytes |}];
  run_ligo_good [ "info"; "measure-contract"; contract "multisig.ligo" ];
  [%expect {|
    533 bytes |}];
  run_ligo_good [ "info"; "measure-contract"; contract "multisig-v2.ligo" ];
  [%expect {|
    1509 bytes |}];
  run_ligo_good [ "info"; "measure-contract"; contract "vote.mligo" ];
  [%expect {|
    420 bytes |}];
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "coase.ligo"
    ; "Buy_single (record [ card_to_buy = 1n ])"
    ];
  [%expect {|
    (Left (Left 1)) |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "coase.ligo"
    ; "record [ cards = (map [] : cards) ; card_patterns = (map [] : card_patterns) ; \
       next_id = 3n ]"
    ];
  [%expect {|
    (Pair (Pair {} {}) 3) |}];
  run_ligo_bad
    [ "compile"
    ; "storage"
    ; contract "coase.ligo"
    ; "Buy_single (record [ card_to_buy = 1n ])"
    ];
  [%expect
    {|
    Invalid command line argument.
    The provided storage does not have the correct type for the contract.
    File "../../test/contracts/coase.ligo", line 122, character 0 to line 127, character 3:
    121 |
    122 | function main (const action : parameter; const s : storage) : return is
    123 |   case action of [
    124 |     Buy_single (bs)      -> buy_single (bs, s)
    125 |   | Sell_single (as)     -> sell_single (as, s)
    126 |   | Transfer_single (at) -> transfer_single (at, s)
    127 |   ]

    Invalid type(s).
    Expected: "storage", but got: "parameter". |}];
  run_ligo_bad
    [ "compile"
    ; "parameter"
    ; contract "coase.ligo"
    ; "record [ cards = (map [] : cards) ; card_patterns = (map [] : card_patterns) ; \
       next_id = 3n ]"
    ];
  [%expect
    {|
    Invalid command line argument.
    The provided parameter does not have the correct type for the given entrypoint.
    File "../../test/contracts/coase.ligo", line 122, character 0 to line 127, character 3:
    121 |
    122 | function main (const action : parameter; const s : storage) : return is
    123 |   case action of [
    124 |     Buy_single (bs)      -> buy_single (bs, s)
    125 |   | Sell_single (as)     -> sell_single (as, s)
    126 |   | Transfer_single (at) -> transfer_single (at, s)
    127 |   ]

    Invalid type(s).
    Expected: "parameter", but got: "storage". |}];
  ()

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "timestamp.ligo"
    ; "(Tezos.get_now ())"
    ; "--now"
    ; "2042-01-01T00:00:00Z"
    ];
  [%expect
    {|
    File "../../test/contracts/timestamp.ligo", line 3, characters 21-22:
      2 |
      3 | function main (const p : unit; const s : storage_) :
      4 |   list (operation) * storage_ is ((nil: list (operation)), Tezos.get_now())
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/timestamp.ligo", line 3, characters 37-38:
      2 |
      3 | function main (const p : unit; const s : storage_) :
      4 |   list (operation) * storage_ is ((nil: list (operation)), Tezos.get_now())
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    "2042-01-01T00:00:29Z" |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "const.mligo" ];
  [%expect
    {|
    File "../../test/contracts/const.mligo", line 1, characters 31-32:
      1 | let const = fun (type a b) (a, b : a * b) : a -> a
      2 |
    :
    Warning: unused variable "b".
    Hint: replace it by "_b" to prevent this warning.

    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "const1.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "const2.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "coase.ligo" ];
  [%expect
    {|
    { parameter
        (or (or (nat %buy_single) (nat %sell_single))
            (pair %transfer_single (nat %card_to_transfer) (address %destination))) ;
      storage
        (pair (pair (map %card_patterns nat (pair (mutez %coefficient) (nat %quantity)))
                    (map %cards nat (pair (address %card_owner) (nat %card_pattern))))
              (nat %next_id)) ;
      code { UNPAIR ;
             IF_LEFT
               { IF_LEFT
                   { SWAP ;
                     DUP ;
                     CAR ;
                     CAR ;
                     DUP 3 ;
                     GET ;
                     IF_NONE { PUSH string "buy_single: No card pattern." ; FAILWITH } {} ;
                     PUSH nat 1 ;
                     DUP 2 ;
                     CDR ;
                     ADD ;
                     DUP 2 ;
                     CAR ;
                     MUL ;
                     AMOUNT ;
                     SWAP ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Not enough money" ; FAILWITH } {} ;
                     DUP ;
                     PUSH nat 1 ;
                     DIG 2 ;
                     CDR ;
                     ADD ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     DUP 2 ;
                     CAR ;
                     CAR ;
                     SWAP ;
                     DUP 4 ;
                     SWAP ;
                     SOME ;
                     SWAP ;
                     UPDATE ;
                     SWAP ;
                     UNPAIR ;
                     CDR ;
                     DIG 2 ;
                     PAIR ;
                     PAIR ;
                     DUP ;
                     CAR ;
                     CDR ;
                     DIG 2 ;
                     SENDER ;
                     PAIR ;
                     DUP 3 ;
                     CDR ;
                     SWAP ;
                     SOME ;
                     SWAP ;
                     UPDATE ;
                     SWAP ;
                     DUP ;
                     CDR ;
                     DUG 2 ;
                     CAR ;
                     CAR ;
                     PAIR ;
                     PAIR ;
                     DUP ;
                     PUSH nat 1 ;
                     DIG 2 ;
                     CDR ;
                     ADD ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     NIL operation }
                   { SWAP ;
                     DUP ;
                     CAR ;
                     CDR ;
                     DUP 3 ;
                     GET ;
                     IF_NONE { PUSH string "sell_single: No card." ; FAILWITH } {} ;
                     SENDER ;
                     DUP 2 ;
                     CAR ;
                     COMPARE ;
                     NEQ ;
                     IF { PUSH string "This card doesn't belong to you" ; FAILWITH } {} ;
                     DUP 2 ;
                     CAR ;
                     CAR ;
                     DUP 2 ;
                     CDR ;
                     GET ;
                     IF_NONE { PUSH string "sell_single: No card pattern." ; FAILWITH } {} ;
                     DUP ;
                     PUSH nat 1 ;
                     DIG 2 ;
                     CDR ;
                     SUB ;
                     ABS ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     DUP 3 ;
                     CAR ;
                     CAR ;
                     DUP 2 ;
                     DIG 3 ;
                     CDR ;
                     SWAP ;
                     SOME ;
                     SWAP ;
                     UPDATE ;
                     DIG 2 ;
                     UNPAIR ;
                     CDR ;
                     DIG 2 ;
                     PAIR ;
                     PAIR ;
                     SWAP ;
                     DUP 2 ;
                     CAR ;
                     CDR ;
                     DIG 3 ;
                     NONE (pair address nat) ;
                     SWAP ;
                     UPDATE ;
                     DIG 2 ;
                     DUP ;
                     CDR ;
                     DUG 2 ;
                     CAR ;
                     CAR ;
                     PAIR ;
                     PAIR ;
                     SWAP ;
                     UNPAIR ;
                     MUL ;
                     SENDER ;
                     CONTRACT unit ;
                     IF_NONE { PUSH string "sell_single: No contract." ; FAILWITH } {} ;
                     SWAP ;
                     UNIT ;
                     TRANSFER_TOKENS ;
                     SWAP ;
                     NIL operation ;
                     DIG 2 ;
                     CONS } }
               { SWAP ;
                 DUP ;
                 CAR ;
                 CDR ;
                 DUP ;
                 DUP 4 ;
                 CAR ;
                 GET ;
                 IF_NONE { PUSH string "transfer_single: No card." ; FAILWITH } {} ;
                 SENDER ;
                 DUP 2 ;
                 CAR ;
                 COMPARE ;
                 NEQ ;
                 IF { PUSH string "This card doesn't belong to you" ; FAILWITH } {} ;
                 CDR ;
                 DUP 4 ;
                 CDR ;
                 PAIR ;
                 DIG 3 ;
                 CAR ;
                 SWAP ;
                 SOME ;
                 SWAP ;
                 UPDATE ;
                 SWAP ;
                 DUP ;
                 CDR ;
                 DUG 2 ;
                 CAR ;
                 CAR ;
                 PAIR ;
                 PAIR ;
                 NIL operation } ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "multisig.ligo" ];
  [%expect
    {|
    { parameter
        (pair (pair (nat %counter) (lambda %message unit (list operation)))
              (list %signatures (pair key_hash signature))) ;
      storage (pair (pair (list %auth key) (nat %counter)) (string %id) (nat %threshold)) ;
      code { UNPAIR ;
             SWAP ;
             DUP ;
             CAR ;
             CDR ;
             DUP 3 ;
             CAR ;
             CAR ;
             COMPARE ;
             NEQ ;
             IF { PUSH string "Counters does not match" ; FAILWITH }
                { CHAIN_ID ;
                  DUP 2 ;
                  CDR ;
                  CAR ;
                  PAIR ;
                  DUP 3 ;
                  CAR ;
                  CAR ;
                  DUP 4 ;
                  CAR ;
                  CDR ;
                  PAIR ;
                  PAIR ;
                  PACK ;
                  PUSH nat 0 ;
                  DUP 3 ;
                  CAR ;
                  CAR ;
                  DUP 5 ;
                  CDR ;
                  ITER { DUP 2 ;
                         IF_CONS
                           { DIG 3 ;
                             DROP ;
                             SWAP ;
                             DUG 2 ;
                             DUP ;
                             HASH_KEY ;
                             DUP 3 ;
                             CAR ;
                             COMPARE ;
                             EQ ;
                             IF { DUP 5 ;
                                  DIG 2 ;
                                  CDR ;
                                  DIG 2 ;
                                  CHECK_SIGNATURE ;
                                  IF { PUSH nat 1 ; DIG 2 ; ADD ; SWAP }
                                     { PUSH string "Invalid signature" ; FAILWITH } }
                                { DROP 2 } }
                           { DROP } } ;
                  DIG 2 ;
                  DROP 2 ;
                  DUP 2 ;
                  CDR ;
                  CDR ;
                  SWAP ;
                  COMPARE ;
                  LT ;
                  IF { PUSH string "Not enough signatures passed the check" ; FAILWITH }
                     { DUP ;
                       DUP ;
                       CDR ;
                       PUSH nat 1 ;
                       DIG 3 ;
                       CAR ;
                       CDR ;
                       ADD ;
                       DIG 2 ;
                       CAR ;
                       CAR ;
                       PAIR ;
                       PAIR } } ;
             UNIT ;
             DIG 2 ;
             CAR ;
             CDR ;
             SWAP ;
             EXEC ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "multisig-v2.ligo" ];
  [%expect
    {|
    { parameter
        (or (or (unit %default) (lambda %send bytes (list operation)))
            (lambda %withdraw bytes (list operation))) ;
      storage
        (pair (pair (pair (set %authorized_addresses address) (nat %max_message_size))
                    (nat %max_proposal)
                    (map %message_store bytes (set address)))
              (pair (map %proposal_counters address nat) (bytes %state_hash))
              (nat %threshold)) ;
      code { UNPAIR ;
             IF_LEFT
               { IF_LEFT
                   { DROP ; NIL operation }
                   { SWAP ;
                     DUP ;
                     CAR ;
                     CAR ;
                     CAR ;
                     SENDER ;
                     MEM ;
                     NOT ;
                     IF { PUSH string "Unauthorized address" ; FAILWITH } {} ;
                     DUP 2 ;
                     PACK ;
                     DUP 2 ;
                     CAR ;
                     CAR ;
                     CDR ;
                     DUP 2 ;
                     SIZE ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Message size exceed maximum limit" ; FAILWITH } {} ;
                     DUP 2 ;
                     CAR ;
                     CDR ;
                     CDR ;
                     DUP 2 ;
                     GET ;
                     IF_NONE
                       { DUP 2 ;
                         DUP ;
                         CDR ;
                         CDR ;
                         DUP 2 ;
                         CDR ;
                         CAR ;
                         CDR ;
                         DUP 5 ;
                         CDR ;
                         CAR ;
                         CAR ;
                         PUSH nat 1 ;
                         DIG 6 ;
                         CDR ;
                         CAR ;
                         CAR ;
                         SENDER ;
                         GET ;
                         IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                         ADD ;
                         SENDER ;
                         SWAP ;
                         SOME ;
                         SWAP ;
                         UPDATE ;
                         PAIR ;
                         PAIR ;
                         SWAP ;
                         CAR ;
                         PAIR ;
                         SWAP ;
                         EMPTY_SET address ;
                         SENDER ;
                         PUSH bool True ;
                         SWAP ;
                         UPDATE }
                       { DUP ;
                         SENDER ;
                         MEM ;
                         IF {}
                            { DUP 3 ;
                              DUP ;
                              CDR ;
                              CDR ;
                              DUP 2 ;
                              CDR ;
                              CAR ;
                              CDR ;
                              DUP 6 ;
                              CDR ;
                              CAR ;
                              CAR ;
                              PUSH nat 1 ;
                              DIG 7 ;
                              CDR ;
                              CAR ;
                              CAR ;
                              SENDER ;
                              GET ;
                              IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                              ADD ;
                              SENDER ;
                              SWAP ;
                              SOME ;
                              SWAP ;
                              UPDATE ;
                              PAIR ;
                              PAIR ;
                              SWAP ;
                              CAR ;
                              PAIR ;
                              DUG 2 } ;
                         SENDER ;
                         PUSH bool True ;
                         SWAP ;
                         UPDATE } ;
                     DUP 3 ;
                     CDR ;
                     CAR ;
                     CAR ;
                     SENDER ;
                     GET ;
                     IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                     DUP 4 ;
                     CAR ;
                     CDR ;
                     CAR ;
                     SWAP ;
                     COMPARE ;
                     GT ;
                     IF { PUSH string "Maximum number of proposal reached" ; FAILWITH } {} ;
                     NIL operation ;
                     DUP 4 ;
                     CDR ;
                     CDR ;
                     DUP 3 ;
                     SIZE ;
                     COMPARE ;
                     GE ;
                     IF { DROP ;
                          DUP 3 ;
                          DUP ;
                          CDR ;
                          DIG 4 ;
                          CAR ;
                          CDR ;
                          CDR ;
                          DUP 5 ;
                          NONE (set address) ;
                          SWAP ;
                          UPDATE ;
                          DUP 3 ;
                          CAR ;
                          CDR ;
                          CAR ;
                          PAIR ;
                          DIG 2 ;
                          CAR ;
                          CAR ;
                          PAIR ;
                          PAIR ;
                          DUG 2 ;
                          DUP 3 ;
                          CDR ;
                          CAR ;
                          CDR ;
                          DIG 4 ;
                          SWAP ;
                          EXEC ;
                          DUP 4 ;
                          DUP ;
                          CDR ;
                          CDR ;
                          DIG 4 ;
                          DIG 5 ;
                          CDR ;
                          CAR ;
                          CDR ;
                          CONCAT ;
                          SHA256 ;
                          DUP 3 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          PAIR ;
                          PAIR ;
                          SWAP ;
                          CAR ;
                          PAIR ;
                          DUG 2 ;
                          DUP 3 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          ITER { UNPAIR ;
                                 DUP 4 ;
                                 DUP 2 ;
                                 MEM ;
                                 IF { DUP 5 ;
                                      DUP ;
                                      CDR ;
                                      CDR ;
                                      DUP 2 ;
                                      CDR ;
                                      CAR ;
                                      CDR ;
                                      DIG 7 ;
                                      CDR ;
                                      CAR ;
                                      CAR ;
                                      PUSH nat 1 ;
                                      DIG 6 ;
                                      SUB ;
                                      ABS ;
                                      DIG 5 ;
                                      SWAP ;
                                      SOME ;
                                      SWAP ;
                                      UPDATE ;
                                      PAIR ;
                                      PAIR ;
                                      SWAP ;
                                      CAR ;
                                      PAIR ;
                                      DUG 2 }
                                    { DROP 2 } } ;
                          SWAP ;
                          DROP }
                        { DIG 4 ;
                          DROP ;
                          DUP 4 ;
                          DUP ;
                          CDR ;
                          DIG 5 ;
                          CAR ;
                          CDR ;
                          CDR ;
                          DIG 4 ;
                          DIG 5 ;
                          SWAP ;
                          SOME ;
                          SWAP ;
                          UPDATE ;
                          DUP 3 ;
                          CAR ;
                          CDR ;
                          CAR ;
                          PAIR ;
                          DIG 2 ;
                          CAR ;
                          CAR ;
                          PAIR ;
                          PAIR ;
                          SWAP } } }
               { PACK ;
                 DUP 2 ;
                 CAR ;
                 CDR ;
                 CDR ;
                 DUP 2 ;
                 GET ;
                 IF_NONE
                   { DROP }
                   { DUP ;
                     SENDER ;
                     PUSH bool False ;
                     SWAP ;
                     UPDATE ;
                     DUP ;
                     SIZE ;
                     DIG 2 ;
                     SIZE ;
                     COMPARE ;
                     NEQ ;
                     IF { DUP 3 ;
                          DUP ;
                          CDR ;
                          CDR ;
                          DUP 2 ;
                          CDR ;
                          CAR ;
                          CDR ;
                          DUP 6 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          PUSH nat 1 ;
                          DIG 7 ;
                          CDR ;
                          CAR ;
                          CAR ;
                          SENDER ;
                          GET ;
                          IF_NONE { PUSH string "MAP FIND" ; FAILWITH } {} ;
                          SUB ;
                          ABS ;
                          SENDER ;
                          SWAP ;
                          SOME ;
                          SWAP ;
                          UPDATE ;
                          PAIR ;
                          PAIR ;
                          SWAP ;
                          CAR ;
                          PAIR ;
                          DUG 2 }
                        {} ;
                     PUSH nat 0 ;
                     DUP 2 ;
                     SIZE ;
                     COMPARE ;
                     EQ ;
                     IF { DROP ;
                          DUP 2 ;
                          DUP ;
                          CDR ;
                          DIG 3 ;
                          CAR ;
                          CDR ;
                          CDR ;
                          DIG 3 ;
                          NONE (set address) ;
                          SWAP ;
                          UPDATE ;
                          DUP 3 ;
                          CAR ;
                          CDR ;
                          CAR ;
                          PAIR ;
                          DIG 2 ;
                          CAR ;
                          CAR }
                        { DUP 3 ;
                          DUP ;
                          CDR ;
                          DIG 4 ;
                          CAR ;
                          CDR ;
                          CDR ;
                          DIG 3 ;
                          DIG 4 ;
                          SWAP ;
                          SOME ;
                          SWAP ;
                          UPDATE ;
                          DUP 3 ;
                          CAR ;
                          CDR ;
                          CAR ;
                          PAIR ;
                          DIG 2 ;
                          CAR ;
                          CAR } ;
                     PAIR ;
                     PAIR } ;
                 NIL operation } ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "vote.mligo" ];
  [%expect
    {|
{ parameter
    (or (pair %reset (pair (timestamp %finish_time) (timestamp %start_time)) (string %title))
        (or %vote (unit %nay) (unit %yea))) ;
  storage
    (pair (pair (pair (timestamp %finish_time) (nat %nay))
                (timestamp %start_time)
                (string %title))
          (set %voters address)
          (nat %yea)) ;
  code { UNPAIR ;
         IF_LEFT
           { SWAP ;
             DROP ;
             PUSH nat 0 ;
             EMPTY_SET address ;
             PAIR ;
             DUP 2 ;
             CDR ;
             DUP 3 ;
             CAR ;
             CDR ;
             PAIR ;
             PUSH nat 0 ;
             DIG 3 ;
             CAR ;
             CAR ;
             PAIR ;
             PAIR }
           { SENDER ;
             SWAP ;
             IF_LEFT
               { DROP ;
                 DUP 2 ;
                 CDR ;
                 DUP 3 ;
                 CAR ;
                 CDR ;
                 PUSH nat 1 ;
                 DUP 5 ;
                 CAR ;
                 CAR ;
                 CDR ;
                 ADD ;
                 DIG 4 ;
                 CAR ;
                 CAR ;
                 CAR ;
                 PAIR ;
                 PAIR }
               { DROP ;
                 PUSH nat 1 ;
                 DUP 3 ;
                 CDR ;
                 CDR ;
                 ADD ;
                 DUP 3 ;
                 CDR ;
                 CAR ;
                 PAIR ;
                 DIG 2 ;
                 CAR } ;
             PAIR ;
             DUP ;
             CDR ;
             CDR ;
             DUP 2 ;
             CDR ;
             CAR ;
             DIG 3 ;
             PUSH bool True ;
             SWAP ;
             UPDATE ;
             PAIR ;
             SWAP ;
             CAR } ;
         PAIR ;
         NIL operation ;
         PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "ticket_wallet.mligo" ];
  [%expect
    {|
{ parameter
    (or (ticket %receive unit)
        (pair %send (contract %destination (ticket unit)) (nat %amount) (address %ticketer))) ;
  storage (pair (address %manager) (big_map %tickets address (ticket unit))) ;
  code { PUSH mutez 0 ;
         AMOUNT ;
         COMPARE ;
         EQ ;
         IF {} { PUSH string "failed assertion" ; FAILWITH } ;
         UNPAIR ;
         SWAP ;
         UNPAIR ;
         DIG 2 ;
         IF_LEFT
           { READ_TICKET ;
             CAR ;
             DIG 3 ;
             NONE (ticket unit) ;
             DUP 3 ;
             GET_AND_UPDATE ;
             IF_NONE
               { DIG 2 }
               { DIG 3 ;
                 PAIR ;
                 JOIN_TICKETS ;
                 IF_NONE { PUSH string "impossible?" ; FAILWITH } {} } ;
             SOME ;
             DIG 2 ;
             GET_AND_UPDATE ;
             DROP ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR }
           { DUP 2 ;
             SENDER ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             DIG 2 ;
             NONE (ticket unit) ;
             DUP 3 ;
             GET 4 ;
             GET_AND_UPDATE ;
             IF_NONE
               { DROP 3 ; PUSH string "no tickets" ; FAILWITH }
               { READ_TICKET ;
                 CDR ;
                 CDR ;
                 DUP 4 ;
                 GET 3 ;
                 DUP ;
                 DIG 2 ;
                 SUB ;
                 ISNAT ;
                 IF_NONE { PUSH string "not enough tickets" ; FAILWITH } {} ;
                 SWAP ;
                 PAIR ;
                 SWAP ;
                 SPLIT_TICKET ;
                 IF_NONE
                   { DROP 3 ; PUSH string "impossible?" ; FAILWITH }
                   { UNPAIR ;
                     DUG 2 ;
                     SOME ;
                     DUP 4 ;
                     GET 4 ;
                     GET_AND_UPDATE ;
                     DROP ;
                     DIG 2 ;
                     CAR ;
                     PUSH mutez 0 ;
                     DIG 3 ;
                     TRANSFER_TOKENS ;
                     SWAP ;
                     DIG 2 ;
                     PAIR ;
                     NIL operation ;
                     DIG 2 ;
                     CONS ;
                     PAIR } } } } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "ticket_builder.mligo"; "--protocol"; "lima" ];
  [%expect
    {|
File "../../test/contracts/ticket_builder.mligo", line 29, characters 28-34:
 28 |       begin
 29 |         let ((ticketer, _), ticket) = (Tezos.read_ticket ticket : (address * (unit * nat)) * unit ticket) in
 30 |         assert (ticketer = Tezos.get_self_address ());
:
Warning: unused variable "ticket".
Hint: replace it by "_ticket" to prevent this warning.

{ parameter
    (or (ticket %burn unit)
        (pair %mint (contract %destination (ticket unit)) (nat %amount))) ;
  storage address ;
  code { PUSH mutez 0 ;
         AMOUNT ;
         COMPARE ;
         EQ ;
         IF {} { PUSH string "failed assertion" ; FAILWITH } ;
         UNPAIR ;
         IF_LEFT
           { READ_TICKET ;
             SWAP ;
             DROP ;
             CAR ;
             SELF_ADDRESS ;
             SWAP ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             NIL operation }
           { DUP 2 ;
             SENDER ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             DUP ;
             CDR ;
             UNIT ;
             TICKET ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             SWAP ;
             CAR ;
             PUSH mutez 0 ;
             DIG 2 ;
             TRANSFER_TOKENS ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CONS } ;
         PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "implicit.mligo" ];
  [%expect
    {|
      File "../../test/contracts/implicit.mligo", line 2, characters 6-7:
        1 | let main2 (p : key_hash) (s : unit) =
        2 |   let c : unit contract = Tezos.implicit_account p
        3 |   in ([] : operation list), unit
      :
      Warning: unused variable "c".
      Hint: replace it by "_c" to prevent this warning.

      File "../../test/contracts/implicit.mligo", line 1, characters 26-27:
        1 | let main2 (p : key_hash) (s : unit) =
        2 |   let c : unit contract = Tezos.implicit_account p
      :
      Warning: unused variable "s".
      Hint: replace it by "_s" to prevent this warning.

      { parameter key_hash ;
        storage unit ;
        code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "amount_lambda.mligo" ];
  (* AMOUNT should occur inside the second lambda, but not the first lambda *)
  [%expect
    {|
    File "../../test/contracts/amount_lambda.mligo", line 4, characters 7-8:
      3 |   let amt : tez = Tezos.get_amount () in
      4 |   fun (x : unit) -> amt
      5 |
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 2, characters 8-9:
      1 | (* should return a constant function *)
      2 | let f1 (x : unit) : unit -> tez =
      3 |   let amt : tez = Tezos.get_amount () in
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 8, characters 7-8:
      7 | let f2 (x : unit) : unit -> tez =
      8 |   fun (x : unit) -> Tezos.get_amount ()
      9 |
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 7, characters 8-9:
      6 | (* should return an impure function *)
      7 | let f2 (x : unit) : unit -> tez =
      8 |   fun (x : unit) -> Tezos.get_amount ()
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 10, characters 12-13:
      9 |
     10 | let main (b,s : bool * (unit -> tez)) : operation list * (unit -> tez) =
     11 |   (([] : operation list), (if b then f1 () else f2 ()))
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter bool ;
      storage (lambda unit mutez) ;
      code { CAR ;
             IF { LAMBDA unit mutez { DROP ; AMOUNT } }
                { LAMBDA unit mutez { DROP ; AMOUNT } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "bad_timestamp.ligo" ];
  [%expect
    {|
    File "../../test/contracts/bad_timestamp.ligo", line 7, characters 30-44:
      6 |   {
      7 |     var stamp : timestamp := ("badtimestamp" : timestamp)
      8 |   }

    Ill-formed timestamp "badtimestamp".
    At this point, a string with a RFC3339 notation or the number of seconds since Epoch is expected. |}]

let%expect_test _ =
  run_ligo_good [ "run"; "dry-run"; contract "redeclaration.ligo"; "unit"; "0" ];
  [%expect
    {|
      File "../../test/contracts/redeclaration.ligo", line 1, characters 20-21:
        1 | function foo (const p : unit) : int is 0
        2 |
      :
      Warning: unused variable "p".
      Hint: replace it by "_p" to prevent this warning.

      File "../../test/contracts/redeclaration.ligo", line 3, characters 21-22:
        2 |
        3 | function main (const p : unit; const s : int) : list (operation) * int is
        4 |   ((nil : list (operation)), foo (unit))
      :
      Warning: unused variable "p".
      Hint: replace it by "_p" to prevent this warning.

      File "../../test/contracts/redeclaration.ligo", line 3, characters 37-38:
        2 |
        3 | function main (const p : unit; const s : int) : list (operation) * int is
        4 |   ((nil : list (operation)), foo (unit))
      :
      Warning: unused variable "s".
      Hint: replace it by "_s" to prevent this warning.

      File "../../test/contracts/redeclaration.ligo", line 6, characters 20-21:
        5 |
        6 | function foo (const p : unit) : int is 1
      :
      Warning: unused variable "p".
      Hint: replace it by "_p" to prevent this warning.

      ( LIST_EMPTY() , 0 ) |}]

let%expect_test _ =
  run_ligo_good [ "run"; "dry-run"; contract "double_main.ligo"; "unit"; "0" ];
  [%expect
    {|
      File "../../test/contracts/double_main.ligo", line 5, characters 20-21:
        4 |
        5 | function main(const p : parameter; const s : storage) : return is
        6 |   ((nil : list(operation)), s+1)
      :
      Warning: unused variable "p".
      Hint: replace it by "_p" to prevent this warning.

      ( LIST_EMPTY() , 2 ) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "subtle_nontail_fail.mligo" ];
  [%expect
    {|
    File "../../test/contracts/subtle_nontail_fail.mligo", line 1, characters 10-12:
      1 | let main (ps : unit * unit) : operation list * unit =
      2 |   if true
    :
    Warning: unused variable "ps".
    Hint: replace it by "_ps" to prevent this warning.

    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH bool True ;
             IF { PUSH string "This contract always fails" ; FAILWITH }
                { PUSH string "This contract still always fails" ; FAILWITH } } } |}]

let%expect_test _ =
  (* TODO should not be bad? *)
  run_ligo_good [ "run"; "dry-run"; contract "subtle_nontail_fail.mligo"; "()"; "()" ];
  [%expect
    {|
    File "../../test/contracts/subtle_nontail_fail.mligo", line 1, characters 10-12:
      1 | let main (ps : unit * unit) : operation list * unit =
      2 |   if true
    :
    Warning: unused variable "ps".
    Hint: replace it by "_ps" to prevent this warning.

    failed with: "This contract always fails" |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "self_in_lambda.mligo" ];
  [%expect
    {| "Tezos.self" must be used directly and cannot be used via another function. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "not_comparable.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/not_comparable.mligo", line 1, characters 21-28:
      1 | let main ((_u, s) : (int set) set * unit) : operation list * unit = ([] : operation list), s
      2 |

    The set constructor needs a comparable type argument, but it was given a non-comparable one. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "contract"; bad_contract "not_comparable.mligo"; "-e"; "main2" ];
  [%expect
    {|
    File "../../test/contracts/negative/not_comparable.mligo", line 3, characters 22-29:
      2 |
      3 | let main2 ((_u, s) : (int set) ticket * unit) : operation list * unit = ([] : operation list), s

    The ticket constructor needs a comparable type argument, but it was given a non-comparable one. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "storage"; contract "big_map.ligo"; "(big_map1,unit)" ];
  [%expect {|
    (Pair { Elt 23 0 ; Elt 42 0 } Unit) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "key_hash_comparable.ligo" ];
  [%expect
    {|
    File "../../test/contracts/key_hash_comparable.ligo", line 8, characters 21-22:
      7 |
      8 | function main (const a : int; const store : storage) : return is
      9 |   ((nil : list (operation)), store)
    :
    Warning: unused variable "a".
    Hint: replace it by "_a" to prevent this warning.

    { parameter int ;
      storage (pair (map %one key_hash nat) (big_map %two key_hash bool)) ;
      code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "long_sum_type_names.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/long_sum_type_names.ligo", line 2, character 2 to line 4, character 18:
      1 | type action is
      2 | | Incrementttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt of int
      3 | // | Increment of int
      4 | | Decrement of int
      5 |

    Ill-formed data constructor "Incrementttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt".
    Data constructors have a maximum length of 32 characters, which is a limitation imposed by annotations in Tezos. |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "dry-run"; contract "super-counter.mligo"; "test_param"; "test_storage" ];
  [%expect {|
    ( LIST_EMPTY() , 3 ) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; contract "redundant_constructors.mligo"
    ];
  [%expect {| (Pair (Left (Left 42)) (Left 42)) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; contract "redundant_constructors_but_annotated.mligo"
    ];
  [%expect {| (Pair {} (Left 1)) |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "create_contract_toplevel.mligo" ];
  [%expect
    {|
File "../../test/contracts/negative/create_contract_toplevel.mligo", line 4, character 35 to line 8, character 8:
  3 | let main (_, store : string * string) : return =
  4 |   let toto : operation * address = Tezos.create_contract
  5 |     (fun (_p, _s : nat * string) -> (([] : operation list), store))
  6 |     (None: key_hash option)
  7 |     300tz
  8 |     "un"
  9 |   in

Not all free variables could be inlined in Tezos.create_contract usage: gen#175. |}];
  run_ligo_good [ "compile"; "contract"; contract "create_contract_var.mligo" ];
  [%expect
    {|
    File "../../test/contracts/create_contract_var.mligo", line 7, characters 10-11:
      6 |   let toto : operation * address = Tezos.create_contract
      7 |     (fun (p, s : nat * int) -> (([] : operation list), a))
      8 |     (None: key_hash option)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/create_contract_var.mligo", line 7, characters 13-14:
      6 |   let toto : operation * address = Tezos.create_contract
      7 |     (fun (p, s : nat * int) -> (([] : operation list), a))
      8 |     (None: key_hash option)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/create_contract_var.mligo", line 5, characters 10-16:
      4 |
      5 | let main (action, store : string * string) : return =
      6 |   let toto : operation * address = Tezos.create_contract
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter string ;
      storage string ;
      code { CDR ;
             PUSH int 1 ;
             PUSH mutez 300000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter nat ;
                 storage int ;
                 code { DROP ; PUSH int 2 ; NIL operation ; PAIR } } ;
             PAIR ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CAR ;
             CONS ;
             PAIR } } |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "create_contract_modfv.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/create_contract_modfv.mligo", line 8, characters 10-11:
      7 |   let toto : operation * address = Tezos.create_contract
      8 |     (fun (p, s : nat * string) -> (([] : operation list), Foo.store))
      9 |     (None: key_hash option)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 8, characters 13-14:
      7 |   let toto : operation * address = Tezos.create_contract
      8 |     (fun (p, s : nat * string) -> (([] : operation list), Foo.store))
      9 |     (None: key_hash option)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 3, characters 10-16:
      2 |
      3 | let main (action, store : string * string) : return =
      4 |   module Foo = struct
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 7, character 35 to line 11, character 8:
      6 |   end in
      7 |   let toto : operation * address = Tezos.create_contract
      8 |     (fun (p, s : nat * string) -> (([] : operation list), Foo.store))
      9 |     (None: key_hash option)
     10 |     300tz
     11 |     "un"
     12 |   in

    Not all free variables could be inlined in Tezos.create_contract usage: gen#176. |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "create_contract_no_inline.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 5, characters 20-21:
      4 |
      5 | let dummy_contract (p, s : nat * int) : return =
      6 |  (([] : operation list), foo)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 5, characters 23-24:
      4 |
      5 | let dummy_contract (p, s : nat * int) : return =
      6 |  (([] : operation list), foo)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 9, characters 11-15:
      8 | let main (action, store : int * int) : return =
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
     10 |   let toto : operation list = [ op ] in
    :
    Warning: unused variable "addr".
    Hint: replace it by "_addr" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 8, characters 10-16:
      7 |
      8 | let main (action, store : int * int) : return =
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 8, characters 18-23:
      7 |
      8 | let main (action, store : int * int) : return =
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 9, characters 19-89:
      8 | let main (action, store : int * int) : return =
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
     10 |   let toto : operation list = [ op ] in

    Not all free variables could be inlined in Tezos.create_contract usage: foo#186. |}];
  run_ligo_good [ "compile"; "contract"; contract "create_contract.mligo" ];
  [%expect
    {|
    File "../../test/contracts/create_contract.mligo", line 5, characters 10-11:
      4 |   let toto : operation * address = Tezos.create_contract
      5 |     (fun (p, s : nat * string) -> (([] : operation list), "one"))
      6 |     (None: key_hash option)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/create_contract.mligo", line 5, characters 13-14:
      4 |   let toto : operation * address = Tezos.create_contract
      5 |     (fun (p, s : nat * string) -> (([] : operation list), "one"))
      6 |     (None: key_hash option)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/create_contract.mligo", line 3, characters 10-16:
      2 |
      3 | let main (action, store : string * string) : return =
      4 |   let toto : operation * address = Tezos.create_contract
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    { parameter string ;
      storage string ;
      code { CDR ;
             PUSH string "un" ;
             PUSH mutez 300000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter nat ;
                 storage string ;
                 code { DROP ; PUSH string "one" ; NIL operation ; PAIR } } ;
             PAIR ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CAR ;
             CONS ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "self_type_annotation_warn.ligo" ];
  [%expect
    {|
    File "../../test/contracts/self_type_annotation_warn.ligo", line 8, characters 10-23:
      7 |   {
      8 |     const self_contract: contract(int) = Tezos.self ("%default");
      9 |   }
    :
    Warning: unused variable "self_contract".
    Hint: replace it by "_self_contract" to prevent this warning.

    File "../../test/contracts/self_type_annotation_warn.ligo", line 6, characters 21-22:
      5 |
      6 | function main (const p : parameter; const s : storage) : return is
      7 |   {
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/self_type_annotation_warn.ligo", line 8, characters 41-64:
      7 |   {
      8 |     const self_contract: contract(int) = Tezos.self ("%default");
      9 |   }

    Warning: Tezos.self type annotation.
    Annotation "contract (int)" was given, but contract being compiled would expect "contract (nat)".
    Note that "Tezos.self" refers to the current contract, so the parameters should be generally the same.
    { parameter nat ; storage int ; code { CDR ; NIL operation ; PAIR } } |}];
  run_ligo_good [ "compile"; "contract"; contract "self_type_annotation.ligo" ];
  [%expect
    {|
    { parameter nat ;
      storage address ;
      code { DROP ; SELF %default ; ADDRESS ; NIL operation ; PAIR } } |}];
  run_ligo_good
    [ "compile"; "contract"; contract "self_default_with_variant_parameter.mligo" ];
  [%expect
    {|
      { parameter (or (address %one) (unit %two)) ;
        storage address ;
        code { DROP ; SELF %default ; ADDRESS ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_contract.mligo" ];
  [%expect
    {|
File "../../test/contracts/negative/bad_contract.mligo", line 4, characters 10-16:
  3 |
  4 | let main (action, store : parameter * storage) : storage =
  5 |   store + 1
:
Warning: unused variable "action".
Hint: replace it by "_action" to prevent this warning.

File "../../test/contracts/negative/bad_contract.mligo", line 4, characters 4-8:
  3 |
  4 | let main (action, store : parameter * storage) : storage =
  5 |   store + 1

Invalid type for entrypoint "main".
An entrypoint must of type "parameter * storage -> operation list * storage". |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_contract2.mligo" ];
  [%expect
    {|
File "../../test/contracts/negative/bad_contract2.mligo", line 5, characters 10-16:
  4 |
  5 | let main (action, store : parameter * storage) : return =
  6 |   ("bad",store + 1)
:
Warning: unused variable "action".
Hint: replace it by "_action" to prevent this warning.

File "../../test/contracts/negative/bad_contract2.mligo", line 5, character 0 to line 6, character 19:
  4 |
  5 | let main (action, store : parameter * storage) : return =
  6 |   ("bad",store + 1)

Invalid type for entrypoint "main".
An entrypoint must of type "parameter * storage -> operation list * storage".
We expected a list of operations but we got string |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_contract3.mligo" ];
  [%expect
    {|
File "../../test/contracts/negative/bad_contract3.mligo", line 5, characters 10-16:
  4 |
  5 | let main (action, store : parameter * storage) : return =
  6 |   (([]: operation list),"bad")
:
Warning: unused variable "action".
Hint: replace it by "_action" to prevent this warning.

File "../../test/contracts/negative/bad_contract3.mligo", line 5, characters 18-23:
  4 |
  5 | let main (action, store : parameter * storage) : return =
  6 |   (([]: operation list),"bad")
:
Warning: unused variable "store".
Hint: replace it by "_store" to prevent this warning.

File "../../test/contracts/negative/bad_contract3.mligo", line 5, character 0 to line 6, character 30:
  4 |
  5 | let main (action, store : parameter * storage) : return =
  6 |   (([]: operation list),"bad")

Invalid type for entrypoint "main".
The storage type "int" of the function parameter must be the same as the storage type "string" of the return value. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "self_with_entrypoint.ligo" ];
  [%expect
    {|
    File "../../test/contracts/self_with_entrypoint.ligo", line 6, characters 21-22:
      5 |
      6 | function main (const p : parameter; const s : storage) : return is
      7 |   {
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    { parameter (or (unit %default) (int %toto)) ;
      storage nat ;
      code { CDR ;
             SELF %toto ;
             PUSH mutez 300000000 ;
             PUSH int 2 ;
             TRANSFER_TOKENS ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}];
  run_ligo_good [ "compile"; "contract"; contract "self_without_entrypoint.ligo" ];
  [%expect
    {|
    File "../../test/contracts/self_without_entrypoint.ligo", line 6, characters 21-22:
      5 |
      6 | function main (const p : parameter; const s : storage) : return is
      7 |   {
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    { parameter int ;
      storage nat ;
      code { CDR ;
             SELF %default ;
             PUSH mutez 300000000 ;
             PUSH int 2 ;
             TRANSFER_TOKENS ;
             SWAP ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "self_bad_entrypoint_format.ligo" ];
  [%expect
    {|
File "../../test/contracts/negative/self_bad_entrypoint_format.ligo", line 6, characters 21-22:
  5 |
  6 | function main (const p : parameter; const s : storage) : return is
  7 |   {
:
Warning: unused variable "p".
Hint: replace it by "_p" to prevent this warning.

File "../../test/contracts/negative/self_bad_entrypoint_format.ligo", line 8, characters 41-59:
  7 |   {
  8 |     const self_contract: contract(int) = Tezos.self("Toto") ;
  9 |     const op : operation = Tezos.transaction (2, 300tz, self_contract) ;

Invalid entrypoint "Toto". One of the following patterns is expected:
* "%bar" is expected for entrypoint "Bar"
* "%default" when no entrypoint is used.
Valid characters in annotation: ('a' .. 'z' | 'A' .. 'Z' | '_' | '.' | '%' | '@' | '0' .. '9'). |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "duplicate_record_field.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/duplicate_record_field.mligo", line 1, characters 9-34:
      1 | type r = { foo : int ; foo : int }
      2 |

    Duplicated field or variant name.
    Hint: Change the name. |}]

(* uncurrying example *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "uncurry_contract.mligo" ];
  let output = [%expect.output] in
  let lines = String.split_lines output in
  let lines = List.take lines 4 in
  let output = String.concat ~sep:"\n" lines in
  print_string output;
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { LAMBDA (pair unit unit unit unit) unit { DROP ; UNIT } ;
             LAMBDA (pair nat nat) nat { UNPAIR ; MUL } ; |}]

(* old uncurry bugs: *)
let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "let f (y : int) (x : int) (y : int) = (x, y) in f 1 2 3"
    ; "-s"
    ; "cameligo"
    ];
  [%expect {| ( 2 , 3 ) |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "let f (x0 : int) (x1 : int) (x2 : int) (x3 : int) (x4 : int) (x5 : int) (x6 : \
       int) (x7 : int) (x8 : int) (x9 : int) (x10 : int) : int list = [x0; x1; x2; x3; \
       x4; x5; x6; x7; x8; x9; x10] in f 0 1 2 3 4 5 6 7 8 9 10"
    ; "-s"
    ; "cameligo"
    ];
  [%expect
    {|
    CONS(0 ,
         CONS(1 ,
              CONS(2 ,
                   CONS(3 ,
                        CONS(4 ,
                             CONS(5 ,
                                  CONS(6 ,
                                       CONS(7 ,
                                            CONS(8 ,
                                                 CONS(9 ,
                                                      CONS(10 , LIST_EMPTY()))))))))))) |}]

(* uncurrying w/ interpret (old bug) *)
let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "mul 3n 4n"; "--init-file"; contract "uncurry_contract.mligo" ];
  [%expect {| +12 |}]

(* Edo combs example *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "edo_combs.mligo" ];
  [%expect
    {|
    File "../../test/contracts/edo_combs.mligo", line 10, characters 13-14:
      9 |
     10 | let main (p, s : param * int) : operation list * int =
     11 |   let { x = x; y = y; z = z; w = w } = p in
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter (pair (int %x) (int %y) (int %z) (int %w)) ;
      storage int ;
      code { CAR ; UNPAIR 4 ; ADD ; ADD ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "warning_duplicate3.mligo"
    ; "--protocol"
    ; "kathmandu"
    ];
  [%expect
    {|
    { parameter (pair (chest %c) (chest_key %ck)) ;
      storage int ;
      code { DROP ; PUSH int 1 ; NIL operation ; PAIR } } |}]

(* warning layout attribute on constructor *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "B 42n"
    ; "--init-file"
    ; contract "warning_layout.mligo"
    ];
  [%expect
    {|
    File "../../test/contracts/warning_layout.mligo", line 3, character 4 to line 6, character 13:
      2 |   [@layout comb]
      3 |     B of nat
      4 |   | C of int
      5 |   | D of string
      6 |   | A of unit
      7 |

    Warning: layout attribute only applying to B, probably ignored.


    Warning: The type of "B(+42)" is ambiguous: Inferred type is "parameter_ok" but could be of type "parameter_warns".
    Hint: You might want to add a type annotation.

    (Left 42)
  |}]

(* never test for PascaLIGO *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "never.ligo" ];
  [%expect
    {|
    { parameter (or (never %extend) (int %increment)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { SWAP ; DROP ; NEVER } { ADD } ;
             NIL operation ;
             PAIR } } |}]

(* never test for CameLIGO *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "never.mligo" ];
  [%expect
    {|
    { parameter (or (never %extend) (int %increment)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { SWAP ; DROP ; NEVER } { ADD } ;
             NIL operation ;
             PAIR } } |}]

(* never test for JsLIGO *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "never.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/never.jsligo", line 8, character 0 to line 15, character 1:
      7 |
      8 | let main = ([action, store] : [parameter, storage]) : [list<operation>, storage] => {
      9 |   return [
     10 |    (list([]) as list <operation>),
     11 |    (match (action, {
     12 |     Increment: (n : int) => store + n,
     13 |     Extend: (k : never) => (Tezos.never(k) as storage)}))
     14 |   ]
     15 | };

    Toplevel let declaration are silently change to const declaration.

    { parameter (or (never %extend) (int %increment)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { SWAP ; DROP ; NEVER } { ADD } ;
             NIL operation ;
             PAIR } } |}]

(* annotations and self *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "self_annotations.mligo" ];
  [%expect
    {|
    { parameter (or (unit %foo) (unit %b)) ;
      storage unit ;
      code { DROP ;
             SELF %foo ;
             PUSH mutez 0 ;
             UNIT ;
             TRANSFER_TOKENS ;
             UNIT ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "error_self_annotations.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/error_self_annotations.mligo", line 6, characters 10-44:
      5 | let main (_,_ : param * unit) : operation list * unit =
      6 |   let c = (Tezos.self("%a") : unit contract) in
      7 |   let op = Tezos.transaction () 0mutez c in

    Invalid entrypoint value.
    The entrypoint value does not match a constructor of the contract parameter. |}]

(* entrypoint check *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_get_entrypoint.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/bad_get_entrypoint.mligo", line 2, character 10 to line 4, character 85:
      1 | let main ((_, _) : (unit * unit)) : operation list * unit =
      2 |   let v = (Tezos.get_entrypoint_opt
      3 |            "foo"
      4 |            ("tz1fakefakefakefakefakefakefakcphLA5" : address) : unit contract option) in
      5 |   let u : unit = match v with

    Invalid entrypoint "foo". One of the following patterns is expected:
    * "%bar" is expected for entrypoint "Bar"
    * "%default" when no entrypoint is used.
    Valid characters in annotation: ('a' .. 'z' | 'A' .. 'Z' | '_' | '.' | '%' | '@' | '0' .. '9'). |}]

(* using test in compilation *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "compile_test.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/compile_test.mligo", line 12, character 0 to line 17, character 22:
     11 |    the smart contract parameter. *)
     12 | let main (action, store : parameter * storage) : return =
     13 |  ([] : operation list),    // No operations
     14 |  (match action with
     15 |    Increment (n) -> let _ = Test.log "foo" in add (store, n)
     16 |  | Decrement (n) -> sub (store, n)
     17 |  | Reset         -> 0)
     18 | let _test () =

    Invalid usage of a Test primitive or type in object ligo. |}]

(* remove unused declarations *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "remove_unused_module.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "remove_unused_toptup.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ; PUSH nat 2 ; PUSH nat 1 ; DIG 2 ; ADD ; ADD ; NIL operation ; PAIR } } |}]

(* wrong annotation in Bytes.unpack *)
let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; bad_contract "bad_annotation_unpack.mligo"
    ];
  [%expect
    {|
    File "../../test/contracts/negative/bad_annotation_unpack.mligo", line 1, characters 9-42:
      1 | let x = (Bytes.unpack (Bytes.pack "hello") : string)

    Invalid type(s)
    Cannot unify "option (^a)" with "string".
    Hint: "^a" represent placeholder type(s). |}]

(* check annotations' capitalization *)
let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "annotation_cases.mligo"; "-e"; "main1" ];
  [%expect
    {|
    { parameter (pair (pair (nat %AAA) (nat %fooB)) (nat %cCC)) ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "annotation_cases.mligo"; "-e"; "main2" ];
  [%expect
    {|
    { parameter (or (or (nat %AAA) (nat %fooB)) (nat %cCC)) ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "reuse_variable_name_top.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/reuse_variable_name_top.jsligo", line 2, characters 0-14:
      1 | let dog = 1;
      2 | let dog = true;

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/negative/reuse_variable_name_top.jsligo", line 1, characters 0-11:
      1 | let dog = 1;
      2 | let dog = true;

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/negative/reuse_variable_name_top.jsligo", line 2, characters 10-14:
      1 | let dog = 1;
      2 | let dog = true;

    Cannot redeclare block-scoped variable. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "reuse_variable_name_block.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/reuse_variable_name_block.jsligo", line 1, character 0 to line 5, character 1:
      1 | let foo = (): int => {
      2 |     let x = 2;
      3 |     let x = 2;
      4 |     return x;
      5 | }

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/negative/reuse_variable_name_block.jsligo", line 2, characters 8-9:
      1 | let foo = (): int => {
      2 |     let x = 2;
      3 |     let x = 2;
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    Internal error: Entrypoint main does not exist |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "evaluate-call"; contract "assert.mligo"; "(false, ())"; "-e"; "with_error" ];
  [%expect {| failed with: "my custom error" |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "evaluate-call"
    ; contract "assert.mligo"
    ; "(None: unit option)"
    ; "-e"
    ; "some_with_error"
    ];
  [%expect {| failed with: "my custom error" |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "evaluate-call"
    ; contract "assert.mligo"
    ; "(Some (): unit option)"
    ; "-e"
    ; "none_with_error"
    ];
  [%expect {| failed with: "my custom error" |}]

(* literal type "casting" inside modules *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "literal_type_cast.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage timestamp ;
      code { DROP ; PUSH timestamp 0 ; NIL operation ; PAIR } }
  |}]

(* JsLIGO export testing *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "modules_export_type.jsligo" ];
  [%expect {|
      Internal error: Entrypoint main does not exist |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "modules_export_const.jsligo" ];
  [%expect
    {|
      File "../../test/contracts/negative/modules_export_const.jsligo", line 5, characters 0-15:
        4 |
        5 | let a = Bar.foo;

      Toplevel let declaration are silently change to const declaration.

      File "../../test/contracts/negative/modules_export_const.jsligo", line 2, characters 4-15:
        1 | namespace Bar {
        2 |     let foo = 2
        3 | }

      Toplevel let declaration are silently change to const declaration.

      Internal error: Entrypoint main does not exist |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "modules_export_namespace.jsligo" ];
  [%expect
    {|
      File "../../test/contracts/negative/modules_export_namespace.jsligo", line 3, characters 8-17:
        2 |     namespace Foo {
        3 |         let a = 2;
        4 |     }

      Toplevel let declaration are silently change to const declaration.

      Internal error: Entrypoint main does not exist |}]

(* Test compile contract with Big_map.get_and_update for Hangzhou *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "ticket_wallet.mligo" ];
  [%expect
    {|
    { parameter
        (or (ticket %receive unit)
            (pair %send (contract %destination (ticket unit)) (nat %amount) (address %ticketer))) ;
      storage (pair (address %manager) (big_map %tickets address (ticket unit))) ;
      code { PUSH mutez 0 ;
             AMOUNT ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             UNPAIR ;
             SWAP ;
             UNPAIR ;
             DIG 2 ;
             IF_LEFT
               { READ_TICKET ;
                 CAR ;
                 DIG 3 ;
                 NONE (ticket unit) ;
                 DUP 3 ;
                 GET_AND_UPDATE ;
                 IF_NONE
                   { DIG 2 }
                   { DIG 3 ;
                     PAIR ;
                     JOIN_TICKETS ;
                     IF_NONE { PUSH string "impossible?" ; FAILWITH } {} } ;
                 SOME ;
                 DIG 2 ;
                 GET_AND_UPDATE ;
                 DROP ;
                 SWAP ;
                 PAIR ;
                 NIL operation ;
                 PAIR }
               { DUP 2 ;
                 SENDER ;
                 COMPARE ;
                 EQ ;
                 IF {} { PUSH string "failed assertion" ; FAILWITH } ;
                 DIG 2 ;
                 NONE (ticket unit) ;
                 DUP 3 ;
                 GET 4 ;
                 GET_AND_UPDATE ;
                 IF_NONE
                   { DROP 3 ; PUSH string "no tickets" ; FAILWITH }
                   { READ_TICKET ;
                     CDR ;
                     CDR ;
                     DUP 4 ;
                     GET 3 ;
                     DUP ;
                     DIG 2 ;
                     SUB ;
                     ISNAT ;
                     IF_NONE { PUSH string "not enough tickets" ; FAILWITH } {} ;
                     SWAP ;
                     PAIR ;
                     SWAP ;
                     SPLIT_TICKET ;
                     IF_NONE
                       { DROP 3 ; PUSH string "impossible?" ; FAILWITH }
                       { UNPAIR ;
                         DUG 2 ;
                         SOME ;
                         DUP 4 ;
                         GET 4 ;
                         GET_AND_UPDATE ;
                         DROP ;
                         DIG 2 ;
                         CAR ;
                         PUSH mutez 0 ;
                         DIG 3 ;
                         TRANSFER_TOKENS ;
                         SWAP ;
                         DIG 2 ;
                         PAIR ;
                         NIL operation ;
                         DIG 2 ;
                         CONS ;
                         PAIR } } } } } |}]

(* source location comments *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "noop.mligo"
    ; "--michelson-comments"
    ; "location"
    ; "--michelson-comments"
    ; "env"
    ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { { /* _ */ } ;
             { /* File "../../test/contracts/noop.mligo", line 1, characters 10-15 */
               CDR ;
               { /* _ */ } ;
               { /* File "../../test/contracts/noop.mligo", line 2, character 2 to line 6, character 28 */
                 { /* File "../../test/contracts/noop.mligo", line 2, characters 9-10 */
                   LAMBDA
                     unit
                     unit
                     { { /* x#151 */ } ;
                       { /* File "../../test/contracts/noop.mligo", line 2, characters 9-10 */ } } } ;
                 { /* f#150, _ */ } ;
                 { /* File "../../test/contracts/noop.mligo", line 3, character 2 to line 6, character 28 */
                   { /* File "../../test/contracts/noop.mligo", line 3, characters 18-21 */
                     SWAP ;
                     { /* File "../../test/contracts/noop.mligo", line 3, characters 18-19 */ DUP 2 } ;
                     SWAP ;
                     EXEC } ;
                   { /* s2#152, f#150 */ } ;
                   { /* File "../../test/contracts/noop.mligo", line 4, character 2 to line 6, character 28 */
                     { /* File "../../test/contracts/noop.mligo", line 4, characters 18-22 */
                       { /* File "../../test/contracts/noop.mligo", line 4, characters 20-22 */ } ;
                       { /* File "../../test/contracts/noop.mligo", line 4, characters 18-19 */ DUP 2 } ;
                       SWAP ;
                       EXEC } ;
                     { /* s3#153, f#150 */ } ;
                     { /* File "../../test/contracts/noop.mligo", line 5, character 2 to line 6, character 28 */
                       { /* File "../../test/contracts/noop.mligo", line 5, characters 10-14 */
                         { /* File "../../test/contracts/noop.mligo", line 5, characters 12-14 */ } ;
                         { /* File "../../test/contracts/noop.mligo", line 5, characters 10-11 */ SWAP } ;
                         SWAP ;
                         EXEC } ;
                       { /* s#154 */ } ;
                       { /* File "../../test/contracts/noop.mligo", line 6, characters 3-27 */
                         { /* File "../../test/contracts/noop.mligo", line 6, characters 26-27 */ } ;
                         { /* File "../../test/contracts/noop.mligo", line 6, characters 3-24 */
                           NIL operation
                               /* File "../../test/contracts/noop.mligo", line 6, characters 3-24 */
                           /* File "../../test/contracts/noop.mligo", line 6, characters 3-24 */ } ;
                         PAIR
                         /* File "../../test/contracts/noop.mligo", line 6, characters 3-27 */ } } } } } } } } |}]

(* JSON source location comments *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "noop.mligo"
    ; "--michelson-format"
    ; "json"
    ; "--michelson-comments"
    ; "location"
    ; "--michelson-comments"
    ; "env"
    ];
  [%expect
    {|
    { "types":
        [ { "type_content":
              [ "T_constant",
                { "language": "Michelson", "injection": [ "Unit" ],
                  "parameters": [] } ], "type_meta": null, "orig_var": null,
            "location":
              [ "File",
                { "start":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "1", "pos_bol": "1", "pos_cnum": "25" },
                      "point_num": "101", "point_bol": "76" },
                  "stop":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "1", "pos_bol": "1", "pos_cnum": "29" },
                      "point_num": "105", "point_bol": "76" } } ] },
          { "type_content":
              [ "T_constant",
                { "language": "Michelson", "injection": [ "Unit" ],
                  "parameters": [] } ], "type_meta": null, "orig_var": null,
            "location":
              [ "File",
                { "start":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "2", "pos_bol": "1", "pos_cnum": "13" },
                      "point_num": "146", "point_bol": "133" },
                  "stop":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "2", "pos_bol": "1", "pos_cnum": "17" },
                      "point_num": "150", "point_bol": "133" } } ] },
          { "type_content":
              [ "T_constant",
                { "language": "Michelson", "injection": [ "Unit" ],
                  "parameters": [] } ], "type_meta": null, "orig_var": null,
            "location":
              [ "File",
                { "start":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "2", "pos_bol": "1", "pos_cnum": "21" },
                      "point_num": "154", "point_bol": "133" },
                  "stop":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "2", "pos_bol": "1", "pos_cnum": "25" },
                      "point_num": "158", "point_bol": "133" } } ] },
          { "type_content":
              [ "T_constant",
                { "language": "Michelson", "injection": [ "Unit" ],
                  "parameters": [] } ], "type_meta": null, "orig_var": null,
            "location":
              [ "File",
                { "start":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "3", "pos_bol": "1", "pos_cnum": "11" },
                      "point_num": "177", "point_bol": "166" },
                  "stop":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "3", "pos_bol": "1", "pos_cnum": "15" },
                      "point_num": "181", "point_bol": "166" } } ] },
          { "type_content":
              [ "T_constant",
                { "language": "Michelson", "injection": [ "Unit" ],
                  "parameters": [] } ], "type_meta": null, "orig_var": null,
            "location":
              [ "File",
                { "start":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "4", "pos_bol": "1", "pos_cnum": "11" },
                      "point_num": "202", "point_bol": "191" },
                  "stop":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "4", "pos_bol": "1", "pos_cnum": "15" },
                      "point_num": "206", "point_bol": "191" } } ] },
          { "type_content":
              [ "T_constant",
                { "language": "Michelson", "injection": [ "List" ],
                  "parameters":
                    [ { "type_content":
                          [ "T_constant",
                            { "language": "Michelson",
                              "injection": [ "Operation" ], "parameters": [] } ],
                        "type_meta": null, "orig_var": null,
                        "location":
                          [ "File",
                            { "start":
                                { "byte":
                                    { "pos_fname":
                                        "../../test/contracts/noop.mligo",
                                      "pos_lnum": "6", "pos_bol": "1",
                                      "pos_cnum": "9" }, "point_num": "244",
                                  "point_bol": "235" },
                              "stop":
                                { "byte":
                                    { "pos_fname":
                                        "../../test/contracts/noop.mligo",
                                      "pos_lnum": "6", "pos_bol": "1",
                                      "pos_cnum": "18" }, "point_num": "253",
                                  "point_bol": "235" } } ] } ] } ],
            "type_meta":
              { "type_content":
                  [ "T_app",
                    { "type_operator":
                        { "name": "list", "counter": "0", "generated": false,
                          "location":
                            [ "File",
                              { "start":
                                  { "byte":
                                      { "pos_fname":
                                          "../../test/contracts/noop.mligo",
                                        "pos_lnum": "6", "pos_bol": "1",
                                        "pos_cnum": "9" }, "point_num": "244",
                                    "point_bol": "235" },
                                "stop":
                                  { "byte":
                                      { "pos_fname":
                                          "../../test/contracts/noop.mligo",
                                        "pos_lnum": "6", "pos_bol": "1",
                                        "pos_cnum": "23" }, "point_num": "258",
                                    "point_bol": "235" } } ] },
                      "arguments":
                        [ { "type_content":
                              [ "T_variable",
                                { "name": "operation", "counter": "0",
                                  "generated": false,
                                  "location":
                                    [ "File",
                                      { "start":
                                          { "byte":
                                              { "pos_fname":
                                                  "../../test/contracts/noop.mligo",
                                                "pos_lnum": "6", "pos_bol": "1",
                                                "pos_cnum": "9" },
                                            "point_num": "244",
                                            "point_bol": "235" },
                                        "stop":
                                          { "byte":
                                              { "pos_fname":
                                                  "../../test/contracts/noop.mligo",
                                                "pos_lnum": "6", "pos_bol": "1",
                                                "pos_cnum": "18" },
                                            "point_num": "253",
                                            "point_bol": "235" } } ] } ],
                            "sugar":
                              { "type_content":
                                  [ "T_variable",
                                    { "name": "operation", "counter": "0",
                                      "generated": false,
                                      "location":
                                        [ "File",
                                          { "start":
                                              { "byte":
                                                  { "pos_fname":
                                                      "../../test/contracts/noop.mligo",
                                                    "pos_lnum": "6",
                                                    "pos_bol": "1",
                                                    "pos_cnum": "9" },
                                                "point_num": "244",
                                                "point_bol": "235" },
                                            "stop":
                                              { "byte":
                                                  { "pos_fname":
                                                      "../../test/contracts/noop.mligo",
                                                    "pos_lnum": "6",
                                                    "pos_bol": "1",
                                                    "pos_cnum": "18" },
                                                "point_num": "253",
                                                "point_bol": "235" } } ] } ],
                                "location":
                                  [ "File",
                                    { "start":
                                        { "byte":
                                            { "pos_fname":
                                                "../../test/contracts/noop.mligo",
                                              "pos_lnum": "6", "pos_bol": "1",
                                              "pos_cnum": "9" },
                                          "point_num": "244",
                                          "point_bol": "235" },
                                      "stop":
                                        { "byte":
                                            { "pos_fname":
                                                "../../test/contracts/noop.mligo",
                                              "pos_lnum": "6", "pos_bol": "1",
                                              "pos_cnum": "18" },
                                          "point_num": "253",
                                          "point_bol": "235" } } ] },
                            "location":
                              [ "File",
                                { "start":
                                    { "byte":
                                        { "pos_fname":
                                            "../../test/contracts/noop.mligo",
                                          "pos_lnum": "6", "pos_bol": "1",
                                          "pos_cnum": "9" }, "point_num": "244",
                                      "point_bol": "235" },
                                  "stop":
                                    { "byte":
                                        { "pos_fname":
                                            "../../test/contracts/noop.mligo",
                                          "pos_lnum": "6", "pos_bol": "1",
                                          "pos_cnum": "18" }, "point_num": "253",
                                      "point_bol": "235" } } ] } ] } ],
                "sugar":
                  { "type_content":
                      [ "T_app",
                        { "type_operator":
                            { "name": "list", "counter": "0", "generated": false,
                              "location":
                                [ "File",
                                  { "start":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "6", "pos_bol": "1",
                                            "pos_cnum": "9" },
                                        "point_num": "244", "point_bol": "235" },
                                    "stop":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "6", "pos_bol": "1",
                                            "pos_cnum": "23" },
                                        "point_num": "258", "point_bol": "235" } } ] },
                          "arguments":
                            [ { "type_content":
                                  [ "T_variable",
                                    { "name": "operation", "counter": "0",
                                      "generated": false,
                                      "location":
                                        [ "File",
                                          { "start":
                                              { "byte":
                                                  { "pos_fname":
                                                      "../../test/contracts/noop.mligo",
                                                    "pos_lnum": "6",
                                                    "pos_bol": "1",
                                                    "pos_cnum": "9" },
                                                "point_num": "244",
                                                "point_bol": "235" },
                                            "stop":
                                              { "byte":
                                                  { "pos_fname":
                                                      "../../test/contracts/noop.mligo",
                                                    "pos_lnum": "6",
                                                    "pos_bol": "1",
                                                    "pos_cnum": "18" },
                                                "point_num": "253",
                                                "point_bol": "235" } } ] } ],
                                "location":
                                  [ "File",
                                    { "start":
                                        { "byte":
                                            { "pos_fname":
                                                "../../test/contracts/noop.mligo",
                                              "pos_lnum": "6", "pos_bol": "1",
                                              "pos_cnum": "9" },
                                          "point_num": "244",
                                          "point_bol": "235" },
                                      "stop":
                                        { "byte":
                                            { "pos_fname":
                                                "../../test/contracts/noop.mligo",
                                              "pos_lnum": "6", "pos_bol": "1",
                                              "pos_cnum": "18" },
                                          "point_num": "253",
                                          "point_bol": "235" } } ] } ] } ],
                    "location":
                      [ "File",
                        { "start":
                            { "byte":
                                { "pos_fname": "../../test/contracts/noop.mligo",
                                  "pos_lnum": "6", "pos_bol": "1",
                                  "pos_cnum": "9" }, "point_num": "244",
                              "point_bol": "235" },
                          "stop":
                            { "byte":
                                { "pos_fname": "../../test/contracts/noop.mligo",
                                  "pos_lnum": "6", "pos_bol": "1",
                                  "pos_cnum": "23" }, "point_num": "258",
                              "point_bol": "235" } } ] },
                "location":
                  [ "File",
                    { "start":
                        { "byte":
                            { "pos_fname": "../../test/contracts/noop.mligo",
                              "pos_lnum": "6", "pos_bol": "1", "pos_cnum": "9" },
                          "point_num": "244", "point_bol": "235" },
                      "stop":
                        { "byte":
                            { "pos_fname": "../../test/contracts/noop.mligo",
                              "pos_lnum": "6", "pos_bol": "1", "pos_cnum": "23" },
                          "point_num": "258", "point_bol": "235" } } ] },
            "orig_var": null,
            "location":
              [ "File",
                { "start":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "6", "pos_bol": "1", "pos_cnum": "9" },
                      "point_num": "244", "point_bol": "235" },
                  "stop":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "6", "pos_bol": "1", "pos_cnum": "23" },
                      "point_num": "258", "point_bol": "235" } } ] },
          { "type_content":
              [ "T_record",
                { "fields":
                    { "1":
                        { "associated_type":
                            { "type_content":
                                [ "T_constant",
                                  { "language": "Michelson",
                                    "injection": [ "Unit" ], "parameters": [] } ],
                              "type_meta": null, "orig_var": null,
                              "location":
                                [ "File",
                                  { "start":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "2", "pos_bol": "1",
                                            "pos_cnum": "21" },
                                        "point_num": "154", "point_bol": "133" },
                                    "stop":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "2", "pos_bol": "1",
                                            "pos_cnum": "25" },
                                        "point_num": "158", "point_bol": "133" } } ] },
                          "michelson_annotation": null, "decl_pos": "1" },
                      "0":
                        { "associated_type":
                            { "type_content":
                                [ "T_constant",
                                  { "language": "Michelson",
                                    "injection": [ "List" ],
                                    "parameters":
                                      [ { "type_content":
                                            [ "T_constant",
                                              { "language": "Michelson",
                                                "injection": [ "Operation" ],
                                                "parameters": [] } ],
                                          "type_meta": null, "orig_var": null,
                                          "location":
                                            [ "File",
                                              { "start":
                                                  { "byte":
                                                      { "pos_fname":
                                                          "../../test/contracts/noop.mligo",
                                                        "pos_lnum": "6",
                                                        "pos_bol": "1",
                                                        "pos_cnum": "9" },
                                                    "point_num": "244",
                                                    "point_bol": "235" },
                                                "stop":
                                                  { "byte":
                                                      { "pos_fname":
                                                          "../../test/contracts/noop.mligo",
                                                        "pos_lnum": "6",
                                                        "pos_bol": "1",
                                                        "pos_cnum": "18" },
                                                    "point_num": "253",
                                                    "point_bol": "235" } } ] } ] } ],
                              "type_meta":
                                { "type_content":
                                    [ "T_app",
                                      { "type_operator":
                                          { "name": "list", "counter": "0",
                                            "generated": false,
                                            "location":
                                              [ "File",
                                                { "start":
                                                    { "byte":
                                                        { "pos_fname":
                                                            "../../test/contracts/noop.mligo",
                                                          "pos_lnum": "6",
                                                          "pos_bol": "1",
                                                          "pos_cnum": "9" },
                                                      "point_num": "244",
                                                      "point_bol": "235" },
                                                  "stop":
                                                    { "byte":
                                                        { "pos_fname":
                                                            "../../test/contracts/noop.mligo",
                                                          "pos_lnum": "6",
                                                          "pos_bol": "1",
                                                          "pos_cnum": "23" },
                                                      "point_num": "258",
                                                      "point_bol": "235" } } ] },
                                        "arguments":
                                          [ { "type_content":
                                                [ "T_variable",
                                                  { "name": "operation",
                                                    "counter": "0",
                                                    "generated": false,
                                                    "location":
                                                      [ "File",
                                                        { "start":
                                                            { "byte":
                                                                { "pos_fname":
                                                                    "../../test/contracts/noop.mligo",
                                                                  "pos_lnum": "6",
                                                                  "pos_bol": "1",
                                                                  "pos_cnum": "9" },
                                                              "point_num": "244",
                                                              "point_bol": "235" },
                                                          "stop":
                                                            { "byte":
                                                                { "pos_fname":
                                                                    "../../test/contracts/noop.mligo",
                                                                  "pos_lnum": "6",
                                                                  "pos_bol": "1",
                                                                  "pos_cnum":
                                                                    "18" },
                                                              "point_num": "253",
                                                              "point_bol": "235" } } ] } ],
                                              "sugar":
                                                { "type_content":
                                                    [ "T_variable",
                                                      { "name": "operation",
                                                        "counter": "0",
                                                        "generated": false,
                                                        "location":
                                                          [ "File",
                                                            { "start":
                                                                { "byte":
                                                                    { "pos_fname":
                                                                        "../../test/contracts/noop.mligo",
                                                                      "pos_lnum":
                                                                        "6",
                                                                      "pos_bol":
                                                                        "1",
                                                                      "pos_cnum":
                                                                        "9" },
                                                                  "point_num":
                                                                    "244",
                                                                  "point_bol":
                                                                    "235" },
                                                              "stop":
                                                                { "byte":
                                                                    { "pos_fname":
                                                                        "../../test/contracts/noop.mligo",
                                                                      "pos_lnum":
                                                                        "6",
                                                                      "pos_bol":
                                                                        "1",
                                                                      "pos_cnum":
                                                                        "18" },
                                                                  "point_num":
                                                                    "253",
                                                                  "point_bol":
                                                                    "235" } } ] } ],
                                                  "location":
                                                    [ "File",
                                                      { "start":
                                                          { "byte":
                                                              { "pos_fname":
                                                                  "../../test/contracts/noop.mligo",
                                                                "pos_lnum": "6",
                                                                "pos_bol": "1",
                                                                "pos_cnum": "9" },
                                                            "point_num": "244",
                                                            "point_bol": "235" },
                                                        "stop":
                                                          { "byte":
                                                              { "pos_fname":
                                                                  "../../test/contracts/noop.mligo",
                                                                "pos_lnum": "6",
                                                                "pos_bol": "1",
                                                                "pos_cnum": "18" },
                                                            "point_num": "253",
                                                            "point_bol": "235" } } ] },
                                              "location":
                                                [ "File",
                                                  { "start":
                                                      { "byte":
                                                          { "pos_fname":
                                                              "../../test/contracts/noop.mligo",
                                                            "pos_lnum": "6",
                                                            "pos_bol": "1",
                                                            "pos_cnum": "9" },
                                                        "point_num": "244",
                                                        "point_bol": "235" },
                                                    "stop":
                                                      { "byte":
                                                          { "pos_fname":
                                                              "../../test/contracts/noop.mligo",
                                                            "pos_lnum": "6",
                                                            "pos_bol": "1",
                                                            "pos_cnum": "18" },
                                                        "point_num": "253",
                                                        "point_bol": "235" } } ] } ] } ],
                                  "sugar":
                                    { "type_content":
                                        [ "T_app",
                                          { "type_operator":
                                              { "name": "list", "counter": "0",
                                                "generated": false,
                                                "location":
                                                  [ "File",
                                                    { "start":
                                                        { "byte":
                                                            { "pos_fname":
                                                                "../../test/contracts/noop.mligo",
                                                              "pos_lnum": "6",
                                                              "pos_bol": "1",
                                                              "pos_cnum": "9" },
                                                          "point_num": "244",
                                                          "point_bol": "235" },
                                                      "stop":
                                                        { "byte":
                                                            { "pos_fname":
                                                                "../../test/contracts/noop.mligo",
                                                              "pos_lnum": "6",
                                                              "pos_bol": "1",
                                                              "pos_cnum": "23" },
                                                          "point_num": "258",
                                                          "point_bol": "235" } } ] },
                                            "arguments":
                                              [ { "type_content":
                                                    [ "T_variable",
                                                      { "name": "operation",
                                                        "counter": "0",
                                                        "generated": false,
                                                        "location":
                                                          [ "File",
                                                            { "start":
                                                                { "byte":
                                                                    { "pos_fname":
                                                                        "../../test/contracts/noop.mligo",
                                                                      "pos_lnum":
                                                                        "6",
                                                                      "pos_bol":
                                                                        "1",
                                                                      "pos_cnum":
                                                                        "9" },
                                                                  "point_num":
                                                                    "244",
                                                                  "point_bol":
                                                                    "235" },
                                                              "stop":
                                                                { "byte":
                                                                    { "pos_fname":
                                                                        "../../test/contracts/noop.mligo",
                                                                      "pos_lnum":
                                                                        "6",
                                                                      "pos_bol":
                                                                        "1",
                                                                      "pos_cnum":
                                                                        "18" },
                                                                  "point_num":
                                                                    "253",
                                                                  "point_bol":
                                                                    "235" } } ] } ],
                                                  "location":
                                                    [ "File",
                                                      { "start":
                                                          { "byte":
                                                              { "pos_fname":
                                                                  "../../test/contracts/noop.mligo",
                                                                "pos_lnum": "6",
                                                                "pos_bol": "1",
                                                                "pos_cnum": "9" },
                                                            "point_num": "244",
                                                            "point_bol": "235" },
                                                        "stop":
                                                          { "byte":
                                                              { "pos_fname":
                                                                  "../../test/contracts/noop.mligo",
                                                                "pos_lnum": "6",
                                                                "pos_bol": "1",
                                                                "pos_cnum": "18" },
                                                            "point_num": "253",
                                                            "point_bol": "235" } } ] } ] } ],
                                      "location":
                                        [ "File",
                                          { "start":
                                              { "byte":
                                                  { "pos_fname":
                                                      "../../test/contracts/noop.mligo",
                                                    "pos_lnum": "6",
                                                    "pos_bol": "1",
                                                    "pos_cnum": "9" },
                                                "point_num": "244",
                                                "point_bol": "235" },
                                            "stop":
                                              { "byte":
                                                  { "pos_fname":
                                                      "../../test/contracts/noop.mligo",
                                                    "pos_lnum": "6",
                                                    "pos_bol": "1",
                                                    "pos_cnum": "23" },
                                                "point_num": "258",
                                                "point_bol": "235" } } ] },
                                  "location":
                                    [ "File",
                                      { "start":
                                          { "byte":
                                              { "pos_fname":
                                                  "../../test/contracts/noop.mligo",
                                                "pos_lnum": "6", "pos_bol": "1",
                                                "pos_cnum": "9" },
                                            "point_num": "244",
                                            "point_bol": "235" },
                                        "stop":
                                          { "byte":
                                              { "pos_fname":
                                                  "../../test/contracts/noop.mligo",
                                                "pos_lnum": "6", "pos_bol": "1",
                                                "pos_cnum": "23" },
                                            "point_num": "258",
                                            "point_bol": "235" } } ] },
                              "orig_var": null,
                              "location":
                                [ "File",
                                  { "start":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "6", "pos_bol": "1",
                                            "pos_cnum": "9" },
                                        "point_num": "244", "point_bol": "235" },
                                    "stop":
                                      { "byte":
                                          { "pos_fname":
                                              "../../test/contracts/noop.mligo",
                                            "pos_lnum": "6", "pos_bol": "1",
                                            "pos_cnum": "23" },
                                        "point_num": "258", "point_bol": "235" } } ] },
                          "michelson_annotation": null, "decl_pos": "0" } },
                  "layout": [ "L_tree" ] } ], "type_meta": null,
            "orig_var": null,
            "location":
              [ "File",
                { "start":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "6", "pos_bol": "1", "pos_cnum": "3" },
                      "point_num": "238", "point_bol": "235" },
                  "stop":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "6", "pos_bol": "1", "pos_cnum": "27" },
                      "point_num": "262", "point_bol": "235" } } ] },
          { "type_content":
              [ "T_arrow",
                { "type1":
                    { "type_content":
                        [ "T_constant",
                          { "language": "Michelson", "injection": [ "Unit" ],
                            "parameters": [] } ], "type_meta": null,
                      "orig_var": null,
                      "location":
                        [ "File",
                          { "start":
                              { "byte":
                                  { "pos_fname":
                                      "../../test/contracts/noop.mligo",
                                    "pos_lnum": "2", "pos_bol": "1",
                                    "pos_cnum": "13" }, "point_num": "146",
                                "point_bol": "133" },
                            "stop":
                              { "byte":
                                  { "pos_fname":
                                      "../../test/contracts/noop.mligo",
                                    "pos_lnum": "2", "pos_bol": "1",
                                    "pos_cnum": "17" }, "point_num": "150",
                                "point_bol": "133" } } ] },
                  "type2":
                    { "type_content":
                        [ "T_constant",
                          { "language": "Michelson", "injection": [ "Unit" ],
                            "parameters": [] } ], "type_meta": null,
                      "orig_var": null,
                      "location":
                        [ "File",
                          { "start":
                              { "byte":
                                  { "pos_fname":
                                      "../../test/contracts/noop.mligo",
                                    "pos_lnum": "2", "pos_bol": "1",
                                    "pos_cnum": "21" }, "point_num": "154",
                                "point_bol": "133" },
                            "stop":
                              { "byte":
                                  { "pos_fname":
                                      "../../test/contracts/noop.mligo",
                                    "pos_lnum": "2", "pos_bol": "1",
                                    "pos_cnum": "25" }, "point_num": "158",
                                "point_bol": "133" } } ] } } ],
            "type_meta": null, "orig_var": null,
            "location":
              [ "File",
                { "start":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "2", "pos_bol": "1", "pos_cnum": "9" },
                      "point_num": "142", "point_bol": "133" },
                  "stop":
                    { "byte":
                        { "pos_fname": "../../test/contracts/noop.mligo",
                          "pos_lnum": "2", "pos_bol": "1", "pos_cnum": "10" },
                      "point_num": "143", "point_bol": "133" } } ] } ],
      "michelson":
        { "expression":
            [ { "prim": "parameter", "args": [ { "prim": "unit" } ] },
              { "prim": "storage", "args": [ { "prim": "unit" } ] },
              { "prim": "code",
                "args":
                  [ [ [],
                      [ { "prim": "CDR" }, [],
                        [ [ { "prim": "LAMBDA",
                              "args":
                                [ { "prim": "unit" }, { "prim": "unit" },
                                  [ [], [] ] ] } ], [],
                          [ [ { "prim": "SWAP" },
                              [ { "prim": "DUP", "args": [ { "int": "2" } ] } ],
                              { "prim": "SWAP" }, { "prim": "EXEC" } ], [],
                            [ [ [],
                                [ { "prim": "DUP", "args": [ { "int": "2" } ] } ],
                                { "prim": "SWAP" }, { "prim": "EXEC" } ], [],
                              [ [ [], [ { "prim": "SWAP" } ], { "prim": "SWAP" },
                                  { "prim": "EXEC" } ], [],
                                [ [],
                                  [ { "prim": "NIL",
                                      "args": [ { "prim": "operation" } ] } ],
                                  { "prim": "PAIR" } ] ] ] ] ] ] ] ] } ],
          "locations":
            [ {}, {}, {}, {}, {}, {}, {}, { "environment": [ null ] },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "1",
                        "col": "10" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "1",
                        "col": "15" } }, "source_type": "6" }, {},
              { "environment": [ { "source_type": "0" } ] },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "2",
                        "col": "2" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "28" } }, "source_type": "6" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "2",
                        "col": "9" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "2",
                        "col": "10" } }, "source_type": "7" }, {}, {}, {}, {},
              { "environment": [ { "name": "x#151", "source_type": "1" } ] },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "2",
                        "col": "9" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "2",
                        "col": "10" } }, "source_type": "2" },
              { "environment":
                  [ { "name": "f#150", "source_type": "7" },
                    { "source_type": "0" } ] },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "3",
                        "col": "2" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "28" } }, "source_type": "6" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "3",
                        "col": "18" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "3",
                        "col": "21" } }, "source_type": "3" }, {},
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "3",
                        "col": "18" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "3",
                        "col": "19" } }, "source_type": "7" }, {}, {}, {}, {},
              { "environment":
                  [ { "name": "s2#152", "source_type": "3" },
                    { "name": "f#150", "source_type": "7" } ] },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "4",
                        "col": "2" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "28" } }, "source_type": "6" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "4",
                        "col": "18" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "4",
                        "col": "22" } }, "source_type": "4" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "4",
                        "col": "20" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "4",
                        "col": "22" } }, "source_type": "3" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "4",
                        "col": "18" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "4",
                        "col": "19" } }, "source_type": "7" }, {}, {}, {}, {},
              { "environment":
                  [ { "name": "s3#153", "source_type": "4" },
                    { "name": "f#150", "source_type": "7" } ] },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "5",
                        "col": "2" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "28" } }, "source_type": "6" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "5",
                        "col": "10" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "5",
                        "col": "14" } }, "source_type": "2" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "5",
                        "col": "12" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "5",
                        "col": "14" } }, "source_type": "4" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "5",
                        "col": "10" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "5",
                        "col": "11" } }, "source_type": "7" }, {}, {}, {},
              { "environment": [ { "name": "s#154", "source_type": "2" } ] },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "3" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "27" } }, "source_type": "6" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "26" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "27" } }, "source_type": "2" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "3" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "24" } }, "source_type": "5" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "3" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "24" } }, "source_type": "5" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "3" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "24" } }, "source_type": "5" },
              { "location":
                  { "start":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "3" },
                    "stop":
                      { "file": "../../test/contracts/noop.mligo", "line": "6",
                        "col": "27" } }, "source_type": "6" } ] } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "storage"; contract "module_contract_simple.mligo"; "999" ];
  [%expect {| 999 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "parameter"; contract "module_contract_simple.mligo"; "Add 999" ];
  [%expect {| (Left (Left 999)) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "module_contract_complex.mligo"
    ; "{ number = 999 ; previous_action = Reset }"
    ];
  [%expect {| (Pair 999 (Left (Right Unit))) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "parameter"; contract "module_contract_complex.mligo"; "Add 999" ];
  [%expect {| (Left (Left 999)) |}]

(* Global constants *)

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "global_constant.mligo"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ;
             constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2" ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "global_constant.mligo"
    ; "--constants"
    ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
    ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { CDR ;
             constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2" ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "global_constant.mligo"
    ; "()"
    ; "--constants"
    ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
    ];
  [%expect {| Unit |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "global_constant.mligo"
    ; "v"
    ; "--constants"
    ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
    ];
  [%expect {| 128 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "global_constant.mligo"
    ; "42"
    ; "--constants"
    ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
    ];
  [%expect {| 42 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "global_constant.mligo"
    ; "42"
    ; "--file-constants"
    ; contract_resource "const.json"
    ];
  [%expect {| 42 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "v"
    ; "--init-file"
    ; contract "global_constant.mligo"
    ; "--constants"
    ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
    ];
  [%expect {| 128 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "v"
    ; "--init-file"
    ; contract "global_constant.mligo"
    ; "--file-constants"
    ; contract_resource "const.json"
    ];
  [%expect {|
    128 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "global_constant_lambda.mligo"
    ; "s"
    ; "--constants"
    ; "{ PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }"
    ];
  [%expect {|
    (Pair 1 { PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "global_constant_lambda.mligo"
    ; "s"
    ; "--file-constants"
    ; contract_resource "const.json"
    ];
  [%expect {|
    (Pair 1 { PUSH int 2 ; PUSH int 3 ; DIG 2 ; MUL ; ADD }) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "constant"
    ; "cameligo"
    ; "fun (x : int) -> if x > 3 then x * 2 else x * String.length \"fja\" + 1"
    ];
  [%expect
    {|
    Michelson constant as JSON string:
    "{ PUSH int 3 ;\n  DUP 2 ;\n  COMPARE ;\n  GT ;\n  IF { PUSH int 2 ; SWAP ; MUL }\n     { PUSH int 1 ; PUSH string \"fja\" ; SIZE ; DIG 2 ; MUL ; ADD } }"
    This string can be passed in `--constants` argument when compiling a contract.

    Remember to register it in the network, e.g.:
    > tezos-client register global constant "{ PUSH int 3 ;
      DUP 2 ;
      COMPARE ;
      GT ;
      IF { PUSH int 2 ; SWAP ; MUL }
         { PUSH int 1 ; PUSH string \"fja\" ; SIZE ; DIG 2 ; MUL ; ADD } }" from bootstrap1

    Constant hash:
    exprtr7GE1A1cR39zNGRGF44aGfAX23tC7szWrnLzs9fkUhasLEcQT |}]

(* Test pairing_check and bls12_381_g1/g2/fr literals *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "test"
    ; "--init-file"
    ; contract "pairing_check.mligo"
    ];
  [%expect {| Unit |}]

(* Test decompilation of bls12_381_g1/g2/fr literals *)
let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "(alpha, beta, input_x)"
    ; "--init-file"
    ; contract "pairing_check.mligo"
    ];
  [%expect
    {|
    ( bls12_381_g1 0x024142bc89bf29017a38d0ee97711098639aa0bbc5b54b5104cc88b1c0fd09330fb8341e3da91e7a50f0da5c988517db0f52df51f745392ecdd3ffbb50f8a25fcdec6f48886b650de26821e244cb8ab69d49722d290a420ce1284b909d3e15a0 ,
      bls12_381_g2 0x0050b3ab4877c99ce7f180e879d91eb4df24b1e20ed88f1fdde42f91dfe0e7e451aa35d1457dd15ab507fc8f2b3180550ca7b4ea9b67810e346456c35060c8d542f37ee5fe2b1461e2f02fefac55a9863e94cab5c16befad3b866a42ee20835b1351f3f9c20a05586c1d647d756efb5c575d7ab23fbf5b3e1a6ffe024633a63a668a01fcab440866035ea2c0d4bfe30a1242f67119650e2aa605289ade2684287192382d6a01d7865fcd9e1507264a80f387b6441e37438c888159827a4efa67 ,
      bls12_381_fr 0xe406000000000000000000000000000000000000000000000000000000000000 ) |}]

(* Example contracts from getting-started *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "increment.mligo" ];
  [%expect
    {|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "increment.ligo" ];
  [%expect
    {|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "increment.jsligo" ];
  [%expect
    {|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]

(* Test compiling a contract with a get_entrypoint_opt to a capitalized entrypoint *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "get_capitalized_entrypoint.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             SENDER ;
             CONTRACT %Upper unit ;
             IF_NONE
               { PUSH string "lol" ; FAILWITH }
               { PUSH mutez 0 ;
                 UNIT ;
                 TRANSFER_TOKENS ;
                 UNIT ;
                 NIL operation ;
                 DIG 2 ;
                 CONS ;
                 PAIR } } } |}]

(* Test compiling parameter in a file which uses test primitives *)
let%expect_test _ =
  run_ligo_good [ "compile"; "parameter"; contract "increment_with_test.mligo"; "z.1" ];
  [%expect {| (Left (Right 32)) |}]

(* Test compiling storage in a file which uses test primitives *)
let%expect_test _ =
  run_ligo_good [ "compile"; "storage"; contract "increment_with_test.mligo"; "z.0 + 10" ];
  [%expect {| 42 |}]

(* Test compiling expression with curried recursive function *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "foo 2 \"titi\""
    ; "--init-file"
    ; contract "recursion_uncurry.mligo"
    ];
  [%expect {|
    "tititotototo" |}]

(* Test compiling contract with curried recursive function *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "recursion_uncurry.mligo" ];
  [%expect
    {|
    { parameter int ;
      storage string ;
      code { LEFT string ;
             LOOP_LEFT
               { UNPAIR ;
                 PUSH int 0 ;
                 DUP 2 ;
                 COMPARE ;
                 EQ ;
                 IF { DROP ; RIGHT (pair int string) }
                    { PUSH string "toto" ;
                      DIG 2 ;
                      CONCAT ;
                      PUSH int 1 ;
                      DIG 2 ;
                      SUB ;
                      PAIR ;
                      LEFT string } } ;
             NIL operation ;
             PAIR } } |}]

(* voting power *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "voting.mligo" ];
  [%expect
    {|
{ parameter key ;
  storage (pair nat nat) ;
  code { CAR ;
         HASH_KEY ;
         VOTING_POWER ;
         TOTAL_VOTING_POWER ;
         SWAP ;
         PAIR ;
         NIL operation ;
         PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "michelson_typed_opt.mligo"
    ; "-e"
    ; "main2"
    ; "--enable-michelson-typed-opt"
    ];
  [%expect
    {|
    { parameter (or (pair %one (nat %x) (int %y)) (pair %two (nat %x) (int %y))) ;
      storage nat ;
      code { CAR ; IF_LEFT {} {} ; CAR ; NIL operation ; PAIR } }  |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "michelson_typed_opt.mligo"
    ; "-e"
    ; "main3"
    ; "--enable-michelson-typed-opt"
    ];
  [%expect
    {|
    { parameter (or (pair %onee (nat %x) (int %y)) (pair %three (nat %x) (int %z))) ;
      storage nat ;
      code { CAR ; IF_LEFT {} {} ; CAR ; NIL operation ; PAIR } }
           |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "michelson_typed_opt.mligo"
    ; "-e"
    ; "main4"
    ; "--enable-michelson-typed-opt"
    ];
  [%expect
    {|
    { parameter (or (pair %four (nat %x) (timestamp %y)) (pair %oneee (nat %x) (int %y))) ;
      storage nat ;
      code { CAR ; IF_LEFT { CAR } { CAR } ; NIL operation ; PAIR } }
           |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "michelson_typed_opt.mligo"; "-e"; "main2" ];
  [%expect
    {|
    { parameter (or (pair %one (nat %x) (int %y)) (pair %two (nat %x) (int %y))) ;
      storage nat ;
      code { CAR ; IF_LEFT { CAR } { CAR } ; NIL operation ; PAIR } }
           |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "michelson_typed_opt.mligo"; "-e"; "main3" ];
  [%expect
    {|
    { parameter (or (pair %onee (nat %x) (int %y)) (pair %three (nat %x) (int %z))) ;
      storage nat ;
      code { CAR ; IF_LEFT { CAR } { CAR } ; NIL operation ; PAIR } }
           |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "michelson_typed_opt.mligo"; "-e"; "main4" ];
  [%expect
    {|
    { parameter (or (pair %four (nat %x) (timestamp %y)) (pair %oneee (nat %x) (int %y))) ;
      storage nat ;
      code { CAR ; IF_LEFT { CAR } { CAR } ; NIL operation ; PAIR } }
           |}]

(* check get contract with error typing *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "cbo"
    ; "--init-file"
    ; contract "get_contract_with_error.ligo"
    ];
  [%expect
    {|
{ PUSH string "contract not found" ;
  SENDER ;
  CONTRACT unit ;
  IF_NONE { FAILWITH } { SWAP ; DROP } ;
  SWAP ;
  NIL operation ;
  DIG 2 ;
  PUSH mutez 0 ;
  UNIT ;
  TRANSFER_TOKENS ;
  CONS ;
  PAIR } |}]

(* extend built-in modules *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "pascaligo"
    ; "y"
    ; "--init-file"
    ; contract "extend_builtin.ligo"
    ];
  [%expect {|
44 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "y"
    ; "--init-file"
    ; contract "extend_builtin.mligo"
    ];
  [%expect {|
44 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "y"
    ; "--init-file"
    ; contract "extend_builtin.jsligo"
    ];
  [%expect
    {|
File "../../test/contracts/extend_builtin.jsligo", line 6, characters 0-24:
  5 |
  6 | let y = Tezos.f(Tezos.x);

Toplevel let declaration are silently change to const declaration.

File "../../test/contracts/extend_builtin.jsligo", line 2, characters 9-19:
  1 | namespace Tezos {
  2 |   export let x = 42;
  3 |   export let f = (x  : int) : int => x + 2;

Toplevel let declaration are silently change to const declaration.

File "../../test/contracts/extend_builtin.jsligo", line 3, characters 9-42:
  2 |   export let x = 42;
  3 |   export let f = (x  : int) : int => x + 2;
  4 | }

Toplevel let declaration are silently change to const declaration.

44 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "thunk.mligo" ];
  [%expect
    {|
{ parameter string ;
  storage string ;
  code { CDR ;
         SENDER ;
         PUSH mutez 1000000 ;
         NONE key_hash ;
         CREATE_CONTRACT
           { parameter nat ;
             storage address ;
             code { DROP ; SENDER ; NIL operation ; PAIR } } ;
         PAIR ;
         SWAP ;
         NIL operation ;
         DIG 2 ;
         CAR ;
         CONS ;
         PAIR } } |}]

(* check compiling many (more than 10) views *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "views_many.ligo" ];
  [%expect
    {|
{ parameter unit ;
  storage nat ;
  code { CDR ; NIL operation ; PAIR } ;
  view "view_1" unit int { CDR ; PUSH int 1 ; ADD } ;
  view "view_2" unit int { CDR ; PUSH int 2 ; ADD } ;
  view "view_3" unit int { CDR ; PUSH int 3 ; ADD } ;
  view "view_4" unit int { CDR ; PUSH int 4 ; ADD } ;
  view "view_5" unit int { CDR ; PUSH int 5 ; ADD } ;
  view "view_6" unit int { CDR ; PUSH int 6 ; ADD } ;
  view "view_7" unit int { CDR ; PUSH int 7 ; ADD } ;
  view "view_8" unit int { CDR ; PUSH int 8 ; ADD } ;
  view "view_9" unit int { CDR ; PUSH int 9 ; ADD } ;
  view "view_10" unit int { CDR ; PUSH int 10 ; ADD } ;
  view "view_11" unit int { CDR ; PUSH int 11 ; ADD } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "call_view_impure.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH address "tz1fakefakefakefakefakefakefakcphLA5" ;
             SENDER ;
             VIEW "foo" unit ;
             IF_NONE { UNIT } {} ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "shadowed_sum_type.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/shadowed_sum_type.mligo", line 13, characters 8-12:
     12 |
     13 | let x = A 42
     14 |

    Constructor "A" not found.
  |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_contract_return_type.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/bad_contract_return_type.mligo", line 5, characters 4-8:
      4 |
      5 | let main (_,s : paramater * storage) : _return =
      6 |     [], s, 1tez

    Invalid type for entrypoint "main".
    An entrypoint must of type "parameter * storage -> operation list * storage".
  |}]

(* ignore in JsLIGO *)
let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "jsligo"; "test"; "--init-file"; contract "ignore.jsligo" ];
  [%expect {| 1 |}]

(* bytes literals using raw_code *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "tests"
    ; "--init-file"
    ; contract "bytes_literals.mligo"
    ];
  [%expect {| { True ; True ; True } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "tests"
    ; "--init-file"
    ; contract "bytes_literals.jsligo"
    ];
  [%expect {| (Pair (Pair True True) True) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "realistic"
    ; "--init-file"
    ; contract "bytes_literals.jsligo"
    ];
  [%expect
    {| 0x0a202020207b0a20202020226e616d65223a226e616d65222c0a20202020226465736372697074696f6e223a226465736372697074696f6e222c0a202020202276657273696f6e223a22302e302e30222c0a20202020226c6963656e7365223a7b226e616d65223a226c696e616d65227d2c0a2020202022617574686f7273223a5b22617574686f7273225d2c0a2020202022686f6d6570616765223a22222c0a2020202022736f75726365223a7b22746f6f6c73223a5b22746f6f6c73225d2c20226c6f636174696f6e223a226c6f636174696f6e227d2c0a2020202022696e7465726661636573223a5b22545a4950225d2c0a20202020226572726f7273223a5b5d2c0a20202020227669657773223a5b5d0a202020207d0a2020 |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_contract "bytes_literals.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/bytes_literals.jsligo", line 2, characters 18-23:
      1 | const shame = () => {
      2 |   const x = bytes `foo` as nat;
      3 |   return x

    Invalid type(s).
    Expected "nat", but got: "bytes". |}]

(* get_entrypoint_opt in uncurried language *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "get_entrypoint.jsligo" ];
  [%expect
    {|
    { parameter unit ;
      storage address ;
      code { DROP ;
             SENDER ;
             CONTRACT %foo int ;
             IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
             ADDRESS ;
             NIL operation ;
             PAIR } } |}]

(* make sure that in compile storage/expression we can check ENTRYPOINT/EMIT *)
let%expect_test _ =
  run_ligo_good [ "compile"; "storage"; contract "emit.mligo"; "()" ];
  [%expect {| Unit |}]

(* make sure that in compile storage/expression we can check SELF *)
let%expect_test _ =
  run_ligo_good [ "compile"; "storage"; contract "self_annotations.mligo"; "()" ];
  [%expect {| Unit |}]

(* check tag in Tezos.emit *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "emit_bad_tag.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/emit_bad_tag.mligo", line 2, characters 3-31:
      1 | let main (_,_ : unit * string ) : operation list * string =
      2 |   [Tezos.emit "%hello world" 12], "bye"

    Invalid entrypoint "%hello world". One of the following patterns is expected:
    * "%bar" is expected for entrypoint "Bar"
    * "%default" when no entrypoint is used.
    Valid characters in annotation: ('a' .. 'z' | 'A' .. 'Z' | '_' | '.' | '%' | '@' | '0' .. '9'). |}]

(* make sure that in compile storage we annotate the type *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "annotated_storage_and_parameter.mligo"
    ; "Map.empty"
    ];
  [%expect {| {} |}]

(* make sure that in compile parameter we annotate the type *)
let%expect_test _ =
  run_ligo_good
    [ "compile"; "parameter"; contract "annotated_storage_and_parameter.mligo"; "[]" ];
  [%expect {| {} |}]

(* make sure that in compile parameter we do not allow polymorphic type *)
let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "parameter"
    ; bad_contract "annotated_storage_and_parameter.mligo"
    ; "([] : int list)"
    ];
  [%expect
    {|
    File "../../test/contracts/negative/annotated_storage_and_parameter.mligo", line 4, character 0 to line 5, character 8:
      3 |
      4 | let main ((_p, s) : parameter * storage) : operation list * storage =
      5 |  ([], s)

    Invalid type for entrypoint "main".
    The parameter type "funtype 'a : * . list ('a)" of the entrypoint function must not contain polymorphic variables. |}]
