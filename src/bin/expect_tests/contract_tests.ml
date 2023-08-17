open Cli_expect

let contract = test
let contract_resource name = test ("res/" ^ name)
let bad_contract = bad_test

(* avoid pretty printing *)
let () = Ligo_unix.putenv ~key:"TERM" ~data:"dumb"

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "f(list([1,2,3,4,5]))"
    ; "--init-file"
    ; contract "implicit_cast.jsligo"
    ];
  [%expect {|
    3 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "b2"
    ; "--init-file"
    ; contract "implicit_cast.mligo"
    ];
  [%expect {|
    True |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "ball"
    ; "--init-file"
    ; contract "implicit_cast.mligo"
    ];
  [%expect {|
    True |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "add2(x, x)"
    ; "--init-file"
    ; contract "modules.jsligo"
    ];
  [%expect
    {|
    File "../../test/contracts/modules.jsligo", line 2, character 0 to line 4, character 1:
      1 | // @foo
      2 | namespace B {
          ^^^^^^^^^^^^^
      3 |   export type titi = int;
          ^^^^^^^^^^^^^^^^^^^^^^^^^
      4 | };
          ^
      5 |

    Warning: unsupported attribute, ignored.

    84 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "FA1.2.interface.mligo"; "-m"; "FA12_ENTRIES" ];
  [%expect
    {|
    { parameter
        (or (or (or (pair %approve (address %spender) (nat %value))
                    (pair %getAllowance
                       (pair %request (address %owner) (address %spender))
                       (contract %callback nat)))
                (or (pair %getBalance (address %owner) (contract %callback nat))
                    (pair %getTotalSupply (unit %request) (contract %callback nat))))
            (pair %transfer (address %from) (address %to) (nat %value))) ;
      storage
        (pair (pair (big_map %allowances (pair (address %owner) (address %spender)) nat)
                    (big_map %tokens address nat))
              (nat %total_supply)) ;
      code { UNPAIR ;
             IF_LEFT
               { IF_LEFT
                   { IF_LEFT
                       { DUP 2 ;
                         CAR ;
                         CAR ;
                         DUP 2 ;
                         CAR ;
                         SENDER ;
                         PAIR ;
                         PUSH nat 0 ;
                         DUP 4 ;
                         CDR ;
                         COMPARE ;
                         GT ;
                         PUSH nat 0 ;
                         DUP 4 ;
                         DUP 4 ;
                         GET ;
                         IF_NONE { PUSH nat 0 } {} ;
                         COMPARE ;
                         GT ;
                         AND ;
                         IF { PUSH string "UnsafeAllowanceChange" ; FAILWITH } {} ;
                         DUP 4 ;
                         DIG 4 ;
                         CAR ;
                         DIG 4 ;
                         CDR ;
                         DIG 4 ;
                         PUSH nat 0 ;
                         DUP 3 ;
                         COMPARE ;
                         EQ ;
                         IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                         DIG 4 ;
                         UPDATE ;
                         UPDATE 1 ;
                         UPDATE 1 ;
                         NIL operation }
                       { DUP 2 ;
                         NIL operation ;
                         DUP 3 ;
                         CDR ;
                         PUSH mutez 0 ;
                         DIG 5 ;
                         CAR ;
                         CAR ;
                         DIG 5 ;
                         CAR ;
                         GET ;
                         IF_NONE { PUSH nat 0 } {} ;
                         TRANSFER_TOKENS ;
                         CONS } }
                   { IF_LEFT
                       { DUP 2 ;
                         NIL operation ;
                         DUP 3 ;
                         CDR ;
                         PUSH mutez 0 ;
                         DIG 5 ;
                         CAR ;
                         CDR ;
                         DIG 5 ;
                         CAR ;
                         GET ;
                         IF_NONE { PUSH nat 0 } {} ;
                         TRANSFER_TOKENS }
                       { DUP 2 ;
                         NIL operation ;
                         DIG 2 ;
                         CDR ;
                         PUSH mutez 0 ;
                         DIG 4 ;
                         CDR ;
                         TRANSFER_TOKENS } ;
                     CONS } }
               { DUP 2 ;
                 CAR ;
                 CAR ;
                 DUP 3 ;
                 CAR ;
                 CDR ;
                 DUP 3 ;
                 CAR ;
                 SENDER ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP }
                    { SENDER ;
                      DUP 4 ;
                      CAR ;
                      PAIR ;
                      DUP 4 ;
                      GET 4 ;
                      DUP 4 ;
                      DUP 3 ;
                      GET ;
                      IF_NONE { PUSH nat 0 } {} ;
                      SUB ;
                      ISNAT ;
                      IF_NONE { PUSH string "NotEnoughAllowance" ; FAILWITH } {} ;
                      DIG 3 ;
                      PUSH nat 0 ;
                      DUP 3 ;
                      COMPARE ;
                      EQ ;
                      IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                      DIG 2 ;
                      UPDATE } ;
                 DUP 3 ;
                 GET 4 ;
                 DUP 3 ;
                 DUP 5 ;
                 CAR ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 SUB ;
                 ISNAT ;
                 IF_NONE { PUSH string "NotEnoughBalance" ; FAILWITH } {} ;
                 DIG 2 ;
                 PUSH nat 0 ;
                 DUP 3 ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                 DUP 4 ;
                 CAR ;
                 UPDATE ;
                 DUP 3 ;
                 GET 4 ;
                 DUP 2 ;
                 DUP 5 ;
                 GET 3 ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 ADD ;
                 DUP 5 ;
                 DIG 5 ;
                 CAR ;
                 DIG 3 ;
                 PUSH nat 0 ;
                 DUP 5 ;
                 COMPARE ;
                 EQ ;
                 IF { DIG 3 ; DROP ; NONE nat } { DIG 3 ; SOME } ;
                 DIG 5 ;
                 GET 3 ;
                 UPDATE ;
                 UPDATE 2 ;
                 UPDATE 1 ;
                 DUP ;
                 CAR ;
                 DIG 2 ;
                 UPDATE 1 ;
                 UPDATE 1 ;
                 NIL operation } ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "t"
    ; "--init-file"
    ; contract "jsligo_uppercase_generic.jsligo"
    ];
  [%expect {|
    { 1 ; 2 ; 3 } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "jsligo_list_concat.jsligo" ];
  [%expect
    {|
    { parameter (list int) ;
      storage (list int) ;
      code { UNPAIR ;
             NIL int ;
             SWAP ;
             ITER { CONS } ;
             ITER { CONS } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "export_attribute.jsligo"; "-m"; "Foo" ];
  [%expect
    {|
    { parameter unit ; storage int ; code { CDR ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "of_file.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             UNIT ;
             PUSH mutez 300000000 ;
             NONE key_hash ;
             CREATE_CONTRACT
               { parameter unit ;
                 storage unit ;
                 code { DROP ; UNIT ; NIL operation ; PAIR } } ;
             SWAP ;
             DROP ;
             UNIT ;
             NIL operation ;
             DIG 2 ;
             CONS ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "expression"; "cameligo"; "s"; "--init-file"; contract "of_file.mligo" ];
  [%expect
    {xxx|
    "let s = [%of_file \"./of_file.mligo\"]\n\nlet m () = [%michelson ({| { PUSH unit Unit ; PUSH mutez 300000000 ; NONE key_hash ; CREATE_CONTRACT (codestr $0) ; PAIR } |} [%of_file \"./interpreter_tests/contract_under_test/compiled.tz\"] : operation * address)]\n\nlet main (_ : unit) (_ : unit) : operation list * unit =\n  let op, _ = m () in\n  [op], ()\n" |xxx}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "[j, k]"
    ; "--init-file"
    ; contract "option.jsligo"
    ];
  [%expect {|
    (Pair 42 1) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "const.mligo" ];
  [%expect
    {|
    File "../../test/contracts/const.mligo", line 1, characters 31-32:
      1 | let const = fun (type a b) (a, b : a * b) : a -> a
                                         ^
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
             PAIR ;
             PAIR }
           { SENDER ;
             SWAP ;
             IF_LEFT
               { DROP ;
                 DUP 2 ;
                 DUP 3 ;
                 CAR ;
                 DUP ;
                 CAR ;
                 PUSH nat 1 ;
                 DIG 5 ;
                 CAR ;
                 CAR ;
                 CDR ;
                 ADD ;
                 UPDATE 2 ;
                 UPDATE 1 ;
                 UPDATE 1 }
               { DROP ;
                 DUP 2 ;
                 DUP 3 ;
                 CDR ;
                 PUSH nat 1 ;
                 DIG 4 ;
                 CDR ;
                 CDR ;
                 ADD ;
                 UPDATE 2 ;
                 UPDATE 2 } ;
             DUP ;
             DUP 2 ;
             CDR ;
             DIG 2 ;
             CDR ;
             CAR ;
             DIG 3 ;
             PUSH bool True ;
             SWAP ;
             UPDATE ;
             UPDATE 1 ;
             UPDATE 2 } ;
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
  run_ligo_good [ "compile"; "contract"; contract "ticket_builder.mligo" ];
  [%expect
    {|
File "../../test/contracts/ticket_builder.mligo", line 28, characters 28-34:
 27 |       begin
 28 |         let ((ticketer, _), ticket) = (Tezos.read_ticket ticket : (address * (unit * nat)) * unit ticket) in
                                  ^^^^^^
 29 |         assert (ticketer = Tezos.get_self_address ());
:
Warning: unused variable "ticket".
Hint: replace it by "_ticket" to prevent this warning.

{ parameter
    (or (ticket %burn unit)
        (pair %mint (contract %destination (ticket unit)) (nat %amount))) ;
  storage address ;
  code { UNPAIR ;
         PUSH mutez 0 ;
         AMOUNT ;
         COMPARE ;
         EQ ;
         IF {} { PUSH string "failed assertion" ; FAILWITH } ;
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
                  ^
        3 |   in ([] : operation list), unit
      :
      Warning: unused variable "c".
      Hint: replace it by "_c" to prevent this warning.

      File "../../test/contracts/implicit.mligo", line 1, characters 26-27:
        1 | let main2 (p : key_hash) (s : unit) =
                                      ^
        2 |   let c : unit contract = Tezos.implicit_account p
      :
      Warning: unused variable "s".
      Hint: replace it by "_s" to prevent this warning.

      { parameter key_hash ;
        storage unit ;
        code { DROP ; UNIT ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "capture_big_map.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/capture_big_map.mligo", line 13, characters 4-17:
     12 |   let supply (ledger:l) (_:nat) =
     13 |     ledger.supply
              ^^^^^^^^^^^^^
     14 |

    Invalid capturing, term captures the type big_map (address ,
    nat).
    Hint: Uncurry or use tuples instead of high-order functions. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "expression"; "cameligo"; "fun (x : operation) -> fun (y : int) -> x" ];
  [%expect
    {|
    Invalid capturing, term captures the type operation.
    Hint: Uncurry or use tuples instead of high-order functions. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "fun (x : 4 sapling_transaction) -> fun (y : int) -> x"
    ];
  [%expect
    {|
    Invalid capturing, term captures the type sapling_transaction (4).
    Hint: Uncurry or use tuples instead of high-order functions. |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "fun (x : operation -> int contract) -> fun (y : int) -> x"
    ];
  [%expect
    {|
    { LAMBDA
        (pair (lambda operation (contract int)) int)
        (lambda operation (contract int))
        { CAR } ;
      DUP 2 ;
      APPLY ;
      SWAP ;
      DROP } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "amount_lambda.mligo" ];
  (* AMOUNT should occur inside the second lambda, but not the first lambda *)
  [%expect
    {|
    File "../../test/contracts/amount_lambda.mligo", line 4, characters 7-8:
      3 |   let amt : tez = Tezos.get_amount () in
      4 |   fun (x : unit) -> amt
                 ^
      5 |
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 2, characters 8-9:
      1 | (* should return a constant function *)
      2 | let f1 (x : unit) : unit -> tez =
                  ^
      3 |   let amt : tez = Tezos.get_amount () in
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 8, characters 7-8:
      7 | let f2 (x : unit) : unit -> tez =
      8 |   fun (x : unit) -> Tezos.get_amount ()
                 ^
      9 |
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 7, characters 8-9:
      6 | (* should return an impure function *)
      7 | let f2 (x : unit) : unit -> tez =
                  ^
      8 |   fun (x : unit) -> Tezos.get_amount ()
    :
    Warning: unused variable "x".
    Hint: replace it by "_x" to prevent this warning.

    File "../../test/contracts/amount_lambda.mligo", line 10, characters 21-22:
      9 |
     10 | let main (b : bool) (s : (unit -> tez)) : operation list * (unit -> tez) =
                               ^
     11 |   (([] : operation list), (if b then f1 () else f2 ()))
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter bool ;
      storage (lambda unit mutez) ;
      code { CAR ;
             IF { AMOUNT ;
                  LAMBDA (pair mutez unit) mutez { CAR } ;
                  DUP 2 ;
                  APPLY ;
                  SWAP ;
                  DROP }
                { LAMBDA unit mutez { DROP ; AMOUNT } } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "subtle_nontail_fail.mligo" ];
  [%expect
    {|
    File "../../test/contracts/subtle_nontail_fail.mligo", line 1, characters 10-12:
      1 | let main (ps : unit * unit) : operation list * unit =
                    ^^
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
                    ^^
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
  run_ligo_bad [ "compile"; "contract"; bad_contract "self_in_lambdarec.mligo" ];
  [%expect
    {| "Tezos.self" must be used directly and cannot be used via another function. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "not_comparable.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/not_comparable.mligo", line 1, characters 16-19:
      1 | let main (_u : (int set) set) (s : unit) : operation list * unit = ([] : operation list), s
                          ^^^
      2 |

    This type is used inside:
    File "../../test/contracts/negative/not_comparable.mligo", line 1, characters 15-24:
      1 | let main (_u : (int set) set) (s : unit) : operation list * unit = ([] : operation list), s
                         ^^^^^^^^^
      2 |

    The set constructor needs a comparable type argument, but it was given a non-comparable one. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "contract"; bad_contract "not_comparable.mligo"; "-e"; "main2" ];
  [%expect
    {|
    File "../../test/contracts/negative/not_comparable.mligo", line 3, characters 17-20:
      2 |
      3 | let main2 (_u : (int set) ticket) (s : unit) : operation list * unit = ([] : operation list), s
                           ^^^

    This type is used inside:
    File "../../test/contracts/negative/not_comparable.mligo", line 3, characters 16-25:
      2 |
      3 | let main2 (_u : (int set) ticket) (s : unit) : operation list * unit = ([] : operation list), s
                          ^^^^^^^^^

    The ticket constructor needs a comparable type argument, but it was given a non-comparable one. |}]

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
  3 | let main (_ : string) (store : string) : return =
  4 |   let toto : operation * address = Tezos.create_contract
                                         ^^^^^^^^^^^^^^^^^^^^^
  5 |     (fun (_p : nat) (_s : string) -> (([] : operation list), store))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  6 |     (None: key_hash option)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  7 |     300tz
      ^^^^^^^^^^
  8 |     "un"
      ^^^^^^^^
  9 |   in

Not all free variables could be inlined in Tezos.create_contract usage: gen#240. |}];
  run_ligo_good [ "compile"; "contract"; contract "create_contract_var.mligo" ];
  [%expect
    {|
    File "../../test/contracts/create_contract_var.mligo", line 7, characters 20-21:
      6 |   let toto : operation * address = Tezos.create_contract
      7 |     (fun (p : nat) (s : int) -> (([] : operation list), a))
                              ^
      8 |     (None: key_hash option)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/create_contract_var.mligo", line 7, characters 10-11:
      6 |   let toto : operation * address = Tezos.create_contract
      7 |     (fun (p : nat) (s : int) -> (([] : operation list), a))
                    ^
      8 |     (None: key_hash option)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/create_contract_var.mligo", line 5, characters 10-16:
      4 |
      5 | let main (action : string) (store : string) : return =
                    ^^^^^^
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
    File "../../test/contracts/negative/create_contract_modfv.mligo", line 8, characters 20-21:
      7 |   let toto : operation * address = Tezos.create_contract
      8 |     (fun (p : nat) (s : string) -> (([] : operation list), Foo.store))
                              ^
      9 |     (None: key_hash option)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 8, characters 10-11:
      7 |   let toto : operation * address = Tezos.create_contract
      8 |     (fun (p : nat) (s : string) -> (([] : operation list), Foo.store))
                    ^
      9 |     (None: key_hash option)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 3, characters 10-16:
      2 |
      3 | let main (action : string) (store : string) : return =
                    ^^^^^^
      4 |   module Foo = struct
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/negative/create_contract_modfv.mligo", line 7, character 35 to line 11, character 8:
      6 |   end in
      7 |   let toto : operation * address = Tezos.create_contract
                                             ^^^^^^^^^^^^^^^^^^^^^
      8 |     (fun (p : nat) (s : string) -> (([] : operation list), Foo.store))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      9 |     (None: key_hash option)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |     300tz
          ^^^^^^^^^^
     11 |     "un"
          ^^^^^^^^
     12 |   in

    Not all free variables could be inlined in Tezos.create_contract usage: gen#241. |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "create_contract_no_inline.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 5, characters 30-31:
      4 |
      5 | let dummy_contract (p : nat) (s : int) : return =
                                        ^
      6 |  (([] : operation list), foo)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 5, characters 20-21:
      4 |
      5 | let dummy_contract (p : nat) (s : int) : return =
                              ^
      6 |  (([] : operation list), foo)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 9, characters 11-15:
      8 | let main (action : int) (store : int) : return =
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
                     ^^^^
     10 |   let toto : operation list = [ op ] in
    :
    Warning: unused variable "addr".
    Hint: replace it by "_addr" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 8, characters 25-30:
      7 |
      8 | let main (action : int) (store : int) : return =
                                   ^^^^^
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 8, characters 10-16:
      7 |
      8 | let main (action : int) (store : int) : return =
                    ^^^^^^
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
    :
    Warning: unused variable "action".
    Hint: replace it by "_action" to prevent this warning.

    File "../../test/contracts/negative/create_contract_no_inline.mligo", line 9, characters 19-89:
      8 | let main (action : int) (store : int) : return =
      9 |   let (op, addr) = Tezos.create_contract dummy_contract ((None: key_hash option)) 300tz 1 in
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 |   let toto : operation list = [ op ] in

    Not all free variables could be inlined in Tezos.create_contract usage: foo#255. |}];
  run_ligo_good [ "compile"; "contract"; contract "create_contract.mligo" ];
  [%expect
    {|
    File "../../test/contracts/create_contract.mligo", line 5, characters 20-21:
      4 |   let toto : operation * address = Tezos.create_contract
      5 |     (fun (p : nat) (s : string) -> (([] : operation list), "one"))
                              ^
      6 |     (None: key_hash option)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    File "../../test/contracts/create_contract.mligo", line 5, characters 10-11:
      4 |   let toto : operation * address = Tezos.create_contract
      5 |     (fun (p : nat) (s : string) -> (([] : operation list), "one"))
                    ^
      6 |     (None: key_hash option)
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    File "../../test/contracts/create_contract.mligo", line 3, characters 10-16:
      2 |
      3 | let main (action : string) (store : string) : return =
                    ^^^^^^
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
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_contract.mligo" ];
  [%expect
    {|
File "../../test/contracts/negative/bad_contract.mligo", line 4, characters 4-8:
  3 |
  4 | let main (action : parameter) (store : storage) : storage =
          ^^^^
  5 |   store + 1

Invalid type for entrypoint "main".
An entrypoint must of type "parameter * storage -> operation list * storage". |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_contract2.mligo" ];
  [%expect
    {|
File "../../test/contracts/negative/bad_contract2.mligo", line 5, character 0 to line 6, character 19:
  4 |
  5 | let main (action : parameter) (store : storage) : return =
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  6 |   ("bad",store + 1)
      ^^^^^^^^^^^^^^^^^^^

Invalid type for entrypoint "main".
An entrypoint must of type "parameter * storage -> operation list * storage".
We expected a list of operations but we got string |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_contract3.mligo" ];
  [%expect
    {|
File "../../test/contracts/negative/bad_contract3.mligo", line 5, character 0 to line 6, character 30:
  4 |
  5 | let main (action, store : parameter * storage) : return =
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  6 |   (([]: operation list),"bad")
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Invalid type for entrypoint "main".
The storage type "int" of the function parameter must be the same as the storage type "string" of the return value. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "duplicate_record_field.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/duplicate_record_field.mligo", line 1, characters 9-34:
      1 | type r = { foo : int ; foo : int }
                   ^^^^^^^^^^^^^^^^^^^^^^^^^
      2 |

    Repeated type variable in type.
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
    File "../../test/contracts/edo_combs.mligo", line 10, characters 22-23:
      9 |
     10 | let main (p : param) (s : int) : operation list * int =
                                ^
     11 |   let { x = x; y = y; z = z; w = w } = p in
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter (pair (int %x) (int %y) (int %z) (int %w)) ;
      storage int ;
      code { CAR ; UNPAIR 4 ; ADD ; ADD ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_duplicate3.mligo" ];
  [%expect
    {|
    { parameter (pair (nat %c) (nat %ck)) ;
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
    Warning: The type of "B(+42)" is ambiguous: Inferred type is "parameter_ok" but could be of type "parameter_warns".
    Hint: You might want to add a type annotation.

    (Left 42)
  |}]

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
      8 | let main = (action : parameter, store : storage) : [list<operation>, storage] => {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      9 |   return [
          ^^^^^^^^^^
     10 |    (list([]) as list <operation>),
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     11 |    (match (action, {
          ^^^^^^^^^^^^^^^^^^^^
     12 |     Increment: (n : int) => store + n,
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |     Extend: (k : never) => (Tezos.never(k) as storage)}))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     14 |   ]
          ^^^
     15 | };
          ^

    Toplevel let declaration is silently changed to const declaration.

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
      5 | let main (_ : param) (_ : unit) : operation list * unit =
      6 |   let c = (Tezos.self("%a") : unit contract) in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |   let op = Tezos.transaction () 0mutez c in

    Invalid entrypoint value.
    The entrypoint value does not match a constructor of the contract parameter. |}]

(* entrypoint check *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_get_entrypoint.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/bad_get_entrypoint.mligo", line 2, character 10 to line 4, character 85:
      1 | let main (_ : unit) (_ : unit) : operation list * unit =
      2 |   let v = (Tezos.get_entrypoint_opt
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
      3 |            "foo"
          ^^^^^^^^^^^^^^^^
      4 |            ("tz1fakefakefakefakefakefakefakcphLA5" : address) : unit contract option) in
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
     12 | let main (action : parameter) (store : storage) : return =
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |  ([] : operation list),    // No operations
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     14 |  (match action with
          ^^^^^^^^^^^^^^^^^^^
     15 |    Increment (n) -> let _ = Test.log "foo" in add store n
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     16 |  | Decrement (n) -> sub store n
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     17 |  | Reset         -> 0)
          ^^^^^^^^^^^^^^^^^^^^^^
     18 | let _test () =

    Invalid usage of a Test primitive: cannot be translated to Michelson. |}]

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
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
    File "../../test/contracts/negative/reuse_variable_name_top.jsligo", line 2, characters 4-7:
      1 | let dog = 1;
      2 | let dog = true;
              ^^^

    Duplicate identifier. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "reuse_variable_name_block.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/reuse_variable_name_block.jsligo", line 3, characters 8-9:
      2 |     let x = 2;
      3 |     let x = 2;
                  ^
      4 |     return x;

    Duplicate identifier. |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "evaluate-call"; contract "assert.mligo"; "with_error"; "(false, ())" ];
  [%expect {| failed with: "my custom error" |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "evaluate-call"
    ; contract "assert.mligo"
    ; "some_with_error"
    ; "(None: unit option)"
    ];
  [%expect {| failed with: "my custom error" |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "evaluate-call"
    ; contract "assert.mligo"
    ; "none_with_error"
    ; "(Some (): unit option)"
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
  [%expect
    {|
      File "../../test/contracts/negative/modules_export_type.jsligo", line 5, characters 9-16:
        4 |
        5 | type a = Bar.foo
                     ^^^^^^^

      Type "foo" not found. |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "modules_export_const.jsligo" ];
  [%expect
    {|
      File "../../test/contracts/negative/modules_export_const.jsligo", line 2, characters 4-15:
        1 | namespace Bar {
        2 |     let foo = 2
                ^^^^^^^^^^^
        3 | }

      Toplevel let declaration is silently changed to const declaration.

      File "../../test/contracts/negative/modules_export_const.jsligo", line 5, characters 0-15:
        4 |
        5 | let a = Bar.foo;
            ^^^^^^^^^^^^^^^

      Toplevel let declaration is silently changed to const declaration.

      File "../../test/contracts/negative/modules_export_const.jsligo", line 5, characters 8-15:
        4 |
        5 | let a = Bar.foo;
                    ^^^^^^^

      Variable "foo" not found. |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "modules_export_namespace.jsligo" ];
  [%expect
    {|
      File "../../test/contracts/negative/modules_export_namespace.jsligo", line 3, characters 8-17:
        2 |     namespace Foo {
        3 |         let a = 2;
                    ^^^^^^^^^
        4 |     }

      Toplevel let declaration is silently changed to const declaration.

      File "../../test/contracts/negative/modules_export_namespace.jsligo", line 7, characters 13-20:
        6 |
        7 | import Foo = Bar.Foo
                         ^^^^^^^

       Module "Bar.Foo" not found. |}];
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "y"
    ; "--init-file"
    ; bad_contract "modules_export_importer.jsligo"
    ];
  [%expect
    {|
      File "../../test/contracts/negative/modules_export_importer.jsligo", line 3, characters 10-13:
        2 |
        3 | const y : M.t = M.x;
                      ^^^

      Type "t" not found. |}]

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
             CDR ;
             { /* _ */ } ;
             { /* File "../../test/contracts/noop.mligo", line 2, character 2 to line 6, character 28 */
               { /* File "../../test/contracts/noop.mligo", line 2, characters 28-29 */
                 LAMBDA
                   unit
                   unit
                   { { /* x#216 */ } ;
                     { /* File "../../test/contracts/noop.mligo", line 2, characters 28-29 */ } } } ;
               { /* f#215, _ */ } ;
               { /* File "../../test/contracts/noop.mligo", line 3, character 2 to line 6, character 28 */
                 { /* File "../../test/contracts/noop.mligo", line 3, characters 18-21 */
                   SWAP ;
                   { /* File "../../test/contracts/noop.mligo", line 3, characters 18-19 */ DUP 2 } ;
                   SWAP ;
                   EXEC } ;
                 { /* s2#217, f#215 */ } ;
                 { /* File "../../test/contracts/noop.mligo", line 4, character 2 to line 6, character 28 */
                   { /* File "../../test/contracts/noop.mligo", line 4, characters 18-22 */
                     { /* File "../../test/contracts/noop.mligo", line 4, characters 20-22 */ } ;
                     { /* File "../../test/contracts/noop.mligo", line 4, characters 18-19 */ DUP 2 } ;
                     SWAP ;
                     EXEC } ;
                   { /* s3#218, f#215 */ } ;
                   { /* File "../../test/contracts/noop.mligo", line 5, character 2 to line 6, character 28 */
                     { /* File "../../test/contracts/noop.mligo", line 5, characters 10-14 */
                       { /* File "../../test/contracts/noop.mligo", line 5, characters 12-14 */ } ;
                       { /* File "../../test/contracts/noop.mligo", line 5, characters 10-11 */ SWAP } ;
                       SWAP ;
                       EXEC } ;
                     { /* s#219 */ } ;
                     { /* File "../../test/contracts/noop.mligo", line 6, characters 3-27 */
                       { /* File "../../test/contracts/noop.mligo", line 6, characters 26-27 */ } ;
                       { /* File "../../test/contracts/noop.mligo", line 6, characters 3-24 */
                         NIL operation
                             /* File "../../test/contracts/noop.mligo", line 6, characters 3-24 */
                         /* File "../../test/contracts/noop.mligo", line 6, characters 3-24 */ } ;
                       PAIR } } } } } } } |}]

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
  (* hmm, just checking that there is no error, should not use
     ppx_expect... *)
  let _ = [%expect.output] in
  [%expect {| |}]

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
  run_ligo_good [ "compile"; "contract"; contract "increment.jsligo" ];
  [%expect
    {|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]

(* Example contract from getting-started in PascaLIGO using --deprecated *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "increment.ligo"; "--deprecated" ];
  [%expect
    {|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "increment.ligo" ];
  [%expect
    {|
    Invalid file extension for '../../test/contracts/increment.ligo'.
    PascaLIGO is deprecated.
    Hint: You can enable its support using the --deprecated flag. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "expression"; "pascaligo"; "1 + 1" ];
  [%expect
    {|
    Invalid syntax.
    PascaLIGO is deprecated.
    Hint: You can enable its support using the --deprecated flag. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "expression"; "pascaligo"; "1 + 1"; "--deprecated" ];
  [%expect {|
    2 |}]

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

(* Compiling contract with non-tail recursion *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "lambdarec.mligo" ];
  [%expect
    {|
    { parameter (list int) ;
      storage (list int) ;
      code { LAMBDA_REC
               (pair (list int) (list int))
               (list int)
               { UNPAIR ;
                 IF_CONS
                   { DUG 2 ; PAIR ; DIG 2 ; SWAP ; EXEC ; SWAP ; CONS }
                   { SWAP ; DROP } } ;
             SWAP ;
             EXEC ;
             NIL operation ;
             PAIR } } |}]

(* Compiling CameLIGO expression with non-tail recursion *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "cat [fib 1; fib 2; fib 3] [fib 4; fib 5; fib 6;  fib 7]"
    ; "--init-file"
    ; contract "lambdarec.mligo"
    ];
  [%expect {|
    { 1 ; 2 ; 3 ; 5 ; 8 ; 13 ; 21 } |}]

(* Compiling CameLIGO expression with non-tail recursion *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "foo"
    ; "--init-file"
    ; contract "lambdarec2.mligo"
    ];
  [%expect
    {|
    (Lambda_rec
       { LEFT int ;
         LOOP_LEFT { DUP ; DUP 3 ; SWAP ; EXEC ; CONS ; LEFT int } ;
         SWAP ;
         DROP }) |}]

(* Compiling CameLIGO Ackermann expression with non-tail recursion *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "ackermann (3n, 2n)"
    ; "--init-file"
    ; contract "lambdarec.mligo"
    ];
  [%expect {|
    29 |}]

(* Compiling JsLIGO expression with non-tail recursion using env. *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "fib2()(5)"
    ; "--init-file"
    ; contract "lambdarec.jsligo"
    ];
  [%expect {|
    8 |}]

(* Compiling JsLIGO expression with non-tail recursion *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "cat(list([1,2,3]), list([4,fib(5)]))"
    ; "--init-file"
    ; contract "lambdarec.jsligo"
    ];
  [%expect {|
    { 1 ; 2 ; 3 ; 4 ; 8 } |}]

(* Compiling JsLIGO expression with non-tail recursion *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "wrong"
    ; "--init-file"
    ; contract "lambdarec2.jsligo"
    ];
  [%expect
    {|
    (Lambda_rec
       { PUSH int 0 ;
         NIL int ;
         DUP 3 ;
         CONS ;
         DIG 2 ;
         CONS ;
         ITER { DUP 3 ; SWAP ; EXEC ; ADD } ;
         SWAP ;
         DROP }) |}]

(* Compiling expression with non-tail recursion for rose tree map *)
let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "sum t"
    ; "--init-file"
    ; contract "rose_tree.mligo"
    ];
  [%expect {|
    53 |}]

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
File "../../test/contracts/extend_builtin.jsligo", line 2, characters 9-19:
  1 | namespace Tezos {
  2 |   export let x = 42;
               ^^^^^^^^^^
  3 |   export let f = (x  : int) : int => x + 2;

Toplevel let declaration is silently changed to const declaration.

File "../../test/contracts/extend_builtin.jsligo", line 3, characters 9-42:
  2 |   export let x = 42;
  3 |   export let f = (x  : int) : int => x + 2;
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  4 | }

Toplevel let declaration is silently changed to const declaration.

File "../../test/contracts/extend_builtin.jsligo", line 6, characters 0-24:
  5 |
  6 | let y = Tezos.f(Tezos.x);
      ^^^^^^^^^^^^^^^^^^^^^^^^

Toplevel let declaration is silently changed to const declaration.

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

(* check compiling view call for a non-literal *)
let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "call_view_not_litstr.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/call_view_not_litstr.mligo", line 2, characters 16-122:
      1 | let main ((s, _) : string * unit) : operation list * unit =
      2 |   let u = match (Tezos.call_view s (Tezos.get_sender ()) ("tz1fakefakefakefakefakefakefakcphLA5" : address) : unit option) with
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      3 |     | Some x -> x

    Invalid argument.
    View name must be a string literal. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "shadowed_sum_type.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/shadowed_sum_type.mligo", line 13, characters 8-12:
     12 |
     13 | let x = A 42
                  ^^^^
     14 |

    Constructor "A" not found.
  |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "bad_contract_return_type.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/bad_contract_return_type.mligo", line 5, characters 14-23:
      4 |
      5 | let main (_ : parameter) (s : storage) : _return =
                        ^^^^^^^^^
      6 |     [], s, 1tez

    Type "parameter" not found.
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
    File "../../test/contracts/negative/bytes_literals.jsligo", line 2, characters 12-23:
      1 | const shame = () => {
      2 |   const x = bytes `foo` as nat;
                      ^^^^^^^^^^^
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
      1 | let main (_ : unit) (_ : string) : operation list * string =
      2 |   [Tezos.emit "%hello world" 12], "bye"
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
      4 | let main (_p : parameter) (s : storage) : operation list * storage =
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      5 |  ([], s)
          ^^^^^^^^

    Invalid type for entrypoint "main".
    The parameter type "funtype 'a : * . list ('a)" of the entrypoint function must not contain polymorphic variables. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "disc_union_vbar.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/disc_union_vbar.jsligo", line 16, characters 11-21:
     15 |     let planetType = p.planetType;
     16 |     switch(planetType.kind) {
                     ^^^^^^^^^^
     17 |     case "Tellurian":
    :
    Warning: unused variable "planetType".
    Hint: replace it by "_planetType" to prevent this warning.

    File "../../test/contracts/disc_union_vbar.jsligo", line 16, characters 11-21:
     15 |     let planetType = p.planetType;
     16 |     switch(planetType.kind) {
                     ^^^^^^^^^^
     17 |     case "Tellurian":
    :
    Warning: unused variable "planetType".
    Hint: replace it by "_planetType" to prevent this warning.

    File "../../test/contracts/disc_union_vbar.jsligo", line 16, characters 11-21:
     15 |     let planetType = p.planetType;
     16 |     switch(planetType.kind) {
                     ^^^^^^^^^^
     17 |     case "Tellurian":
    :
    Warning: unused variable "planetType".
    Hint: replace it by "_planetType" to prevent this warning.

    { parameter
        (pair (pair (option %lord address) (string %name))
              (or %planetType (or (unit %gaseous) (unit %other)) (unit %tellurian))) ;
      storage int ;
      code { CAR ;
             PUSH int 0 ;
             SWAP ;
             CDR ;
             IF_LEFT
               { IF_LEFT { DROP ; PUSH int 2 ; SWAP ; SUB } { DROP 2 ; PUSH int 0 } }
               { DROP ; PUSH int 1 ; ADD } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "layout_comb.jsligo" ];
  [%expect
    {|
               { parameter (or (unit %a) (or (int %b) (pair %c int int))) ;
                 storage (pair (int %x) (int %y) (int %z)) ;
                 code { CAR ;
                        IF_LEFT
                          { DROP ; PUSH int 10 ; PUSH int 10 ; PUSH int 10 }
                          { IF_LEFT
                              { DROP ; PUSH int 20 ; PUSH int 20 ; PUSH int 20 }
                              { DROP ; PUSH int 20 ; PUSH int 20 ; PUSH int 20 } } ;
                        PAIR 3 ;
                        NIL operation ;
                        PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "contract_type_vars_let_fun.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage (list int) ;
      code { DROP ;
             NIL int ;
             PUSH int 3 ;
             CONS ;
             PUSH int 2 ;
             CONS ;
             PUSH int 1 ;
             CONS ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "used_var_in_local_module.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage int ;
      code { DROP ; PUSH int 1 ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "entrypoint_in_module.mligo"; "-m"; "C" ];
  [%expect
    {|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } ;
      view "foo" int int { UNPAIR ; ADD } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "entrypoint_in_module.mligo"
    ; "-m"
    ; "C"
    ];
  [%expect{|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } ;
      view "foo" int int { UNPAIR ; ADD } } |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "contract"; contract "entrypoint_in_module.mligo"; "-m"; "Bar" ];
  [%expect {|
    Internal error: Entrypoint main does not exist |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"; "contract"; contract "entrypoint_in_module.mligo"; "-m"; "Barrau" ];
  [%expect
    {|
    An internal error ocurred. Please, contact the developers.
    Module Barrau not found with last Barrau. |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "entrypoint_in_module.mligo"
    ; "Increment 32"
    ; "-m"
    ; "C"
    ];
  [%expect {|
    (Left (Right 32)) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "storage"; contract "entrypoint_in_module.mligo"; "5"; "-m"; "C" ];
  [%expect {|
    5 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "parameter"; contract "top_level_entry.mligo"; "42"; "-e"; "increment" ];
  [%expect {|
      42 |}];
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "top_level_entry.mligo"
    ; "Increment 42"
    ; "-e"
    ; "increment,decrement"
    ];
  [%expect {|
      (Right 42) |}];
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "top_level_entry.mligo"
    ; "Decrement 21"
    ; "-e"
    ; "increment,decrement,reset"
    ];
  [%expect {|
      (Left (Left 21)) |}];
  run_ligo_good
    [ "compile"; "storage"; contract "top_level_entry.mligo"; "42"; "-e"; "increment" ];
  [%expect {|
      42 |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "top_level_entry.mligo"
    ; "42"
    ; "-e"
    ; "increment,decrement"
    ];
  [%expect {|
  42 |}];
  run_ligo_good
    [ "compile"
    ; "storage"
    ; contract "top_level_entry.mligo"
    ; "42"
    ; "-e"
    ; "increment,decrement,reset"
    ];
  [%expect {|
      42 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "parameter"; contract "two_contracts.mligo"; "()" ];
  [%expect {|
      Unit |}];
  run_ligo_good [ "compile"; "storage"; contract "two_contracts.mligo"; "false" ];
  [%expect {|
      False |}];
  run_ligo_good
    [ "compile"; "parameter"; contract "two_contracts.mligo"; "\"Hello\""; "-e"; "ep2" ];
  [%expect {|
      "Hello" |}];
  run_ligo_good
    [ "compile"; "storage"; contract "two_contracts.mligo"; "42 * 2"; "-e"; "ep2" ];
  [%expect {|
      84 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "dry-run"
    ; contract "entrypoint_in_module.mligo"
    ; "Increment 5"
    ; "37"
    ; "-m"
    ; "C"
    ];
  [%expect {|
    ( LIST_EMPTY() , 42 ) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "entrypoint_in_module.jsligo"; "-m"; "M.C" ];
  [%expect
    {|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } ;
      view "foo" int int { UNPAIR ; ADD } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; bad_contract "entrypoint_no_type.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/entrypoint_no_type.jsligo", line 8, character 15 to line 10, character 1:
      7 | @entry
      8 | const unique = (_ : organization, _ : storage) => {
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      9 |     return failwith("You need to be part of Tezos organization to activate an organization");
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     10 | };
          ^

    Invalid type for entrypoint "unique".
    An entrypoint must of type "parameter * storage -> operation list * storage". |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "increment_module.jsligo"; "-m"; "C" ];
  [%expect
    {|
    { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "FA1.2.entries.mligo" ];
  [%expect
    {|
    { parameter
        (or (or (or (pair %approve (address %spender) (nat %value))
                    (pair %getAllowance
                       (pair %request (address %owner) (address %spender))
                       (contract %callback nat)))
                (or (pair %getBalance (address %owner) (contract %callback nat))
                    (pair %getTotalSupply (unit %request) (contract %callback nat))))
            (pair %transfer (address %from) (address %to) (nat %value))) ;
      storage
        (pair (pair (big_map %allowances (pair (address %owner) (address %spender)) nat)
                    (big_map %tokens address nat))
              (nat %total_supply)) ;
      code { UNPAIR ;
             IF_LEFT
               { IF_LEFT
                   { IF_LEFT
                       { DUP 2 ;
                         CAR ;
                         CAR ;
                         DUP 2 ;
                         CAR ;
                         SENDER ;
                         PAIR ;
                         PUSH nat 0 ;
                         DUP 4 ;
                         CDR ;
                         COMPARE ;
                         GT ;
                         PUSH nat 0 ;
                         DUP 4 ;
                         DUP 4 ;
                         GET ;
                         IF_NONE { PUSH nat 0 } {} ;
                         COMPARE ;
                         GT ;
                         AND ;
                         IF { PUSH string "UnsafeAllowanceChange" ; FAILWITH } {} ;
                         DUP 4 ;
                         DIG 4 ;
                         CAR ;
                         DIG 4 ;
                         CDR ;
                         DIG 4 ;
                         PUSH nat 0 ;
                         DUP 3 ;
                         COMPARE ;
                         EQ ;
                         IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                         DIG 4 ;
                         UPDATE ;
                         UPDATE 1 ;
                         UPDATE 1 ;
                         NIL operation }
                       { DUP 2 ;
                         NIL operation ;
                         DUP 3 ;
                         CDR ;
                         PUSH mutez 0 ;
                         DIG 5 ;
                         CAR ;
                         CAR ;
                         DIG 5 ;
                         CAR ;
                         GET ;
                         IF_NONE { PUSH nat 0 } {} ;
                         TRANSFER_TOKENS ;
                         CONS } }
                   { IF_LEFT
                       { DUP 2 ;
                         NIL operation ;
                         DUP 3 ;
                         CDR ;
                         PUSH mutez 0 ;
                         DIG 5 ;
                         CAR ;
                         CDR ;
                         DIG 5 ;
                         CAR ;
                         GET ;
                         IF_NONE { PUSH nat 0 } {} ;
                         TRANSFER_TOKENS }
                       { DUP 2 ;
                         NIL operation ;
                         DIG 2 ;
                         CDR ;
                         PUSH mutez 0 ;
                         DIG 4 ;
                         CDR ;
                         TRANSFER_TOKENS } ;
                     CONS } }
               { DUP 2 ;
                 CAR ;
                 CAR ;
                 DUP 3 ;
                 CAR ;
                 CDR ;
                 DUP 3 ;
                 CAR ;
                 SENDER ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP }
                    { SENDER ;
                      DUP 4 ;
                      CAR ;
                      PAIR ;
                      DUP 4 ;
                      GET 4 ;
                      DUP 4 ;
                      DUP 3 ;
                      GET ;
                      IF_NONE { PUSH nat 0 } {} ;
                      SUB ;
                      ISNAT ;
                      IF_NONE { PUSH string "NotEnoughAllowance" ; FAILWITH } {} ;
                      DIG 3 ;
                      PUSH nat 0 ;
                      DUP 3 ;
                      COMPARE ;
                      EQ ;
                      IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                      DIG 2 ;
                      UPDATE } ;
                 DUP 3 ;
                 GET 4 ;
                 DUP 3 ;
                 DUP 5 ;
                 CAR ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 SUB ;
                 ISNAT ;
                 IF_NONE { PUSH string "NotEnoughBalance" ; FAILWITH } {} ;
                 DIG 2 ;
                 PUSH nat 0 ;
                 DUP 3 ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                 DUP 4 ;
                 CAR ;
                 UPDATE ;
                 DUP 3 ;
                 GET 4 ;
                 DUP 2 ;
                 DUP 5 ;
                 GET 3 ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 ADD ;
                 DUP 5 ;
                 DIG 5 ;
                 CAR ;
                 DIG 3 ;
                 PUSH nat 0 ;
                 DUP 5 ;
                 COMPARE ;
                 EQ ;
                 IF { DIG 3 ; DROP ; NONE nat } { DIG 3 ; SOME } ;
                 DIG 5 ;
                 GET 3 ;
                 UPDATE ;
                 UPDATE 2 ;
                 UPDATE 1 ;
                 DUP ;
                 CAR ;
                 DIG 2 ;
                 UPDATE 1 ;
                 UPDATE 1 ;
                 NIL operation } ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "parameter"
    ; contract "FA1.2.entries.mligo"
    ; "Approve { spender = (\"tz1fakefakefakefakefakefakefakcphLA5\" : address) ; value \
       = 3n }"
    ];
  [%expect {| (Left (Left (Left (Pair "tz1fakefakefakefakefakefakefakcphLA5" 3)))) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "pokeGame.jsligo" ];
  [%expect
    {|
    { parameter
        (or (or (pair %init address nat) (unit %poke)) (address %pokeAndGetFeedback)) ;
      storage
        (pair (pair (string %feedback)
                    (map %pokeTraces address (pair (string %feedback) (address %receiver))))
              (map %ticketOwnership address (ticket string))) ;
      code { UNPAIR ;
             IF_LEFT
               { IF_LEFT
                   { SWAP ;
                     UNPAIR ;
                     UNPAIR ;
                     PUSH nat 0 ;
                     DUP 5 ;
                     CDR ;
                     COMPARE ;
                     EQ ;
                     IF { DIG 3 ; DROP ; DIG 2 }
                        { DUP 4 ;
                          CDR ;
                          PUSH string "can_poke" ;
                          TICKET ;
                          IF_NONE { PUSH string "option is None" ; FAILWITH } {} ;
                          DIG 3 ;
                          SWAP ;
                          DIG 4 ;
                          CAR ;
                          SWAP ;
                          SOME ;
                          SWAP ;
                          UPDATE } ;
                     DUG 2 ;
                     PAIR ;
                     PAIR ;
                     NIL operation ;
                     PAIR }
                   { DROP ;
                     UNPAIR ;
                     UNPAIR ;
                     DIG 2 ;
                     NONE (ticket string) ;
                     SOURCE ;
                     GET_AND_UPDATE ;
                     IF_NONE
                       { DROP 3 ;
                         PUSH string "User does not have tickets => not allowed" ;
                         FAILWITH }
                       { DROP ;
                         DIG 2 ;
                         SELF_ADDRESS ;
                         PUSH string "" ;
                         PAIR ;
                         SOURCE ;
                         SWAP ;
                         SOME ;
                         SWAP ;
                         UPDATE ;
                         DIG 2 ;
                         PAIR ;
                         PAIR ;
                         NIL operation ;
                         PAIR } } }
               { SWAP ;
                 UNPAIR ;
                 CDR ;
                 SWAP ;
                 NONE (ticket string) ;
                 SOURCE ;
                 GET_AND_UPDATE ;
                 DUP 4 ;
                 UNIT ;
                 VIEW "feedback" string ;
                 SWAP ;
                 IF_NONE
                   { DROP 4 ;
                     PUSH string "User does not have tickets => not allowed" ;
                     FAILWITH }
                   { DROP ;
                     IF_NONE
                       { DROP 3 ;
                         PUSH string "Cannot find view feedback on given oracle address" ;
                         FAILWITH }
                       { SWAP ;
                         DIG 2 ;
                         DIG 3 ;
                         DUP 4 ;
                         PAIR ;
                         SOURCE ;
                         SWAP ;
                         SOME ;
                         SWAP ;
                         UPDATE ;
                         DIG 2 ;
                         PAIR ;
                         PAIR ;
                         NIL operation ;
                         PAIR } } } } ;
      view "feedback" unit string { CDR ; CAR ; CAR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "parameter"; contract "pokeGame.jsligo"; "Poke()" ];
  [%expect {| (Left (Right Unit)) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "contract_of.jsligo" ];
  [%expect
    {|
    { parameter int ;
      storage int ;
      code { UNPAIR ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "entries_in_module.mligo"; "-m"; "C" ];
  [%expect
    {|
    { parameter (or (or (int %add) (unit %reset)) (int %sub)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT { IF_LEFT { ADD } { DROP 2 ; PUSH int 0 } } { SWAP ; SUB } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "bytes_bitwise.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH nat 8 ;
             PUSH bytes 0x06 ;
             LSL ;
             PUSH nat 1 ;
             PUSH bytes 0x0006 ;
             LSR ;
             PUSH bytes 0x0003 ;
             SWAP ;
             COMPARE ;
             EQ ;
             PUSH bytes 0x0600 ;
             DIG 2 ;
             COMPARE ;
             EQ ;
             PUSH bytes 0x0103 ;
             PUSH bytes 0x0106 ;
             PUSH bytes 0x0005 ;
             XOR ;
             COMPARE ;
             EQ ;
             PUSH bytes 0x0107 ;
             PUSH bytes 0x0106 ;
             PUSH bytes 0x0005 ;
             OR ;
             COMPARE ;
             EQ ;
             PUSH bytes 0x0004 ;
             PUSH bytes 0x0106 ;
             PUSH bytes 0x0005 ;
             AND ;
             COMPARE ;
             EQ ;
             AND ;
             AND ;
             AND ;
             AND ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             UNIT ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good [ "run"; "dry-run"; contract "bytes_bitwise.mligo"; "()"; "()" ];
  [%expect {| ( LIST_EMPTY() , unit ) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "bytes_int_nat_conv.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH bytes 0x123456 ;
             DUP ;
             NAT ;
             BYTES ;
             DUP 2 ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             DUP ;
             INT ;
             BYTES ;
             SWAP ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             PUSH int 1234 ;
             BYTES ;
             INT ;
             PUSH int 1234 ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             PUSH nat 4567 ;
             BYTES ;
             NAT ;
             PUSH nat 4567 ;
             COMPARE ;
             EQ ;
             IF {} { PUSH string "failed assertion" ; FAILWITH } ;
             UNIT ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good [ "run"; "dry-run"; contract "bytes_int_nat_conv.mligo"; "()"; "()" ];
  [%expect {| ( LIST_EMPTY() , unit ) |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"; "contract"; contract "increment_prefix.jsligo"; "-m"; "IncDec" ];
  [%expect
    {|
    { parameter (or (or (unit %decrement) (unit %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
             IF_LEFT
               { IF_LEFT { DROP ; PUSH int 1 ; SWAP ; SUB } { DROP ; PUSH int 1 ; ADD } }
               { DROP 2 ; PUSH int 0 } ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good [ "run"; "test"; contract "increment_prefix.jsligo" ];
  [%expect
    {|
    Everything at the top-level was executed.
    - test_increment exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "reverse_string_for_loop.jsligo" ];
  [%expect
    {|
    { parameter unit ;
      storage string ;
      code { CDR ;
             PUSH string "" ;
             PUSH int 1 ;
             DUP 3 ;
             SIZE ;
             SUB ;
             PUSH bool True ;
             LOOP { PUSH int 0 ;
                    DUP 2 ;
                    COMPARE ;
                    GE ;
                    DUP ;
                    IF { DUP 2 ;
                         ABS ;
                         DUP 5 ;
                         PUSH nat 1 ;
                         DIG 2 ;
                         SLICE ;
                         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} ;
                         DIG 3 ;
                         CONCAT ;
                         DUG 2 ;
                         PUSH int 1 ;
                         DIG 2 ;
                         SUB ;
                         SWAP }
                       {} } ;
             DIG 2 ;
             DROP 2 ;
             NIL operation ;
             PAIR } } |}];
  run_ligo_good [ "run"; "test"; contract "reverse_string_for_loop.jsligo" ];
  [%expect
    {|
    "reverse"
    Everything at the top-level was executed.
    - test exited with value (). |}]
