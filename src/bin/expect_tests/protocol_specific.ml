open Cli_expect

let contract basename = "../../test/contracts/" ^ basename
let bad_contract basename = "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "protocol_dalphanet.mligo" ];
  [%expect
    {|
    File "../../test/contracts/protocol_dalphanet.mligo", line 18, characters 22-23:
     17 | [@entry]
     18 | let main (p : bls_l) (s : bool) : operation list * bool =
                                ^
     19 |   (([] : operation list), Tezos.pairing_check p)
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter (list (pair bls12_381_g1 bls12_381_g2)) ;
      storage bool ;
      code { CAR ; PAIRING_CHECK ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "contract"
    ; contract "sapling.mligo"
    ; "--disable-michelson-typechecking"
    ];
  [%expect
    {|
    File "../../test/contracts/sapling.mligo", line 10, characters 27-32:
      9 | [@entry]
     10 | let main (tr : parameter) (store : storage) : return =
                                     ^^^^^
     11 |   ([] : operation list),
    :
    Warning: unused variable "store".
    Hint: replace it by "_store" to prevent this warning.

    { parameter (sapling_transaction 8) ;
      storage (pair int (sapling_state 8)) ;
      code { CAR ;
             SAPLING_EMPTY_STATE 8 ;
             SWAP ;
             SAPLING_VERIFY_UPDATE ;
             IF_NONE { PUSH string "failed" ; FAILWITH } { CDR } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "rollup.mligo" ];
  [%expect
    {|
    File "../../test/contracts/rollup.mligo", line 2, characters 14-34:
      1 | [@entry]
      2 | let main (_ : tx_rollup_l2_address) (_ : unit) : operation list * unit =
                        ^^^^^^^^^^^^^^^^^^^^
      3 |   (failwith "roll up !" : operation list * unit)

    Type "tx_rollup_l2_address" not found. |}];
  run_ligo_good [ "compile"; "contract"; contract "min_block_time.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage nat ;
      code { DROP ; MIN_BLOCK_TIME ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "emit.mligo" ];
  [%expect
    {|
    { parameter (pair int int) ;
      storage unit ;
      code { CAR ;
             UNIT ;
             NIL operation ;
             DUP 3 ;
             CAR ;
             EMIT %bar int ;
             CONS ;
             DIG 2 ;
             EMIT %foo (pair int int) ;
             CONS ;
             PAIR } } |}];
  run_ligo_bad [ "compile"; "contract"; bad_contract "emit.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/emit.mligo", line 4, characters 3-18:
      3 |   let x = "%lol" in
      4 |   [Tezos.emit x 12], x
             ^^^^^^^^^^^^^^^

    Invalid event tag.
    The tag must be a string literal. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "rollup_address.mligo" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH address "sr1R23ax3Gj8NDQFbQRfNnzuKEZhth5qvWVP" ;
             CONTRACT unit ;
             IF_NONE { PUSH string "Err" ; FAILWITH } {} ;
             UNIT ;
             NIL operation ;
             DIG 2 ;
             PUSH mutez 0 ;
             UNIT ;
             TRANSFER_TOKENS ;
             CONS ;
             PAIR } } |}]

(* Test if pre alpha protocol works in this case it's mumbai, but in future this 
   will change
   UPDATE: it seems that we do not have pre alpha anymore? Switching to oxford
 *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "unit.mligo"; "--protocol"; "oxford" ];
  [%expect
    {|
    { parameter unit ;
      storage unit ;
      code { DROP ; UNIT ; NIL operation ; PAIR } } |}]
