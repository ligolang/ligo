open Cli_expect

let contract basename = "../../test/contracts/" ^ basename
let bad_contract basename = "../../test/contracts/negative/" ^ basename

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "protocol_dalphanet.mligo" ];
  [%expect
    {|
    File "../../test/contracts/protocol_dalphanet.mligo", line 12, characters 22-23:
     11 |
     12 | let main (p : bls_l) (s : bool) : operation list * bool =
                                ^
     13 |  (([] : operation list), Tezos.pairing_check p)
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
    File "../../test/contracts/sapling.mligo", line 8, characters 27-32:
      7 |
      8 | let main (tr : parameter) (store : storage) : return =
                                     ^^^^^
      9 |  ([] : operation list),
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
    File "../../test/contracts/negative/emit.mligo", line 3, characters 3-18:
      2 |   let x = "%lol" in
      3 |   [Tezos.emit x 12],x
             ^^^^^^^^^^^^^^^

    Invalid event tag.
    The tag must be a string literal. |}]
