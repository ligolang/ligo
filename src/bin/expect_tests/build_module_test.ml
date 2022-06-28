open Cli_expect

let contract basename =
  "../../test/contracts/build/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    `-- 3 -- ../../test/contracts/build/cycle_A.mligo
        `-- 2 -- ../../test/contracts/build/cycle_B.mligo
            `-- 1 -- ../../test/contracts/build/cycle_C.mligo
                `-- 3 -- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "cycle_A.mligo"; "--format" ; "json" ] ;
  [%expect {|
    {
      "root": "../../test/contracts/build/cycle_A.mligo",
      "child": {
        "file": "../../test/contracts/build/cycle_B.mligo",
        "child": {
          "file": "../../test/contracts/build/cycle_C.mligo",
          "child": {
            "file": "../../test/contracts/build/cycle_A.mligo",
            "child": { "file": "../../test/contracts/build/cycle_B.mligo" }
          }
        }
      }
    } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "D.mligo" ] ;
  [%expect {|
    `-- 7 -- ../../test/contracts/build/D.mligo
        |-- 5 -- ../../test/contracts/build/C.mligo
        |   |-- 1 -- ../../test/contracts/build/A.mligo
        |   `-- 2 -- ../../test/contracts/build/B.mligo
        |       `-- 1 -- ../../test/contracts/build/A.mligo
        `-- 6 -- ../../test/contracts/build/E.mligo
            |-- 3 -- ../../test/contracts/build/F.mligo
            `-- 4 -- ../../test/contracts/build/G.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "D.mligo"; "--format" ; "json" ] ;
  [%expect {|
    {
      "root": "../../test/contracts/build/D.mligo",
      "child": {
        "file": "../../test/contracts/build/C.mligo",
        "child": { "file": "../../test/contracts/build/A.mligo" },
        "child": {
          "file": "../../test/contracts/build/B.mligo",
          "child": { "file": "../../test/contracts/build/A.mligo" }
        }
      },
      "child": {
        "file": "../../test/contracts/build/E.mligo",
        "child": { "file": "../../test/contracts/build/F.mligo" },
        "child": { "file": "../../test/contracts/build/G.mligo" }
      }
    } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "B.mligo" ; "-e" ; "f" ] ;
  [%expect{|
    { parameter unit ;
      storage int ;
      code { PUSH int 1 ;
             PUSH int 42 ;
             DUP 2 ;
             ADD ;
             DIG 2 ;
             CDR ;
             SWAP ;
             DUG 2 ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "D.mligo" ] ;
  [%expect {|
    const toto = ADD(E.toto , C.B.A.toto)
    const fb = record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main =
      lambda (gen#5 : ( int * int )) return  match gen#5 with
                                              | ( p , s ) ->
                                              let s = ADD(ADD(p , s) ,
                                              toto) in ( LIST_EMPTY() , s ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let <../../test/contracts/build/A.mligo#0><Tezos#0>balance =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>amount =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>now =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>sender =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>source =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>level =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>self_address =
  SELF_ADDRESS()[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>chain_id =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>get_now =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>get_self_address =
  fun _u -> (SELF_ADDRESS())[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>min_block_time =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>get_min_block_time =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>voting_power =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>implicit_account =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>pairing_check =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bitwise#0>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bitwise#0>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bitwise#0>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/A.mligo#0><String#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/A.mligo#0><String#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/A.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bytes#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bytes#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bytes#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#0>blake2b =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#0>sha256 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#0>sha512 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#0>sha3 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#0>keccak =
  fun b -> (({ KECCAK })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#0>hash_key =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#0>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let <../../test/contracts/build/A.mligo#0>assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let <../../test/contracts/build/A.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/A.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/A.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/A.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/A.mligo#0>unit = UNIT()[@inline] in
let poly_failwith_105 = { FAILWITH }[@inline] in
let poly_failwith_104 = { FAILWITH }[@inline] in
let poly_failwith_103 = { FAILWITH }[@inline] in
let poly_failwith_102 = { FAILWITH }[@inline] in
let poly_failwith_101 = { FAILWITH }[@inline] in
let poly_failwith_100 = { FAILWITH }[@inline] in
let poly_failwith_99 = { FAILWITH }[@inline] in
let poly_failwith_98 = { FAILWITH }[@inline] in
let poly_failwith_97 = { FAILWITH }[@inline] in
let poly_failwith_96 = { FAILWITH }[@inline] in
let poly_failwith_95 = { FAILWITH }[@inline] in
let poly_failwith_94 = { FAILWITH }[@inline] in
let poly_failwith_93 = { FAILWITH }[@inline] in
let poly_failwith_92 = { FAILWITH }[@inline] in
let poly_failwith_91 = { FAILWITH }[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>originate_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_failwith_105)@(L("TEST MODE")))))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>set_source =
  fun _a -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>set_baker =
  fun _a -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>set_baker_policy =
  fun _bp -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_91)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_102)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>get_storage_of_address =
  fun _a -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>get_balance =
  fun _a -> ((poly_failwith_104)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_failwith_103)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_failwith_91)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_failwith_91)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>get_voting_power =
  fun _kh -> ((poly_failwith_102)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>get_total_voting_power =
  (poly_failwith_102)@(L("TEST MODE"))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_failwith_96)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>nth_bootstrap_account =
  fun _i -> ((poly_failwith_96)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>last_originations =
  fun _u -> ((poly_failwith_101)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_failwith_92)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>add_account =
  fun _s -> (fun _k -> ((poly_failwith_91)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>new_account =
  fun _u -> ((poly_failwith_100)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_failwith_91)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>register_delegate =
  fun _kh -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>register_constant =
  fun _m -> ((poly_failwith_99)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_failwith_98)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_failwith_97)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>constant_to_michelson_program =
  fun _s -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>restore_context =
  fun _u -> ((poly_failwith_91)@(L("TEST_POP_CONTEXT")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>save_context =
  fun _u -> ((poly_failwith_91)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>drop_context =
  fun _u -> ((poly_failwith_91)@(L("TEST_DROP_CONTEXT")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>read_contract_from_file =
  fun _fn -> ((poly_failwith_91)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>compile_contract_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_failwith_91)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>originate_contract =
  fun _c ->
  (fun _s -> (fun _t -> ((poly_failwith_96)@(L("TEST_ORIGINATE")))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>size =
  fun _c -> ((poly_failwith_95)@(L("TEST_SIZE")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>get_bootstrap_account =
  fun _n -> ((poly_failwith_94)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>sign =
  fun _sk -> (fun _d -> ((poly_failwith_93)@(L("TEST_SIGN"))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>chr =
  fun _n -> ((poly_failwith_92)@(L("TEST_CHR")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>nl =
  L("NEWLINE")[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>println =
  fun _v -> ((poly_failwith_91)@(L("TEST_PRINTLN")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>print =
  fun _v -> ((poly_failwith_91)@(L("TEST_PRINT")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>eprint =
  fun _v -> ((poly_failwith_91)@(L("TEST_EPRINTL")))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>balance =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>amount =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>now =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>sender =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>source =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>level =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>self_address =
  SELF_ADDRESS()[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>chain_id =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>get_now =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>get_self_address =
  fun _u -> (SELF_ADDRESS())[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>voting_power =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>implicit_account =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>pairing_check =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#2>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bitwise#3>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bitwise#3>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bitwise#3>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/A.mligo#0><String#8>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/A.mligo#0><String#8>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/A.mligo#0><String#8>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bytes#10>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bytes#10>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bytes#10>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#11>blake2b =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#11>sha256 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#11>sha512 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#11>sha3 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#11>keccak =
  fun b -> (({ KECCAK })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#11>hash_key =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let <../../test/contracts/build/A.mligo#0><Crypto#11>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let <../../test/contracts/build/A.mligo#0>assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let <../../test/contracts/build/A.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/A.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/A.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/A.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/A.mligo#0>unit = UNIT()[@inline] in
let <../../test/contracts/build/A.mligo#0>toto = L(1) in
let <../../test/contracts/build/B.mligo#0><Tezos#0>balance =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>amount =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>now =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>sender =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>source =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>level =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>self_address =
  SELF_ADDRESS()[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>chain_id =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>get_now =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>get_self_address =
  fun _u -> (SELF_ADDRESS())[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>min_block_time =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>get_min_block_time =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>voting_power =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>implicit_account =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>pairing_check =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bitwise#0>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bitwise#0>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bitwise#0>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/B.mligo#0><String#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/B.mligo#0><String#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/B.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bytes#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bytes#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bytes#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#0>blake2b =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#0>sha256 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#0>sha512 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#0>sha3 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#0>keccak =
  fun b -> (({ KECCAK })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#0>hash_key =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#0>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let <../../test/contracts/build/B.mligo#0>assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let <../../test/contracts/build/B.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/B.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/B.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/B.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/B.mligo#0>unit = UNIT()[@inline] in
let poly_failwith_90 = { FAILWITH }[@inline] in
let poly_failwith_89 = { FAILWITH }[@inline] in
let poly_failwith_88 = { FAILWITH }[@inline] in
let poly_failwith_87 = { FAILWITH }[@inline] in
let poly_failwith_86 = { FAILWITH }[@inline] in
let poly_failwith_85 = { FAILWITH }[@inline] in
let poly_failwith_84 = { FAILWITH }[@inline] in
let poly_failwith_83 = { FAILWITH }[@inline] in
let poly_failwith_82 = { FAILWITH }[@inline] in
let poly_failwith_81 = { FAILWITH }[@inline] in
let poly_failwith_80 = { FAILWITH }[@inline] in
let poly_failwith_79 = { FAILWITH }[@inline] in
let poly_failwith_78 = { FAILWITH }[@inline] in
let poly_failwith_77 = { FAILWITH }[@inline] in
let poly_failwith_76 = { FAILWITH }[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>originate_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_failwith_90)@(L("TEST MODE")))))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>set_source =
  fun _a -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>set_baker =
  fun _a -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>set_baker_policy =
  fun _bp -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_76)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_87)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>get_storage_of_address =
  fun _a -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>get_balance =
  fun _a -> ((poly_failwith_89)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_failwith_88)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_failwith_76)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_failwith_76)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>get_voting_power =
  fun _kh -> ((poly_failwith_87)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>get_total_voting_power =
  (poly_failwith_87)@(L("TEST MODE"))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_failwith_81)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>nth_bootstrap_account =
  fun _i -> ((poly_failwith_81)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>last_originations =
  fun _u -> ((poly_failwith_86)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_failwith_77)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>add_account =
  fun _s -> (fun _k -> ((poly_failwith_76)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>new_account =
  fun _u -> ((poly_failwith_85)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_failwith_76)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>register_delegate =
  fun _kh -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>register_constant =
  fun _m -> ((poly_failwith_84)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_failwith_83)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_failwith_82)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>constant_to_michelson_program =
  fun _s -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>restore_context =
  fun _u -> ((poly_failwith_76)@(L("TEST_POP_CONTEXT")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>save_context =
  fun _u -> ((poly_failwith_76)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>drop_context =
  fun _u -> ((poly_failwith_76)@(L("TEST_DROP_CONTEXT")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>read_contract_from_file =
  fun _fn -> ((poly_failwith_76)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>compile_contract_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_failwith_76)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>originate_contract =
  fun _c ->
  (fun _s -> (fun _t -> ((poly_failwith_81)@(L("TEST_ORIGINATE")))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>size =
  fun _c -> ((poly_failwith_80)@(L("TEST_SIZE")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>get_bootstrap_account =
  fun _n -> ((poly_failwith_79)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>sign =
  fun _sk -> (fun _d -> ((poly_failwith_78)@(L("TEST_SIGN"))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>chr =
  fun _n -> ((poly_failwith_77)@(L("TEST_CHR")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>nl =
  L("NEWLINE")[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>println =
  fun _v -> ((poly_failwith_76)@(L("TEST_PRINTLN")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>print =
  fun _v -> ((poly_failwith_76)@(L("TEST_PRINT")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>eprint =
  fun _v -> ((poly_failwith_76)@(L("TEST_EPRINTL")))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>balance =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>amount =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>now =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>sender =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>source =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>level =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>self_address =
  SELF_ADDRESS()[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>chain_id =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>get_now =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>get_self_address =
  fun _u -> (SELF_ADDRESS())[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>voting_power =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>implicit_account =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>pairing_check =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#12>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bitwise#13>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bitwise#13>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bitwise#13>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/B.mligo#0><String#18>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/B.mligo#0><String#18>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/B.mligo#0><String#18>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bytes#20>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bytes#20>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bytes#20>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#21>blake2b =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#21>sha256 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#21>sha512 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#21>sha3 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#21>keccak =
  fun b -> (({ KECCAK })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#21>hash_key =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let <../../test/contracts/build/B.mligo#0><Crypto#21>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let <../../test/contracts/build/B.mligo#0>assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let <../../test/contracts/build/B.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let <../../test/contracts/build/B.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/B.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/B.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/B.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/B.mligo#0>unit = UNIT()[@inline] in
let <../../test/contracts/build/B.mligo#0>toto = L(32) in
let <../../test/contracts/build/B.mligo#0>titi =
  ADD(<../../test/contracts/build/A.mligo#0>toto , L(42)) in
let <../../test/contracts/build/B.mligo#0>f =
  fun gen#1529 ->
  (let (gen#5131, gen#5132) = gen#1529 in
   let gen#1530 = gen#5131 in
   let x = gen#5132 in
   let x =
     ADD(ADD(x , <../../test/contracts/build/A.mligo#0>toto) ,
         <../../test/contracts/build/B.mligo#0>titi) in
   PAIR(LIST_EMPTY() , x)) in
let <../../test/contracts/build/F.mligo#0><Tezos#0>balance =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>amount =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>now =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>sender =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>source =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>level =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>self_address =
  SELF_ADDRESS()[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>chain_id =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>get_now =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>get_self_address =
  fun _u -> (SELF_ADDRESS())[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>min_block_time =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>get_min_block_time =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>voting_power =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>implicit_account =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>pairing_check =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bitwise#0>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bitwise#0>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bitwise#0>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/F.mligo#0><String#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/F.mligo#0><String#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/F.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bytes#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bytes#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bytes#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#0>blake2b =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#0>sha256 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#0>sha512 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#0>sha3 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#0>keccak =
  fun b -> (({ KECCAK })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#0>hash_key =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#0>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let <../../test/contracts/build/F.mligo#0>assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let <../../test/contracts/build/F.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/F.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/F.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/F.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/F.mligo#0>unit = UNIT()[@inline] in
let poly_failwith_75 = { FAILWITH }[@inline] in
let poly_failwith_74 = { FAILWITH }[@inline] in
let poly_failwith_73 = { FAILWITH }[@inline] in
let poly_failwith_72 = { FAILWITH }[@inline] in
let poly_failwith_71 = { FAILWITH }[@inline] in
let poly_failwith_70 = { FAILWITH }[@inline] in
let poly_failwith_69 = { FAILWITH }[@inline] in
let poly_failwith_68 = { FAILWITH }[@inline] in
let poly_failwith_67 = { FAILWITH }[@inline] in
let poly_failwith_66 = { FAILWITH }[@inline] in
let poly_failwith_65 = { FAILWITH }[@inline] in
let poly_failwith_64 = { FAILWITH }[@inline] in
let poly_failwith_63 = { FAILWITH }[@inline] in
let poly_failwith_62 = { FAILWITH }[@inline] in
let poly_failwith_61 = { FAILWITH }[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>originate_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_failwith_75)@(L("TEST MODE")))))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>set_source =
  fun _a -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>set_baker =
  fun _a -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>set_baker_policy =
  fun _bp -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_61)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_72)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>get_storage_of_address =
  fun _a -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>get_balance =
  fun _a -> ((poly_failwith_74)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_failwith_73)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_failwith_61)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_failwith_61)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>get_voting_power =
  fun _kh -> ((poly_failwith_72)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>get_total_voting_power =
  (poly_failwith_72)@(L("TEST MODE"))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_failwith_66)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>nth_bootstrap_account =
  fun _i -> ((poly_failwith_66)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>last_originations =
  fun _u -> ((poly_failwith_71)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_failwith_62)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>add_account =
  fun _s -> (fun _k -> ((poly_failwith_61)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>new_account =
  fun _u -> ((poly_failwith_70)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_failwith_61)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>register_delegate =
  fun _kh -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>register_constant =
  fun _m -> ((poly_failwith_69)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_failwith_68)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_failwith_67)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>constant_to_michelson_program =
  fun _s -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>restore_context =
  fun _u -> ((poly_failwith_61)@(L("TEST_POP_CONTEXT")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>save_context =
  fun _u -> ((poly_failwith_61)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>drop_context =
  fun _u -> ((poly_failwith_61)@(L("TEST_DROP_CONTEXT")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>read_contract_from_file =
  fun _fn -> ((poly_failwith_61)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>compile_contract_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_failwith_61)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>originate_contract =
  fun _c ->
  (fun _s -> (fun _t -> ((poly_failwith_66)@(L("TEST_ORIGINATE")))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>size =
  fun _c -> ((poly_failwith_65)@(L("TEST_SIZE")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>get_bootstrap_account =
  fun _n -> ((poly_failwith_64)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>sign =
  fun _sk -> (fun _d -> ((poly_failwith_63)@(L("TEST_SIGN"))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>chr =
  fun _n -> ((poly_failwith_62)@(L("TEST_CHR")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>nl =
  L("NEWLINE")[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>println =
  fun _v -> ((poly_failwith_61)@(L("TEST_PRINTLN")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>print =
  fun _v -> ((poly_failwith_61)@(L("TEST_PRINT")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>eprint =
  fun _v -> ((poly_failwith_61)@(L("TEST_EPRINTL")))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>balance =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>amount =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>now =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>sender =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>source =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>level =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>self_address =
  SELF_ADDRESS()[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>chain_id =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>get_now =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>get_self_address =
  fun _u -> (SELF_ADDRESS())[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>voting_power =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>implicit_account =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>pairing_check =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#22>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bitwise#23>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bitwise#23>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bitwise#23>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/F.mligo#0><String#28>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/F.mligo#0><String#28>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/F.mligo#0><String#28>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bytes#30>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bytes#30>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bytes#30>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#31>blake2b =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#31>sha256 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#31>sha512 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#31>sha3 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#31>keccak =
  fun b -> (({ KECCAK })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#31>hash_key =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let <../../test/contracts/build/F.mligo#0><Crypto#31>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let <../../test/contracts/build/F.mligo#0>assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let <../../test/contracts/build/F.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/F.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/F.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/F.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/F.mligo#0>unit = UNIT()[@inline] in
let <../../test/contracts/build/F.mligo#0>toto = L(44) in
let <../../test/contracts/build/G.mligo#0><Tezos#0>balance =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>amount =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>now =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>sender =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>source =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>level =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>self_address =
  SELF_ADDRESS()[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>chain_id =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>get_now =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>get_self_address =
  fun _u -> (SELF_ADDRESS())[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>min_block_time =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>get_min_block_time =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>voting_power =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>implicit_account =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>pairing_check =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bitwise#0>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bitwise#0>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bitwise#0>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/G.mligo#0><String#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/G.mligo#0><String#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/G.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bytes#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bytes#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bytes#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#0>blake2b =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#0>sha256 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#0>sha512 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#0>sha3 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#0>keccak =
  fun b -> (({ KECCAK })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#0>hash_key =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#0>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let <../../test/contracts/build/G.mligo#0>assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let <../../test/contracts/build/G.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/G.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/G.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/G.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/G.mligo#0>unit = UNIT()[@inline] in
let poly_failwith_60 = { FAILWITH }[@inline] in
let poly_failwith_59 = { FAILWITH }[@inline] in
let poly_failwith_58 = { FAILWITH }[@inline] in
let poly_failwith_57 = { FAILWITH }[@inline] in
let poly_failwith_56 = { FAILWITH }[@inline] in
let poly_failwith_55 = { FAILWITH }[@inline] in
let poly_failwith_54 = { FAILWITH }[@inline] in
let poly_failwith_53 = { FAILWITH }[@inline] in
let poly_failwith_52 = { FAILWITH }[@inline] in
let poly_failwith_51 = { FAILWITH }[@inline] in
let poly_failwith_50 = { FAILWITH }[@inline] in
let poly_failwith_49 = { FAILWITH }[@inline] in
let poly_failwith_48 = { FAILWITH }[@inline] in
let poly_failwith_47 = { FAILWITH }[@inline] in
let poly_failwith_46 = { FAILWITH }[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>originate_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_failwith_60)@(L("TEST MODE")))))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>set_source =
  fun _a -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>set_baker =
  fun _a -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>set_baker_policy =
  fun _bp -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_46)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_57)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>get_storage_of_address =
  fun _a -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>get_balance =
  fun _a -> ((poly_failwith_59)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_failwith_58)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_failwith_46)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_failwith_46)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>get_voting_power =
  fun _kh -> ((poly_failwith_57)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>get_total_voting_power =
  (poly_failwith_57)@(L("TEST MODE"))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_failwith_51)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>nth_bootstrap_account =
  fun _i -> ((poly_failwith_51)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>last_originations =
  fun _u -> ((poly_failwith_56)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_failwith_47)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>add_account =
  fun _s -> (fun _k -> ((poly_failwith_46)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>new_account =
  fun _u -> ((poly_failwith_55)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_failwith_46)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>register_delegate =
  fun _kh -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>register_constant =
  fun _m -> ((poly_failwith_54)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_failwith_53)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_failwith_52)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>constant_to_michelson_program =
  fun _s -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>restore_context =
  fun _u -> ((poly_failwith_46)@(L("TEST_POP_CONTEXT")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>save_context =
  fun _u -> ((poly_failwith_46)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>drop_context =
  fun _u -> ((poly_failwith_46)@(L("TEST_DROP_CONTEXT")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>read_contract_from_file =
  fun _fn -> ((poly_failwith_46)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>compile_contract_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_failwith_46)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>originate_contract =
  fun _c ->
  (fun _s -> (fun _t -> ((poly_failwith_51)@(L("TEST_ORIGINATE")))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>size =
  fun _c -> ((poly_failwith_50)@(L("TEST_SIZE")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>get_bootstrap_account =
  fun _n -> ((poly_failwith_49)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>sign =
  fun _sk -> (fun _d -> ((poly_failwith_48)@(L("TEST_SIGN"))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>chr =
  fun _n -> ((poly_failwith_47)@(L("TEST_CHR")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>nl =
  L("NEWLINE")[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>println =
  fun _v -> ((poly_failwith_46)@(L("TEST_PRINTLN")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>print =
  fun _v -> ((poly_failwith_46)@(L("TEST_PRINT")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>eprint =
  fun _v -> ((poly_failwith_46)@(L("TEST_EPRINTL")))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>balance =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>amount =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>now =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>sender =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>source =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>level =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>self_address =
  SELF_ADDRESS()[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>chain_id =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>get_now =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>get_self_address =
  fun _u -> (SELF_ADDRESS())[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>voting_power =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>implicit_account =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>pairing_check =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#32>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bitwise#33>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bitwise#33>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bitwise#33>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/G.mligo#0><String#38>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/G.mligo#0><String#38>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/G.mligo#0><String#38>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bytes#40>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bytes#40>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bytes#40>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#41>blake2b =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#41>sha256 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#41>sha512 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#41>sha3 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#41>keccak =
  fun b -> (({ KECCAK })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#41>hash_key =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let <../../test/contracts/build/G.mligo#0><Crypto#41>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let <../../test/contracts/build/G.mligo#0>assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let <../../test/contracts/build/G.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/G.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/G.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/G.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/G.mligo#0>unit = UNIT()[@inline] in
let <../../test/contracts/build/G.mligo#0>toto = L(43) in
let <../../test/contracts/build/C.mligo#0><Tezos#0>balance =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>amount =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>now =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>sender =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>source =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>level =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>self_address =
  SELF_ADDRESS()[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>chain_id =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>get_now =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>get_self_address =
  fun _u -> (SELF_ADDRESS())[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>min_block_time =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>get_min_block_time =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>voting_power =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>implicit_account =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>pairing_check =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bitwise#0>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bitwise#0>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bitwise#0>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/C.mligo#0><String#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/C.mligo#0><String#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/C.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bytes#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bytes#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bytes#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#0>blake2b =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#0>sha256 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#0>sha512 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#0>sha3 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#0>keccak =
  fun b -> (({ KECCAK })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#0>hash_key =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#0>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let <../../test/contracts/build/C.mligo#0>assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let <../../test/contracts/build/C.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/C.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/C.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/C.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/C.mligo#0>unit = UNIT()[@inline] in
let poly_failwith_45 = { FAILWITH }[@inline] in
let poly_failwith_44 = { FAILWITH }[@inline] in
let poly_failwith_43 = { FAILWITH }[@inline] in
let poly_failwith_42 = { FAILWITH }[@inline] in
let poly_failwith_41 = { FAILWITH }[@inline] in
let poly_failwith_40 = { FAILWITH }[@inline] in
let poly_failwith_39 = { FAILWITH }[@inline] in
let poly_failwith_38 = { FAILWITH }[@inline] in
let poly_failwith_37 = { FAILWITH }[@inline] in
let poly_failwith_36 = { FAILWITH }[@inline] in
let poly_failwith_35 = { FAILWITH }[@inline] in
let poly_failwith_34 = { FAILWITH }[@inline] in
let poly_failwith_33 = { FAILWITH }[@inline] in
let poly_failwith_32 = { FAILWITH }[@inline] in
let poly_failwith_31 = { FAILWITH }[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>originate_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_failwith_45)@(L("TEST MODE")))))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>set_source =
  fun _a -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>set_baker =
  fun _a -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>set_baker_policy =
  fun _bp -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_31)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_42)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>get_storage_of_address =
  fun _a -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>get_balance =
  fun _a -> ((poly_failwith_44)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_failwith_43)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_failwith_31)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_failwith_31)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>get_voting_power =
  fun _kh -> ((poly_failwith_42)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>get_total_voting_power =
  (poly_failwith_42)@(L("TEST MODE"))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_failwith_36)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>nth_bootstrap_account =
  fun _i -> ((poly_failwith_36)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>last_originations =
  fun _u -> ((poly_failwith_41)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_failwith_32)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>add_account =
  fun _s -> (fun _k -> ((poly_failwith_31)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>new_account =
  fun _u -> ((poly_failwith_40)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_failwith_31)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>register_delegate =
  fun _kh -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>register_constant =
  fun _m -> ((poly_failwith_39)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_failwith_38)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_failwith_37)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>constant_to_michelson_program =
  fun _s -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>restore_context =
  fun _u -> ((poly_failwith_31)@(L("TEST_POP_CONTEXT")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>save_context =
  fun _u -> ((poly_failwith_31)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>drop_context =
  fun _u -> ((poly_failwith_31)@(L("TEST_DROP_CONTEXT")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>read_contract_from_file =
  fun _fn -> ((poly_failwith_31)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>compile_contract_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_failwith_31)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>originate_contract =
  fun _c ->
  (fun _s -> (fun _t -> ((poly_failwith_36)@(L("TEST_ORIGINATE")))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>size =
  fun _c -> ((poly_failwith_35)@(L("TEST_SIZE")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>get_bootstrap_account =
  fun _n -> ((poly_failwith_34)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>sign =
  fun _sk -> (fun _d -> ((poly_failwith_33)@(L("TEST_SIGN"))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>chr =
  fun _n -> ((poly_failwith_32)@(L("TEST_CHR")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>nl =
  L("NEWLINE")[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>println =
  fun _v -> ((poly_failwith_31)@(L("TEST_PRINTLN")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>print =
  fun _v -> ((poly_failwith_31)@(L("TEST_PRINT")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>eprint =
  fun _v -> ((poly_failwith_31)@(L("TEST_EPRINTL")))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>balance =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>amount =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>now =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>sender =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>source =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>level =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>self_address =
  SELF_ADDRESS()[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>chain_id =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>get_now =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>get_self_address =
  fun _u -> (SELF_ADDRESS())[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>voting_power =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>implicit_account =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>pairing_check =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#42>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bitwise#43>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bitwise#43>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bitwise#43>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/C.mligo#0><String#48>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/C.mligo#0><String#48>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/C.mligo#0><String#48>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bytes#50>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bytes#50>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bytes#50>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#51>blake2b =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#51>sha256 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#51>sha512 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#51>sha3 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#51>keccak =
  fun b -> (({ KECCAK })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#51>hash_key =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let <../../test/contracts/build/C.mligo#0><Crypto#51>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let <../../test/contracts/build/C.mligo#0>assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let <../../test/contracts/build/C.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let <../../test/contracts/build/C.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/C.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/C.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/C.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/C.mligo#0>unit = UNIT()[@inline] in
let <../../test/contracts/build/C.mligo#0>tata =
  ADD(<../../test/contracts/build/A.mligo#0>toto ,
      <../../test/contracts/build/B.mligo#0>titi) in
let <../../test/contracts/build/C.mligo#0>foo =
  (<../../test/contracts/build/B.mligo#0>f)@(PAIR(L(unit) , L(3))) in
let <../../test/contracts/build/E.mligo#0><Tezos#0>balance =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>amount =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>now =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>sender =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>source =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>level =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>self_address =
  SELF_ADDRESS()[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>chain_id =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>get_now =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>get_self_address =
  fun _u -> (SELF_ADDRESS())[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>min_block_time =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>get_min_block_time =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>voting_power =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>implicit_account =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>pairing_check =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bitwise#0>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bitwise#0>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bitwise#0>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/E.mligo#0><String#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/E.mligo#0><String#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/E.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bytes#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bytes#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bytes#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#0>blake2b =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#0>sha256 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#0>sha512 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#0>sha3 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#0>keccak =
  fun b -> (({ KECCAK })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#0>hash_key =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#0>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let <../../test/contracts/build/E.mligo#0>assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let <../../test/contracts/build/E.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/E.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/E.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/E.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/E.mligo#0>unit = UNIT()[@inline] in
let poly_failwith_30 = { FAILWITH }[@inline] in
let poly_failwith_29 = { FAILWITH }[@inline] in
let poly_failwith_28 = { FAILWITH }[@inline] in
let poly_failwith_27 = { FAILWITH }[@inline] in
let poly_failwith_26 = { FAILWITH }[@inline] in
let poly_failwith_25 = { FAILWITH }[@inline] in
let poly_failwith_24 = { FAILWITH }[@inline] in
let poly_failwith_23 = { FAILWITH }[@inline] in
let poly_failwith_22 = { FAILWITH }[@inline] in
let poly_failwith_21 = { FAILWITH }[@inline] in
let poly_failwith_20 = { FAILWITH }[@inline] in
let poly_failwith_19 = { FAILWITH }[@inline] in
let poly_failwith_18 = { FAILWITH }[@inline] in
let poly_failwith_17 = { FAILWITH }[@inline] in
let poly_failwith_16 = { FAILWITH }[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>originate_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_failwith_30)@(L("TEST MODE")))))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>set_source =
  fun _a -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>set_baker =
  fun _a -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>set_baker_policy =
  fun _bp -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_16)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_27)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>get_storage_of_address =
  fun _a -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>get_balance =
  fun _a -> ((poly_failwith_29)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_failwith_28)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_failwith_16)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_failwith_16)@(L("TEST MODE")))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>get_voting_power =
  fun _kh -> ((poly_failwith_27)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>get_total_voting_power =
  (poly_failwith_27)@(L("TEST MODE"))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_failwith_21)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>nth_bootstrap_account =
  fun _i -> ((poly_failwith_21)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>last_originations =
  fun _u -> ((poly_failwith_26)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_failwith_17)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>add_account =
  fun _s -> (fun _k -> ((poly_failwith_16)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>new_account =
  fun _u -> ((poly_failwith_25)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_failwith_16)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>register_delegate =
  fun _kh -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>register_constant =
  fun _m -> ((poly_failwith_24)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_failwith_23)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_failwith_22)@(L("TEST MODE"))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>constant_to_michelson_program =
  fun _s -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>restore_context =
  fun _u -> ((poly_failwith_16)@(L("TEST_POP_CONTEXT")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>save_context =
  fun _u -> ((poly_failwith_16)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>drop_context =
  fun _u -> ((poly_failwith_16)@(L("TEST_DROP_CONTEXT")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>read_contract_from_file =
  fun _fn -> ((poly_failwith_16)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>compile_contract_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_failwith_16)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>originate_contract =
  fun _c ->
  (fun _s -> (fun _t -> ((poly_failwith_21)@(L("TEST_ORIGINATE")))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>size =
  fun _c -> ((poly_failwith_20)@(L("TEST_SIZE")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>get_bootstrap_account =
  fun _n -> ((poly_failwith_19)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>sign =
  fun _sk -> (fun _d -> ((poly_failwith_18)@(L("TEST_SIGN"))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>chr =
  fun _n -> ((poly_failwith_17)@(L("TEST_CHR")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>nl =
  L("NEWLINE")[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>println =
  fun _v -> ((poly_failwith_16)@(L("TEST_PRINTLN")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>print =
  fun _v -> ((poly_failwith_16)@(L("TEST_PRINT")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>eprint =
  fun _v -> ((poly_failwith_16)@(L("TEST_EPRINTL")))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>balance =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>amount =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>now =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>sender =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>source =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>level =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>self_address =
  SELF_ADDRESS()[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>chain_id =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>get_now =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>get_self_address =
  fun _u -> (SELF_ADDRESS())[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>voting_power =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>implicit_account =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>pairing_check =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#52>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bitwise#53>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bitwise#53>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bitwise#53>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/E.mligo#0><String#58>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/E.mligo#0><String#58>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/E.mligo#0><String#58>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bytes#60>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bytes#60>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bytes#60>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#61>blake2b =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#61>sha256 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#61>sha512 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#61>sha3 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#61>keccak =
  fun b -> (({ KECCAK })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#61>hash_key =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let <../../test/contracts/build/E.mligo#0><Crypto#61>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let <../../test/contracts/build/E.mligo#0>assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let <../../test/contracts/build/E.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/E.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/E.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/E.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/E.mligo#0>unit = UNIT()[@inline] in
let <../../test/contracts/build/E.mligo#0>toto = L(10) in
let <../../test/contracts/build/E.mligo#0>foo = L("bar") in
let <Tezos#0>balance = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <Tezos#0>amount = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <Tezos#0>now = ({ DROP ; NOW })@(L(unit))[@inline] in
let <Tezos#0>sender = ({ DROP ; SENDER })@(L(unit))[@inline] in
let <Tezos#0>source = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <Tezos#0>level = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <Tezos#0>self_address = SELF_ADDRESS()[@inline] in
let <Tezos#0>chain_id = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <Tezos#0>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <Tezos#0>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <Tezos#0>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <Tezos#0>get_now = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <Tezos#0>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <Tezos#0>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <Tezos#0>get_level = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <Tezos#0>get_self_address = fun _u -> (SELF_ADDRESS())[@inline] in
let <Tezos#0>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <Tezos#0>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <Tezos#0>min_block_time = { DROP ; MIN_BLOCK_TIME }[@inline] in
let <Tezos#0>get_min_block_time = { DROP ; MIN_BLOCK_TIME }[@inline] in
let <Tezos#0>voting_power = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <Tezos#0>implicit_account = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <Tezos#0>pairing_check = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <Tezos#0>set_delegate = fun o -> (SET_DELEGATE(o))[@inline] in
let <Bitwise#0>xor = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <Bitwise#0>shift_left = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <Bitwise#0>shift_right = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <String#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <String#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <String#0>length = fun b -> (({ SIZE })@(b))[@inline] in
let <Bytes#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <Bytes#0>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <Bytes#0>length = fun b -> (({ SIZE })@(b))[@inline] in
let <Crypto#0>blake2b = fun b -> (({ BLAKE2B })@(b))[@inline] in
let <Crypto#0>sha256 = fun b -> (({ SHA256 })@(b))[@inline] in
let <Crypto#0>sha512 = fun b -> (({ SHA512 })@(b))[@inline] in
let <Crypto#0>sha3 = fun b -> (({ SHA3 })@(b))[@inline] in
let <Crypto#0>keccak = fun b -> (({ KECCAK })@(b))[@inline] in
let <Crypto#0>hash_key = fun k -> (({ HASH_KEY })@(k))[@inline] in
let <Crypto#0>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let abs = fun i -> (({ ABS })@(i))[@inline] in
let is_nat = fun i -> (({ ISNAT })@(i))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let poly_failwith_15 = { FAILWITH }[@inline] in
let poly_failwith_14 = { FAILWITH }[@inline] in
let poly_failwith_13 = { FAILWITH }[@inline] in
let poly_failwith_12 = { FAILWITH }[@inline] in
let poly_failwith_11 = { FAILWITH }[@inline] in
let poly_failwith_10 = { FAILWITH }[@inline] in
let poly_failwith_9 = { FAILWITH }[@inline] in
let poly_failwith_8 = { FAILWITH }[@inline] in
let poly_failwith_7 = { FAILWITH }[@inline] in
let poly_failwith_6 = { FAILWITH }[@inline] in
let poly_failwith_5 = { FAILWITH }[@inline] in
let poly_failwith_4 = { FAILWITH }[@inline] in
let poly_failwith_3 = { FAILWITH }[@inline] in
let poly_failwith_2 = { FAILWITH }[@inline] in
let poly_failwith_1 = { FAILWITH }[@inline] in
let <Test#0>originate_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_failwith_15)@(L("TEST MODE")))))))[@inline] in
let <Test#0>set_source =
  fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let <Test#0>set_baker =
  fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let <Test#0>set_baker_policy =
  fun _bp -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let <Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_1)@(L("TEST MODE")))))[@inline] in
let <Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_12)@(L("TEST MODE")))))[@inline] in
let <Test#0>get_storage_of_address =
  fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let <Test#0>get_balance =
  fun _a -> ((poly_failwith_14)@(L("TEST MODE")))[@inline] in
let <Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_failwith_13)@(L("TEST MODE"))))[@inline] in
let <Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_failwith_1)@(L("TEST MODE"))))[@inline] in
let <Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_failwith_1)@(L("TEST MODE")))))[@inline] in
let <Test#0>get_voting_power =
  fun _kh -> ((poly_failwith_12)@(L("TEST MODE")))[@inline] in
let <Test#0>get_total_voting_power =
  (poly_failwith_12)@(L("TEST MODE"))[@inline] in
let <Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
let <Test#0>nth_bootstrap_account =
  fun _i -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
let <Test#0>last_originations =
  fun _u -> ((poly_failwith_11)@(L("TEST MODE")))[@inline] in
let <Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_failwith_2)@(L("TEST MODE"))))[@inline] in
let <Test#0>add_account =
  fun _s -> (fun _k -> ((poly_failwith_1)@(L("TEST MODE"))))[@inline] in
let <Test#0>new_account =
  fun _u -> ((poly_failwith_10)@(L("TEST MODE")))[@inline] in
let <Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_failwith_1)@(L("TEST MODE"))))[@inline] in
let <Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let <Test#0>register_delegate =
  fun _kh -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let <Test#0>register_constant =
  fun _m -> ((poly_failwith_9)@(L("TEST MODE")))[@inline] in
let <Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_failwith_8)@(L("TEST MODE"))))[@inline] in
let <Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_failwith_7)@(L("TEST MODE"))))[@inline] in
let <Test#0>constant_to_michelson_program =
  fun _s -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let <Test#0>restore_context =
  fun _u -> ((poly_failwith_1)@(L("TEST_POP_CONTEXT")))[@inline] in
let <Test#0>save_context =
  fun _u -> ((poly_failwith_1)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let <Test#0>drop_context =
  fun _u -> ((poly_failwith_1)@(L("TEST_DROP_CONTEXT")))[@inline] in
let <Test#0>read_contract_from_file =
  fun _fn -> ((poly_failwith_1)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let <Test#0>compile_contract_from_file =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_failwith_1)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let <Test#0>originate_contract =
  fun _c -> (fun _s -> (fun _t -> ((poly_failwith_6)@(L("TEST_ORIGINATE")))))[@inline] in
let <Test#0>size = fun _c -> ((poly_failwith_5)@(L("TEST_SIZE")))[@inline] in
let <Test#0>get_bootstrap_account =
  fun _n -> ((poly_failwith_4)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let <Test#0>sign =
  fun _sk -> (fun _d -> ((poly_failwith_3)@(L("TEST_SIGN"))))[@inline] in
let <Test#0>chr = fun _n -> ((poly_failwith_2)@(L("TEST_CHR")))[@inline] in
let <Test#0>nl = L("NEWLINE")[@inline] in
let <Test#0>println =
  fun _v -> ((poly_failwith_1)@(L("TEST_PRINTLN")))[@inline] in
let <Test#0>print =
  fun _v -> ((poly_failwith_1)@(L("TEST_PRINT")))[@inline] in
let <Test#0>eprint =
  fun _v -> ((poly_failwith_1)@(L("TEST_EPRINTL")))[@inline] in
let <Tezos#62>balance = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let <Tezos#62>amount = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let <Tezos#62>now = ({ DROP ; NOW })@(L(unit))[@inline] in
let <Tezos#62>sender = ({ DROP ; SENDER })@(L(unit))[@inline] in
let <Tezos#62>source = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let <Tezos#62>level = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let <Tezos#62>self_address = SELF_ADDRESS()[@inline] in
let <Tezos#62>chain_id = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let <Tezos#62>total_voting_power =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let <Tezos#62>get_balance =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let <Tezos#62>get_amount =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let <Tezos#62>get_now = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let <Tezos#62>get_sender =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let <Tezos#62>get_source =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let <Tezos#62>get_level =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let <Tezos#62>get_self_address = fun _u -> (SELF_ADDRESS())[@inline] in
let <Tezos#62>get_chain_id =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let <Tezos#62>get_total_voting_power =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let <Tezos#62>voting_power = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let <Tezos#62>implicit_account = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let <Tezos#62>pairing_check = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let <Tezos#62>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <Tezos#62>set_delegate = fun o -> (SET_DELEGATE(o))[@inline] in
let <Bitwise#63>xor = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <Bitwise#63>shift_left = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <Bitwise#63>shift_right = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <String#68>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <String#68>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <String#68>length = fun b -> (({ SIZE })@(b))[@inline] in
let <Bytes#70>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <Bytes#70>sub =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let <Bytes#70>length = fun b -> (({ SIZE })@(b))[@inline] in
let <Crypto#71>blake2b = fun b -> (({ BLAKE2B })@(b))[@inline] in
let <Crypto#71>sha256 = fun b -> (({ SHA256 })@(b))[@inline] in
let <Crypto#71>sha512 = fun b -> (({ SHA512 })@(b))[@inline] in
let <Crypto#71>sha3 = fun b -> (({ SHA3 })@(b))[@inline] in
let <Crypto#71>keccak = fun b -> (({ KECCAK })@(b))[@inline] in
let <Crypto#71>hash_key = fun k -> (({ HASH_KEY })@(k))[@inline] in
let <Crypto#71>check =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let assert =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let abs = fun i -> (({ ABS })@(i))[@inline] in
let is_nat = fun i -> (({ ISNAT })@(i))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let toto =
  ADD(<../../test/contracts/build/E.mligo#0>toto ,
      <../../test/contracts/build/A.mligo#0>toto) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#5127 ->
  (let (gen#5133, gen#5134) = gen#5127 in
   let p = gen#5133 in
   let s = gen#5134 in
   let s = ADD(ADD(p , s) , toto) in PAIR(LIST_EMPTY() , s)) in
L(unit) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "D.mligo" ] ;
  [%expect{|
    { parameter int ;
      storage int ;
      code { PUSH int 1 ;
             PUSH int 10 ;
             ADD ;
             SWAP ;
             UNPAIR ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    Dependency cycle detected :
     `-- 3 -- ../../test/contracts/build/cycle_A.mligo
        `-- 2 -- ../../test/contracts/build/cycle_B.mligo
            `-- 1 -- ../../test/contracts/build/cycle_C.mligo
                `-- 3 -- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "type_B.mligo" ] ;
  [%expect {|
    File "../../test/contracts/build/type_B.mligo", line 5, characters 5-6:
      4 | 	let s = s + 1 in
      5 | 	let p = p ^ "titi" in
      6 | 	([] : operation list), s
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    { parameter string ;
      storage int ;
      code { CDR ; PUSH int 1 ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "tata" ; "--init-file" ; contract "C.mligo" ] ;
  [%expect {| 44 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ;  contract "C1.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; contract "C_test.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "Xmain.mligo" ] ;
  [%expect {|
    { 1 ; 2 ; 3 } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "Xmain.mligo"; "--format" ; "dev" ] ;
  [%expect {|
    `-- 4 -- ../../test/contracts/build/Xmain.mligo
        |-- 3 -- ../../test/contracts/build/Xfoo.mligo
        |   |-- 1 -- ../../test/contracts/build/Xlist.mligo
        |   `-- 2 -- ../../test/contracts/build/Xset.mligo
        `-- 1 -- ../../test/contracts/build/Xlist.mligo |}]

let%expect_test _ =
  run_ligo_bad ["run"; "interpret"; "--init-file"; contract "module_scoping_bug.mligo" ; "x"; ] ;
  [%expect {|
    File "../../test/contracts/build/module_scoping_bug.mligo", line 24, characters 10-11:
     23 |
     24 | let x = B.A.a

    Module "A" not found. |}]
