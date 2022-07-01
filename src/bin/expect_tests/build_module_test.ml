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
let <../../test/contracts/build/A.mligo#0><Tezos#0>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/A.mligo#0><Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bitwise#0>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bitwise#0>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bitwise#0>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/A.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
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
let <../../test/contracts/build/A.mligo#0><Bytes#0>length =
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
let <../../test/contracts/build/A.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/A.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/A.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/A.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/A.mligo#0>unit = UNIT()[@inline] in
let <../../test/contracts/build/A.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let poly_stub_105 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_104 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_103 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_102 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_101 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_100 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_99 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_98 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_97 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_96 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_95 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_94 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_93 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_92 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_91 = fun x -> (({ FAILWITH })@(x))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>get_total_voting_power =
  (poly_stub_99)@(L(unit))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>set_source =
  fun _a -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>get_storage_of_address =
  fun _a -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>get_balance =
  fun _a -> ((poly_stub_105)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>print =
  fun _v -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>eprint =
  fun _v -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>get_voting_power =
  fun _kh -> ((poly_stub_99)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_stub_93)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>nth_bootstrap_account =
  fun _i -> ((poly_stub_93)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>get_bootstrap_account =
  fun _n -> ((poly_stub_104)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>last_originations =
  fun _u -> ((poly_stub_103)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>new_account =
  fun _u -> ((poly_stub_102)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>register_delegate =
  fun _kh -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>register_constant =
  fun _m -> ((poly_stub_101)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>constant_to_michelson_program =
  fun _s -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>restore_context =
  fun _u -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>save_context =
  fun _u -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>drop_context =
  fun _u -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>set_baker_policy =
  fun _bp -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>set_baker =
  fun _a -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>size =
  fun _c -> ((poly_stub_100)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>read_contract_from_file =
  fun _fn -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>chr =
  fun _n -> ((poly_stub_98)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>nl =
  L("NEWLINE")[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>println =
  fun _v -> ((poly_stub_92)@(L(unit)))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_92)@(L(unit)))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_99)@(L(unit)))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_stub_92)@(L(unit))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_stub_92)@(L(unit)))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_stub_98)@(L(unit))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>sign =
  fun _sk -> (fun _d -> ((poly_stub_97)@(L(unit))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>add_account =
  fun _s -> (fun _k -> ((poly_stub_92)@(L(unit))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_stub_92)@(L(unit))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_stub_96)@(L(unit))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_stub_95)@(L(unit))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_stub_94)@(L(unit))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>originate_contract =
  fun _c -> (fun _s -> (fun _t -> ((poly_stub_93)@(L(unit)))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>compile_contract_from_file =
  fun _fn -> (fun _e -> (fun _v -> ((poly_stub_92)@(L(unit)))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Test#0>originate_from_file =
  fun _fn ->
  (fun _e -> (fun _v -> (fun _s -> (fun _t -> ((poly_stub_91)@(L(unit)))))))[@inline] in
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
let <../../test/contracts/build/B.mligo#0><Tezos#0>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/B.mligo#0><Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bitwise#0>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bitwise#0>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Bitwise#0>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/B.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
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
let <../../test/contracts/build/B.mligo#0><Bytes#0>length =
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
let <../../test/contracts/build/B.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/B.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/B.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/B.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/B.mligo#0>unit = UNIT()[@inline] in
let <../../test/contracts/build/B.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let poly_stub_90 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_89 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_88 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_87 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_86 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_85 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_84 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_83 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_82 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_81 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_80 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_79 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_78 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_77 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_76 = fun x -> (({ FAILWITH })@(x))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>get_total_voting_power =
  (poly_stub_84)@(L(unit))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>set_source =
  fun _a -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>get_storage_of_address =
  fun _a -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>get_balance =
  fun _a -> ((poly_stub_90)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>print =
  fun _v -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>eprint =
  fun _v -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>get_voting_power =
  fun _kh -> ((poly_stub_84)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_stub_78)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>nth_bootstrap_account =
  fun _i -> ((poly_stub_78)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>get_bootstrap_account =
  fun _n -> ((poly_stub_89)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>last_originations =
  fun _u -> ((poly_stub_88)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>new_account =
  fun _u -> ((poly_stub_87)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>register_delegate =
  fun _kh -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>register_constant =
  fun _m -> ((poly_stub_86)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>constant_to_michelson_program =
  fun _s -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>restore_context =
  fun _u -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>save_context =
  fun _u -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>drop_context =
  fun _u -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>set_baker_policy =
  fun _bp -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>set_baker =
  fun _a -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>size =
  fun _c -> ((poly_stub_85)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>read_contract_from_file =
  fun _fn -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>chr =
  fun _n -> ((poly_stub_83)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>nl =
  L("NEWLINE")[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>println =
  fun _v -> ((poly_stub_77)@(L(unit)))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_77)@(L(unit)))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_84)@(L(unit)))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_stub_77)@(L(unit))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_stub_77)@(L(unit)))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_stub_83)@(L(unit))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>sign =
  fun _sk -> (fun _d -> ((poly_stub_82)@(L(unit))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>add_account =
  fun _s -> (fun _k -> ((poly_stub_77)@(L(unit))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_stub_77)@(L(unit))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_stub_81)@(L(unit))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_stub_80)@(L(unit))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_stub_79)@(L(unit))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>originate_contract =
  fun _c -> (fun _s -> (fun _t -> ((poly_stub_78)@(L(unit)))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>compile_contract_from_file =
  fun _fn -> (fun _e -> (fun _v -> ((poly_stub_77)@(L(unit)))))[@inline] in
let <../../test/contracts/build/B.mligo#0><Test#0>originate_from_file =
  fun _fn ->
  (fun _e -> (fun _v -> (fun _s -> (fun _t -> ((poly_stub_76)@(L(unit)))))))[@inline] in
let <../../test/contracts/build/B.mligo#0>toto = L(32) in
let <../../test/contracts/build/B.mligo#0>titi =
  ADD(<../../test/contracts/build/A.mligo#0>toto , L(42)) in
let <../../test/contracts/build/B.mligo#0>f =
  fun gen#937 ->
  (let (gen#3164, gen#3165) = gen#937 in
   let gen#938 = gen#3164 in
   let x = gen#3165 in
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
let <../../test/contracts/build/F.mligo#0><Tezos#0>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/F.mligo#0><Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bitwise#0>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bitwise#0>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bitwise#0>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/F.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
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
let <../../test/contracts/build/F.mligo#0><Bytes#0>length =
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
let <../../test/contracts/build/F.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/F.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/F.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/F.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/F.mligo#0>unit = UNIT()[@inline] in
let <../../test/contracts/build/F.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let poly_stub_75 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_74 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_73 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_72 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_71 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_70 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_69 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_68 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_67 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_66 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_65 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_64 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_63 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_62 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_61 = fun x -> (({ FAILWITH })@(x))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>get_total_voting_power =
  (poly_stub_69)@(L(unit))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>set_source =
  fun _a -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>get_storage_of_address =
  fun _a -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>get_balance =
  fun _a -> ((poly_stub_75)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>print =
  fun _v -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>eprint =
  fun _v -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>get_voting_power =
  fun _kh -> ((poly_stub_69)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_stub_63)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>nth_bootstrap_account =
  fun _i -> ((poly_stub_63)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>get_bootstrap_account =
  fun _n -> ((poly_stub_74)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>last_originations =
  fun _u -> ((poly_stub_73)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>new_account =
  fun _u -> ((poly_stub_72)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>register_delegate =
  fun _kh -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>register_constant =
  fun _m -> ((poly_stub_71)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>constant_to_michelson_program =
  fun _s -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>restore_context =
  fun _u -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>save_context =
  fun _u -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>drop_context =
  fun _u -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>set_baker_policy =
  fun _bp -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>set_baker =
  fun _a -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>size =
  fun _c -> ((poly_stub_70)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>read_contract_from_file =
  fun _fn -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>chr =
  fun _n -> ((poly_stub_68)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>nl =
  L("NEWLINE")[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>println =
  fun _v -> ((poly_stub_62)@(L(unit)))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_62)@(L(unit)))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_69)@(L(unit)))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_stub_62)@(L(unit))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_stub_62)@(L(unit)))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_stub_68)@(L(unit))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>sign =
  fun _sk -> (fun _d -> ((poly_stub_67)@(L(unit))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>add_account =
  fun _s -> (fun _k -> ((poly_stub_62)@(L(unit))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_stub_62)@(L(unit))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_stub_66)@(L(unit))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_stub_65)@(L(unit))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_stub_64)@(L(unit))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>originate_contract =
  fun _c -> (fun _s -> (fun _t -> ((poly_stub_63)@(L(unit)))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>compile_contract_from_file =
  fun _fn -> (fun _e -> (fun _v -> ((poly_stub_62)@(L(unit)))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Test#0>originate_from_file =
  fun _fn ->
  (fun _e -> (fun _v -> (fun _s -> (fun _t -> ((poly_stub_61)@(L(unit)))))))[@inline] in
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
let <../../test/contracts/build/G.mligo#0><Tezos#0>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/G.mligo#0><Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bitwise#0>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bitwise#0>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bitwise#0>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/G.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
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
let <../../test/contracts/build/G.mligo#0><Bytes#0>length =
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
let <../../test/contracts/build/G.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/G.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/G.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/G.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/G.mligo#0>unit = UNIT()[@inline] in
let <../../test/contracts/build/G.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let poly_stub_60 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_59 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_58 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_57 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_56 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_55 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_54 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_53 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_52 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_51 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_50 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_49 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_48 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_47 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_46 = fun x -> (({ FAILWITH })@(x))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>get_total_voting_power =
  (poly_stub_54)@(L(unit))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>set_source =
  fun _a -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>get_storage_of_address =
  fun _a -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>get_balance =
  fun _a -> ((poly_stub_60)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>print =
  fun _v -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>eprint =
  fun _v -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>get_voting_power =
  fun _kh -> ((poly_stub_54)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_stub_48)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>nth_bootstrap_account =
  fun _i -> ((poly_stub_48)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>get_bootstrap_account =
  fun _n -> ((poly_stub_59)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>last_originations =
  fun _u -> ((poly_stub_58)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>new_account =
  fun _u -> ((poly_stub_57)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>register_delegate =
  fun _kh -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>register_constant =
  fun _m -> ((poly_stub_56)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>constant_to_michelson_program =
  fun _s -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>restore_context =
  fun _u -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>save_context =
  fun _u -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>drop_context =
  fun _u -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>set_baker_policy =
  fun _bp -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>set_baker =
  fun _a -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>size =
  fun _c -> ((poly_stub_55)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>read_contract_from_file =
  fun _fn -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>chr =
  fun _n -> ((poly_stub_53)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>nl =
  L("NEWLINE")[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>println =
  fun _v -> ((poly_stub_47)@(L(unit)))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_47)@(L(unit)))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_54)@(L(unit)))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_stub_47)@(L(unit))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_stub_47)@(L(unit)))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_stub_53)@(L(unit))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>sign =
  fun _sk -> (fun _d -> ((poly_stub_52)@(L(unit))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>add_account =
  fun _s -> (fun _k -> ((poly_stub_47)@(L(unit))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_stub_47)@(L(unit))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_stub_51)@(L(unit))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_stub_50)@(L(unit))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_stub_49)@(L(unit))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>originate_contract =
  fun _c -> (fun _s -> (fun _t -> ((poly_stub_48)@(L(unit)))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>compile_contract_from_file =
  fun _fn -> (fun _e -> (fun _v -> ((poly_stub_47)@(L(unit)))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Test#0>originate_from_file =
  fun _fn ->
  (fun _e -> (fun _v -> (fun _s -> (fun _t -> ((poly_stub_46)@(L(unit)))))))[@inline] in
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
let <../../test/contracts/build/C.mligo#0><Tezos#0>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/C.mligo#0><Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bitwise#0>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bitwise#0>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Bitwise#0>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/C.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
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
let <../../test/contracts/build/C.mligo#0><Bytes#0>length =
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
let <../../test/contracts/build/C.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/C.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/C.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/C.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/C.mligo#0>unit = UNIT()[@inline] in
let <../../test/contracts/build/C.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let poly_stub_45 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_44 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_43 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_42 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_41 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_40 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_39 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_38 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_37 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_36 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_35 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_34 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_33 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_32 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_31 = fun x -> (({ FAILWITH })@(x))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>get_total_voting_power =
  (poly_stub_39)@(L(unit))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>set_source =
  fun _a -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>get_storage_of_address =
  fun _a -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>get_balance =
  fun _a -> ((poly_stub_45)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>print =
  fun _v -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>eprint =
  fun _v -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>get_voting_power =
  fun _kh -> ((poly_stub_39)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_stub_33)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>nth_bootstrap_account =
  fun _i -> ((poly_stub_33)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>get_bootstrap_account =
  fun _n -> ((poly_stub_44)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>last_originations =
  fun _u -> ((poly_stub_43)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>new_account =
  fun _u -> ((poly_stub_42)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>register_delegate =
  fun _kh -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>register_constant =
  fun _m -> ((poly_stub_41)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>constant_to_michelson_program =
  fun _s -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>restore_context =
  fun _u -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>save_context =
  fun _u -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>drop_context =
  fun _u -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>set_baker_policy =
  fun _bp -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>set_baker =
  fun _a -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>size =
  fun _c -> ((poly_stub_40)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>read_contract_from_file =
  fun _fn -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>chr =
  fun _n -> ((poly_stub_38)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>nl =
  L("NEWLINE")[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>println =
  fun _v -> ((poly_stub_32)@(L(unit)))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_32)@(L(unit)))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_39)@(L(unit)))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_stub_32)@(L(unit))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_stub_32)@(L(unit)))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_stub_38)@(L(unit))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>sign =
  fun _sk -> (fun _d -> ((poly_stub_37)@(L(unit))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>add_account =
  fun _s -> (fun _k -> ((poly_stub_32)@(L(unit))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_stub_32)@(L(unit))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_stub_36)@(L(unit))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_stub_35)@(L(unit))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_stub_34)@(L(unit))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>originate_contract =
  fun _c -> (fun _s -> (fun _t -> ((poly_stub_33)@(L(unit)))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>compile_contract_from_file =
  fun _fn -> (fun _e -> (fun _v -> ((poly_stub_32)@(L(unit)))))[@inline] in
let <../../test/contracts/build/C.mligo#0><Test#0>originate_from_file =
  fun _fn ->
  (fun _e -> (fun _v -> (fun _s -> (fun _t -> ((poly_stub_31)@(L(unit)))))))[@inline] in
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
let <../../test/contracts/build/E.mligo#0><Tezos#0>set_delegate =
  fun o -> (SET_DELEGATE(o))[@inline] in
let <../../test/contracts/build/E.mligo#0><Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bitwise#0>xor =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bitwise#0>shift_left =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bitwise#0>shift_right =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <../../test/contracts/build/E.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
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
let <../../test/contracts/build/E.mligo#0><Bytes#0>length =
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
let <../../test/contracts/build/E.mligo#0>abs =
  fun i -> (({ ABS })@(i))[@inline] in
let <../../test/contracts/build/E.mligo#0>is_nat =
  fun i -> (({ ISNAT })@(i))[@inline] in
let <../../test/contracts/build/E.mligo#0>true = TRUE()[@inline] in
let <../../test/contracts/build/E.mligo#0>false = FALSE()[@inline] in
let <../../test/contracts/build/E.mligo#0>unit = UNIT()[@inline] in
let <../../test/contracts/build/E.mligo#0>assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let poly_stub_30 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_29 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_28 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_27 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_26 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_25 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_24 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_23 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_22 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_21 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_20 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_19 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_18 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_17 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_16 = fun x -> (({ FAILWITH })@(x))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>get_total_voting_power =
  (poly_stub_24)@(L(unit))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>set_source =
  fun _a -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>get_storage_of_address =
  fun _a -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>get_balance =
  fun _a -> ((poly_stub_30)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>print =
  fun _v -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>eprint =
  fun _v -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>get_voting_power =
  fun _kh -> ((poly_stub_24)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_stub_18)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>nth_bootstrap_account =
  fun _i -> ((poly_stub_18)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>get_bootstrap_account =
  fun _n -> ((poly_stub_29)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>last_originations =
  fun _u -> ((poly_stub_28)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>new_account =
  fun _u -> ((poly_stub_27)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>register_delegate =
  fun _kh -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>register_constant =
  fun _m -> ((poly_stub_26)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>constant_to_michelson_program =
  fun _s -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>restore_context =
  fun _u -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>save_context =
  fun _u -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>drop_context =
  fun _u -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>set_baker_policy =
  fun _bp -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>set_baker =
  fun _a -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>size =
  fun _c -> ((poly_stub_25)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>read_contract_from_file =
  fun _fn -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>chr =
  fun _n -> ((poly_stub_23)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>nl =
  L("NEWLINE")[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>println =
  fun _v -> ((poly_stub_17)@(L(unit)))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_17)@(L(unit)))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_24)@(L(unit)))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_stub_17)@(L(unit))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_stub_17)@(L(unit)))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_stub_23)@(L(unit))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>sign =
  fun _sk -> (fun _d -> ((poly_stub_22)@(L(unit))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>add_account =
  fun _s -> (fun _k -> ((poly_stub_17)@(L(unit))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_stub_17)@(L(unit))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_stub_21)@(L(unit))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_stub_20)@(L(unit))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_stub_19)@(L(unit))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>originate_contract =
  fun _c -> (fun _s -> (fun _t -> ((poly_stub_18)@(L(unit)))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>compile_contract_from_file =
  fun _fn -> (fun _e -> (fun _v -> ((poly_stub_17)@(L(unit)))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Test#0>originate_from_file =
  fun _fn ->
  (fun _e -> (fun _v -> (fun _s -> (fun _t -> ((poly_stub_16)@(L(unit)))))))[@inline] in
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
let <Tezos#0>set_delegate = fun o -> (SET_DELEGATE(o))[@inline] in
let <Tezos#0>open_chest =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let <Bitwise#0>xor = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let <Bitwise#0>shift_left = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let <Bitwise#0>shift_right = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let <String#0>length = fun b -> (({ SIZE })@(b))[@inline] in
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
let <Bytes#0>length = fun b -> (({ SIZE })@(b))[@inline] in
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
let abs = fun i -> (({ ABS })@(i))[@inline] in
let is_nat = fun i -> (({ ISNAT })@(i))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let poly_stub_15 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_14 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_13 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_12 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_11 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_10 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_9 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_8 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_7 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_6 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_5 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_4 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_3 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_2 = fun x -> (({ FAILWITH })@(x))[@inline] in
let poly_stub_1 = fun x -> (({ FAILWITH })@(x))[@inline] in
let <Test#0>get_total_voting_power = (poly_stub_9)@(L(unit))[@inline] in
let <Test#0>set_source = fun _a -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>get_storage_of_address =
  fun _a -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>get_balance = fun _a -> ((poly_stub_15)@(L(unit)))[@inline] in
let <Test#0>print = fun _v -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>eprint = fun _v -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>get_voting_power =
  fun _kh -> ((poly_stub_9)@(L(unit)))[@inline] in
let <Test#0>nth_bootstrap_contract =
  fun _i -> ((poly_stub_3)@(L(unit)))[@inline] in
let <Test#0>nth_bootstrap_account =
  fun _i -> ((poly_stub_3)@(L(unit)))[@inline] in
let <Test#0>get_bootstrap_account =
  fun _n -> ((poly_stub_14)@(L(unit)))[@inline] in
let <Test#0>last_originations =
  fun _u -> ((poly_stub_13)@(L(unit)))[@inline] in
let <Test#0>new_account = fun _u -> ((poly_stub_12)@(L(unit)))[@inline] in
let <Test#0>bake_until_n_cycle_end =
  fun _n -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>register_delegate =
  fun _kh -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>register_constant =
  fun _m -> ((poly_stub_11)@(L(unit)))[@inline] in
let <Test#0>constant_to_michelson_program =
  fun _s -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>restore_context = fun _u -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>save_context = fun _u -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>drop_context = fun _u -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>set_baker_policy =
  fun _bp -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>set_baker = fun _a -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>size = fun _c -> ((poly_stub_10)@(L(unit)))[@inline] in
let <Test#0>read_contract_from_file =
  fun _fn -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>chr = fun _n -> ((poly_stub_8)@(L(unit)))[@inline] in
let <Test#0>nl = L("NEWLINE")[@inline] in
let <Test#0>println = fun _v -> ((poly_stub_2)@(L(unit)))[@inline] in
let <Test#0>transfer =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_2)@(L(unit)))))[@inline] in
let <Test#0>transfer_exn =
  fun _a -> (fun _s -> (fun _t -> ((poly_stub_9)@(L(unit)))))[@inline] in
let <Test#0>reset_state =
  fun _n -> (fun _l -> ((poly_stub_2)@(L(unit))))[@inline] in
let <Test#0>reset_state_at =
  fun _t -> (fun _n -> (fun _l -> ((poly_stub_2)@(L(unit)))))[@inline] in
let <Test#0>save_mutation =
  fun _s -> (fun _m -> ((poly_stub_8)@(L(unit))))[@inline] in
let <Test#0>sign =
  fun _sk -> (fun _d -> ((poly_stub_7)@(L(unit))))[@inline] in
let <Test#0>add_account =
  fun _s -> (fun _k -> ((poly_stub_2)@(L(unit))))[@inline] in
let <Test#0>baker_account =
  fun _p -> (fun _o -> ((poly_stub_2)@(L(unit))))[@inline] in
let <Test#0>create_chest =
  fun _b -> (fun _n -> ((poly_stub_6)@(L(unit))))[@inline] in
let <Test#0>create_chest_key =
  fun _c -> (fun _n -> ((poly_stub_5)@(L(unit))))[@inline] in
let <Test#0>michelson_equal =
  fun _m1 -> (fun _m2 -> ((poly_stub_4)@(L(unit))))[@inline] in
let <Test#0>originate_contract =
  fun _c -> (fun _s -> (fun _t -> ((poly_stub_3)@(L(unit)))))[@inline] in
let <Test#0>compile_contract_from_file =
  fun _fn -> (fun _e -> (fun _v -> ((poly_stub_2)@(L(unit)))))[@inline] in
let <Test#0>originate_from_file =
  fun _fn ->
  (fun _e -> (fun _v -> (fun _s -> (fun _t -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto =
  ADD(<../../test/contracts/build/E.mligo#0>toto ,
      <../../test/contracts/build/A.mligo#0>toto) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#3160 ->
  (let (gen#3166, gen#3167) = gen#3160 in
   let p = gen#3166 in
   let s = gen#3167 in
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
