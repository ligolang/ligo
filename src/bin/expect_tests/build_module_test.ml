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
<<<<<<< HEAD
let #../../test/contracts/build/A.mligo#Tezos#balance#52 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#amount#53 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#now#54 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#sender#55 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#source#56 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#level#57 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#self_address#58 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#chain_id#59 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#total_voting_power#60 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_balance#61 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_amount#62 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_now#63 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_sender#64 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_source#65 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_level#66 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_self_address#67 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_chain_id#68 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_total_voting_power#69 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#voting_power#70 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#implicit_account#72 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#pairing_check#78 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#open_chest#79 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#set_delegate#83 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#xor#84 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#shift_left#85 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#shift_right#86 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#String#concat#127 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#128 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/A.mligo#String#length#129 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#concat#132 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#sub#133 =
=======
let <../../test/contracts/build/A.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bytes#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bytes#0>sub =
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/A.mligo#Bytes#length#136 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#blake2b#137 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha256#138 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha512#139 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha3#140 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#keccak#141 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#hash_key#142 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#check#143 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/A.mligo#assert#144 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#assert_with_error#145 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/A.mligo#abs#150 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/A.mligo#is_nat#151 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/A.mligo#true#152 = TRUE()[@inline] in
let #../../test/contracts/build/A.mligo#false#153 = FALSE()[@inline] in
let #../../test/contracts/build/A.mligo#unit#154 = UNIT()[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_105 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_104 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_103 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_102 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_101 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_100 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_99 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_98 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_97 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_96 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_95 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_94 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_93 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_92 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#failwith_91 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/A.mligo#Test#originate_from_file#160 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/A.mligo#failwith_105)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_source#162 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_baker#163 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_baker_policy#164 =
  fun _bp ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#transfer#165 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#transfer_exn#166 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#failwith_102)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_storage_of_address#170 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_balance#171 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_104)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#michelson_equal#172 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_103)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#reset_state#174 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#reset_state_at#175 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_voting_power#176 =
  fun _kh ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_102)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_total_voting_power#177 =
  (poly_#../../test/contracts/build/A.mligo#failwith_102)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nth_bootstrap_contract#179 =
  fun _i ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_96)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nth_bootstrap_account#180 =
  fun _i ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_96)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#last_originations#182 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_101)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#save_mutation#185 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_92)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#add_account#192 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#new_account#193 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_100)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#baker_account#194 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#bake_until_n_cycle_end#195 =
  fun _n ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#register_delegate#196 =
  fun _kh ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#register_constant#197 =
  fun _m ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_99)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#create_chest#202 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_98)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#create_chest_key#203 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_97)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#constant_to_michelson_program#204 =
  fun _s ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#restore_context#205 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#save_context#206 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#drop_context#207 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#read_contract_from_file#208 =
  fun _fn ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#compile_contract_from_file#209 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#originate_contract#211 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#failwith_96)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#size#212 =
  fun _c ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_bootstrap_account#213 =
  fun _n ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_94)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#sign#214 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_93)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#chr#215 =
  fun _n ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_92)@(L("TEST_CHR")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nl#216 =
  L("NEWLINE")[@inline] in
let #../../test/contracts/build/A.mligo#Test#println#217 =
  fun _v ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST_PRINTLN")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#print#218 =
  fun _v ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST_PRINT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#eprint#219 =
  fun _v ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST_EPRINTL")))[@inline] in
let #../../test/contracts/build/A.mligo#toto#221 = L(1) in
let #../../test/contracts/build/B.mligo#Tezos#balance#225 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#amount#226 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#now#227 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#sender#228 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#source#229 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#level#230 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#self_address#231 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#chain_id#232 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#total_voting_power#233 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_balance#234 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_amount#235 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_now#236 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_sender#237 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_source#238 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_level#239 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_self_address#240 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_chain_id#241 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_total_voting_power#242 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#voting_power#243 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#implicit_account#245 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#pairing_check#251 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#open_chest#252 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#set_delegate#256 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#xor#257 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_left#258 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_right#259 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#300 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#301 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/B.mligo#String#length#302 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#305 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#306 =
=======
let <../../test/contracts/build/A.mligo#0><String#8>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bytes#10>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/A.mligo#0><Bytes#10>sub =
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/B.mligo#Bytes#length#309 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#310 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#311 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#312 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#313 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#314 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#315 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#316 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/B.mligo#assert#317 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#assert_with_error#318 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/B.mligo#abs#323 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/B.mligo#is_nat#324 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/B.mligo#true#325 = TRUE()[@inline] in
let #../../test/contracts/build/B.mligo#false#326 = FALSE()[@inline] in
let #../../test/contracts/build/B.mligo#unit#327 = UNIT()[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_90 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_89 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_88 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_87 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_86 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_85 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_84 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_83 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_82 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_81 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_80 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_79 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_78 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_77 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#failwith_76 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/B.mligo#Test#originate_from_file#333 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/B.mligo#failwith_90)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_source#335 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_baker#336 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_baker_policy#337 =
  fun _bp ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#transfer#338 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#transfer_exn#339 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#failwith_87)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_storage_of_address#343 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_balance#344 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_89)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#michelson_equal#345 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_88)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#reset_state#347 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#reset_state_at#348 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_voting_power#349 =
  fun _kh ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_87)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_total_voting_power#350 =
  (poly_#../../test/contracts/build/B.mligo#failwith_87)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nth_bootstrap_contract#352 =
  fun _i ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_81)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nth_bootstrap_account#353 =
  fun _i ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_81)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#last_originations#355 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_86)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#save_mutation#358 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_77)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#add_account#365 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#new_account#366 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_85)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#baker_account#367 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#bake_until_n_cycle_end#368 =
  fun _n ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#register_delegate#369 =
  fun _kh ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#register_constant#370 =
  fun _m ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_84)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#create_chest#375 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_83)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#create_chest_key#376 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_82)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#constant_to_michelson_program#377 =
  fun _s ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#restore_context#378 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#save_context#379 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#drop_context#380 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#read_contract_from_file#381 =
  fun _fn ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#compile_contract_from_file#382 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#originate_contract#384 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#failwith_81)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#size#385 =
  fun _c ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_bootstrap_account#386 =
  fun _n ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_79)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#sign#387 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_78)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#chr#388 =
  fun _n ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_77)@(L("TEST_CHR")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nl#389 =
  L("NEWLINE")[@inline] in
let #../../test/contracts/build/B.mligo#Test#println#390 =
  fun _v ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST_PRINTLN")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#print#391 =
  fun _v ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST_PRINT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#eprint#392 =
  fun _v ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST_EPRINTL")))[@inline] in
let #../../test/contracts/build/B.mligo#toto#394 = L(32) in
let #../../test/contracts/build/B.mligo#titi#395 =
  ADD(#../../test/contracts/build/A.mligo#toto#221 , L(42)) in
let #../../test/contracts/build/B.mligo#f#396 =
  fun gen#2126 ->
  (let (gen#4328, gen#4329) = gen#2126 in
   let gen#2127 = gen#4328 in
   let x = gen#4329 in
   let x =
     ADD(ADD(x , #../../test/contracts/build/A.mligo#toto#221) ,
         #../../test/contracts/build/B.mligo#titi#395) in
   PAIR(LIST_EMPTY() , x)) in
let #../../test/contracts/build/F.mligo#Tezos#balance#400 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#amount#401 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#now#402 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#sender#403 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#source#404 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#level#405 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#self_address#406 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#chain_id#407 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#total_voting_power#408 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_balance#409 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_amount#410 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_now#411 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_sender#412 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_source#413 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_level#414 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_self_address#415 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_chain_id#416 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_total_voting_power#417 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#voting_power#418 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#implicit_account#420 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#pairing_check#426 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#open_chest#427 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#set_delegate#431 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#xor#432 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_left#433 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_right#434 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#475 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#476 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/F.mligo#String#length#477 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#480 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#481 =
=======
let <../../test/contracts/build/F.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bytes#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/F.mligo#0><Bytes#0>sub =
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/F.mligo#Bytes#length#484 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#485 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#486 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#487 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#488 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#489 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#490 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#491 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/F.mligo#assert#492 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#assert_with_error#493 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/F.mligo#abs#498 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/F.mligo#is_nat#499 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/F.mligo#true#500 = TRUE()[@inline] in
let #../../test/contracts/build/F.mligo#false#501 = FALSE()[@inline] in
let #../../test/contracts/build/F.mligo#unit#502 = UNIT()[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_75 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_74 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_73 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_72 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_71 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_70 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_69 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_68 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_67 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_66 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_65 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_64 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_63 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_62 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#failwith_61 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/F.mligo#Test#originate_from_file#508 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/F.mligo#failwith_75)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_source#510 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_baker#511 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_baker_policy#512 =
  fun _bp ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#transfer#513 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#transfer_exn#514 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#failwith_72)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_storage_of_address#518 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_balance#519 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_74)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#michelson_equal#520 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_73)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#reset_state#522 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#reset_state_at#523 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_voting_power#524 =
  fun _kh ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_72)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_total_voting_power#525 =
  (poly_#../../test/contracts/build/F.mligo#failwith_72)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nth_bootstrap_contract#527 =
  fun _i ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_66)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nth_bootstrap_account#528 =
  fun _i ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_66)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#last_originations#530 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_71)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#save_mutation#533 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_62)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#add_account#540 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#new_account#541 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_70)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#baker_account#542 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#bake_until_n_cycle_end#543 =
  fun _n ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#register_delegate#544 =
  fun _kh ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#register_constant#545 =
  fun _m ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_69)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#create_chest#550 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_68)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#create_chest_key#551 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_67)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#constant_to_michelson_program#552 =
  fun _s ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#restore_context#553 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#save_context#554 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#drop_context#555 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#read_contract_from_file#556 =
  fun _fn ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#compile_contract_from_file#557 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#originate_contract#559 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#failwith_66)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#size#560 =
  fun _c ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_bootstrap_account#561 =
  fun _n ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_64)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#sign#562 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_63)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#chr#563 =
  fun _n ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_62)@(L("TEST_CHR")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nl#564 =
  L("NEWLINE")[@inline] in
let #../../test/contracts/build/F.mligo#Test#println#565 =
  fun _v ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST_PRINTLN")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#print#566 =
  fun _v ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST_PRINT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#eprint#567 =
  fun _v ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST_EPRINTL")))[@inline] in
let #../../test/contracts/build/F.mligo#toto#569 = L(44) in
let #../../test/contracts/build/G.mligo#Tezos#balance#573 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#amount#574 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#now#575 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#sender#576 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#source#577 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#level#578 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#self_address#579 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#chain_id#580 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#total_voting_power#581 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_balance#582 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_amount#583 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_now#584 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_sender#585 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_source#586 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_level#587 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_self_address#588 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_chain_id#589 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_total_voting_power#590 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#voting_power#591 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#implicit_account#593 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#pairing_check#599 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#open_chest#600 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#set_delegate#604 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#xor#605 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_left#606 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_right#607 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#648 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#649 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/G.mligo#String#length#650 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#653 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#654 =
=======
let <../../test/contracts/build/G.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bytes#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bytes#0>sub =
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/G.mligo#Bytes#length#657 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#658 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#659 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#660 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#661 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#662 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#663 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#664 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/G.mligo#assert#665 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#assert_with_error#666 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/G.mligo#abs#671 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/G.mligo#is_nat#672 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/G.mligo#true#673 = TRUE()[@inline] in
let #../../test/contracts/build/G.mligo#false#674 = FALSE()[@inline] in
let #../../test/contracts/build/G.mligo#unit#675 = UNIT()[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_60 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_59 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_58 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_57 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_56 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_55 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_54 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_53 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_52 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_51 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_50 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_49 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_48 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_47 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#failwith_46 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/G.mligo#Test#originate_from_file#681 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/G.mligo#failwith_60)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_source#683 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_baker#684 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_baker_policy#685 =
  fun _bp ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#transfer#686 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#transfer_exn#687 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#failwith_57)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_storage_of_address#691 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_balance#692 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_59)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#michelson_equal#693 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_58)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#reset_state#695 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#reset_state_at#696 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_voting_power#697 =
  fun _kh ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_57)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_total_voting_power#698 =
  (poly_#../../test/contracts/build/G.mligo#failwith_57)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nth_bootstrap_contract#700 =
  fun _i ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_51)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nth_bootstrap_account#701 =
  fun _i ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_51)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#last_originations#703 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_56)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#save_mutation#706 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_47)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#add_account#713 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#new_account#714 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_55)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#baker_account#715 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#bake_until_n_cycle_end#716 =
  fun _n ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#register_delegate#717 =
  fun _kh ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#register_constant#718 =
  fun _m ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_54)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#create_chest#723 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_53)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#create_chest_key#724 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_52)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#constant_to_michelson_program#725 =
  fun _s ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#restore_context#726 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#save_context#727 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#drop_context#728 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#read_contract_from_file#729 =
  fun _fn ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#compile_contract_from_file#730 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#originate_contract#732 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#failwith_51)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#size#733 =
  fun _c ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_bootstrap_account#734 =
  fun _n ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_49)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#sign#735 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_48)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#chr#736 =
  fun _n ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_47)@(L("TEST_CHR")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nl#737 =
  L("NEWLINE")[@inline] in
let #../../test/contracts/build/G.mligo#Test#println#738 =
  fun _v ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST_PRINTLN")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#print#739 =
  fun _v ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST_PRINT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#eprint#740 =
  fun _v ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST_EPRINTL")))[@inline] in
let #../../test/contracts/build/G.mligo#toto#742 = L(43) in
let #../../test/contracts/build/C.mligo#Tezos#balance#746 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#amount#747 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#now#748 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#sender#749 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#source#750 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#level#751 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#self_address#752 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#chain_id#753 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#total_voting_power#754 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_balance#755 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_amount#756 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_now#757 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_sender#758 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_source#759 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_level#760 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_self_address#761 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_chain_id#762 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_total_voting_power#763 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#voting_power#764 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#implicit_account#766 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#pairing_check#772 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#open_chest#773 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#set_delegate#777 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#xor#778 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_left#779 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_right#780 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#821 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#822 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/C.mligo#String#length#823 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#826 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#827 =
=======
let <../../test/contracts/build/G.mligo#0><String#38>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bytes#40>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/G.mligo#0><Bytes#40>sub =
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/C.mligo#Bytes#length#830 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#831 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#832 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#833 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#834 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#835 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#836 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#837 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/C.mligo#assert#838 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#assert_with_error#839 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/C.mligo#abs#844 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/C.mligo#is_nat#845 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/C.mligo#true#846 = TRUE()[@inline] in
let #../../test/contracts/build/C.mligo#false#847 = FALSE()[@inline] in
let #../../test/contracts/build/C.mligo#unit#848 = UNIT()[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_45 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_44 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_43 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_42 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_41 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_40 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_39 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_38 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_37 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_36 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_35 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_34 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_33 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_32 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#failwith_31 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/C.mligo#Test#originate_from_file#854 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/C.mligo#failwith_45)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_source#856 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_baker#857 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_baker_policy#858 =
  fun _bp ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#transfer#859 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#transfer_exn#860 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#failwith_42)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_storage_of_address#864 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_balance#865 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_44)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#michelson_equal#866 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_43)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#reset_state#868 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#reset_state_at#869 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_voting_power#870 =
  fun _kh ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_42)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_total_voting_power#871 =
  (poly_#../../test/contracts/build/C.mligo#failwith_42)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nth_bootstrap_contract#873 =
  fun _i ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_36)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nth_bootstrap_account#874 =
  fun _i ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_36)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#last_originations#876 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_41)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#save_mutation#879 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_32)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#add_account#886 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#new_account#887 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_40)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#baker_account#888 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#bake_until_n_cycle_end#889 =
  fun _n ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#register_delegate#890 =
  fun _kh ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#register_constant#891 =
  fun _m ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_39)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#create_chest#896 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_38)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#create_chest_key#897 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_37)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#constant_to_michelson_program#898 =
  fun _s ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#restore_context#899 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#save_context#900 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#drop_context#901 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#read_contract_from_file#902 =
  fun _fn ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#compile_contract_from_file#903 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#originate_contract#905 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#failwith_36)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#size#906 =
  fun _c ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_bootstrap_account#907 =
  fun _n ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_34)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#sign#908 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_33)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#chr#909 =
  fun _n ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_32)@(L("TEST_CHR")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nl#910 =
  L("NEWLINE")[@inline] in
let #../../test/contracts/build/C.mligo#Test#println#911 =
  fun _v ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST_PRINTLN")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#print#912 =
  fun _v ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST_PRINT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#eprint#913 =
  fun _v ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST_EPRINTL")))[@inline] in
let #../../test/contracts/build/C.mligo#tata#915 =
  ADD(#../../test/contracts/build/A.mligo#toto#221 ,
      #../../test/contracts/build/B.mligo#titi#395) in
let #../../test/contracts/build/C.mligo#foo#916 =
  (#../../test/contracts/build/B.mligo#f#396)@(PAIR(L(unit) , L(3))) in
let #../../test/contracts/build/E.mligo#Tezos#balance#920 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#amount#921 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#now#922 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#sender#923 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#source#924 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#level#925 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#self_address#926 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#chain_id#927 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#total_voting_power#928 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_balance#929 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_amount#930 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_now#931 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_sender#932 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_source#933 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_level#934 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_self_address#935 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_chain_id#936 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_total_voting_power#937 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#voting_power#938 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#implicit_account#940 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#pairing_check#946 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#open_chest#947 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#set_delegate#951 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#xor#952 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_left#953 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_right#954 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#995 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#996 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/E.mligo#String#length#997 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#1000 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#1001 =
=======
let <../../test/contracts/build/E.mligo#0><String#0>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bytes#0>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bytes#0>sub =
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/E.mligo#Bytes#length#1004 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#1005 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#1006 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#1007 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#1008 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#1009 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#1010 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#1011 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
<<<<<<< HEAD
let #../../test/contracts/build/E.mligo#assert#1012 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#assert_with_error#1013 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/E.mligo#abs#1018 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/E.mligo#is_nat#1019 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/E.mligo#true#1020 = TRUE()[@inline] in
let #../../test/contracts/build/E.mligo#false#1021 = FALSE()[@inline] in
let #../../test/contracts/build/E.mligo#unit#1022 = UNIT()[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_30 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_29 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_28 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_27 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_26 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_25 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_24 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_23 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_22 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_21 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_20 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_19 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_18 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_17 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#failwith_16 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/E.mligo#Test#originate_from_file#1028 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/E.mligo#failwith_30)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_source#1030 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_baker#1031 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_baker_policy#1032 =
  fun _bp ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#transfer#1033 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#transfer_exn#1034 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#failwith_27)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_storage_of_address#1038 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_balance#1039 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_29)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#michelson_equal#1040 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_28)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#reset_state#1042 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#reset_state_at#1043 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_voting_power#1044 =
  fun _kh ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_27)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_total_voting_power#1045 =
  (poly_#../../test/contracts/build/E.mligo#failwith_27)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nth_bootstrap_contract#1047 =
  fun _i ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_21)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nth_bootstrap_account#1048 =
  fun _i ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_21)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#last_originations#1050 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_26)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#save_mutation#1053 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_17)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#add_account#1060 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#new_account#1061 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_25)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#baker_account#1062 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#bake_until_n_cycle_end#1063 =
  fun _n ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#register_delegate#1064 =
  fun _kh ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#register_constant#1065 =
  fun _m ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_24)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#create_chest#1070 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_23)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#create_chest_key#1071 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_22)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#constant_to_michelson_program#1072 =
  fun _s ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#restore_context#1073 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#save_context#1074 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#drop_context#1075 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#read_contract_from_file#1076 =
  fun _fn ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#compile_contract_from_file#1077 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#originate_contract#1079 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#failwith_21)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#size#1080 =
  fun _c ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_bootstrap_account#1081 =
  fun _n ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_19)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#sign#1082 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_18)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#chr#1083 =
  fun _n ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_17)@(L("TEST_CHR")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nl#1084 =
  L("NEWLINE")[@inline] in
let #../../test/contracts/build/E.mligo#Test#println#1085 =
  fun _v ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST_PRINTLN")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#print#1086 =
  fun _v ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST_PRINT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#eprint#1087 =
  fun _v ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST_EPRINTL")))[@inline] in
let #../../test/contracts/build/E.mligo#toto#1089 = L(10) in
let #../../test/contracts/build/E.mligo#foo#1090 = L("bar") in
let #Tezos#balance#1094 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #Tezos#amount#1095 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #Tezos#now#1096 = ({ DROP ; NOW })@(L(unit))[@inline] in
let #Tezos#sender#1097 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let #Tezos#source#1098 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #Tezos#level#1099 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #Tezos#self_address#1100 = SELF_ADDRESS()[@inline] in
let #Tezos#chain_id#1101 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #Tezos#total_voting_power#1102 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #Tezos#get_balance#1103 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #Tezos#get_amount#1104 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #Tezos#get_now#1105 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #Tezos#get_sender#1106 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #Tezos#get_source#1107 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #Tezos#get_level#1108 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #Tezos#get_self_address#1109 = fun _u -> (SELF_ADDRESS())[@inline] in
let #Tezos#get_chain_id#1110 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #Tezos#get_total_voting_power#1111 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #Tezos#voting_power#1112 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #Tezos#implicit_account#1114 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #Tezos#pairing_check#1120 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #Tezos#open_chest#1121 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #Tezos#set_delegate#1125 = fun o -> (SET_DELEGATE(o))[@inline] in
let #Bitwise#xor#1126 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #Bitwise#shift_left#1127 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #Bitwise#shift_right#1128 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #String#concat#1169 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #String#sub#1170 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #String#length#1171 = fun b -> (({ SIZE })@(b))[@inline] in
let #Bytes#concat#1174 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #Bytes#sub#1175 =
=======
let <../../test/contracts/build/E.mligo#0><String#58>length =
  fun b -> (({ SIZE })@(b))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bytes#60>concat =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let <../../test/contracts/build/E.mligo#0><Bytes#60>sub =
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
<<<<<<< HEAD
let #Bytes#length#1178 = fun b -> (({ SIZE })@(b))[@inline] in
let #Crypto#blake2b#1179 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let #Crypto#sha256#1180 = fun b -> (({ SHA256 })@(b))[@inline] in
let #Crypto#sha512#1181 = fun b -> (({ SHA512 })@(b))[@inline] in
let #Crypto#sha3#1182 = fun b -> (({ SHA3 })@(b))[@inline] in
let #Crypto#keccak#1183 = fun b -> (({ KECCAK })@(b))[@inline] in
let #Crypto#hash_key#1184 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let #Crypto#check#1185 =
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
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
<<<<<<< HEAD
let #Test#originate_from_file#1187 =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_failwith_15)@(L("TEST MODE")))))))[@inline] in
let #Test#set_source#1189 =
  fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#set_baker#1190 =
  fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#set_baker_policy#1191 =
  fun _bp -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#transfer#1192 =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_1)@(L("TEST MODE")))))[@inline] in
let #Test#transfer_exn#1193 =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_12)@(L("TEST MODE")))))[@inline] in
let #Test#get_storage_of_address#1197 =
  fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#get_balance#1198 =
  fun _a -> ((poly_failwith_14)@(L("TEST MODE")))[@inline] in
let #Test#michelson_equal#1199 =
  fun _m1 -> (fun _m2 -> ((poly_failwith_13)@(L("TEST MODE"))))[@inline] in
let #Test#reset_state#1201 =
  fun _n -> (fun _l -> ((poly_failwith_1)@(L("TEST MODE"))))[@inline] in
let #Test#reset_state_at#1202 =
  fun _t -> (fun _n -> (fun _l -> ((poly_failwith_1)@(L("TEST MODE")))))[@inline] in
let #Test#get_voting_power#1203 =
  fun _kh -> ((poly_failwith_12)@(L("TEST MODE")))[@inline] in
let #Test#get_total_voting_power#1204 =
  (poly_failwith_12)@(L("TEST MODE"))[@inline] in
let #Test#nth_bootstrap_contract#1206 =
  fun _i -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
let #Test#nth_bootstrap_account#1207 =
  fun _i -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
let #Test#last_originations#1209 =
  fun _u -> ((poly_failwith_11)@(L("TEST MODE")))[@inline] in
let #Test#save_mutation#1212 =
  fun _s -> (fun _m -> ((poly_failwith_2)@(L("TEST MODE"))))[@inline] in
let #Test#add_account#1219 =
  fun _s -> (fun _k -> ((poly_failwith_1)@(L("TEST MODE"))))[@inline] in
let #Test#new_account#1220 =
  fun _u -> ((poly_failwith_10)@(L("TEST MODE")))[@inline] in
let #Test#baker_account#1221 =
  fun _p -> (fun _o -> ((poly_failwith_1)@(L("TEST MODE"))))[@inline] in
let #Test#bake_until_n_cycle_end#1222 =
  fun _n -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#register_delegate#1223 =
  fun _kh -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#register_constant#1224 =
  fun _m -> ((poly_failwith_9)@(L("TEST MODE")))[@inline] in
let #Test#create_chest#1229 =
  fun _b -> (fun _n -> ((poly_failwith_8)@(L("TEST MODE"))))[@inline] in
let #Test#create_chest_key#1230 =
  fun _c -> (fun _n -> ((poly_failwith_7)@(L("TEST MODE"))))[@inline] in
let #Test#constant_to_michelson_program#1231 =
  fun _s -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#restore_context#1232 =
  fun _u -> ((poly_failwith_1)@(L("TEST_POP_CONTEXT")))[@inline] in
let #Test#save_context#1233 =
  fun _u -> ((poly_failwith_1)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #Test#drop_context#1234 =
  fun _u -> ((poly_failwith_1)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #Test#read_contract_from_file#1235 =
  fun _fn -> ((poly_failwith_1)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #Test#compile_contract_from_file#1236 =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_failwith_1)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #Test#originate_contract#1238 =
  fun _c -> (fun _s -> (fun _t -> ((poly_failwith_6)@(L("TEST_ORIGINATE")))))[@inline] in
let #Test#size#1239 =
  fun _c -> ((poly_failwith_5)@(L("TEST_SIZE")))[@inline] in
let #Test#get_bootstrap_account#1240 =
  fun _n -> ((poly_failwith_4)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #Test#sign#1241 =
  fun _sk -> (fun _d -> ((poly_failwith_3)@(L("TEST_SIGN"))))[@inline] in
let #Test#chr#1242 =
  fun _n -> ((poly_failwith_2)@(L("TEST_CHR")))[@inline] in
let #Test#nl#1243 = L("NEWLINE")[@inline] in
let #Test#println#1244 =
  fun _v -> ((poly_failwith_1)@(L("TEST_PRINTLN")))[@inline] in
let #Test#print#1245 =
  fun _v -> ((poly_failwith_1)@(L("TEST_PRINT")))[@inline] in
let #Test#eprint#1246 =
  fun _v -> ((poly_failwith_1)@(L("TEST_EPRINTL")))[@inline] in
let toto =
  ADD(#../../test/contracts/build/E.mligo#toto#1089 ,
      #../../test/contracts/build/A.mligo#toto#221) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#4324 ->
  (let (gen#4330, gen#4331) = gen#4324 in
   let p = gen#4330 in
   let s = gen#4331 in
=======
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
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
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
