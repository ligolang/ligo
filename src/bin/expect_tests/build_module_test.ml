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
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/A.mligo#String#length#129 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#concat#132 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#sub#133 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
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
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
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
  ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_baker#163 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_baker_policy#164 =
  fun _bp ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#transfer#165 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#transfer_exn#166 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#failwith_102)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_storage_of_address#170 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST MODE")))[@inline] in
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
   ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#reset_state_at#175 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_voting_power#176 =
  fun _kh ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_102)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_total_voting_power#177 =
  (poly_#../../test/contracts/build/A.mligo#failwith_102)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nth_bootstrap_contract#179 =
  fun _i ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_94)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nth_bootstrap_account#180 =
  fun _i ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_94)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#last_originations#182 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_101)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#save_mutation#185 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_100)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#add_account#192 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#new_account#193 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_99)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#baker_account#194 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#bake_until_n_cycle_end#195 =
  fun _n ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#register_delegate#196 =
  fun _kh ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#register_constant#197 =
  fun _m ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_98)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#create_chest#202 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_97)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#create_chest_key#203 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_96)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#constant_to_michelson_program#204 =
  fun _s ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#restore_context#205 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#save_context#206 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#drop_context#207 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#read_contract_from_file#208 =
  fun _fn ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#compile_contract_from_file#209 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/A.mligo#failwith_95)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#originate_contract#211 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#failwith_94)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#size#212 =
  fun _c ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_93)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_bootstrap_account#213 =
  fun _n ->
  ((poly_#../../test/contracts/build/A.mligo#failwith_92)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#sign#214 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/A.mligo#failwith_91)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/A.mligo#toto#215 = L(1) in
let #../../test/contracts/build/B.mligo#Tezos#balance#219 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#amount#220 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#now#221 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#sender#222 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#source#223 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#level#224 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#self_address#225 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#chain_id#226 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#total_voting_power#227 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_balance#228 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_amount#229 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_now#230 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_sender#231 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_source#232 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_level#233 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_self_address#234 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_chain_id#235 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_total_voting_power#236 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#voting_power#237 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#implicit_account#239 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#pairing_check#245 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#open_chest#246 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#set_delegate#250 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#xor#251 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_left#252 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_right#253 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#294 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#295 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/B.mligo#String#length#296 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#299 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#300 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#length#303 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#304 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#305 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#306 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#307 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#308 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#309 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#310 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/B.mligo#assert#311 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#assert_with_error#312 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/B.mligo#abs#317 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/B.mligo#is_nat#318 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/B.mligo#true#319 = TRUE()[@inline] in
let #../../test/contracts/build/B.mligo#false#320 = FALSE()[@inline] in
let #../../test/contracts/build/B.mligo#unit#321 = UNIT()[@inline] in
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
let #../../test/contracts/build/B.mligo#Test#originate_from_file#327 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/B.mligo#failwith_90)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_source#329 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_baker#330 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_baker_policy#331 =
  fun _bp ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#transfer#332 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#transfer_exn#333 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#failwith_87)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_storage_of_address#337 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_balance#338 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_89)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#michelson_equal#339 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_88)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#reset_state#341 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#reset_state_at#342 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_voting_power#343 =
  fun _kh ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_87)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_total_voting_power#344 =
  (poly_#../../test/contracts/build/B.mligo#failwith_87)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nth_bootstrap_contract#346 =
  fun _i ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_79)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nth_bootstrap_account#347 =
  fun _i ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_79)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#last_originations#349 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_86)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#save_mutation#352 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_85)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#add_account#359 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#new_account#360 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_84)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#baker_account#361 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#bake_until_n_cycle_end#362 =
  fun _n ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#register_delegate#363 =
  fun _kh ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#register_constant#364 =
  fun _m ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_83)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#create_chest#369 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_82)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#create_chest_key#370 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_81)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#constant_to_michelson_program#371 =
  fun _s ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#restore_context#372 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#save_context#373 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#drop_context#374 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#read_contract_from_file#375 =
  fun _fn ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#compile_contract_from_file#376 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/B.mligo#failwith_80)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#originate_contract#378 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#failwith_79)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#size#379 =
  fun _c ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_78)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_bootstrap_account#380 =
  fun _n ->
  ((poly_#../../test/contracts/build/B.mligo#failwith_77)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#sign#381 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/B.mligo#failwith_76)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/B.mligo#toto#382 = L(32) in
let #../../test/contracts/build/B.mligo#titi#383 =
  ADD(#../../test/contracts/build/A.mligo#toto#215 , L(42)) in
let #../../test/contracts/build/B.mligo#f#384 =
  fun gen#2062 ->
  (let (gen#4209, gen#4210) = gen#2062 in
   let gen#2063 = gen#4209 in
   let x = gen#4210 in
   let x =
     ADD(ADD(x , #../../test/contracts/build/A.mligo#toto#215) ,
         #../../test/contracts/build/B.mligo#titi#383) in
   PAIR(LIST_EMPTY() , x)) in
let #../../test/contracts/build/F.mligo#Tezos#balance#388 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#amount#389 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#now#390 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#sender#391 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#source#392 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#level#393 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#self_address#394 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#chain_id#395 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#total_voting_power#396 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_balance#397 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_amount#398 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_now#399 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_sender#400 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_source#401 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_level#402 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_self_address#403 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_chain_id#404 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_total_voting_power#405 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#voting_power#406 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#implicit_account#408 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#pairing_check#414 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#open_chest#415 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#set_delegate#419 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#xor#420 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_left#421 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_right#422 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#463 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#464 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/F.mligo#String#length#465 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#468 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#469 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#length#472 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#473 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#474 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#475 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#476 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#477 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#478 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#479 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/F.mligo#assert#480 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#assert_with_error#481 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/F.mligo#abs#486 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/F.mligo#is_nat#487 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/F.mligo#true#488 = TRUE()[@inline] in
let #../../test/contracts/build/F.mligo#false#489 = FALSE()[@inline] in
let #../../test/contracts/build/F.mligo#unit#490 = UNIT()[@inline] in
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
let #../../test/contracts/build/F.mligo#Test#originate_from_file#496 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/F.mligo#failwith_75)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_source#498 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_baker#499 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_baker_policy#500 =
  fun _bp ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#transfer#501 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#transfer_exn#502 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#failwith_72)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_storage_of_address#506 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_balance#507 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_74)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#michelson_equal#508 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_73)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#reset_state#510 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#reset_state_at#511 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_voting_power#512 =
  fun _kh ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_72)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_total_voting_power#513 =
  (poly_#../../test/contracts/build/F.mligo#failwith_72)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nth_bootstrap_contract#515 =
  fun _i ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_64)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nth_bootstrap_account#516 =
  fun _i ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_64)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#last_originations#518 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_71)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#save_mutation#521 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_70)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#add_account#528 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#new_account#529 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_69)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#baker_account#530 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#bake_until_n_cycle_end#531 =
  fun _n ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#register_delegate#532 =
  fun _kh ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#register_constant#533 =
  fun _m ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_68)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#create_chest#538 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_67)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#create_chest_key#539 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_66)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#constant_to_michelson_program#540 =
  fun _s ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#restore_context#541 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#save_context#542 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#drop_context#543 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#read_contract_from_file#544 =
  fun _fn ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#compile_contract_from_file#545 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/F.mligo#failwith_65)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#originate_contract#547 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#failwith_64)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#size#548 =
  fun _c ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_63)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_bootstrap_account#549 =
  fun _n ->
  ((poly_#../../test/contracts/build/F.mligo#failwith_62)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#sign#550 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/F.mligo#failwith_61)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/F.mligo#toto#551 = L(44) in
let #../../test/contracts/build/G.mligo#Tezos#balance#555 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#amount#556 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#now#557 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#sender#558 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#source#559 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#level#560 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#self_address#561 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#chain_id#562 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#total_voting_power#563 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_balance#564 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_amount#565 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_now#566 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_sender#567 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_source#568 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_level#569 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_self_address#570 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_chain_id#571 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_total_voting_power#572 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#voting_power#573 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#implicit_account#575 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#pairing_check#581 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#open_chest#582 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#set_delegate#586 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#xor#587 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_left#588 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_right#589 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#630 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#631 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/G.mligo#String#length#632 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#635 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#636 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#length#639 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#640 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#641 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#642 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#643 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#644 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#645 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#646 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/G.mligo#assert#647 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#assert_with_error#648 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/G.mligo#abs#653 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/G.mligo#is_nat#654 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/G.mligo#true#655 = TRUE()[@inline] in
let #../../test/contracts/build/G.mligo#false#656 = FALSE()[@inline] in
let #../../test/contracts/build/G.mligo#unit#657 = UNIT()[@inline] in
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
let #../../test/contracts/build/G.mligo#Test#originate_from_file#663 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/G.mligo#failwith_60)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_source#665 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_baker#666 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_baker_policy#667 =
  fun _bp ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#transfer#668 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#transfer_exn#669 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#failwith_57)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_storage_of_address#673 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_balance#674 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_59)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#michelson_equal#675 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_58)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#reset_state#677 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#reset_state_at#678 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_voting_power#679 =
  fun _kh ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_57)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_total_voting_power#680 =
  (poly_#../../test/contracts/build/G.mligo#failwith_57)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nth_bootstrap_contract#682 =
  fun _i ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_49)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nth_bootstrap_account#683 =
  fun _i ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_49)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#last_originations#685 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_56)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#save_mutation#688 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_55)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#add_account#695 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#new_account#696 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_54)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#baker_account#697 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#bake_until_n_cycle_end#698 =
  fun _n ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#register_delegate#699 =
  fun _kh ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#register_constant#700 =
  fun _m ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_53)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#create_chest#705 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_52)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#create_chest_key#706 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_51)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#constant_to_michelson_program#707 =
  fun _s ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#restore_context#708 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#save_context#709 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#drop_context#710 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#read_contract_from_file#711 =
  fun _fn ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#compile_contract_from_file#712 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/G.mligo#failwith_50)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#originate_contract#714 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#failwith_49)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#size#715 =
  fun _c ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_48)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_bootstrap_account#716 =
  fun _n ->
  ((poly_#../../test/contracts/build/G.mligo#failwith_47)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#sign#717 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/G.mligo#failwith_46)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/G.mligo#toto#718 = L(43) in
let #../../test/contracts/build/C.mligo#Tezos#balance#722 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#amount#723 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#now#724 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#sender#725 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#source#726 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#level#727 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#self_address#728 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#chain_id#729 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#total_voting_power#730 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_balance#731 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_amount#732 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_now#733 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_sender#734 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_source#735 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_level#736 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_self_address#737 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_chain_id#738 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_total_voting_power#739 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#voting_power#740 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#implicit_account#742 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#pairing_check#748 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#open_chest#749 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#set_delegate#753 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#xor#754 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_left#755 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_right#756 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#797 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#798 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/C.mligo#String#length#799 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#802 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#803 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#length#806 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#807 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#808 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#809 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#810 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#811 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#812 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#813 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/C.mligo#assert#814 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#assert_with_error#815 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/C.mligo#abs#820 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/C.mligo#is_nat#821 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/C.mligo#true#822 = TRUE()[@inline] in
let #../../test/contracts/build/C.mligo#false#823 = FALSE()[@inline] in
let #../../test/contracts/build/C.mligo#unit#824 = UNIT()[@inline] in
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
let #../../test/contracts/build/C.mligo#Test#originate_from_file#830 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/C.mligo#failwith_45)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_source#832 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_baker#833 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_baker_policy#834 =
  fun _bp ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#transfer#835 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#transfer_exn#836 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#failwith_42)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_storage_of_address#840 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_balance#841 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_44)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#michelson_equal#842 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_43)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#reset_state#844 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#reset_state_at#845 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_voting_power#846 =
  fun _kh ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_42)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_total_voting_power#847 =
  (poly_#../../test/contracts/build/C.mligo#failwith_42)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nth_bootstrap_contract#849 =
  fun _i ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_34)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nth_bootstrap_account#850 =
  fun _i ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_34)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#last_originations#852 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_41)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#save_mutation#855 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_40)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#add_account#862 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#new_account#863 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_39)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#baker_account#864 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#bake_until_n_cycle_end#865 =
  fun _n ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#register_delegate#866 =
  fun _kh ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#register_constant#867 =
  fun _m ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_38)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#create_chest#872 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_37)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#create_chest_key#873 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_36)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#constant_to_michelson_program#874 =
  fun _s ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#restore_context#875 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#save_context#876 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#drop_context#877 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#read_contract_from_file#878 =
  fun _fn ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#compile_contract_from_file#879 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/C.mligo#failwith_35)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#originate_contract#881 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#failwith_34)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#size#882 =
  fun _c ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_33)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_bootstrap_account#883 =
  fun _n ->
  ((poly_#../../test/contracts/build/C.mligo#failwith_32)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#sign#884 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/C.mligo#failwith_31)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/C.mligo#tata#885 =
  ADD(#../../test/contracts/build/A.mligo#toto#215 ,
      #../../test/contracts/build/B.mligo#titi#383) in
let #../../test/contracts/build/C.mligo#foo#886 =
  (#../../test/contracts/build/B.mligo#f#384)@(PAIR(L(unit) , L(3))) in
let #../../test/contracts/build/E.mligo#Tezos#balance#890 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#amount#891 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#now#892 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#sender#893 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#source#894 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#level#895 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#self_address#896 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#chain_id#897 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#total_voting_power#898 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_balance#899 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_amount#900 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_now#901 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_sender#902 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_source#903 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_level#904 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_self_address#905 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_chain_id#906 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_total_voting_power#907 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#voting_power#908 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#implicit_account#910 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#pairing_check#916 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#open_chest#917 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#set_delegate#921 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#xor#922 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_left#923 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_right#924 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#965 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#966 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/E.mligo#String#length#967 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#970 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#971 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#length#974 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#975 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#976 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#977 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#978 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#979 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#980 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#981 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/E.mligo#assert#982 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#assert_with_error#983 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/E.mligo#abs#988 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/E.mligo#is_nat#989 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/E.mligo#true#990 = TRUE()[@inline] in
let #../../test/contracts/build/E.mligo#false#991 = FALSE()[@inline] in
let #../../test/contracts/build/E.mligo#unit#992 = UNIT()[@inline] in
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
let #../../test/contracts/build/E.mligo#Test#originate_from_file#998 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/E.mligo#failwith_30)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_source#1000 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_baker#1001 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_baker_policy#1002 =
  fun _bp ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#transfer#1003 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#transfer_exn#1004 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#failwith_27)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_storage_of_address#1008 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_balance#1009 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_29)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#michelson_equal#1010 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_28)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#reset_state#1012 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#reset_state_at#1013 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_voting_power#1014 =
  fun _kh ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_27)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_total_voting_power#1015 =
  (poly_#../../test/contracts/build/E.mligo#failwith_27)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nth_bootstrap_contract#1017 =
  fun _i ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_19)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nth_bootstrap_account#1018 =
  fun _i ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_19)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#last_originations#1020 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_26)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#save_mutation#1023 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_25)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#add_account#1030 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#new_account#1031 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_24)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#baker_account#1032 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#bake_until_n_cycle_end#1033 =
  fun _n ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#register_delegate#1034 =
  fun _kh ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#register_constant#1035 =
  fun _m ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_23)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#create_chest#1040 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_22)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#create_chest_key#1041 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_21)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#constant_to_michelson_program#1042 =
  fun _s ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#restore_context#1043 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#save_context#1044 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#drop_context#1045 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#read_contract_from_file#1046 =
  fun _fn ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#compile_contract_from_file#1047 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/E.mligo#failwith_20)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#originate_contract#1049 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#failwith_19)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#size#1050 =
  fun _c ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_18)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_bootstrap_account#1051 =
  fun _n ->
  ((poly_#../../test/contracts/build/E.mligo#failwith_17)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#sign#1052 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/E.mligo#failwith_16)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/E.mligo#toto#1053 = L(10) in
let #../../test/contracts/build/E.mligo#foo#1054 = L("bar") in
let #Tezos#balance#1058 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #Tezos#amount#1059 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #Tezos#now#1060 = ({ DROP ; NOW })@(L(unit))[@inline] in
let #Tezos#sender#1061 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let #Tezos#source#1062 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #Tezos#level#1063 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #Tezos#self_address#1064 = SELF_ADDRESS()[@inline] in
let #Tezos#chain_id#1065 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #Tezos#total_voting_power#1066 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #Tezos#get_balance#1067 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #Tezos#get_amount#1068 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #Tezos#get_now#1069 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #Tezos#get_sender#1070 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #Tezos#get_source#1071 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #Tezos#get_level#1072 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #Tezos#get_self_address#1073 = fun _u -> (SELF_ADDRESS())[@inline] in
let #Tezos#get_chain_id#1074 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #Tezos#get_total_voting_power#1075 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #Tezos#voting_power#1076 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #Tezos#implicit_account#1078 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #Tezos#pairing_check#1084 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #Tezos#open_chest#1085 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #Tezos#set_delegate#1089 = fun o -> (SET_DELEGATE(o))[@inline] in
let #Bitwise#xor#1090 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #Bitwise#shift_left#1091 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #Bitwise#shift_right#1092 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #String#concat#1133 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #String#sub#1134 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #String#length#1135 = fun b -> (({ SIZE })@(b))[@inline] in
let #Bytes#concat#1138 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #Bytes#sub#1139 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #Bytes#length#1142 = fun b -> (({ SIZE })@(b))[@inline] in
let #Crypto#blake2b#1143 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let #Crypto#sha256#1144 = fun b -> (({ SHA256 })@(b))[@inline] in
let #Crypto#sha512#1145 = fun b -> (({ SHA512 })@(b))[@inline] in
let #Crypto#sha3#1146 = fun b -> (({ SHA3 })@(b))[@inline] in
let #Crypto#keccak#1147 = fun b -> (({ KECCAK })@(b))[@inline] in
let #Crypto#hash_key#1148 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let #Crypto#check#1149 =
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
let #Test#originate_from_file#1151 =
  fun _fn ->
  (fun _e ->
   (fun _v -> (fun _s -> (fun _t -> ((poly_failwith_15)@(L("TEST MODE")))))))[@inline] in
let #Test#set_source#1153 =
  fun _a -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
let #Test#set_baker#1154 =
  fun _a -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
let #Test#set_baker_policy#1155 =
  fun _bp -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
let #Test#transfer#1156 =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_5)@(L("TEST MODE")))))[@inline] in
let #Test#transfer_exn#1157 =
  fun _a -> (fun _s -> (fun _t -> ((poly_failwith_12)@(L("TEST MODE")))))[@inline] in
let #Test#get_storage_of_address#1161 =
  fun _a -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
let #Test#get_balance#1162 =
  fun _a -> ((poly_failwith_14)@(L("TEST MODE")))[@inline] in
let #Test#michelson_equal#1163 =
  fun _m1 -> (fun _m2 -> ((poly_failwith_13)@(L("TEST MODE"))))[@inline] in
let #Test#reset_state#1165 =
  fun _n -> (fun _l -> ((poly_failwith_5)@(L("TEST MODE"))))[@inline] in
let #Test#reset_state_at#1166 =
  fun _t -> (fun _n -> (fun _l -> ((poly_failwith_5)@(L("TEST MODE")))))[@inline] in
let #Test#get_voting_power#1167 =
  fun _kh -> ((poly_failwith_12)@(L("TEST MODE")))[@inline] in
let #Test#get_total_voting_power#1168 =
  (poly_failwith_12)@(L("TEST MODE"))[@inline] in
let #Test#nth_bootstrap_contract#1170 =
  fun _i -> ((poly_failwith_4)@(L("TEST MODE")))[@inline] in
let #Test#nth_bootstrap_account#1171 =
  fun _i -> ((poly_failwith_4)@(L("TEST MODE")))[@inline] in
let #Test#last_originations#1173 =
  fun _u -> ((poly_failwith_11)@(L("TEST MODE")))[@inline] in
let #Test#save_mutation#1176 =
  fun _s -> (fun _m -> ((poly_failwith_10)@(L("TEST MODE"))))[@inline] in
let #Test#add_account#1183 =
  fun _s -> (fun _k -> ((poly_failwith_5)@(L("TEST MODE"))))[@inline] in
let #Test#new_account#1184 =
  fun _u -> ((poly_failwith_9)@(L("TEST MODE")))[@inline] in
let #Test#baker_account#1185 =
  fun _p -> (fun _o -> ((poly_failwith_5)@(L("TEST MODE"))))[@inline] in
let #Test#bake_until_n_cycle_end#1186 =
  fun _n -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
let #Test#register_delegate#1187 =
  fun _kh -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
let #Test#register_constant#1188 =
  fun _m -> ((poly_failwith_8)@(L("TEST MODE")))[@inline] in
let #Test#create_chest#1193 =
  fun _b -> (fun _n -> ((poly_failwith_7)@(L("TEST MODE"))))[@inline] in
let #Test#create_chest_key#1194 =
  fun _c -> (fun _n -> ((poly_failwith_6)@(L("TEST MODE"))))[@inline] in
let #Test#constant_to_michelson_program#1195 =
  fun _s -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
let #Test#restore_context#1196 =
  fun _u -> ((poly_failwith_5)@(L("TEST_POP_CONTEXT")))[@inline] in
let #Test#save_context#1197 =
  fun _u -> ((poly_failwith_5)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #Test#drop_context#1198 =
  fun _u -> ((poly_failwith_5)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #Test#read_contract_from_file#1199 =
  fun _fn -> ((poly_failwith_5)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #Test#compile_contract_from_file#1200 =
  fun _fn ->
  (fun _e ->
   (fun _v -> ((poly_failwith_5)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #Test#originate_contract#1202 =
  fun _c -> (fun _s -> (fun _t -> ((poly_failwith_4)@(L("TEST_ORIGINATE")))))[@inline] in
let #Test#size#1203 =
  fun _c -> ((poly_failwith_3)@(L("TEST_SIZE")))[@inline] in
let #Test#get_bootstrap_account#1204 =
  fun _n -> ((poly_failwith_2)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #Test#sign#1205 =
  fun _sk -> (fun _d -> ((poly_failwith_1)@(L("TEST_SIGN"))))[@inline] in
let toto =
  ADD(#../../test/contracts/build/E.mligo#toto#1053 ,
      #../../test/contracts/build/A.mligo#toto#215) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#4205 ->
  (let (gen#4211, gen#4212) = gen#4205 in
   let p = gen#4211 in
   let s = gen#4212 in
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
