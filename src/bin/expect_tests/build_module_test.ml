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
let #../../test/contracts/build/A.mligo#Tezos#balance#94 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#amount#95 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#now#96 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#sender#97 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#source#98 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#level#99 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#self_address#100 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#chain_id#101 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#total_voting_power#102 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_balance#103 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_amount#104 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_now#105 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_sender#106 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_source#107 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_level#108 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_self_address#109 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_chain_id#110 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_total_voting_power#111 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#min_block_time#112 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_min_block_time#113 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#voting_power#114 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#implicit_account#116 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#pairing_check#122 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#open_chest#123 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#set_delegate#127 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#xor#128 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#shift_left#129 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#shift_right#130 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#String#concat#171 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#172 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/A.mligo#String#length#173 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#concat#176 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#sub#177 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#length#180 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#blake2b#181 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha256#182 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha512#183 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha3#184 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#keccak#185 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#hash_key#186 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#check#187 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/A.mligo#assert#188 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#assert_with_error#189 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/A.mligo#abs#194 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/A.mligo#is_nat#195 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/A.mligo#true#196 = TRUE()[@inline] in
let #../../test/contracts/build/A.mligo#false#197 = FALSE()[@inline] in
let #../../test/contracts/build/A.mligo#unit#198 = UNIT()[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5539 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5538 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5537 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5536 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5535 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5534 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5533 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5532 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5531 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5530 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5529 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5528 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5527 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5526 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_5525 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/A.mligo#Test#originate_from_file#204 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5539)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_source#206 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_baker#207 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_baker_policy#208 =
  fun _bp ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#transfer#209 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#transfer_exn#210 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5536)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_storage_of_address#214 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_balance#215 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5538)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#michelson_equal#216 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5537)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#reset_state#218 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#reset_state_at#219 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_voting_power#220 =
  fun _kh ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5536)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_total_voting_power#221 =
  (poly_#../../test/contracts/build/A.mligo#Test#failwith_5536)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nth_bootstrap_contract#223 =
  fun _i ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5528)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nth_bootstrap_account#224 =
  fun _i ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5528)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#last_originations#226 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5535)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#save_mutation#229 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5534)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#add_account#236 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#new_account#237 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5533)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#baker_account#238 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#bake_until_n_cycle_end#239 =
  fun _n ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#register_delegate#240 =
  fun _kh ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#register_constant#241 =
  fun _m ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5532)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#create_chest#246 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5531)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#create_chest_key#247 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5530)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#constant_to_michelson_program#248 =
  fun _s ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#restore_context#249 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#save_context#250 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#drop_context#251 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#read_contract_from_file#252 =
  fun _fn ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#compile_contract_from_file#253 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5529)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#originate_contract#255 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5528)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#size#256 =
  fun _c ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5527)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_bootstrap_account#257 =
  fun _n ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5526)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#sign#258 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_5525)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#balance#262 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#amount#263 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#now#264 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#sender#265 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#source#266 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#level#267 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#self_address#268 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#chain_id#269 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#total_voting_power#270 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_balance#271 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_amount#272 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_now#273 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_sender#274 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_source#275 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_level#276 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_self_address#277 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_chain_id#278 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_total_voting_power#279 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#voting_power#280 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#implicit_account#282 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#pairing_check#288 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#open_chest#289 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#set_delegate#293 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#xor#294 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#shift_left#295 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#shift_right#296 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#String#concat#337 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#338 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/A.mligo#String#length#339 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#concat#342 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#sub#343 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#length#346 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#blake2b#347 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha256#348 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha512#349 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha3#350 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#keccak#351 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#hash_key#352 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#check#353 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/A.mligo#assert#354 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#assert_with_error#355 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/A.mligo#abs#360 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/A.mligo#is_nat#361 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/A.mligo#true#362 = TRUE()[@inline] in
let #../../test/contracts/build/A.mligo#false#363 = FALSE()[@inline] in
let #../../test/contracts/build/A.mligo#unit#364 = UNIT()[@inline] in
let #../../test/contracts/build/A.mligo#toto#368 = L(1) in
let #../../test/contracts/build/B.mligo#Tezos#balance#372 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#amount#373 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#now#374 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#sender#375 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#source#376 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#level#377 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#self_address#378 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#chain_id#379 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#total_voting_power#380 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_balance#381 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_amount#382 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_now#383 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_sender#384 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_source#385 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_level#386 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_self_address#387 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_chain_id#388 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_total_voting_power#389 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#min_block_time#390 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_min_block_time#391 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#voting_power#392 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#implicit_account#394 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#pairing_check#400 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#open_chest#401 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#set_delegate#405 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#xor#406 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_left#407 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_right#408 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#449 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#450 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/B.mligo#String#length#451 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#454 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#455 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#length#458 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#459 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#460 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#461 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#462 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#463 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#464 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#465 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/B.mligo#assert#466 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#assert_with_error#467 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/B.mligo#abs#472 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/B.mligo#is_nat#473 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/B.mligo#true#474 = TRUE()[@inline] in
let #../../test/contracts/build/B.mligo#false#475 = FALSE()[@inline] in
let #../../test/contracts/build/B.mligo#unit#476 = UNIT()[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5524 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5523 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5522 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5521 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5520 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5519 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5518 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5517 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5516 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5515 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5514 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5513 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5512 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5511 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_5510 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/B.mligo#Test#originate_from_file#482 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5524)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_source#484 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_baker#485 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_baker_policy#486 =
  fun _bp ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#transfer#487 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#transfer_exn#488 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5521)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_storage_of_address#492 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_balance#493 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5523)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#michelson_equal#494 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5522)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#reset_state#496 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#reset_state_at#497 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_voting_power#498 =
  fun _kh ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5521)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_total_voting_power#499 =
  (poly_#../../test/contracts/build/B.mligo#Test#failwith_5521)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nth_bootstrap_contract#501 =
  fun _i ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5513)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nth_bootstrap_account#502 =
  fun _i ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5513)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#last_originations#504 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5520)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#save_mutation#507 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5519)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#add_account#514 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#new_account#515 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5518)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#baker_account#516 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#bake_until_n_cycle_end#517 =
  fun _n ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#register_delegate#518 =
  fun _kh ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#register_constant#519 =
  fun _m ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5517)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#create_chest#524 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5516)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#create_chest_key#525 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5515)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#constant_to_michelson_program#526 =
  fun _s ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#restore_context#527 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#save_context#528 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#drop_context#529 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#read_contract_from_file#530 =
  fun _fn ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#compile_contract_from_file#531 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5514)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#originate_contract#533 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5513)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#size#534 =
  fun _c ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5512)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_bootstrap_account#535 =
  fun _n ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5511)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#sign#536 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_5510)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#balance#540 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#amount#541 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#now#542 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#sender#543 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#source#544 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#level#545 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#self_address#546 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#chain_id#547 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#total_voting_power#548 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_balance#549 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_amount#550 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_now#551 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_sender#552 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_source#553 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_level#554 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_self_address#555 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_chain_id#556 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_total_voting_power#557 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#voting_power#558 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#implicit_account#560 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#pairing_check#566 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#open_chest#567 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#set_delegate#571 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#xor#572 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_left#573 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_right#574 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#615 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#616 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/B.mligo#String#length#617 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#620 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#621 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#length#624 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#625 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#626 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#627 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#628 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#629 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#630 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#631 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/B.mligo#assert#632 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#assert_with_error#633 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/B.mligo#abs#638 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/B.mligo#is_nat#639 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/B.mligo#true#640 = TRUE()[@inline] in
let #../../test/contracts/build/B.mligo#false#641 = FALSE()[@inline] in
let #../../test/contracts/build/B.mligo#unit#642 = UNIT()[@inline] in
let #../../test/contracts/build/B.mligo#toto#646 = L(32) in
let #../../test/contracts/build/B.mligo#titi#647 =
  ADD(#../../test/contracts/build/A.mligo#toto#368 , L(42)) in
let #../../test/contracts/build/B.mligo#f#648 =
  fun gen#3428 ->
  (let (gen#6975, gen#6976) = gen#3428 in
   let gen#3429 = gen#6975 in
   let x = gen#6976 in
   let x =
     ADD(ADD(x , #../../test/contracts/build/A.mligo#toto#368) ,
         #../../test/contracts/build/B.mligo#titi#647) in
   PAIR(LIST_EMPTY() , x)) in
let #../../test/contracts/build/F.mligo#Tezos#balance#652 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#amount#653 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#now#654 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#sender#655 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#source#656 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#level#657 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#self_address#658 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#chain_id#659 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#total_voting_power#660 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_balance#661 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_amount#662 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_now#663 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_sender#664 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_source#665 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_level#666 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_self_address#667 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_chain_id#668 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_total_voting_power#669 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#min_block_time#670 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_min_block_time#671 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#voting_power#672 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#implicit_account#674 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#pairing_check#680 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#open_chest#681 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#set_delegate#685 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#xor#686 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_left#687 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_right#688 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#729 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#730 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/F.mligo#String#length#731 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#734 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#735 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#length#738 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#739 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#740 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#741 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#742 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#743 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#744 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#745 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/F.mligo#assert#746 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#assert_with_error#747 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/F.mligo#abs#752 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/F.mligo#is_nat#753 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/F.mligo#true#754 = TRUE()[@inline] in
let #../../test/contracts/build/F.mligo#false#755 = FALSE()[@inline] in
let #../../test/contracts/build/F.mligo#unit#756 = UNIT()[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5509 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5508 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5507 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5506 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5505 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5504 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5503 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5502 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5501 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5500 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5499 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5498 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5497 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5496 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_5495 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/F.mligo#Test#originate_from_file#762 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5509)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_source#764 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_baker#765 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_baker_policy#766 =
  fun _bp ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#transfer#767 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#transfer_exn#768 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5506)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_storage_of_address#772 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_balance#773 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5508)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#michelson_equal#774 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5507)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#reset_state#776 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#reset_state_at#777 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_voting_power#778 =
  fun _kh ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5506)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_total_voting_power#779 =
  (poly_#../../test/contracts/build/F.mligo#Test#failwith_5506)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nth_bootstrap_contract#781 =
  fun _i ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5498)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nth_bootstrap_account#782 =
  fun _i ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5498)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#last_originations#784 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5505)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#save_mutation#787 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5504)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#add_account#794 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#new_account#795 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5503)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#baker_account#796 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#bake_until_n_cycle_end#797 =
  fun _n ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#register_delegate#798 =
  fun _kh ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#register_constant#799 =
  fun _m ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5502)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#create_chest#804 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5501)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#create_chest_key#805 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5500)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#constant_to_michelson_program#806 =
  fun _s ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#restore_context#807 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#save_context#808 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#drop_context#809 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#read_contract_from_file#810 =
  fun _fn ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#compile_contract_from_file#811 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5499)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#originate_contract#813 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5498)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#size#814 =
  fun _c ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5497)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_bootstrap_account#815 =
  fun _n ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5496)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#sign#816 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_5495)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#balance#820 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#amount#821 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#now#822 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#sender#823 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#source#824 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#level#825 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#self_address#826 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#chain_id#827 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#total_voting_power#828 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_balance#829 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_amount#830 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_now#831 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_sender#832 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_source#833 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_level#834 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_self_address#835 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_chain_id#836 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_total_voting_power#837 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#voting_power#838 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#implicit_account#840 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#pairing_check#846 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#open_chest#847 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#set_delegate#851 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#xor#852 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_left#853 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_right#854 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#895 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#896 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/F.mligo#String#length#897 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#900 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#901 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#length#904 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#905 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#906 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#907 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#908 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#909 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#910 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#911 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/F.mligo#assert#912 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#assert_with_error#913 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/F.mligo#abs#918 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/F.mligo#is_nat#919 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/F.mligo#true#920 = TRUE()[@inline] in
let #../../test/contracts/build/F.mligo#false#921 = FALSE()[@inline] in
let #../../test/contracts/build/F.mligo#unit#922 = UNIT()[@inline] in
let #../../test/contracts/build/F.mligo#toto#926 = L(44) in
let #../../test/contracts/build/G.mligo#Tezos#balance#930 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#amount#931 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#now#932 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#sender#933 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#source#934 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#level#935 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#self_address#936 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#chain_id#937 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#total_voting_power#938 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_balance#939 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_amount#940 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_now#941 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_sender#942 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_source#943 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_level#944 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_self_address#945 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_chain_id#946 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_total_voting_power#947 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#min_block_time#948 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_min_block_time#949 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#voting_power#950 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#implicit_account#952 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#pairing_check#958 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#open_chest#959 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#set_delegate#963 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#xor#964 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_left#965 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_right#966 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#1007 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#1008 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/G.mligo#String#length#1009 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#1012 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#1013 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#length#1016 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#1017 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#1018 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#1019 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#1020 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#1021 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#1022 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#1023 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/G.mligo#assert#1024 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#assert_with_error#1025 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/G.mligo#abs#1030 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/G.mligo#is_nat#1031 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/G.mligo#true#1032 = TRUE()[@inline] in
let #../../test/contracts/build/G.mligo#false#1033 = FALSE()[@inline] in
let #../../test/contracts/build/G.mligo#unit#1034 = UNIT()[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5494 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5493 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5492 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5491 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5490 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5489 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5488 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5487 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5486 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5485 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5484 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5483 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5482 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5481 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_5480 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/G.mligo#Test#originate_from_file#1040 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5494)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_source#1042 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_baker#1043 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_baker_policy#1044 =
  fun _bp ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#transfer#1045 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#transfer_exn#1046 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5491)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_storage_of_address#1050 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_balance#1051 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5493)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#michelson_equal#1052 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5492)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#reset_state#1054 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#reset_state_at#1055 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_voting_power#1056 =
  fun _kh ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5491)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_total_voting_power#1057 =
  (poly_#../../test/contracts/build/G.mligo#Test#failwith_5491)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nth_bootstrap_contract#1059 =
  fun _i ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5483)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nth_bootstrap_account#1060 =
  fun _i ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5483)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#last_originations#1062 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5490)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#save_mutation#1065 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5489)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#add_account#1072 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#new_account#1073 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5488)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#baker_account#1074 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#bake_until_n_cycle_end#1075 =
  fun _n ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#register_delegate#1076 =
  fun _kh ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#register_constant#1077 =
  fun _m ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5487)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#create_chest#1082 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5486)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#create_chest_key#1083 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5485)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#constant_to_michelson_program#1084 =
  fun _s ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#restore_context#1085 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#save_context#1086 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#drop_context#1087 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#read_contract_from_file#1088 =
  fun _fn ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#compile_contract_from_file#1089 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5484)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#originate_contract#1091 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5483)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#size#1092 =
  fun _c ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5482)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_bootstrap_account#1093 =
  fun _n ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5481)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#sign#1094 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_5480)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#balance#1098 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#amount#1099 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#now#1100 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#sender#1101 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#source#1102 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#level#1103 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#self_address#1104 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#chain_id#1105 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#total_voting_power#1106 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_balance#1107 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_amount#1108 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_now#1109 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_sender#1110 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_source#1111 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_level#1112 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_self_address#1113 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_chain_id#1114 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_total_voting_power#1115 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#voting_power#1116 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#implicit_account#1118 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#pairing_check#1124 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#open_chest#1125 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#set_delegate#1129 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#xor#1130 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_left#1131 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_right#1132 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#1173 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#1174 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/G.mligo#String#length#1175 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#1178 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#1179 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#length#1182 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#1183 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#1184 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#1185 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#1186 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#1187 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#1188 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#1189 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/G.mligo#assert#1190 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#assert_with_error#1191 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/G.mligo#abs#1196 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/G.mligo#is_nat#1197 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/G.mligo#true#1198 = TRUE()[@inline] in
let #../../test/contracts/build/G.mligo#false#1199 = FALSE()[@inline] in
let #../../test/contracts/build/G.mligo#unit#1200 = UNIT()[@inline] in
let #../../test/contracts/build/G.mligo#toto#1204 = L(43) in
let #../../test/contracts/build/C.mligo#Tezos#balance#1208 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#amount#1209 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#now#1210 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#sender#1211 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#source#1212 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#level#1213 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#self_address#1214 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#chain_id#1215 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#total_voting_power#1216 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_balance#1217 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_amount#1218 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_now#1219 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_sender#1220 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_source#1221 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_level#1222 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_self_address#1223 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_chain_id#1224 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_total_voting_power#1225 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#min_block_time#1226 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_min_block_time#1227 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#voting_power#1228 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#implicit_account#1230 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#pairing_check#1236 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#open_chest#1237 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#set_delegate#1241 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#xor#1242 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_left#1243 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_right#1244 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#1285 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#1286 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/C.mligo#String#length#1287 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#1290 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#1291 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#length#1294 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#1295 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#1296 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#1297 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#1298 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#1299 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#1300 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#1301 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/C.mligo#assert#1302 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#assert_with_error#1303 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/C.mligo#abs#1308 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/C.mligo#is_nat#1309 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/C.mligo#true#1310 = TRUE()[@inline] in
let #../../test/contracts/build/C.mligo#false#1311 = FALSE()[@inline] in
let #../../test/contracts/build/C.mligo#unit#1312 = UNIT()[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5479 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5478 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5477 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5476 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5475 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5474 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5473 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5472 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5471 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5470 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5469 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5468 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5467 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5466 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_5465 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/C.mligo#Test#originate_from_file#1318 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5479)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_source#1320 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_baker#1321 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_baker_policy#1322 =
  fun _bp ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#transfer#1323 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#transfer_exn#1324 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5476)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_storage_of_address#1328 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_balance#1329 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5478)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#michelson_equal#1330 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5477)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#reset_state#1332 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#reset_state_at#1333 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_voting_power#1334 =
  fun _kh ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5476)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_total_voting_power#1335 =
  (poly_#../../test/contracts/build/C.mligo#Test#failwith_5476)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nth_bootstrap_contract#1337 =
  fun _i ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5468)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nth_bootstrap_account#1338 =
  fun _i ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5468)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#last_originations#1340 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5475)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#save_mutation#1343 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5474)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#add_account#1350 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#new_account#1351 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5473)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#baker_account#1352 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#bake_until_n_cycle_end#1353 =
  fun _n ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#register_delegate#1354 =
  fun _kh ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#register_constant#1355 =
  fun _m ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5472)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#create_chest#1360 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5471)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#create_chest_key#1361 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5470)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#constant_to_michelson_program#1362 =
  fun _s ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#restore_context#1363 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#save_context#1364 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#drop_context#1365 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#read_contract_from_file#1366 =
  fun _fn ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#compile_contract_from_file#1367 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5469)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#originate_contract#1369 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5468)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#size#1370 =
  fun _c ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5467)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_bootstrap_account#1371 =
  fun _n ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5466)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#sign#1372 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_5465)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#balance#1376 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#amount#1377 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#now#1378 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#sender#1379 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#source#1380 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#level#1381 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#self_address#1382 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#chain_id#1383 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#total_voting_power#1384 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_balance#1385 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_amount#1386 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_now#1387 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_sender#1388 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_source#1389 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_level#1390 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_self_address#1391 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_chain_id#1392 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_total_voting_power#1393 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#voting_power#1394 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#implicit_account#1396 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#pairing_check#1402 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#open_chest#1403 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#set_delegate#1407 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#xor#1408 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_left#1409 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_right#1410 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#1451 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#1452 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/C.mligo#String#length#1453 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#1456 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#1457 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#length#1460 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#1461 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#1462 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#1463 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#1464 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#1465 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#1466 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#1467 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/C.mligo#assert#1468 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#assert_with_error#1469 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/C.mligo#abs#1474 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/C.mligo#is_nat#1475 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/C.mligo#true#1476 = TRUE()[@inline] in
let #../../test/contracts/build/C.mligo#false#1477 = FALSE()[@inline] in
let #../../test/contracts/build/C.mligo#unit#1478 = UNIT()[@inline] in
let #../../test/contracts/build/C.mligo#tata#1482 =
  ADD(#../../test/contracts/build/A.mligo#toto#368 ,
      #../../test/contracts/build/B.mligo#titi#647) in
let #../../test/contracts/build/C.mligo#foo#1483 =
  (#../../test/contracts/build/B.mligo#f#648)@(PAIR(L(unit) , L(3))) in
let #../../test/contracts/build/E.mligo#Tezos#balance#1487 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#amount#1488 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#now#1489 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#sender#1490 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#source#1491 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#level#1492 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#self_address#1493 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#chain_id#1494 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#total_voting_power#1495 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_balance#1496 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_amount#1497 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_now#1498 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_sender#1499 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_source#1500 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_level#1501 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_self_address#1502 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_chain_id#1503 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_total_voting_power#1504 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#min_block_time#1505 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_min_block_time#1506 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#voting_power#1507 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#implicit_account#1509 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#pairing_check#1515 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#open_chest#1516 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#set_delegate#1520 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#xor#1521 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_left#1522 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_right#1523 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#1564 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#1565 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/E.mligo#String#length#1566 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#1569 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#1570 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#length#1573 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#1574 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#1575 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#1576 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#1577 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#1578 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#1579 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#1580 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/E.mligo#assert#1581 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#assert_with_error#1582 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/E.mligo#abs#1587 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/E.mligo#is_nat#1588 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/E.mligo#true#1589 = TRUE()[@inline] in
let #../../test/contracts/build/E.mligo#false#1590 = FALSE()[@inline] in
let #../../test/contracts/build/E.mligo#unit#1591 = UNIT()[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5464 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5463 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5462 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5461 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5460 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5459 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5458 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5457 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5456 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5455 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5454 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5453 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5452 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5451 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_5450 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/E.mligo#Test#originate_from_file#1597 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5464)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_source#1599 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_baker#1600 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_baker_policy#1601 =
  fun _bp ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#transfer#1602 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#transfer_exn#1603 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5461)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_storage_of_address#1607 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_balance#1608 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5463)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#michelson_equal#1609 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5462)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#reset_state#1611 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#reset_state_at#1612 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_voting_power#1613 =
  fun _kh ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5461)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_total_voting_power#1614 =
  (poly_#../../test/contracts/build/E.mligo#Test#failwith_5461)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nth_bootstrap_contract#1616 =
  fun _i ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5453)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nth_bootstrap_account#1617 =
  fun _i ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5453)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#last_originations#1619 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5460)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#save_mutation#1622 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5459)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#add_account#1629 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#new_account#1630 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5458)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#baker_account#1631 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#bake_until_n_cycle_end#1632 =
  fun _n ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#register_delegate#1633 =
  fun _kh ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#register_constant#1634 =
  fun _m ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5457)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#create_chest#1639 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5456)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#create_chest_key#1640 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5455)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#constant_to_michelson_program#1641 =
  fun _s ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#restore_context#1642 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#save_context#1643 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#drop_context#1644 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#read_contract_from_file#1645 =
  fun _fn ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#compile_contract_from_file#1646 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5454)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#originate_contract#1648 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5453)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#size#1649 =
  fun _c ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5452)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_bootstrap_account#1650 =
  fun _n ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5451)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#sign#1651 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_5450)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#balance#1655 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#amount#1656 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#now#1657 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#sender#1658 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#source#1659 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#level#1660 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#self_address#1661 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#chain_id#1662 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#total_voting_power#1663 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_balance#1664 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_amount#1665 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_now#1666 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_sender#1667 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_source#1668 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_level#1669 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_self_address#1670 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_chain_id#1671 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_total_voting_power#1672 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#voting_power#1673 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#implicit_account#1675 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#pairing_check#1681 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#open_chest#1682 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#set_delegate#1686 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#xor#1687 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_left#1688 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_right#1689 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#1730 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#1731 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/E.mligo#String#length#1732 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#1735 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#1736 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#length#1739 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#1740 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#1741 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#1742 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#1743 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#1744 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#1745 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#1746 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/E.mligo#assert#1747 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#assert_with_error#1748 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/E.mligo#abs#1753 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/E.mligo#is_nat#1754 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/E.mligo#true#1755 = TRUE()[@inline] in
let #../../test/contracts/build/E.mligo#false#1756 = FALSE()[@inline] in
let #../../test/contracts/build/E.mligo#unit#1757 = UNIT()[@inline] in
let #../../test/contracts/build/E.mligo#toto#1761 = L(10) in
let #../../test/contracts/build/E.mligo#foo#1762 = L("bar") in
let #Tezos#balance#1766 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #Tezos#amount#1767 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #Tezos#now#1768 = ({ DROP ; NOW })@(L(unit))[@inline] in
let #Tezos#sender#1769 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let #Tezos#source#1770 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #Tezos#level#1771 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #Tezos#self_address#1772 = SELF_ADDRESS()[@inline] in
let #Tezos#chain_id#1773 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #Tezos#total_voting_power#1774 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #Tezos#get_balance#1775 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #Tezos#get_amount#1776 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #Tezos#get_now#1777 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #Tezos#get_sender#1778 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #Tezos#get_source#1779 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #Tezos#get_level#1780 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #Tezos#get_self_address#1781 = fun _u -> (SELF_ADDRESS())[@inline] in
let #Tezos#get_chain_id#1782 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #Tezos#get_total_voting_power#1783 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #Tezos#min_block_time#1784 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let #Tezos#get_min_block_time#1785 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let #Tezos#voting_power#1786 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #Tezos#implicit_account#1788 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #Tezos#pairing_check#1794 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #Tezos#open_chest#1795 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #Tezos#set_delegate#1799 = fun o -> (SET_DELEGATE(o))[@inline] in
let #Bitwise#xor#1800 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #Bitwise#shift_left#1801 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #Bitwise#shift_right#1802 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #String#concat#1843 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #String#sub#1844 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #String#length#1845 = fun b -> (({ SIZE })@(b))[@inline] in
let #Bytes#concat#1848 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #Bytes#sub#1849 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #Bytes#length#1852 = fun b -> (({ SIZE })@(b))[@inline] in
let #Crypto#blake2b#1853 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let #Crypto#sha256#1854 = fun b -> (({ SHA256 })@(b))[@inline] in
let #Crypto#sha512#1855 = fun b -> (({ SHA512 })@(b))[@inline] in
let #Crypto#sha3#1856 = fun b -> (({ SHA3 })@(b))[@inline] in
let #Crypto#keccak#1857 = fun b -> (({ KECCAK })@(b))[@inline] in
let #Crypto#hash_key#1858 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let #Crypto#check#1859 =
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
let poly_#Test#failwith_5449 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5448 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5447 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5446 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5445 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5444 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5443 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5442 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5441 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5440 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5439 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5438 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5437 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5436 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5435 = { FAILWITH }[@inline] in
let #Test#originate_from_file#1862 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s -> (fun _t -> ((poly_#Test#failwith_5449)@(L("TEST MODE")))))))[@inline] in
let #Test#set_source#1864 =
  fun _a -> ((poly_#Test#failwith_5439)@(L("TEST MODE")))[@inline] in
let #Test#set_baker#1865 =
  fun _a -> ((poly_#Test#failwith_5439)@(L("TEST MODE")))[@inline] in
let #Test#set_baker_policy#1866 =
  fun _bp -> ((poly_#Test#failwith_5439)@(L("TEST MODE")))[@inline] in
let #Test#transfer#1867 =
  fun _a ->
  (fun _s -> (fun _t -> ((poly_#Test#failwith_5439)@(L("TEST MODE")))))[@inline] in
let #Test#transfer_exn#1868 =
  fun _a ->
  (fun _s -> (fun _t -> ((poly_#Test#failwith_5446)@(L("TEST MODE")))))[@inline] in
let #Test#get_storage_of_address#1872 =
  fun _a -> ((poly_#Test#failwith_5439)@(L("TEST MODE")))[@inline] in
let #Test#get_balance#1873 =
  fun _a -> ((poly_#Test#failwith_5448)@(L("TEST MODE")))[@inline] in
let #Test#michelson_equal#1874 =
  fun _m1 -> (fun _m2 -> ((poly_#Test#failwith_5447)@(L("TEST MODE"))))[@inline] in
let #Test#reset_state#1876 =
  fun _n -> (fun _l -> ((poly_#Test#failwith_5439)@(L("TEST MODE"))))[@inline] in
let #Test#reset_state_at#1877 =
  fun _t ->
  (fun _n -> (fun _l -> ((poly_#Test#failwith_5439)@(L("TEST MODE")))))[@inline] in
let #Test#get_voting_power#1878 =
  fun _kh -> ((poly_#Test#failwith_5446)@(L("TEST MODE")))[@inline] in
let #Test#get_total_voting_power#1879 =
  (poly_#Test#failwith_5446)@(L("TEST MODE"))[@inline] in
let #Test#nth_bootstrap_contract#1881 =
  fun _i -> ((poly_#Test#failwith_5438)@(L("TEST MODE")))[@inline] in
let #Test#nth_bootstrap_account#1882 =
  fun _i -> ((poly_#Test#failwith_5438)@(L("TEST MODE")))[@inline] in
let #Test#last_originations#1884 =
  fun _u -> ((poly_#Test#failwith_5445)@(L("TEST MODE")))[@inline] in
let #Test#save_mutation#1887 =
  fun _s -> (fun _m -> ((poly_#Test#failwith_5444)@(L("TEST MODE"))))[@inline] in
let #Test#add_account#1894 =
  fun _s -> (fun _k -> ((poly_#Test#failwith_5439)@(L("TEST MODE"))))[@inline] in
let #Test#new_account#1895 =
  fun _u -> ((poly_#Test#failwith_5443)@(L("TEST MODE")))[@inline] in
let #Test#baker_account#1896 =
  fun _p -> (fun _o -> ((poly_#Test#failwith_5439)@(L("TEST MODE"))))[@inline] in
let #Test#bake_until_n_cycle_end#1897 =
  fun _n -> ((poly_#Test#failwith_5439)@(L("TEST MODE")))[@inline] in
let #Test#register_delegate#1898 =
  fun _kh -> ((poly_#Test#failwith_5439)@(L("TEST MODE")))[@inline] in
let #Test#register_constant#1899 =
  fun _m -> ((poly_#Test#failwith_5442)@(L("TEST MODE")))[@inline] in
let #Test#create_chest#1904 =
  fun _b -> (fun _n -> ((poly_#Test#failwith_5441)@(L("TEST MODE"))))[@inline] in
let #Test#create_chest_key#1905 =
  fun _c -> (fun _n -> ((poly_#Test#failwith_5440)@(L("TEST MODE"))))[@inline] in
let #Test#constant_to_michelson_program#1906 =
  fun _s -> ((poly_#Test#failwith_5439)@(L("TEST MODE")))[@inline] in
let #Test#restore_context#1907 =
  fun _u -> ((poly_#Test#failwith_5439)@(L("TEST_POP_CONTEXT")))[@inline] in
let #Test#save_context#1908 =
  fun _u -> ((poly_#Test#failwith_5439)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #Test#drop_context#1909 =
  fun _u -> ((poly_#Test#failwith_5439)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #Test#read_contract_from_file#1910 =
  fun _fn -> ((poly_#Test#failwith_5439)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #Test#compile_contract_from_file#1911 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#Test#failwith_5439)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #Test#originate_contract#1913 =
  fun _c ->
  (fun _s -> (fun _t -> ((poly_#Test#failwith_5438)@(L("TEST_ORIGINATE")))))[@inline] in
let #Test#size#1914 =
  fun _c -> ((poly_#Test#failwith_5437)@(L("TEST_SIZE")))[@inline] in
let #Test#get_bootstrap_account#1915 =
  fun _n -> ((poly_#Test#failwith_5436)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #Test#sign#1916 =
  fun _sk -> (fun _d -> ((poly_#Test#failwith_5435)@(L("TEST_SIGN"))))[@inline] in
let #Tezos#balance#1920 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #Tezos#amount#1921 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #Tezos#now#1922 = ({ DROP ; NOW })@(L(unit))[@inline] in
let #Tezos#sender#1923 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let #Tezos#source#1924 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #Tezos#level#1925 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #Tezos#self_address#1926 = SELF_ADDRESS()[@inline] in
let #Tezos#chain_id#1927 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #Tezos#total_voting_power#1928 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #Tezos#get_balance#1929 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #Tezos#get_amount#1930 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #Tezos#get_now#1931 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #Tezos#get_sender#1932 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #Tezos#get_source#1933 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #Tezos#get_level#1934 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #Tezos#get_self_address#1935 = fun _u -> (SELF_ADDRESS())[@inline] in
let #Tezos#get_chain_id#1936 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #Tezos#get_total_voting_power#1937 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #Tezos#voting_power#1938 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #Tezos#implicit_account#1940 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #Tezos#pairing_check#1946 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #Tezos#open_chest#1947 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #Tezos#set_delegate#1951 = fun o -> (SET_DELEGATE(o))[@inline] in
let #Bitwise#xor#1952 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #Bitwise#shift_left#1953 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #Bitwise#shift_right#1954 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #String#concat#1995 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #String#sub#1996 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #String#length#1997 = fun b -> (({ SIZE })@(b))[@inline] in
let #Bytes#concat#2000 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #Bytes#sub#2001 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #Bytes#length#2004 = fun b -> (({ SIZE })@(b))[@inline] in
let #Crypto#blake2b#2005 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let #Crypto#sha256#2006 = fun b -> (({ SHA256 })@(b))[@inline] in
let #Crypto#sha512#2007 = fun b -> (({ SHA512 })@(b))[@inline] in
let #Crypto#sha3#2008 = fun b -> (({ SHA3 })@(b))[@inline] in
let #Crypto#keccak#2009 = fun b -> (({ KECCAK })@(b))[@inline] in
let #Crypto#hash_key#2010 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let #Crypto#check#2011 =
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
  ADD(#../../test/contracts/build/E.mligo#toto#1761 ,
      #../../test/contracts/build/A.mligo#toto#368) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#6971 ->
  (let (gen#6977, gen#6978) = gen#6971 in
   let p = gen#6977 in
   let s = gen#6978 in
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
