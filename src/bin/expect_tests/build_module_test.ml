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
let poly_#../../test/contracts/build/A.mligo#Test#failwith_105 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_104 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_103 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_102 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_101 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_100 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_99 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_98 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_97 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_96 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_95 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_94 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_93 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_92 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_91 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/A.mligo#Test#originate_from_file#204 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/A.mligo#Test#failwith_105)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_source#206 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_baker#207 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_baker_policy#208 =
  fun _bp ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#transfer#209 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#transfer_exn#210 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_102)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_storage_of_address#214 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_balance#215 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_104)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#michelson_equal#216 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_103)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#reset_state#218 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#reset_state_at#219 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_voting_power#220 =
  fun _kh ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_102)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_total_voting_power#221 =
  (poly_#../../test/contracts/build/A.mligo#Test#failwith_102)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nth_bootstrap_contract#223 =
  fun _i ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_96)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nth_bootstrap_account#224 =
  fun _i ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_96)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#last_originations#226 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_101)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#save_mutation#229 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_92)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#add_account#236 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#new_account#237 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_100)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#baker_account#238 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#bake_until_n_cycle_end#239 =
  fun _n ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#register_delegate#240 =
  fun _kh ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#register_constant#241 =
  fun _m ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_99)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#create_chest#246 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_98)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#create_chest_key#247 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_97)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#constant_to_michelson_program#248 =
  fun _s ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#restore_context#249 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#save_context#250 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#drop_context#251 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#read_contract_from_file#252 =
  fun _fn ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#compile_contract_from_file#253 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#originate_contract#255 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_96)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#size#256 =
  fun _c ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_95)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_bootstrap_account#257 =
  fun _n ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_94)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#sign#258 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_93)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#chr#259 =
  fun _n ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_92)@(L("TEST_CHR")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nl#260 =
  L("NEWLINE")[@inline] in
let #../../test/contracts/build/A.mligo#Test#println#261 =
  fun _v ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST_PRINTLN")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#print#262 =
  fun _v ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST_PRINT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#eprint#263 =
  fun _v ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_91)@(L("TEST_EPRINTL")))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#balance#268 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#amount#269 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#now#270 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#sender#271 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#source#272 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#level#273 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#self_address#274 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#chain_id#275 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#total_voting_power#276 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_balance#277 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_amount#278 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_now#279 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_sender#280 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_source#281 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_level#282 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_self_address#283 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_chain_id#284 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#get_total_voting_power#285 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#voting_power#286 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#implicit_account#288 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#pairing_check#294 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#open_chest#295 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#set_delegate#299 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#xor#300 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#shift_left#301 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#shift_right#302 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#String#concat#343 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#344 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/A.mligo#String#length#345 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#concat#348 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#sub#349 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#length#352 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#blake2b#353 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha256#354 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha512#355 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha3#356 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#keccak#357 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#hash_key#358 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#check#359 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/A.mligo#assert#360 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/A.mligo#assert_with_error#361 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/A.mligo#abs#366 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/A.mligo#is_nat#367 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/A.mligo#true#368 = TRUE()[@inline] in
let #../../test/contracts/build/A.mligo#false#369 = FALSE()[@inline] in
let #../../test/contracts/build/A.mligo#unit#370 = UNIT()[@inline] in
let #../../test/contracts/build/A.mligo#toto#374 = L(1) in
let #../../test/contracts/build/B.mligo#Tezos#balance#378 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#amount#379 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#now#380 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#sender#381 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#source#382 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#level#383 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#self_address#384 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#chain_id#385 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#total_voting_power#386 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_balance#387 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_amount#388 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_now#389 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_sender#390 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_source#391 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_level#392 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_self_address#393 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_chain_id#394 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_total_voting_power#395 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#min_block_time#396 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_min_block_time#397 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#voting_power#398 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#implicit_account#400 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#pairing_check#406 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#open_chest#407 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#set_delegate#411 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#xor#412 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_left#413 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_right#414 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#455 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#456 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/B.mligo#String#length#457 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#460 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#461 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#length#464 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#465 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#466 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#467 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#468 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#469 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#470 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#471 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/B.mligo#assert#472 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#assert_with_error#473 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/B.mligo#abs#478 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/B.mligo#is_nat#479 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/B.mligo#true#480 = TRUE()[@inline] in
let #../../test/contracts/build/B.mligo#false#481 = FALSE()[@inline] in
let #../../test/contracts/build/B.mligo#unit#482 = UNIT()[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_90 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_89 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_88 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_87 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_86 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_85 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_84 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_83 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_82 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_81 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_80 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_79 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_78 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_77 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_76 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/B.mligo#Test#originate_from_file#488 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/B.mligo#Test#failwith_90)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_source#490 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_baker#491 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_baker_policy#492 =
  fun _bp ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#transfer#493 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#transfer_exn#494 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_87)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_storage_of_address#498 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_balance#499 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_89)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#michelson_equal#500 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_88)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#reset_state#502 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#reset_state_at#503 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_voting_power#504 =
  fun _kh ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_87)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_total_voting_power#505 =
  (poly_#../../test/contracts/build/B.mligo#Test#failwith_87)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nth_bootstrap_contract#507 =
  fun _i ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_81)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nth_bootstrap_account#508 =
  fun _i ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_81)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#last_originations#510 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_86)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#save_mutation#513 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_77)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#add_account#520 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#new_account#521 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_85)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#baker_account#522 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#bake_until_n_cycle_end#523 =
  fun _n ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#register_delegate#524 =
  fun _kh ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#register_constant#525 =
  fun _m ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_84)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#create_chest#530 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_83)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#create_chest_key#531 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_82)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#constant_to_michelson_program#532 =
  fun _s ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#restore_context#533 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#save_context#534 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#drop_context#535 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#read_contract_from_file#536 =
  fun _fn ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#compile_contract_from_file#537 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#originate_contract#539 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_81)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#size#540 =
  fun _c ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_80)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_bootstrap_account#541 =
  fun _n ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_79)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#sign#542 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_78)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#chr#543 =
  fun _n ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_77)@(L("TEST_CHR")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nl#544 =
  L("NEWLINE")[@inline] in
let #../../test/contracts/build/B.mligo#Test#println#545 =
  fun _v ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST_PRINTLN")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#print#546 =
  fun _v ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST_PRINT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#eprint#547 =
  fun _v ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_76)@(L("TEST_EPRINTL")))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#balance#552 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#amount#553 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#now#554 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#sender#555 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#source#556 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#level#557 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#self_address#558 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#chain_id#559 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#total_voting_power#560 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_balance#561 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_amount#562 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_now#563 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_sender#564 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_source#565 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_level#566 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_self_address#567 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_chain_id#568 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#get_total_voting_power#569 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#voting_power#570 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#implicit_account#572 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#pairing_check#578 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#open_chest#579 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#set_delegate#583 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#xor#584 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_left#585 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_right#586 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#627 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#628 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/B.mligo#String#length#629 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#632 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#633 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#length#636 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#637 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#638 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#639 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#640 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#641 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#642 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#643 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/B.mligo#assert#644 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/B.mligo#assert_with_error#645 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/B.mligo#abs#650 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/B.mligo#is_nat#651 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/B.mligo#true#652 = TRUE()[@inline] in
let #../../test/contracts/build/B.mligo#false#653 = FALSE()[@inline] in
let #../../test/contracts/build/B.mligo#unit#654 = UNIT()[@inline] in
let #../../test/contracts/build/B.mligo#toto#658 = L(32) in
let #../../test/contracts/build/B.mligo#titi#659 =
  ADD(#../../test/contracts/build/A.mligo#toto#374 , L(42)) in
let #../../test/contracts/build/B.mligo#f#660 =
  fun gen#3492 ->
  (let (gen#7094, gen#7095) = gen#3492 in
   let gen#3493 = gen#7094 in
   let x = gen#7095 in
   let x =
     ADD(ADD(x , #../../test/contracts/build/A.mligo#toto#374) ,
         #../../test/contracts/build/B.mligo#titi#659) in
   PAIR(LIST_EMPTY() , x)) in
let #../../test/contracts/build/F.mligo#Tezos#balance#664 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#amount#665 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#now#666 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#sender#667 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#source#668 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#level#669 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#self_address#670 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#chain_id#671 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#total_voting_power#672 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_balance#673 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_amount#674 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_now#675 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_sender#676 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_source#677 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_level#678 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_self_address#679 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_chain_id#680 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_total_voting_power#681 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#min_block_time#682 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_min_block_time#683 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#voting_power#684 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#implicit_account#686 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#pairing_check#692 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#open_chest#693 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#set_delegate#697 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#xor#698 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_left#699 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_right#700 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#741 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#742 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/F.mligo#String#length#743 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#746 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#747 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#length#750 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#751 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#752 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#753 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#754 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#755 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#756 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#757 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/F.mligo#assert#758 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#assert_with_error#759 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/F.mligo#abs#764 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/F.mligo#is_nat#765 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/F.mligo#true#766 = TRUE()[@inline] in
let #../../test/contracts/build/F.mligo#false#767 = FALSE()[@inline] in
let #../../test/contracts/build/F.mligo#unit#768 = UNIT()[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_75 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_74 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_73 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_72 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_71 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_70 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_69 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_68 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_67 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_66 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_65 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_64 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_63 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_62 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_61 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/F.mligo#Test#originate_from_file#774 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/F.mligo#Test#failwith_75)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_source#776 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_baker#777 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_baker_policy#778 =
  fun _bp ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#transfer#779 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#transfer_exn#780 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_72)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_storage_of_address#784 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_balance#785 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_74)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#michelson_equal#786 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_73)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#reset_state#788 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#reset_state_at#789 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_voting_power#790 =
  fun _kh ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_72)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_total_voting_power#791 =
  (poly_#../../test/contracts/build/F.mligo#Test#failwith_72)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nth_bootstrap_contract#793 =
  fun _i ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_66)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nth_bootstrap_account#794 =
  fun _i ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_66)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#last_originations#796 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_71)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#save_mutation#799 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_62)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#add_account#806 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#new_account#807 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_70)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#baker_account#808 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#bake_until_n_cycle_end#809 =
  fun _n ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#register_delegate#810 =
  fun _kh ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#register_constant#811 =
  fun _m ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_69)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#create_chest#816 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_68)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#create_chest_key#817 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_67)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#constant_to_michelson_program#818 =
  fun _s ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#restore_context#819 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#save_context#820 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#drop_context#821 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#read_contract_from_file#822 =
  fun _fn ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#compile_contract_from_file#823 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#originate_contract#825 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_66)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#size#826 =
  fun _c ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_65)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_bootstrap_account#827 =
  fun _n ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_64)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#sign#828 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_63)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#chr#829 =
  fun _n ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_62)@(L("TEST_CHR")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nl#830 =
  L("NEWLINE")[@inline] in
let #../../test/contracts/build/F.mligo#Test#println#831 =
  fun _v ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST_PRINTLN")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#print#832 =
  fun _v ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST_PRINT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#eprint#833 =
  fun _v ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_61)@(L("TEST_EPRINTL")))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#balance#838 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#amount#839 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#now#840 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#sender#841 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#source#842 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#level#843 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#self_address#844 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#chain_id#845 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#total_voting_power#846 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_balance#847 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_amount#848 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_now#849 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_sender#850 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_source#851 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_level#852 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_self_address#853 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_chain_id#854 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#get_total_voting_power#855 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#voting_power#856 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#implicit_account#858 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#pairing_check#864 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#open_chest#865 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#set_delegate#869 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#xor#870 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_left#871 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_right#872 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#913 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#914 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/F.mligo#String#length#915 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#918 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#919 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#length#922 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#923 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#924 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#925 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#926 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#927 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#928 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#929 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/F.mligo#assert#930 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/F.mligo#assert_with_error#931 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/F.mligo#abs#936 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/F.mligo#is_nat#937 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/F.mligo#true#938 = TRUE()[@inline] in
let #../../test/contracts/build/F.mligo#false#939 = FALSE()[@inline] in
let #../../test/contracts/build/F.mligo#unit#940 = UNIT()[@inline] in
let #../../test/contracts/build/F.mligo#toto#944 = L(44) in
let #../../test/contracts/build/G.mligo#Tezos#balance#948 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#amount#949 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#now#950 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#sender#951 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#source#952 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#level#953 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#self_address#954 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#chain_id#955 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#total_voting_power#956 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_balance#957 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_amount#958 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_now#959 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_sender#960 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_source#961 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_level#962 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_self_address#963 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_chain_id#964 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_total_voting_power#965 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#min_block_time#966 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_min_block_time#967 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#voting_power#968 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#implicit_account#970 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#pairing_check#976 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#open_chest#977 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#set_delegate#981 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#xor#982 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_left#983 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_right#984 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#1025 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#1026 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/G.mligo#String#length#1027 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#1030 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#1031 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#length#1034 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#1035 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#1036 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#1037 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#1038 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#1039 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#1040 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#1041 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/G.mligo#assert#1042 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#assert_with_error#1043 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/G.mligo#abs#1048 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/G.mligo#is_nat#1049 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/G.mligo#true#1050 = TRUE()[@inline] in
let #../../test/contracts/build/G.mligo#false#1051 = FALSE()[@inline] in
let #../../test/contracts/build/G.mligo#unit#1052 = UNIT()[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_60 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_59 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_58 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_57 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_56 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_55 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_54 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_53 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_52 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_51 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_50 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_49 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_48 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_47 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_46 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/G.mligo#Test#originate_from_file#1058 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/G.mligo#Test#failwith_60)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_source#1060 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_baker#1061 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_baker_policy#1062 =
  fun _bp ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#transfer#1063 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#transfer_exn#1064 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_57)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_storage_of_address#1068 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_balance#1069 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_59)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#michelson_equal#1070 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_58)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#reset_state#1072 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#reset_state_at#1073 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_voting_power#1074 =
  fun _kh ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_57)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_total_voting_power#1075 =
  (poly_#../../test/contracts/build/G.mligo#Test#failwith_57)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nth_bootstrap_contract#1077 =
  fun _i ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_51)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nth_bootstrap_account#1078 =
  fun _i ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_51)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#last_originations#1080 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_56)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#save_mutation#1083 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_47)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#add_account#1090 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#new_account#1091 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_55)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#baker_account#1092 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#bake_until_n_cycle_end#1093 =
  fun _n ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#register_delegate#1094 =
  fun _kh ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#register_constant#1095 =
  fun _m ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_54)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#create_chest#1100 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_53)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#create_chest_key#1101 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_52)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#constant_to_michelson_program#1102 =
  fun _s ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#restore_context#1103 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#save_context#1104 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#drop_context#1105 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#read_contract_from_file#1106 =
  fun _fn ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#compile_contract_from_file#1107 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#originate_contract#1109 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_51)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#size#1110 =
  fun _c ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_50)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_bootstrap_account#1111 =
  fun _n ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_49)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#sign#1112 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_48)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#chr#1113 =
  fun _n ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_47)@(L("TEST_CHR")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nl#1114 =
  L("NEWLINE")[@inline] in
let #../../test/contracts/build/G.mligo#Test#println#1115 =
  fun _v ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST_PRINTLN")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#print#1116 =
  fun _v ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST_PRINT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#eprint#1117 =
  fun _v ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_46)@(L("TEST_EPRINTL")))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#balance#1122 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#amount#1123 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#now#1124 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#sender#1125 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#source#1126 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#level#1127 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#self_address#1128 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#chain_id#1129 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#total_voting_power#1130 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_balance#1131 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_amount#1132 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_now#1133 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_sender#1134 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_source#1135 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_level#1136 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_self_address#1137 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_chain_id#1138 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#get_total_voting_power#1139 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#voting_power#1140 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#implicit_account#1142 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#pairing_check#1148 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#open_chest#1149 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#set_delegate#1153 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#xor#1154 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_left#1155 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_right#1156 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#1197 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#1198 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/G.mligo#String#length#1199 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#1202 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#1203 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#length#1206 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#1207 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#1208 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#1209 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#1210 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#1211 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#1212 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#1213 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/G.mligo#assert#1214 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/G.mligo#assert_with_error#1215 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/G.mligo#abs#1220 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/G.mligo#is_nat#1221 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/G.mligo#true#1222 = TRUE()[@inline] in
let #../../test/contracts/build/G.mligo#false#1223 = FALSE()[@inline] in
let #../../test/contracts/build/G.mligo#unit#1224 = UNIT()[@inline] in
let #../../test/contracts/build/G.mligo#toto#1228 = L(43) in
let #../../test/contracts/build/C.mligo#Tezos#balance#1232 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#amount#1233 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#now#1234 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#sender#1235 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#source#1236 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#level#1237 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#self_address#1238 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#chain_id#1239 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#total_voting_power#1240 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_balance#1241 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_amount#1242 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_now#1243 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_sender#1244 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_source#1245 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_level#1246 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_self_address#1247 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_chain_id#1248 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_total_voting_power#1249 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#min_block_time#1250 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_min_block_time#1251 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#voting_power#1252 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#implicit_account#1254 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#pairing_check#1260 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#open_chest#1261 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#set_delegate#1265 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#xor#1266 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_left#1267 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_right#1268 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#1309 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#1310 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/C.mligo#String#length#1311 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#1314 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#1315 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#length#1318 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#1319 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#1320 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#1321 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#1322 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#1323 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#1324 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#1325 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/C.mligo#assert#1326 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#assert_with_error#1327 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/C.mligo#abs#1332 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/C.mligo#is_nat#1333 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/C.mligo#true#1334 = TRUE()[@inline] in
let #../../test/contracts/build/C.mligo#false#1335 = FALSE()[@inline] in
let #../../test/contracts/build/C.mligo#unit#1336 = UNIT()[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_45 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_44 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_43 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_42 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_41 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_40 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_39 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_38 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_37 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_36 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_35 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_34 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_33 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_32 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_31 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/C.mligo#Test#originate_from_file#1342 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/C.mligo#Test#failwith_45)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_source#1344 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_baker#1345 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_baker_policy#1346 =
  fun _bp ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#transfer#1347 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#transfer_exn#1348 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_42)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_storage_of_address#1352 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_balance#1353 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_44)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#michelson_equal#1354 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_43)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#reset_state#1356 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#reset_state_at#1357 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_voting_power#1358 =
  fun _kh ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_42)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_total_voting_power#1359 =
  (poly_#../../test/contracts/build/C.mligo#Test#failwith_42)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nth_bootstrap_contract#1361 =
  fun _i ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_36)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nth_bootstrap_account#1362 =
  fun _i ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_36)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#last_originations#1364 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_41)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#save_mutation#1367 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_32)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#add_account#1374 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#new_account#1375 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_40)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#baker_account#1376 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#bake_until_n_cycle_end#1377 =
  fun _n ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#register_delegate#1378 =
  fun _kh ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#register_constant#1379 =
  fun _m ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_39)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#create_chest#1384 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_38)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#create_chest_key#1385 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_37)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#constant_to_michelson_program#1386 =
  fun _s ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#restore_context#1387 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#save_context#1388 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#drop_context#1389 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#read_contract_from_file#1390 =
  fun _fn ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#compile_contract_from_file#1391 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#originate_contract#1393 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_36)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#size#1394 =
  fun _c ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_35)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_bootstrap_account#1395 =
  fun _n ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_34)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#sign#1396 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_33)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#chr#1397 =
  fun _n ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_32)@(L("TEST_CHR")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nl#1398 =
  L("NEWLINE")[@inline] in
let #../../test/contracts/build/C.mligo#Test#println#1399 =
  fun _v ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST_PRINTLN")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#print#1400 =
  fun _v ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST_PRINT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#eprint#1401 =
  fun _v ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_31)@(L("TEST_EPRINTL")))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#balance#1406 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#amount#1407 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#now#1408 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#sender#1409 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#source#1410 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#level#1411 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#self_address#1412 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#chain_id#1413 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#total_voting_power#1414 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_balance#1415 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_amount#1416 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_now#1417 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_sender#1418 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_source#1419 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_level#1420 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_self_address#1421 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_chain_id#1422 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#get_total_voting_power#1423 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#voting_power#1424 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#implicit_account#1426 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#pairing_check#1432 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#open_chest#1433 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#set_delegate#1437 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#xor#1438 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_left#1439 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_right#1440 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#1481 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#1482 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/C.mligo#String#length#1483 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#1486 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#1487 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#length#1490 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#1491 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#1492 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#1493 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#1494 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#1495 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#1496 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#1497 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/C.mligo#assert#1498 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/C.mligo#assert_with_error#1499 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/C.mligo#abs#1504 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/C.mligo#is_nat#1505 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/C.mligo#true#1506 = TRUE()[@inline] in
let #../../test/contracts/build/C.mligo#false#1507 = FALSE()[@inline] in
let #../../test/contracts/build/C.mligo#unit#1508 = UNIT()[@inline] in
let #../../test/contracts/build/C.mligo#tata#1512 =
  ADD(#../../test/contracts/build/A.mligo#toto#374 ,
      #../../test/contracts/build/B.mligo#titi#659) in
let #../../test/contracts/build/C.mligo#foo#1513 =
  (#../../test/contracts/build/B.mligo#f#660)@(PAIR(L(unit) , L(3))) in
let #../../test/contracts/build/E.mligo#Tezos#balance#1517 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#amount#1518 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#now#1519 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#sender#1520 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#source#1521 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#level#1522 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#self_address#1523 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#chain_id#1524 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#total_voting_power#1525 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_balance#1526 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_amount#1527 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_now#1528 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_sender#1529 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_source#1530 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_level#1531 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_self_address#1532 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_chain_id#1533 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_total_voting_power#1534 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#min_block_time#1535 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_min_block_time#1536 =
  { DROP ; MIN_BLOCK_TIME }[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#voting_power#1537 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#implicit_account#1539 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#pairing_check#1545 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#open_chest#1546 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#set_delegate#1550 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#xor#1551 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_left#1552 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_right#1553 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#1594 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#1595 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/E.mligo#String#length#1596 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#1599 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#1600 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#length#1603 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#1604 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#1605 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#1606 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#1607 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#1608 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#1609 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#1610 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/E.mligo#assert#1611 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#assert_with_error#1612 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/E.mligo#abs#1617 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/E.mligo#is_nat#1618 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/E.mligo#true#1619 = TRUE()[@inline] in
let #../../test/contracts/build/E.mligo#false#1620 = FALSE()[@inline] in
let #../../test/contracts/build/E.mligo#unit#1621 = UNIT()[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_30 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_29 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_28 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_27 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_26 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_25 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_24 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_23 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_22 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_21 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_20 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_19 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_18 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_17 =
  { FAILWITH }[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_16 =
  { FAILWITH }[@inline] in
let #../../test/contracts/build/E.mligo#Test#originate_from_file#1627 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/E.mligo#Test#failwith_30)@(L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_source#1629 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_baker#1630 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_baker_policy#1631 =
  fun _bp ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#transfer#1632 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#transfer_exn#1633 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_27)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_storage_of_address#1637 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_balance#1638 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_29)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#michelson_equal#1639 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_28)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#reset_state#1641 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#reset_state_at#1642 =
  fun _t ->
  (fun _n ->
   (fun _l ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_voting_power#1643 =
  fun _kh ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_27)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_total_voting_power#1644 =
  (poly_#../../test/contracts/build/E.mligo#Test#failwith_27)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nth_bootstrap_contract#1646 =
  fun _i ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_21)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nth_bootstrap_account#1647 =
  fun _i ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_21)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#last_originations#1649 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_26)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#save_mutation#1652 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_17)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#add_account#1659 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#new_account#1660 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_25)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#baker_account#1661 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#bake_until_n_cycle_end#1662 =
  fun _n ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#register_delegate#1663 =
  fun _kh ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#register_constant#1664 =
  fun _m ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_24)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#create_chest#1669 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_23)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#create_chest_key#1670 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_22)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#constant_to_michelson_program#1671 =
  fun _s ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#restore_context#1672 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#save_context#1673 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#drop_context#1674 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#read_contract_from_file#1675 =
  fun _fn ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#compile_contract_from_file#1676 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#originate_contract#1678 =
  fun _c ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_21)@(L("TEST_ORIGINATE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#size#1679 =
  fun _c ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_20)@(L("TEST_SIZE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_bootstrap_account#1680 =
  fun _n ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_19)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#sign#1681 =
  fun _sk ->
  (fun _d ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_18)@(L("TEST_SIGN"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#chr#1682 =
  fun _n ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_17)@(L("TEST_CHR")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nl#1683 =
  L("NEWLINE")[@inline] in
let #../../test/contracts/build/E.mligo#Test#println#1684 =
  fun _v ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST_PRINTLN")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#print#1685 =
  fun _v ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST_PRINT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#eprint#1686 =
  fun _v ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_16)@(L("TEST_EPRINTL")))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#balance#1691 =
  ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#amount#1692 =
  ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#now#1693 =
  ({ DROP ; NOW })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#sender#1694 =
  ({ DROP ; SENDER })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#source#1695 =
  ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#level#1696 =
  ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#self_address#1697 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#chain_id#1698 =
  ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#total_voting_power#1699 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_balance#1700 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_amount#1701 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_now#1702 =
  fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_sender#1703 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_source#1704 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_level#1705 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_self_address#1706 =
  fun _u -> (SELF_ADDRESS())[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_chain_id#1707 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#get_total_voting_power#1708 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#voting_power#1709 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#implicit_account#1711 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#pairing_check#1717 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#open_chest#1718 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#set_delegate#1722 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#xor#1723 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_left#1724 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_right#1725 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#1766 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#1767 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/E.mligo#String#length#1768 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#1771 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#1772 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#length#1775 =
  fun b -> (({ SIZE })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#1776 =
  fun b -> (({ BLAKE2B })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#1777 =
  fun b -> (({ SHA256 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#1778 =
  fun b -> (({ SHA512 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#1779 =
  fun b -> (({ SHA3 })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#1780 =
  fun b -> (({ KECCAK })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#1781 =
  fun k -> (({ HASH_KEY })@(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#1782 =
  fun k ->
  (fun s ->
   (fun b ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))))[@inline] in
let #../../test/contracts/build/E.mligo#assert#1783 =
  fun b ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
let #../../test/contracts/build/E.mligo#assert_with_error#1784 =
  fun b ->
  (fun s -> (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s))))[@inline] in
let #../../test/contracts/build/E.mligo#abs#1789 =
  fun i -> (({ ABS })@(i))[@inline] in
let #../../test/contracts/build/E.mligo#is_nat#1790 =
  fun i -> (({ ISNAT })@(i))[@inline] in
let #../../test/contracts/build/E.mligo#true#1791 = TRUE()[@inline] in
let #../../test/contracts/build/E.mligo#false#1792 = FALSE()[@inline] in
let #../../test/contracts/build/E.mligo#unit#1793 = UNIT()[@inline] in
let #../../test/contracts/build/E.mligo#toto#1797 = L(10) in
let #../../test/contracts/build/E.mligo#foo#1798 = L("bar") in
let #Tezos#balance#1802 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #Tezos#amount#1803 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #Tezos#now#1804 = ({ DROP ; NOW })@(L(unit))[@inline] in
let #Tezos#sender#1805 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let #Tezos#source#1806 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #Tezos#level#1807 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #Tezos#self_address#1808 = SELF_ADDRESS()[@inline] in
let #Tezos#chain_id#1809 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #Tezos#total_voting_power#1810 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #Tezos#get_balance#1811 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #Tezos#get_amount#1812 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #Tezos#get_now#1813 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #Tezos#get_sender#1814 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #Tezos#get_source#1815 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #Tezos#get_level#1816 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #Tezos#get_self_address#1817 = fun _u -> (SELF_ADDRESS())[@inline] in
let #Tezos#get_chain_id#1818 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #Tezos#get_total_voting_power#1819 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #Tezos#min_block_time#1820 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let #Tezos#get_min_block_time#1821 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let #Tezos#voting_power#1822 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #Tezos#implicit_account#1824 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #Tezos#pairing_check#1830 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #Tezos#open_chest#1831 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #Tezos#set_delegate#1835 = fun o -> (SET_DELEGATE(o))[@inline] in
let #Bitwise#xor#1836 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #Bitwise#shift_left#1837 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #Bitwise#shift_right#1838 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #String#concat#1879 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #String#sub#1880 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #String#length#1881 = fun b -> (({ SIZE })@(b))[@inline] in
let #Bytes#concat#1884 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #Bytes#sub#1885 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #Bytes#length#1888 = fun b -> (({ SIZE })@(b))[@inline] in
let #Crypto#blake2b#1889 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let #Crypto#sha256#1890 = fun b -> (({ SHA256 })@(b))[@inline] in
let #Crypto#sha512#1891 = fun b -> (({ SHA512 })@(b))[@inline] in
let #Crypto#sha3#1892 = fun b -> (({ SHA3 })@(b))[@inline] in
let #Crypto#keccak#1893 = fun b -> (({ KECCAK })@(b))[@inline] in
let #Crypto#hash_key#1894 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let #Crypto#check#1895 =
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
let poly_#Test#failwith_15 = { FAILWITH }[@inline] in
let poly_#Test#failwith_14 = { FAILWITH }[@inline] in
let poly_#Test#failwith_13 = { FAILWITH }[@inline] in
let poly_#Test#failwith_12 = { FAILWITH }[@inline] in
let poly_#Test#failwith_11 = { FAILWITH }[@inline] in
let poly_#Test#failwith_10 = { FAILWITH }[@inline] in
let poly_#Test#failwith_9 = { FAILWITH }[@inline] in
let poly_#Test#failwith_8 = { FAILWITH }[@inline] in
let poly_#Test#failwith_7 = { FAILWITH }[@inline] in
let poly_#Test#failwith_6 = { FAILWITH }[@inline] in
let poly_#Test#failwith_5 = { FAILWITH }[@inline] in
let poly_#Test#failwith_4 = { FAILWITH }[@inline] in
let poly_#Test#failwith_3 = { FAILWITH }[@inline] in
let poly_#Test#failwith_2 = { FAILWITH }[@inline] in
let poly_#Test#failwith_1 = { FAILWITH }[@inline] in
let #Test#originate_from_file#1898 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s -> (fun _t -> ((poly_#Test#failwith_15)@(L("TEST MODE")))))))[@inline] in
let #Test#set_source#1900 =
  fun _a -> ((poly_#Test#failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#set_baker#1901 =
  fun _a -> ((poly_#Test#failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#set_baker_policy#1902 =
  fun _bp -> ((poly_#Test#failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#transfer#1903 =
  fun _a ->
  (fun _s -> (fun _t -> ((poly_#Test#failwith_1)@(L("TEST MODE")))))[@inline] in
let #Test#transfer_exn#1904 =
  fun _a ->
  (fun _s -> (fun _t -> ((poly_#Test#failwith_12)@(L("TEST MODE")))))[@inline] in
let #Test#get_storage_of_address#1908 =
  fun _a -> ((poly_#Test#failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#get_balance#1909 =
  fun _a -> ((poly_#Test#failwith_14)@(L("TEST MODE")))[@inline] in
let #Test#michelson_equal#1910 =
  fun _m1 -> (fun _m2 -> ((poly_#Test#failwith_13)@(L("TEST MODE"))))[@inline] in
let #Test#reset_state#1912 =
  fun _n -> (fun _l -> ((poly_#Test#failwith_1)@(L("TEST MODE"))))[@inline] in
let #Test#reset_state_at#1913 =
  fun _t ->
  (fun _n -> (fun _l -> ((poly_#Test#failwith_1)@(L("TEST MODE")))))[@inline] in
let #Test#get_voting_power#1914 =
  fun _kh -> ((poly_#Test#failwith_12)@(L("TEST MODE")))[@inline] in
let #Test#get_total_voting_power#1915 =
  (poly_#Test#failwith_12)@(L("TEST MODE"))[@inline] in
let #Test#nth_bootstrap_contract#1917 =
  fun _i -> ((poly_#Test#failwith_6)@(L("TEST MODE")))[@inline] in
let #Test#nth_bootstrap_account#1918 =
  fun _i -> ((poly_#Test#failwith_6)@(L("TEST MODE")))[@inline] in
let #Test#last_originations#1920 =
  fun _u -> ((poly_#Test#failwith_11)@(L("TEST MODE")))[@inline] in
let #Test#save_mutation#1923 =
  fun _s -> (fun _m -> ((poly_#Test#failwith_2)@(L("TEST MODE"))))[@inline] in
let #Test#add_account#1930 =
  fun _s -> (fun _k -> ((poly_#Test#failwith_1)@(L("TEST MODE"))))[@inline] in
let #Test#new_account#1931 =
  fun _u -> ((poly_#Test#failwith_10)@(L("TEST MODE")))[@inline] in
let #Test#baker_account#1932 =
  fun _p -> (fun _o -> ((poly_#Test#failwith_1)@(L("TEST MODE"))))[@inline] in
let #Test#bake_until_n_cycle_end#1933 =
  fun _n -> ((poly_#Test#failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#register_delegate#1934 =
  fun _kh -> ((poly_#Test#failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#register_constant#1935 =
  fun _m -> ((poly_#Test#failwith_9)@(L("TEST MODE")))[@inline] in
let #Test#create_chest#1940 =
  fun _b -> (fun _n -> ((poly_#Test#failwith_8)@(L("TEST MODE"))))[@inline] in
let #Test#create_chest_key#1941 =
  fun _c -> (fun _n -> ((poly_#Test#failwith_7)@(L("TEST MODE"))))[@inline] in
let #Test#constant_to_michelson_program#1942 =
  fun _s -> ((poly_#Test#failwith_1)@(L("TEST MODE")))[@inline] in
let #Test#restore_context#1943 =
  fun _u -> ((poly_#Test#failwith_1)@(L("TEST_POP_CONTEXT")))[@inline] in
let #Test#save_context#1944 =
  fun _u -> ((poly_#Test#failwith_1)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #Test#drop_context#1945 =
  fun _u -> ((poly_#Test#failwith_1)@(L("TEST_DROP_CONTEXT")))[@inline] in
let #Test#read_contract_from_file#1946 =
  fun _fn -> ((poly_#Test#failwith_1)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let #Test#compile_contract_from_file#1947 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    ((poly_#Test#failwith_1)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let #Test#originate_contract#1949 =
  fun _c ->
  (fun _s -> (fun _t -> ((poly_#Test#failwith_6)@(L("TEST_ORIGINATE")))))[@inline] in
let #Test#size#1950 =
  fun _c -> ((poly_#Test#failwith_5)@(L("TEST_SIZE")))[@inline] in
let #Test#get_bootstrap_account#1951 =
  fun _n -> ((poly_#Test#failwith_4)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let #Test#sign#1952 =
  fun _sk -> (fun _d -> ((poly_#Test#failwith_3)@(L("TEST_SIGN"))))[@inline] in
let #Test#chr#1953 =
  fun _n -> ((poly_#Test#failwith_2)@(L("TEST_CHR")))[@inline] in
let #Test#nl#1954 = L("NEWLINE")[@inline] in
let #Test#println#1955 =
  fun _v -> ((poly_#Test#failwith_1)@(L("TEST_PRINTLN")))[@inline] in
let #Test#print#1956 =
  fun _v -> ((poly_#Test#failwith_1)@(L("TEST_PRINT")))[@inline] in
let #Test#eprint#1957 =
  fun _v -> ((poly_#Test#failwith_1)@(L("TEST_EPRINTL")))[@inline] in
let #Tezos#balance#1962 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let #Tezos#amount#1963 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let #Tezos#now#1964 = ({ DROP ; NOW })@(L(unit))[@inline] in
let #Tezos#sender#1965 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let #Tezos#source#1966 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let #Tezos#level#1967 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let #Tezos#self_address#1968 = SELF_ADDRESS()[@inline] in
let #Tezos#chain_id#1969 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let #Tezos#total_voting_power#1970 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let #Tezos#get_balance#1971 =
  fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let #Tezos#get_amount#1972 =
  fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let #Tezos#get_now#1973 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let #Tezos#get_sender#1974 =
  fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let #Tezos#get_source#1975 =
  fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let #Tezos#get_level#1976 =
  fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let #Tezos#get_self_address#1977 = fun _u -> (SELF_ADDRESS())[@inline] in
let #Tezos#get_chain_id#1978 =
  fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let #Tezos#get_total_voting_power#1979 =
  fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let #Tezos#voting_power#1980 =
  fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
let #Tezos#implicit_account#1982 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #Tezos#pairing_check#1988 =
  fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
let #Tezos#open_chest#1989 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #Tezos#set_delegate#1993 = fun o -> (SET_DELEGATE(o))[@inline] in
let #Bitwise#xor#1994 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #Bitwise#shift_left#1995 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #Bitwise#shift_right#1996 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #String#concat#2037 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #String#sub#2038 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #String#length#2039 = fun b -> (({ SIZE })@(b))[@inline] in
let #Bytes#concat#2042 =
  fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
let #Bytes#sub#2043 =
  fun s ->
  (fun l ->
   (fun b ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                              b)))))[@inline] in
let #Bytes#length#2046 = fun b -> (({ SIZE })@(b))[@inline] in
let #Crypto#blake2b#2047 = fun b -> (({ BLAKE2B })@(b))[@inline] in
let #Crypto#sha256#2048 = fun b -> (({ SHA256 })@(b))[@inline] in
let #Crypto#sha512#2049 = fun b -> (({ SHA512 })@(b))[@inline] in
let #Crypto#sha3#2050 = fun b -> (({ SHA3 })@(b))[@inline] in
let #Crypto#keccak#2051 = fun b -> (({ KECCAK })@(b))[@inline] in
let #Crypto#hash_key#2052 = fun k -> (({ HASH_KEY })@(k))[@inline] in
let #Crypto#check#2053 =
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
  ADD(#../../test/contracts/build/E.mligo#toto#1797 ,
      #../../test/contracts/build/A.mligo#toto#374) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#7090 ->
  (let (gen#7096, gen#7097) = gen#7090 in
   let p = gen#7096 in
   let s = gen#7097 in
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
