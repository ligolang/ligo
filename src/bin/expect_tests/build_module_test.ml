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
             SWAP ;
             DUP ;
             DUG 2 ;
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
      lambda (gen#6) return  match gen#6 with
                              | ( p , s ) ->
                              let s = ADD(ADD(p , s) ,
                              toto) in ( LIST_EMPTY() , s ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let #../../test/contracts/build/A.mligo#Tezos#balance#12 =
  BALANCE()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#amount#13 =
  AMOUNT()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#now#14 = NOW()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#sender#15 =
  SENDER()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#source#16 =
  SOURCE()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#level#17 = LEVEL()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#self_address#18 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#chain_id#19 =
  CHAIN_ID()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#total_voting_power#20 =
  TOTAL_VOTING_POWER()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#voting_power#21 =
  fun kh -> (VOTING_POWER(kh))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#implicit_account#23 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#pairing_check#29 =
  fun l -> (PAIRING_CHECK(l))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#open_chest#30 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#set_delegate#34 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#xor#35 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#shift_left#36 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#shift_right#37 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#String#concat#78 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#79 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/A.mligo#String#length#80 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#concat#83 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#sub#84 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#length#87 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#blake2b#88 =
  fun b -> (BLAKE2b(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha256#89 =
  fun b -> (SHA256(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha512#90 =
  fun b -> (SHA512(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha3#91 =
  fun b -> (SHA3(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#keccak#92 =
  fun b -> (KECCAK(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#hash_key#93 =
  fun k -> (HASH_KEY(k))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#check#94 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let #../../test/contracts/build/A.mligo#assert#95 =
  fun b -> (ASSERTION(b))[@inline] in
let #../../test/contracts/build/A.mligo#assert_with_error#96 =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let #../../test/contracts/build/A.mligo#abs#101 =
  fun i -> (ABS(i))[@inline] in
let #../../test/contracts/build/A.mligo#is_nat#102 =
  fun i -> (IS_NAT(i))[@inline] in
let #../../test/contracts/build/A.mligo#true#103 = TRUE()[@inline] in
let #../../test/contracts/build/A.mligo#false#104 = FALSE()[@inline] in
let #../../test/contracts/build/A.mligo#unit#105 = UNIT()[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4239 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4238 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4237 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4236 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4235 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4234 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4233 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4232 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4231 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4230 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4229 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4228 =
  fun v -> (FAILWITH(v))[@inline] in
let #../../test/contracts/build/A.mligo#Test#originate_from_file#111 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4239)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_source#113 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_baker#114 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#transfer#115 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#transfer_exn#116 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4236)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_storage_of_address#120 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_balance#121 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4238)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#michelson_equal#122 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4237)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#reset_state#124 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_voting_power#125 =
  fun _kh ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4236)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_total_voting_power#126 =
  (poly_#../../test/contracts/build/A.mligo#Test#failwith_4236)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nth_bootstrap_contract#128 =
  fun _i ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4235)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nth_bootstrap_account#129 =
  fun _i ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4235)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#last_originations#131 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4234)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#save_mutation#134 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4233)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#add_account#141 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#new_account#142 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4232)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#baker_account#143 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#bake_until_n_cycle_end#144 =
  fun _n ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#register_delegate#145 =
  fun _kh ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#register_constant#146 =
  fun _m ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4231)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#create_chest#151 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4230)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#create_chest_key#152 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4229)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#constant_to_michelson_program#153 =
  fun _s ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#restore_context#154 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#save_context#155 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#toto#156 = L(1) in
let #../../test/contracts/build/B.mligo#Tezos#balance#160 =
  BALANCE()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#amount#161 =
  AMOUNT()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#now#162 = NOW()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#sender#163 =
  SENDER()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#source#164 =
  SOURCE()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#level#165 = LEVEL()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#self_address#166 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#chain_id#167 =
  CHAIN_ID()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#total_voting_power#168 =
  TOTAL_VOTING_POWER()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#voting_power#169 =
  fun kh -> (VOTING_POWER(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#implicit_account#171 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#pairing_check#177 =
  fun l -> (PAIRING_CHECK(l))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#open_chest#178 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#set_delegate#182 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#xor#183 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_left#184 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_right#185 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#226 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#227 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/B.mligo#String#length#228 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#231 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#232 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#length#235 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#236 =
  fun b -> (BLAKE2b(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#237 =
  fun b -> (SHA256(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#238 =
  fun b -> (SHA512(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#239 =
  fun b -> (SHA3(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#240 =
  fun b -> (KECCAK(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#241 =
  fun k -> (HASH_KEY(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#242 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let #../../test/contracts/build/B.mligo#assert#243 =
  fun b -> (ASSERTION(b))[@inline] in
let #../../test/contracts/build/B.mligo#assert_with_error#244 =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let #../../test/contracts/build/B.mligo#abs#249 =
  fun i -> (ABS(i))[@inline] in
let #../../test/contracts/build/B.mligo#is_nat#250 =
  fun i -> (IS_NAT(i))[@inline] in
let #../../test/contracts/build/B.mligo#true#251 = TRUE()[@inline] in
let #../../test/contracts/build/B.mligo#false#252 = FALSE()[@inline] in
let #../../test/contracts/build/B.mligo#unit#253 = UNIT()[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4227 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4226 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4225 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4224 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4223 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4222 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4221 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4220 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4219 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4218 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4217 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4216 =
  fun v -> (FAILWITH(v))[@inline] in
let #../../test/contracts/build/B.mligo#Test#originate_from_file#259 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4227)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_source#261 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_baker#262 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#transfer#263 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#transfer_exn#264 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4224)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_storage_of_address#268 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_balance#269 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4226)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#michelson_equal#270 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4225)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#reset_state#272 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_voting_power#273 =
  fun _kh ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4224)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_total_voting_power#274 =
  (poly_#../../test/contracts/build/B.mligo#Test#failwith_4224)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nth_bootstrap_contract#276 =
  fun _i ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4223)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nth_bootstrap_account#277 =
  fun _i ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4223)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#last_originations#279 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4222)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#save_mutation#282 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4221)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#add_account#289 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#new_account#290 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4220)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#baker_account#291 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#bake_until_n_cycle_end#292 =
  fun _n ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#register_delegate#293 =
  fun _kh ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#register_constant#294 =
  fun _m ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4219)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#create_chest#299 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4218)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#create_chest_key#300 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4217)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#constant_to_michelson_program#301 =
  fun _s ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#restore_context#302 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#save_context#303 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#toto#304 = L(32) in
let #../../test/contracts/build/B.mligo#titi#305 =
  ADD(#../../test/contracts/build/A.mligo#toto#156 , L(42)) in
let #../../test/contracts/build/B.mligo#f#306 =
  fun gen#2 ->
  (let (gen#1034, gen#1035) = gen#2 in
   let gen#3 = gen#1034 in
   let x = gen#1035 in
   let x =
     ADD(ADD(x , #../../test/contracts/build/A.mligo#toto#156) ,
         #../../test/contracts/build/B.mligo#titi#305) in
   PAIR(LIST_EMPTY() , x)) in
let #../../test/contracts/build/F.mligo#Tezos#balance#310 =
  BALANCE()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#amount#311 =
  AMOUNT()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#now#312 = NOW()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#sender#313 =
  SENDER()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#source#314 =
  SOURCE()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#level#315 = LEVEL()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#self_address#316 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#chain_id#317 =
  CHAIN_ID()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#total_voting_power#318 =
  TOTAL_VOTING_POWER()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#voting_power#319 =
  fun kh -> (VOTING_POWER(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#implicit_account#321 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#pairing_check#327 =
  fun l -> (PAIRING_CHECK(l))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#open_chest#328 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#set_delegate#332 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#xor#333 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_left#334 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_right#335 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#376 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#377 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/F.mligo#String#length#378 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#381 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#382 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#length#385 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#386 =
  fun b -> (BLAKE2b(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#387 =
  fun b -> (SHA256(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#388 =
  fun b -> (SHA512(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#389 =
  fun b -> (SHA3(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#390 =
  fun b -> (KECCAK(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#391 =
  fun k -> (HASH_KEY(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#392 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let #../../test/contracts/build/F.mligo#assert#393 =
  fun b -> (ASSERTION(b))[@inline] in
let #../../test/contracts/build/F.mligo#assert_with_error#394 =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let #../../test/contracts/build/F.mligo#abs#399 =
  fun i -> (ABS(i))[@inline] in
let #../../test/contracts/build/F.mligo#is_nat#400 =
  fun i -> (IS_NAT(i))[@inline] in
let #../../test/contracts/build/F.mligo#true#401 = TRUE()[@inline] in
let #../../test/contracts/build/F.mligo#false#402 = FALSE()[@inline] in
let #../../test/contracts/build/F.mligo#unit#403 = UNIT()[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4215 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4214 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4213 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4212 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4211 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4210 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4209 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4208 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4207 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4206 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4205 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4204 =
  fun v -> (FAILWITH(v))[@inline] in
let #../../test/contracts/build/F.mligo#Test#originate_from_file#409 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4215)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_source#411 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_baker#412 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#transfer#413 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#transfer_exn#414 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4212)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_storage_of_address#418 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_balance#419 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4214)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#michelson_equal#420 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4213)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#reset_state#422 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_voting_power#423 =
  fun _kh ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4212)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_total_voting_power#424 =
  (poly_#../../test/contracts/build/F.mligo#Test#failwith_4212)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nth_bootstrap_contract#426 =
  fun _i ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4211)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nth_bootstrap_account#427 =
  fun _i ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4211)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#last_originations#429 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4210)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#save_mutation#432 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4209)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#add_account#439 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#new_account#440 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4208)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#baker_account#441 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#bake_until_n_cycle_end#442 =
  fun _n ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#register_delegate#443 =
  fun _kh ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#register_constant#444 =
  fun _m ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4207)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#create_chest#449 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4206)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#create_chest_key#450 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4205)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#constant_to_michelson_program#451 =
  fun _s ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#restore_context#452 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#save_context#453 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#toto#454 = L(44) in
let #../../test/contracts/build/G.mligo#Tezos#balance#458 =
  BALANCE()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#amount#459 =
  AMOUNT()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#now#460 = NOW()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#sender#461 =
  SENDER()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#source#462 =
  SOURCE()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#level#463 = LEVEL()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#self_address#464 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#chain_id#465 =
  CHAIN_ID()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#total_voting_power#466 =
  TOTAL_VOTING_POWER()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#voting_power#467 =
  fun kh -> (VOTING_POWER(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#implicit_account#469 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#pairing_check#475 =
  fun l -> (PAIRING_CHECK(l))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#open_chest#476 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#set_delegate#480 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#xor#481 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_left#482 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_right#483 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#524 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#525 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/G.mligo#String#length#526 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#529 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#530 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#length#533 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#534 =
  fun b -> (BLAKE2b(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#535 =
  fun b -> (SHA256(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#536 =
  fun b -> (SHA512(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#537 =
  fun b -> (SHA3(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#538 =
  fun b -> (KECCAK(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#539 =
  fun k -> (HASH_KEY(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#540 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let #../../test/contracts/build/G.mligo#assert#541 =
  fun b -> (ASSERTION(b))[@inline] in
let #../../test/contracts/build/G.mligo#assert_with_error#542 =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let #../../test/contracts/build/G.mligo#abs#547 =
  fun i -> (ABS(i))[@inline] in
let #../../test/contracts/build/G.mligo#is_nat#548 =
  fun i -> (IS_NAT(i))[@inline] in
let #../../test/contracts/build/G.mligo#true#549 = TRUE()[@inline] in
let #../../test/contracts/build/G.mligo#false#550 = FALSE()[@inline] in
let #../../test/contracts/build/G.mligo#unit#551 = UNIT()[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4203 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4202 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4201 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4200 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4199 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4198 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4197 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4196 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4195 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4194 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4193 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4192 =
  fun v -> (FAILWITH(v))[@inline] in
let #../../test/contracts/build/G.mligo#Test#originate_from_file#557 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4203)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_source#559 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_baker#560 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#transfer#561 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#transfer_exn#562 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4200)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_storage_of_address#566 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_balance#567 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4202)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#michelson_equal#568 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4201)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#reset_state#570 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_voting_power#571 =
  fun _kh ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4200)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_total_voting_power#572 =
  (poly_#../../test/contracts/build/G.mligo#Test#failwith_4200)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nth_bootstrap_contract#574 =
  fun _i ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4199)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nth_bootstrap_account#575 =
  fun _i ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4199)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#last_originations#577 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4198)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#save_mutation#580 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4197)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#add_account#587 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#new_account#588 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4196)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#baker_account#589 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#bake_until_n_cycle_end#590 =
  fun _n ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#register_delegate#591 =
  fun _kh ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#register_constant#592 =
  fun _m ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4195)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#create_chest#597 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4194)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#create_chest_key#598 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4193)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#constant_to_michelson_program#599 =
  fun _s ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#restore_context#600 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#save_context#601 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#toto#602 = L(43) in
let #../../test/contracts/build/C.mligo#Tezos#balance#606 =
  BALANCE()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#amount#607 =
  AMOUNT()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#now#608 = NOW()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#sender#609 =
  SENDER()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#source#610 =
  SOURCE()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#level#611 = LEVEL()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#self_address#612 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#chain_id#613 =
  CHAIN_ID()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#total_voting_power#614 =
  TOTAL_VOTING_POWER()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#voting_power#615 =
  fun kh -> (VOTING_POWER(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#implicit_account#617 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#pairing_check#623 =
  fun l -> (PAIRING_CHECK(l))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#open_chest#624 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#set_delegate#628 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#xor#629 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_left#630 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_right#631 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#672 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#673 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/C.mligo#String#length#674 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#677 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#678 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#length#681 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#682 =
  fun b -> (BLAKE2b(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#683 =
  fun b -> (SHA256(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#684 =
  fun b -> (SHA512(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#685 =
  fun b -> (SHA3(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#686 =
  fun b -> (KECCAK(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#687 =
  fun k -> (HASH_KEY(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#688 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let #../../test/contracts/build/C.mligo#assert#689 =
  fun b -> (ASSERTION(b))[@inline] in
let #../../test/contracts/build/C.mligo#assert_with_error#690 =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let #../../test/contracts/build/C.mligo#abs#695 =
  fun i -> (ABS(i))[@inline] in
let #../../test/contracts/build/C.mligo#is_nat#696 =
  fun i -> (IS_NAT(i))[@inline] in
let #../../test/contracts/build/C.mligo#true#697 = TRUE()[@inline] in
let #../../test/contracts/build/C.mligo#false#698 = FALSE()[@inline] in
let #../../test/contracts/build/C.mligo#unit#699 = UNIT()[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4191 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4190 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4189 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4188 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4187 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4186 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4185 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4184 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4183 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4182 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4181 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4180 =
  fun v -> (FAILWITH(v))[@inline] in
let #../../test/contracts/build/C.mligo#Test#originate_from_file#705 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4191)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_source#707 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_baker#708 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#transfer#709 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#transfer_exn#710 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4188)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_storage_of_address#714 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_balance#715 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4190)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#michelson_equal#716 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4189)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#reset_state#718 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_voting_power#719 =
  fun _kh ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4188)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_total_voting_power#720 =
  (poly_#../../test/contracts/build/C.mligo#Test#failwith_4188)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nth_bootstrap_contract#722 =
  fun _i ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4187)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nth_bootstrap_account#723 =
  fun _i ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4187)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#last_originations#725 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4186)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#save_mutation#728 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4185)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#add_account#735 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#new_account#736 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4184)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#baker_account#737 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#bake_until_n_cycle_end#738 =
  fun _n ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#register_delegate#739 =
  fun _kh ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#register_constant#740 =
  fun _m ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4183)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#create_chest#745 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4182)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#create_chest_key#746 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4181)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#constant_to_michelson_program#747 =
  fun _s ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#restore_context#748 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#save_context#749 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#tata#750 =
  ADD(#../../test/contracts/build/A.mligo#toto#156 ,
      #../../test/contracts/build/B.mligo#titi#305) in
let #../../test/contracts/build/C.mligo#foo#751 =
  (#../../test/contracts/build/B.mligo#f#306)@(PAIR(L(unit) , L(3))) in
let #../../test/contracts/build/E.mligo#Tezos#balance#755 =
  BALANCE()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#amount#756 =
  AMOUNT()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#now#757 = NOW()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#sender#758 =
  SENDER()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#source#759 =
  SOURCE()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#level#760 = LEVEL()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#self_address#761 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#chain_id#762 =
  CHAIN_ID()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#total_voting_power#763 =
  TOTAL_VOTING_POWER()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#voting_power#764 =
  fun kh -> (VOTING_POWER(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#implicit_account#766 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#pairing_check#772 =
  fun l -> (PAIRING_CHECK(l))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#open_chest#773 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#set_delegate#777 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#xor#778 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_left#779 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_right#780 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#821 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#822 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/E.mligo#String#length#823 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#826 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#827 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#length#830 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#831 =
  fun b -> (BLAKE2b(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#832 =
  fun b -> (SHA256(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#833 =
  fun b -> (SHA512(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#834 =
  fun b -> (SHA3(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#835 =
  fun b -> (KECCAK(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#836 =
  fun k -> (HASH_KEY(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#837 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let #../../test/contracts/build/E.mligo#assert#838 =
  fun b -> (ASSERTION(b))[@inline] in
let #../../test/contracts/build/E.mligo#assert_with_error#839 =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let #../../test/contracts/build/E.mligo#abs#844 =
  fun i -> (ABS(i))[@inline] in
let #../../test/contracts/build/E.mligo#is_nat#845 =
  fun i -> (IS_NAT(i))[@inline] in
let #../../test/contracts/build/E.mligo#true#846 = TRUE()[@inline] in
let #../../test/contracts/build/E.mligo#false#847 = FALSE()[@inline] in
let #../../test/contracts/build/E.mligo#unit#848 = UNIT()[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4179 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4178 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4177 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4176 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4175 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4174 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4173 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4172 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4171 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4170 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4169 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4168 =
  fun v -> (FAILWITH(v))[@inline] in
let #../../test/contracts/build/E.mligo#Test#originate_from_file#854 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4179)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_source#856 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_baker#857 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#transfer#858 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#transfer_exn#859 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4176)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_storage_of_address#863 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_balance#864 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4178)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#michelson_equal#865 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4177)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#reset_state#867 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_voting_power#868 =
  fun _kh ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4176)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_total_voting_power#869 =
  (poly_#../../test/contracts/build/E.mligo#Test#failwith_4176)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nth_bootstrap_contract#871 =
  fun _i ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4175)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nth_bootstrap_account#872 =
  fun _i ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4175)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#last_originations#874 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4174)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#save_mutation#877 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4173)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#add_account#884 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#new_account#885 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4172)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#baker_account#886 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#bake_until_n_cycle_end#887 =
  fun _n ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#register_delegate#888 =
  fun _kh ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#register_constant#889 =
  fun _m ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4171)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#create_chest#894 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4170)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#create_chest_key#895 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4169)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#constant_to_michelson_program#896 =
  fun _s ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#restore_context#897 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#save_context#898 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#toto#899 = L(10) in
let #../../test/contracts/build/E.mligo#foo#900 = L("bar") in
let #Tezos#balance#904 = BALANCE()[@inline] in
let #Tezos#amount#905 = AMOUNT()[@inline] in
let #Tezos#now#906 = NOW()[@inline] in
let #Tezos#sender#907 = SENDER()[@inline] in
let #Tezos#source#908 = SOURCE()[@inline] in
let #Tezos#level#909 = LEVEL()[@inline] in
let #Tezos#self_address#910 = SELF_ADDRESS()[@inline] in
let #Tezos#chain_id#911 = CHAIN_ID()[@inline] in
let #Tezos#total_voting_power#912 = TOTAL_VOTING_POWER()[@inline] in
let #Tezos#voting_power#913 = fun kh -> (VOTING_POWER(kh))[@inline] in
let #Tezos#implicit_account#915 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #Tezos#pairing_check#921 = fun l -> (PAIRING_CHECK(l))[@inline] in
let #Tezos#open_chest#922 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #Tezos#set_delegate#926 = fun o -> (SET_DELEGATE(o))[@inline] in
let #Bitwise#xor#927 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #Bitwise#shift_left#928 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #Bitwise#shift_right#929 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #String#concat#970 = fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #String#sub#971 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #String#length#972 = fun b -> (SIZE(b))[@inline] in
let #Bytes#concat#975 = fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #Bytes#sub#976 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #Bytes#length#979 = fun b -> (SIZE(b))[@inline] in
let #Crypto#blake2b#980 = fun b -> (BLAKE2b(b))[@inline] in
let #Crypto#sha256#981 = fun b -> (SHA256(b))[@inline] in
let #Crypto#sha512#982 = fun b -> (SHA512(b))[@inline] in
let #Crypto#sha3#983 = fun b -> (SHA3(b))[@inline] in
let #Crypto#keccak#984 = fun b -> (KECCAK(b))[@inline] in
let #Crypto#hash_key#985 = fun k -> (HASH_KEY(k))[@inline] in
let #Crypto#check#986 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let assert = fun b -> (ASSERTION(b))[@inline] in
let assert_with_error =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let abs = fun i -> (ABS(i))[@inline] in
let is_nat = fun i -> (IS_NAT(i))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let poly_#Test#failwith_4167 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4166 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4165 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4164 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4163 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4162 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4161 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4160 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4159 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4158 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4157 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4156 = fun v -> (FAILWITH(v))[@inline] in
let #Test#originate_from_file#989 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s -> (fun _t -> ((poly_#Test#failwith_4167)@(L("TEST MODE")))))))[@inline] in
let #Test#set_source#991 =
  fun _a -> ((poly_#Test#failwith_4156)@(L("TEST MODE")))[@inline] in
let #Test#set_baker#992 =
  fun _a -> ((poly_#Test#failwith_4156)@(L("TEST MODE")))[@inline] in
let #Test#transfer#993 =
  fun _a ->
  (fun _s -> (fun _t -> ((poly_#Test#failwith_4156)@(L("TEST MODE")))))[@inline] in
let #Test#transfer_exn#994 =
  fun _a ->
  (fun _s -> (fun _t -> ((poly_#Test#failwith_4164)@(L("TEST MODE")))))[@inline] in
let #Test#get_storage_of_address#998 =
  fun _a -> ((poly_#Test#failwith_4156)@(L("TEST MODE")))[@inline] in
let #Test#get_balance#999 =
  fun _a -> ((poly_#Test#failwith_4166)@(L("TEST MODE")))[@inline] in
let #Test#michelson_equal#1000 =
  fun _m1 -> (fun _m2 -> ((poly_#Test#failwith_4165)@(L("TEST MODE"))))[@inline] in
let #Test#reset_state#1002 =
  fun _n -> (fun _l -> ((poly_#Test#failwith_4156)@(L("TEST MODE"))))[@inline] in
let #Test#get_voting_power#1003 =
  fun _kh -> ((poly_#Test#failwith_4164)@(L("TEST MODE")))[@inline] in
let #Test#get_total_voting_power#1004 =
  (poly_#Test#failwith_4164)@(L("TEST MODE"))[@inline] in
let #Test#nth_bootstrap_contract#1006 =
  fun _i -> ((poly_#Test#failwith_4163)@(L("TEST MODE")))[@inline] in
let #Test#nth_bootstrap_account#1007 =
  fun _i -> ((poly_#Test#failwith_4163)@(L("TEST MODE")))[@inline] in
let #Test#last_originations#1009 =
  fun _u -> ((poly_#Test#failwith_4162)@(L("TEST MODE")))[@inline] in
let #Test#save_mutation#1012 =
  fun _s -> (fun _m -> ((poly_#Test#failwith_4161)@(L("TEST MODE"))))[@inline] in
let #Test#add_account#1019 =
  fun _s -> (fun _k -> ((poly_#Test#failwith_4156)@(L("TEST MODE"))))[@inline] in
let #Test#new_account#1020 =
  fun _u -> ((poly_#Test#failwith_4160)@(L("TEST MODE")))[@inline] in
let #Test#baker_account#1021 =
  fun _p -> (fun _o -> ((poly_#Test#failwith_4156)@(L("TEST MODE"))))[@inline] in
let #Test#bake_until_n_cycle_end#1022 =
  fun _n -> ((poly_#Test#failwith_4156)@(L("TEST MODE")))[@inline] in
let #Test#register_delegate#1023 =
  fun _kh -> ((poly_#Test#failwith_4156)@(L("TEST MODE")))[@inline] in
let #Test#register_constant#1024 =
  fun _m -> ((poly_#Test#failwith_4159)@(L("TEST MODE")))[@inline] in
let #Test#create_chest#1029 =
  fun _b -> (fun _n -> ((poly_#Test#failwith_4158)@(L("TEST MODE"))))[@inline] in
let #Test#create_chest_key#1030 =
  fun _c -> (fun _n -> ((poly_#Test#failwith_4157)@(L("TEST MODE"))))[@inline] in
let #Test#constant_to_michelson_program#1031 =
  fun _s -> ((poly_#Test#failwith_4156)@(L("TEST MODE")))[@inline] in
let #Test#restore_context#1032 =
  fun _u -> ((poly_#Test#failwith_4156)@(L("TEST_POP_CONTEXT")))[@inline] in
let #Test#save_context#1033 =
  fun _u -> ((poly_#Test#failwith_4156)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let toto =
  ADD(#../../test/contracts/build/E.mligo#toto#899 ,
      #../../test/contracts/build/A.mligo#toto#156) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#5 ->
  (let (gen#1036, gen#1037) = gen#5 in
   let p = gen#1036 in
   let s = gen#1037 in
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
