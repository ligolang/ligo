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
let #../../test/contracts/build/A.mligo#Tezos#balance#11 =
  BALANCE()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#amount#12 =
  AMOUNT()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#now#13 = NOW()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#sender#14 =
  SENDER()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#source#15 =
  SOURCE()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#level#16 = LEVEL()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#self_address#17 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#chain_id#18 =
  CHAIN_ID()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#total_voting_power#19 =
  TOTAL_VOTING_POWER()[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#voting_power#20 =
  fun kh -> (VOTING_POWER(kh))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#implicit_account#22 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#pairing_check#28 =
  fun l -> (PAIRING_CHECK(l))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#open_chest#29 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/A.mligo#Tezos#set_delegate#33 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#xor#34 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#shift_left#35 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#Bitwise#shift_right#36 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/A.mligo#String#concat#75 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/A.mligo#String#sub#76 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/A.mligo#String#length#77 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#concat#80 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#sub#81 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/A.mligo#Bytes#length#84 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#blake2b#85 =
  fun b -> (BLAKE2b(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha256#86 =
  fun b -> (SHA256(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha512#87 =
  fun b -> (SHA512(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#sha3#88 =
  fun b -> (SHA3(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#keccak#89 =
  fun b -> (KECCAK(b))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#hash_key#90 =
  fun k -> (HASH_KEY(k))[@inline] in
let #../../test/contracts/build/A.mligo#Crypto#check#91 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let #../../test/contracts/build/A.mligo#assert#92 =
  fun b -> (ASSERTION(b))[@inline] in
let #../../test/contracts/build/A.mligo#assert_with_error#93 =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let #../../test/contracts/build/A.mligo#abs#98 =
  fun i -> (ABS(i))[@inline] in
let #../../test/contracts/build/A.mligo#is_nat#99 =
  fun i -> (IS_NAT(i))[@inline] in
let #../../test/contracts/build/A.mligo#true#100 = TRUE()[@inline] in
let #../../test/contracts/build/A.mligo#false#101 = FALSE()[@inline] in
let #../../test/contracts/build/A.mligo#unit#102 = UNIT()[@inline] in
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
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4227 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4226 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4225 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/A.mligo#Test#failwith_4224 =
  fun v -> (FAILWITH(v))[@inline] in
let #../../test/contracts/build/A.mligo#Test#originate_from_file#108 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4235)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_source#110 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4224)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#set_baker#111 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4224)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#transfer#112 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4224)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#transfer_exn#113 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4232)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_storage_of_address#117 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4224)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_balance#118 =
  fun _a ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4234)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#michelson_equal#119 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4233)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#reset_state#121 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4224)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_voting_power#122 =
  fun _kh ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4232)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#get_total_voting_power#123 =
  (poly_#../../test/contracts/build/A.mligo#Test#failwith_4232)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nth_bootstrap_contract#125 =
  fun _i ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4231)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#nth_bootstrap_account#126 =
  fun _i ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4231)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#last_originations#128 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4230)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#save_mutation#131 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4229)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#add_account#138 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4224)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#new_account#139 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4228)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#baker_account#140 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4224)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#bake_until_n_cycle_end#141 =
  fun _n ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4224)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#register_delegate#142 =
  fun _kh ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4224)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#register_constant#143 =
  fun _m ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4227)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#create_chest#148 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4226)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#create_chest_key#149 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4225)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/A.mligo#Test#constant_to_michelson_program#150 =
  fun _s ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4224)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#restore_context#151 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4224)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#Test#save_context#152 =
  fun _u ->
  ((poly_#../../test/contracts/build/A.mligo#Test#failwith_4224)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/A.mligo#toto#153 = L(1) in
let #../../test/contracts/build/B.mligo#Tezos#balance#156 =
  BALANCE()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#amount#157 =
  AMOUNT()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#now#158 = NOW()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#sender#159 =
  SENDER()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#source#160 =
  SOURCE()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#level#161 = LEVEL()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#self_address#162 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#chain_id#163 =
  CHAIN_ID()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#total_voting_power#164 =
  TOTAL_VOTING_POWER()[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#voting_power#165 =
  fun kh -> (VOTING_POWER(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#implicit_account#167 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#pairing_check#173 =
  fun l -> (PAIRING_CHECK(l))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#open_chest#174 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/B.mligo#Tezos#set_delegate#178 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#xor#179 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_left#180 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#Bitwise#shift_right#181 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/B.mligo#String#concat#220 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/B.mligo#String#sub#221 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/B.mligo#String#length#222 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#concat#225 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#sub#226 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/B.mligo#Bytes#length#229 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#blake2b#230 =
  fun b -> (BLAKE2b(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha256#231 =
  fun b -> (SHA256(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha512#232 =
  fun b -> (SHA512(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#sha3#233 =
  fun b -> (SHA3(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#keccak#234 =
  fun b -> (KECCAK(b))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#hash_key#235 =
  fun k -> (HASH_KEY(k))[@inline] in
let #../../test/contracts/build/B.mligo#Crypto#check#236 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let #../../test/contracts/build/B.mligo#assert#237 =
  fun b -> (ASSERTION(b))[@inline] in
let #../../test/contracts/build/B.mligo#assert_with_error#238 =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let #../../test/contracts/build/B.mligo#abs#243 =
  fun i -> (ABS(i))[@inline] in
let #../../test/contracts/build/B.mligo#is_nat#244 =
  fun i -> (IS_NAT(i))[@inline] in
let #../../test/contracts/build/B.mligo#true#245 = TRUE()[@inline] in
let #../../test/contracts/build/B.mligo#false#246 = FALSE()[@inline] in
let #../../test/contracts/build/B.mligo#unit#247 = UNIT()[@inline] in
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
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4215 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4214 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4213 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/B.mligo#Test#failwith_4212 =
  fun v -> (FAILWITH(v))[@inline] in
let #../../test/contracts/build/B.mligo#Test#originate_from_file#253 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4223)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_source#255 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4212)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#set_baker#256 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4212)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#transfer#257 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4212)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#transfer_exn#258 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4220)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_storage_of_address#262 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4212)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_balance#263 =
  fun _a ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4222)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#michelson_equal#264 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4221)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#reset_state#266 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4212)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_voting_power#267 =
  fun _kh ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4220)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#get_total_voting_power#268 =
  (poly_#../../test/contracts/build/B.mligo#Test#failwith_4220)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nth_bootstrap_contract#270 =
  fun _i ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4219)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#nth_bootstrap_account#271 =
  fun _i ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4219)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#last_originations#273 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4218)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#save_mutation#276 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4217)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#add_account#283 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4212)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#new_account#284 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4216)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#baker_account#285 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4212)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#bake_until_n_cycle_end#286 =
  fun _n ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4212)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#register_delegate#287 =
  fun _kh ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4212)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#register_constant#288 =
  fun _m ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4215)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#create_chest#293 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4214)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#create_chest_key#294 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4213)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/B.mligo#Test#constant_to_michelson_program#295 =
  fun _s ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4212)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#restore_context#296 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4212)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#Test#save_context#297 =
  fun _u ->
  ((poly_#../../test/contracts/build/B.mligo#Test#failwith_4212)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/B.mligo#toto#298 = L(32) in
let #../../test/contracts/build/B.mligo#titi#299 =
  ADD(#../../test/contracts/build/A.mligo#toto#153 , L(42)) in
let #../../test/contracts/build/B.mligo#f#300 =
  fun gen#2 ->
  (let (gen#1013, gen#1014) = gen#2 in
   let gen#3 = gen#1013 in
   let x = gen#1014 in
   let x =
     ADD(ADD(x , #../../test/contracts/build/A.mligo#toto#153) ,
         #../../test/contracts/build/B.mligo#titi#299) in
   PAIR(LIST_EMPTY() , x)) in
let #../../test/contracts/build/F.mligo#Tezos#balance#303 =
  BALANCE()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#amount#304 =
  AMOUNT()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#now#305 = NOW()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#sender#306 =
  SENDER()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#source#307 =
  SOURCE()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#level#308 = LEVEL()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#self_address#309 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#chain_id#310 =
  CHAIN_ID()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#total_voting_power#311 =
  TOTAL_VOTING_POWER()[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#voting_power#312 =
  fun kh -> (VOTING_POWER(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#implicit_account#314 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#pairing_check#320 =
  fun l -> (PAIRING_CHECK(l))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#open_chest#321 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/F.mligo#Tezos#set_delegate#325 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#xor#326 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_left#327 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#Bitwise#shift_right#328 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/F.mligo#String#concat#367 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/F.mligo#String#sub#368 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/F.mligo#String#length#369 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#concat#372 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#sub#373 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/F.mligo#Bytes#length#376 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#blake2b#377 =
  fun b -> (BLAKE2b(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha256#378 =
  fun b -> (SHA256(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha512#379 =
  fun b -> (SHA512(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#sha3#380 =
  fun b -> (SHA3(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#keccak#381 =
  fun b -> (KECCAK(b))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#hash_key#382 =
  fun k -> (HASH_KEY(k))[@inline] in
let #../../test/contracts/build/F.mligo#Crypto#check#383 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let #../../test/contracts/build/F.mligo#assert#384 =
  fun b -> (ASSERTION(b))[@inline] in
let #../../test/contracts/build/F.mligo#assert_with_error#385 =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let #../../test/contracts/build/F.mligo#abs#390 =
  fun i -> (ABS(i))[@inline] in
let #../../test/contracts/build/F.mligo#is_nat#391 =
  fun i -> (IS_NAT(i))[@inline] in
let #../../test/contracts/build/F.mligo#true#392 = TRUE()[@inline] in
let #../../test/contracts/build/F.mligo#false#393 = FALSE()[@inline] in
let #../../test/contracts/build/F.mligo#unit#394 = UNIT()[@inline] in
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
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4203 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4202 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4201 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/F.mligo#Test#failwith_4200 =
  fun v -> (FAILWITH(v))[@inline] in
let #../../test/contracts/build/F.mligo#Test#originate_from_file#400 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4211)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_source#402 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4200)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#set_baker#403 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4200)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#transfer#404 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4200)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#transfer_exn#405 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4208)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_storage_of_address#409 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4200)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_balance#410 =
  fun _a ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4210)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#michelson_equal#411 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4209)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#reset_state#413 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4200)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_voting_power#414 =
  fun _kh ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4208)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#get_total_voting_power#415 =
  (poly_#../../test/contracts/build/F.mligo#Test#failwith_4208)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nth_bootstrap_contract#417 =
  fun _i ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4207)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#nth_bootstrap_account#418 =
  fun _i ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4207)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#last_originations#420 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4206)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#save_mutation#423 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4205)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#add_account#430 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4200)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#new_account#431 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4204)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#baker_account#432 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4200)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#bake_until_n_cycle_end#433 =
  fun _n ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4200)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#register_delegate#434 =
  fun _kh ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4200)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#register_constant#435 =
  fun _m ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4203)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#create_chest#440 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4202)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#create_chest_key#441 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4201)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/F.mligo#Test#constant_to_michelson_program#442 =
  fun _s ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4200)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#restore_context#443 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4200)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#Test#save_context#444 =
  fun _u ->
  ((poly_#../../test/contracts/build/F.mligo#Test#failwith_4200)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/F.mligo#toto#445 = L(44) in
let #../../test/contracts/build/G.mligo#Tezos#balance#448 =
  BALANCE()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#amount#449 =
  AMOUNT()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#now#450 = NOW()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#sender#451 =
  SENDER()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#source#452 =
  SOURCE()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#level#453 = LEVEL()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#self_address#454 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#chain_id#455 =
  CHAIN_ID()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#total_voting_power#456 =
  TOTAL_VOTING_POWER()[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#voting_power#457 =
  fun kh -> (VOTING_POWER(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#implicit_account#459 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#pairing_check#465 =
  fun l -> (PAIRING_CHECK(l))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#open_chest#466 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/G.mligo#Tezos#set_delegate#470 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#xor#471 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_left#472 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#Bitwise#shift_right#473 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/G.mligo#String#concat#512 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/G.mligo#String#sub#513 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/G.mligo#String#length#514 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#concat#517 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#sub#518 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/G.mligo#Bytes#length#521 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#blake2b#522 =
  fun b -> (BLAKE2b(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha256#523 =
  fun b -> (SHA256(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha512#524 =
  fun b -> (SHA512(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#sha3#525 =
  fun b -> (SHA3(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#keccak#526 =
  fun b -> (KECCAK(b))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#hash_key#527 =
  fun k -> (HASH_KEY(k))[@inline] in
let #../../test/contracts/build/G.mligo#Crypto#check#528 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let #../../test/contracts/build/G.mligo#assert#529 =
  fun b -> (ASSERTION(b))[@inline] in
let #../../test/contracts/build/G.mligo#assert_with_error#530 =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let #../../test/contracts/build/G.mligo#abs#535 =
  fun i -> (ABS(i))[@inline] in
let #../../test/contracts/build/G.mligo#is_nat#536 =
  fun i -> (IS_NAT(i))[@inline] in
let #../../test/contracts/build/G.mligo#true#537 = TRUE()[@inline] in
let #../../test/contracts/build/G.mligo#false#538 = FALSE()[@inline] in
let #../../test/contracts/build/G.mligo#unit#539 = UNIT()[@inline] in
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
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4191 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4190 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4189 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/G.mligo#Test#failwith_4188 =
  fun v -> (FAILWITH(v))[@inline] in
let #../../test/contracts/build/G.mligo#Test#originate_from_file#545 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4199)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_source#547 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4188)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#set_baker#548 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4188)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#transfer#549 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4188)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#transfer_exn#550 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4196)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_storage_of_address#554 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4188)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_balance#555 =
  fun _a ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4198)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#michelson_equal#556 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4197)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#reset_state#558 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4188)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_voting_power#559 =
  fun _kh ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4196)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#get_total_voting_power#560 =
  (poly_#../../test/contracts/build/G.mligo#Test#failwith_4196)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nth_bootstrap_contract#562 =
  fun _i ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4195)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#nth_bootstrap_account#563 =
  fun _i ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4195)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#last_originations#565 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4194)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#save_mutation#568 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4193)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#add_account#575 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4188)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#new_account#576 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4192)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#baker_account#577 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4188)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#bake_until_n_cycle_end#578 =
  fun _n ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4188)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#register_delegate#579 =
  fun _kh ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4188)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#register_constant#580 =
  fun _m ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4191)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#create_chest#585 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4190)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#create_chest_key#586 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4189)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/G.mligo#Test#constant_to_michelson_program#587 =
  fun _s ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4188)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#restore_context#588 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4188)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#Test#save_context#589 =
  fun _u ->
  ((poly_#../../test/contracts/build/G.mligo#Test#failwith_4188)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/G.mligo#toto#590 = L(43) in
let #../../test/contracts/build/C.mligo#Tezos#balance#593 =
  BALANCE()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#amount#594 =
  AMOUNT()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#now#595 = NOW()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#sender#596 =
  SENDER()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#source#597 =
  SOURCE()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#level#598 = LEVEL()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#self_address#599 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#chain_id#600 =
  CHAIN_ID()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#total_voting_power#601 =
  TOTAL_VOTING_POWER()[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#voting_power#602 =
  fun kh -> (VOTING_POWER(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#implicit_account#604 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#pairing_check#610 =
  fun l -> (PAIRING_CHECK(l))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#open_chest#611 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/C.mligo#Tezos#set_delegate#615 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#xor#616 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_left#617 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#Bitwise#shift_right#618 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/C.mligo#String#concat#657 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/C.mligo#String#sub#658 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/C.mligo#String#length#659 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#concat#662 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#sub#663 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/C.mligo#Bytes#length#666 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#blake2b#667 =
  fun b -> (BLAKE2b(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha256#668 =
  fun b -> (SHA256(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha512#669 =
  fun b -> (SHA512(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#sha3#670 =
  fun b -> (SHA3(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#keccak#671 =
  fun b -> (KECCAK(b))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#hash_key#672 =
  fun k -> (HASH_KEY(k))[@inline] in
let #../../test/contracts/build/C.mligo#Crypto#check#673 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let #../../test/contracts/build/C.mligo#assert#674 =
  fun b -> (ASSERTION(b))[@inline] in
let #../../test/contracts/build/C.mligo#assert_with_error#675 =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let #../../test/contracts/build/C.mligo#abs#680 =
  fun i -> (ABS(i))[@inline] in
let #../../test/contracts/build/C.mligo#is_nat#681 =
  fun i -> (IS_NAT(i))[@inline] in
let #../../test/contracts/build/C.mligo#true#682 = TRUE()[@inline] in
let #../../test/contracts/build/C.mligo#false#683 = FALSE()[@inline] in
let #../../test/contracts/build/C.mligo#unit#684 = UNIT()[@inline] in
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
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4179 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4178 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4177 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/C.mligo#Test#failwith_4176 =
  fun v -> (FAILWITH(v))[@inline] in
let #../../test/contracts/build/C.mligo#Test#originate_from_file#690 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4187)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_source#692 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4176)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#set_baker#693 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4176)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#transfer#694 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4176)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#transfer_exn#695 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4184)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_storage_of_address#699 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4176)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_balance#700 =
  fun _a ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4186)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#michelson_equal#701 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4185)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#reset_state#703 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4176)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_voting_power#704 =
  fun _kh ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4184)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#get_total_voting_power#705 =
  (poly_#../../test/contracts/build/C.mligo#Test#failwith_4184)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nth_bootstrap_contract#707 =
  fun _i ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4183)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#nth_bootstrap_account#708 =
  fun _i ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4183)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#last_originations#710 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4182)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#save_mutation#713 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4181)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#add_account#720 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4176)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#new_account#721 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4180)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#baker_account#722 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4176)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#bake_until_n_cycle_end#723 =
  fun _n ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4176)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#register_delegate#724 =
  fun _kh ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4176)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#register_constant#725 =
  fun _m ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4179)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#create_chest#730 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4178)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#create_chest_key#731 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4177)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/C.mligo#Test#constant_to_michelson_program#732 =
  fun _s ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4176)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#restore_context#733 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4176)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#Test#save_context#734 =
  fun _u ->
  ((poly_#../../test/contracts/build/C.mligo#Test#failwith_4176)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/C.mligo#tata#735 =
  ADD(#../../test/contracts/build/A.mligo#toto#153 ,
      #../../test/contracts/build/B.mligo#titi#299) in
let #../../test/contracts/build/C.mligo#foo#736 =
  (#../../test/contracts/build/B.mligo#f#300)@(PAIR(L(unit) , L(3))) in
let #../../test/contracts/build/E.mligo#Tezos#balance#739 =
  BALANCE()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#amount#740 =
  AMOUNT()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#now#741 = NOW()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#sender#742 =
  SENDER()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#source#743 =
  SOURCE()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#level#744 = LEVEL()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#self_address#745 =
  SELF_ADDRESS()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#chain_id#746 =
  CHAIN_ID()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#total_voting_power#747 =
  TOTAL_VOTING_POWER()[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#voting_power#748 =
  fun kh -> (VOTING_POWER(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#implicit_account#750 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#pairing_check#756 =
  fun l -> (PAIRING_CHECK(l))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#open_chest#757 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #../../test/contracts/build/E.mligo#Tezos#set_delegate#761 =
  fun o -> (SET_DELEGATE(o))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#xor#762 =
  fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_left#763 =
  fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#Bitwise#shift_right#764 =
  fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #../../test/contracts/build/E.mligo#String#concat#803 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/E.mligo#String#sub#804 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/E.mligo#String#length#805 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#concat#808 =
  fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#sub#809 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #../../test/contracts/build/E.mligo#Bytes#length#812 =
  fun b -> (SIZE(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#blake2b#813 =
  fun b -> (BLAKE2b(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha256#814 =
  fun b -> (SHA256(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha512#815 =
  fun b -> (SHA512(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#sha3#816 =
  fun b -> (SHA3(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#keccak#817 =
  fun b -> (KECCAK(b))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#hash_key#818 =
  fun k -> (HASH_KEY(k))[@inline] in
let #../../test/contracts/build/E.mligo#Crypto#check#819 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let #../../test/contracts/build/E.mligo#assert#820 =
  fun b -> (ASSERTION(b))[@inline] in
let #../../test/contracts/build/E.mligo#assert_with_error#821 =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let #../../test/contracts/build/E.mligo#abs#826 =
  fun i -> (ABS(i))[@inline] in
let #../../test/contracts/build/E.mligo#is_nat#827 =
  fun i -> (IS_NAT(i))[@inline] in
let #../../test/contracts/build/E.mligo#true#828 = TRUE()[@inline] in
let #../../test/contracts/build/E.mligo#false#829 = FALSE()[@inline] in
let #../../test/contracts/build/E.mligo#unit#830 = UNIT()[@inline] in
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
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4167 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4166 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4165 =
  fun v -> (FAILWITH(v))[@inline] in
let poly_#../../test/contracts/build/E.mligo#Test#failwith_4164 =
  fun v -> (FAILWITH(v))[@inline] in
let #../../test/contracts/build/E.mligo#Test#originate_from_file#836 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s ->
     (fun _t ->
      ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4175)@(
       L("TEST MODE")))))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_source#838 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4164)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#set_baker#839 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4164)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#transfer#840 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4164)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#transfer_exn#841 =
  fun _a ->
  (fun _s ->
   (fun _t ->
    ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4172)@(L("TEST MODE")))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_storage_of_address#845 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4164)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_balance#846 =
  fun _a ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4174)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#michelson_equal#847 =
  fun _m1 ->
  (fun _m2 ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4173)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#reset_state#849 =
  fun _n ->
  (fun _l ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4164)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_voting_power#850 =
  fun _kh ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4172)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#get_total_voting_power#851 =
  (poly_#../../test/contracts/build/E.mligo#Test#failwith_4172)@(L("TEST MODE"))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nth_bootstrap_contract#853 =
  fun _i ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4171)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#nth_bootstrap_account#854 =
  fun _i ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4171)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#last_originations#856 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4170)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#save_mutation#859 =
  fun _s ->
  (fun _m ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4169)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#add_account#866 =
  fun _s ->
  (fun _k ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4164)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#new_account#867 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4168)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#baker_account#868 =
  fun _p ->
  (fun _o ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4164)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#bake_until_n_cycle_end#869 =
  fun _n ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4164)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#register_delegate#870 =
  fun _kh ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4164)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#register_constant#871 =
  fun _m ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4167)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#create_chest#876 =
  fun _b ->
  (fun _n ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4166)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#create_chest_key#877 =
  fun _c ->
  (fun _n ->
   ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4165)@(L("TEST MODE"))))[@inline] in
let #../../test/contracts/build/E.mligo#Test#constant_to_michelson_program#878 =
  fun _s ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4164)@(L("TEST MODE")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#restore_context#879 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4164)@(L("TEST_POP_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#Test#save_context#880 =
  fun _u ->
  ((poly_#../../test/contracts/build/E.mligo#Test#failwith_4164)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let #../../test/contracts/build/E.mligo#toto#881 = L(10) in
let #../../test/contracts/build/E.mligo#foo#882 = L("bar") in
let #Tezos#balance#885 = BALANCE()[@inline] in
let #Tezos#amount#886 = AMOUNT()[@inline] in
let #Tezos#now#887 = NOW()[@inline] in
let #Tezos#sender#888 = SENDER()[@inline] in
let #Tezos#source#889 = SOURCE()[@inline] in
let #Tezos#level#890 = LEVEL()[@inline] in
let #Tezos#self_address#891 = SELF_ADDRESS()[@inline] in
let #Tezos#chain_id#892 = CHAIN_ID()[@inline] in
let #Tezos#total_voting_power#893 = TOTAL_VOTING_POWER()[@inline] in
let #Tezos#voting_power#894 = fun kh -> (VOTING_POWER(kh))[@inline] in
let #Tezos#implicit_account#896 =
  fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
let #Tezos#pairing_check#902 = fun l -> (PAIRING_CHECK(l))[@inline] in
let #Tezos#open_chest#903 =
  fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
let #Tezos#set_delegate#907 = fun o -> (SET_DELEGATE(o))[@inline] in
let #Bitwise#xor#908 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
let #Bitwise#shift_left#909 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
let #Bitwise#shift_right#910 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
let #String#concat#949 = fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #String#sub#950 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #String#length#951 = fun b -> (SIZE(b))[@inline] in
let #Bytes#concat#954 = fun b1 -> (fun b2 -> (CONCAT(b1 , b2)))[@inline] in
let #Bytes#sub#955 =
  fun s -> (fun l -> (fun b -> (SLICE(s , l , b))))[@inline] in
let #Bytes#length#958 = fun b -> (SIZE(b))[@inline] in
let #Crypto#blake2b#959 = fun b -> (BLAKE2b(b))[@inline] in
let #Crypto#sha256#960 = fun b -> (SHA256(b))[@inline] in
let #Crypto#sha512#961 = fun b -> (SHA512(b))[@inline] in
let #Crypto#sha3#962 = fun b -> (SHA3(b))[@inline] in
let #Crypto#keccak#963 = fun b -> (KECCAK(b))[@inline] in
let #Crypto#hash_key#964 = fun k -> (HASH_KEY(k))[@inline] in
let #Crypto#check#965 =
  fun k -> (fun s -> (fun b -> (CHECK_SIGNATURE(k , s , b))))[@inline] in
let assert = fun b -> (ASSERTION(b))[@inline] in
let assert_with_error =
  fun b -> (fun s -> (ASSERTION_WITH_ERROR(b , s)))[@inline] in
let abs = fun i -> (ABS(i))[@inline] in
let is_nat = fun i -> (IS_NAT(i))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let poly_#Test#failwith_4163 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4162 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4161 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4160 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4159 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4158 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4157 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4156 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4155 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4154 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4153 = fun v -> (FAILWITH(v))[@inline] in
let poly_#Test#failwith_4152 = fun v -> (FAILWITH(v))[@inline] in
let #Test#originate_from_file#968 =
  fun _fn ->
  (fun _e ->
   (fun _v ->
    (fun _s -> (fun _t -> ((poly_#Test#failwith_4163)@(L("TEST MODE")))))))[@inline] in
let #Test#set_source#970 =
  fun _a -> ((poly_#Test#failwith_4152)@(L("TEST MODE")))[@inline] in
let #Test#set_baker#971 =
  fun _a -> ((poly_#Test#failwith_4152)@(L("TEST MODE")))[@inline] in
let #Test#transfer#972 =
  fun _a ->
  (fun _s -> (fun _t -> ((poly_#Test#failwith_4152)@(L("TEST MODE")))))[@inline] in
let #Test#transfer_exn#973 =
  fun _a ->
  (fun _s -> (fun _t -> ((poly_#Test#failwith_4160)@(L("TEST MODE")))))[@inline] in
let #Test#get_storage_of_address#977 =
  fun _a -> ((poly_#Test#failwith_4152)@(L("TEST MODE")))[@inline] in
let #Test#get_balance#978 =
  fun _a -> ((poly_#Test#failwith_4162)@(L("TEST MODE")))[@inline] in
let #Test#michelson_equal#979 =
  fun _m1 -> (fun _m2 -> ((poly_#Test#failwith_4161)@(L("TEST MODE"))))[@inline] in
let #Test#reset_state#981 =
  fun _n -> (fun _l -> ((poly_#Test#failwith_4152)@(L("TEST MODE"))))[@inline] in
let #Test#get_voting_power#982 =
  fun _kh -> ((poly_#Test#failwith_4160)@(L("TEST MODE")))[@inline] in
let #Test#get_total_voting_power#983 =
  (poly_#Test#failwith_4160)@(L("TEST MODE"))[@inline] in
let #Test#nth_bootstrap_contract#985 =
  fun _i -> ((poly_#Test#failwith_4159)@(L("TEST MODE")))[@inline] in
let #Test#nth_bootstrap_account#986 =
  fun _i -> ((poly_#Test#failwith_4159)@(L("TEST MODE")))[@inline] in
let #Test#last_originations#988 =
  fun _u -> ((poly_#Test#failwith_4158)@(L("TEST MODE")))[@inline] in
let #Test#save_mutation#991 =
  fun _s -> (fun _m -> ((poly_#Test#failwith_4157)@(L("TEST MODE"))))[@inline] in
let #Test#add_account#998 =
  fun _s -> (fun _k -> ((poly_#Test#failwith_4152)@(L("TEST MODE"))))[@inline] in
let #Test#new_account#999 =
  fun _u -> ((poly_#Test#failwith_4156)@(L("TEST MODE")))[@inline] in
let #Test#baker_account#1000 =
  fun _p -> (fun _o -> ((poly_#Test#failwith_4152)@(L("TEST MODE"))))[@inline] in
let #Test#bake_until_n_cycle_end#1001 =
  fun _n -> ((poly_#Test#failwith_4152)@(L("TEST MODE")))[@inline] in
let #Test#register_delegate#1002 =
  fun _kh -> ((poly_#Test#failwith_4152)@(L("TEST MODE")))[@inline] in
let #Test#register_constant#1003 =
  fun _m -> ((poly_#Test#failwith_4155)@(L("TEST MODE")))[@inline] in
let #Test#create_chest#1008 =
  fun _b -> (fun _n -> ((poly_#Test#failwith_4154)@(L("TEST MODE"))))[@inline] in
let #Test#create_chest_key#1009 =
  fun _c -> (fun _n -> ((poly_#Test#failwith_4153)@(L("TEST MODE"))))[@inline] in
let #Test#constant_to_michelson_program#1010 =
  fun _s -> ((poly_#Test#failwith_4152)@(L("TEST MODE")))[@inline] in
let #Test#restore_context#1011 =
  fun _u -> ((poly_#Test#failwith_4152)@(L("TEST_POP_CONTEXT")))[@inline] in
let #Test#save_context#1012 =
  fun _u -> ((poly_#Test#failwith_4152)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let toto =
  ADD(#../../test/contracts/build/E.mligo#toto#881 ,
      #../../test/contracts/build/A.mligo#toto#153) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#5 ->
  (let (gen#1015, gen#1016) = gen#5 in
   let p = gen#1015 in
   let s = gen#1016 in
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
