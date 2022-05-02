open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #A#a#135 = 42 in
    let #B#b#136 = 1 in
    let x = #A#a#135 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #A#a#135 = 40 in
    let #B#b#138 = let #LOCAL#inA#ba#136 = 1 in
    let #LOCAL#inA#baa#137 = #LOCAL#inA#ba#136 in
    ADD(#LOCAL#inA#ba#136 ,
    #LOCAL#inA#baa#137) in
    let x = ADD(#A#a#135 , #B#b#138) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #A#a#135 = 1 in
    let #A_s#as#136 = 42 in
    let #B#x#137 = #A#a#135 in
    let #B#b#138 = #A_s#as#136 in
    let x = #A_s#as#136 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #A_s#as#135 = 20 in
  let #A#s_as#136 = 22 in
  let x = ADD(#A_s#as#135 , #A#s_as#136) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #A#a#135 = 1 in
  let #A#A_s#as#136 = 42 in
  let #A#A_s#as#137 = 3 in
  let x = #A#A_s#as#136 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#135 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#136 = x in
  let #LOCAL#inFoo#y#137 = #Foo#x#135 in
  let #LOCAL#inFoo#z#138 = #LOCAL#inFoo#y#137 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#136 , #LOCAL#inFoo#y#137) , x) ,
  #LOCAL#inFoo#z#138) in
  let x = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #A#v#135 = 40 in
  let #A#B#v#136 = ADD(#A#v#135 , 1) in
  let #A#B#C#v#137 = ADD(#A#B#v#136 , 1) in
  let x = #A#B#C#v#137 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#135 = 41 in
  let x = 1 in
  let #TFoo#x#136 = x in
  let #TFoo#y#137 = #Foo#x#135 in
  let u = ADD(#TFoo#x#136 , #TFoo#y#137) in
  let x = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#135 = 41 in
  let #A#B#x#136 = ADD(#A#B#x#135 , 1) in
  let x = #A#B#x#136 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#135 = 42 in
  let #A#B#x#136 = 2 in
  let #A#y#137 = #A#B#x#135 in
  let x = #A#y#137 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#135 = 19 in
  let #Foo#y#136 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#135 in
  let v = #Foo#y#136 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #F#F#a#135 = 42 in
  let #F#F#x#136 = #F#F#a#135 in
  let x = #F#F#x#136 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #A#current_turn#138 = lambda (i) return ADD(i , +1) in
  let #A#other#139 = lambda (n) return let current_turn = (#A#current_turn#138)@(+1) in
  (assert)@(EQ(n ,
  current_turn)) in
  let main = lambda (gen#2) return  match gen#2 with
                                     | ( _p , _s ) ->
                                     ( LIST_EMPTY() , (#A#other#139)@(+2) ) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "bug_alias13.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH nat 1 ;
             PUSH nat 1 ;
             ADD ;
             PUSH nat 2 ;
             COMPARE ;
             EQ ;
             IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "effects.mligo" ] ;
  [%expect{|
    { parameter int ;
      storage int ;
      code { CDR ; PUSH string "foo" ; FAILWITH } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "bug_module_record.ligo" ] ;
  [%expect{|
    let #Tezos#balance#74 = BALANCE()[@inline] in
    let #Tezos#amount#75 = AMOUNT()[@inline] in
    let #Tezos#now#76 = NOW()[@inline] in
    let #Tezos#sender#77 = SENDER()[@inline] in
    let #Tezos#source#78 = SOURCE()[@inline] in
    let #Tezos#level#79 = LEVEL()[@inline] in
    let #Tezos#self_address#80 = SELF_ADDRESS()[@inline] in
    let #Tezos#chain_id#81 = CHAIN_ID()[@inline] in
    let #Tezos#total_voting_power#82 = TOTAL_VOTING_POWER()[@inline] in
    let #Tezos#voting_power#83 = fun kh -> (VOTING_POWER(kh))[@inline] in
    let #Tezos#implicit_account#85 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
    let #Tezos#pairing_check#91 = fun l -> (PAIRING_CHECK(l))[@inline] in
    let #Tezos#open_chest#92 =
      fun gen#5 ->
      (let (gen#205, gen#206) = gen#5 in
       let (gen#207, gen#208) = gen#205 in
       let ck = gen#207 in
       let c = gen#208 in let n = gen#206 in OPEN_CHEST(ck , c , n))[@inline] in
    let #Tezos#set_delegate#96 = fun o -> (SET_DELEGATE(o))[@inline] in
    let #Bitwise#xor#97 =
      fun gen#8 ->
      (let (gen#209, gen#210) = gen#8 in
       let l = gen#209 in let r = gen#210 in XOR(l , r))[@inline] in
    let #Bitwise#shift_left#98 =
      fun gen#9 ->
      (let (gen#211, gen#212) = gen#9 in
       let l = gen#211 in let r = gen#212 in LSL(l , r))[@inline] in
    let #Bitwise#shift_right#99 =
      fun gen#10 ->
      (let (gen#213, gen#214) = gen#10 in
       let l = gen#213 in let r = gen#214 in LSR(l , r))[@inline] in
    let #String#concat#140 =
      fun gen#41 ->
      (let (gen#215, gen#216) = gen#41 in
       let b1 = gen#215 in let b2 = gen#216 in CONCAT(b1 , b2))[@inline] in
    let #String#sub#141 =
      fun gen#42 ->
      (let (gen#217, gen#218) = gen#42 in
       let (gen#219, gen#220) = gen#217 in
       let s = gen#219 in let l = gen#220 in let b = gen#218 in SLICE(s , l , b))[@inline] in
    let #String#length#142 = fun b -> (SIZE(b))[@inline] in
    let #Bytes#concat#145 =
      fun gen#44 ->
      (let (gen#221, gen#222) = gen#44 in
       let b1 = gen#221 in let b2 = gen#222 in CONCAT(b1 , b2))[@inline] in
    let #Bytes#sub#146 =
      fun gen#45 ->
      (let (gen#223, gen#224) = gen#45 in
       let (gen#225, gen#226) = gen#223 in
       let s = gen#225 in let l = gen#226 in let b = gen#224 in SLICE(s , l , b))[@inline] in
    let #Bytes#length#149 = fun b -> (SIZE(b))[@inline] in
    let #Crypto#blake2b#150 = fun b -> (BLAKE2b(b))[@inline] in
    let #Crypto#sha256#151 = fun b -> (SHA256(b))[@inline] in
    let #Crypto#sha512#152 = fun b -> (SHA512(b))[@inline] in
    let #Crypto#sha3#153 = fun b -> (SHA3(b))[@inline] in
    let #Crypto#keccak#154 = fun b -> (KECCAK(b))[@inline] in
    let #Crypto#hash_key#155 = fun k -> (HASH_KEY(k))[@inline] in
    let #Crypto#check#156 =
      fun gen#46 ->
      (let (gen#227, gen#228) = gen#46 in
       let (gen#229, gen#230) = gen#227 in
       let k = gen#229 in
       let s = gen#230 in let b = gen#228 in CHECK_SIGNATURE(k , s , b))[@inline] in
    let assert = fun b -> (ASSERTION(b))[@inline] in
    let assert_with_error =
      fun gen#47 ->
      (let (gen#231, gen#232) = gen#47 in
       let b = gen#231 in let s = gen#232 in ASSERTION_WITH_ERROR(b , s))[@inline] in
    let abs = fun i -> (ABS(i))[@inline] in
    let is_nat = fun i -> (IS_NAT(i))[@inline] in
    let true = TRUE()[@inline] in
    let false = FALSE()[@inline] in
    let unit = UNIT()[@inline] in
    let poly_#Test#failwith_5235 = fun v -> (FAILWITH(v))[@inline] in
    let poly_#Test#failwith_5234 = fun v -> (FAILWITH(v))[@inline] in
    let poly_#Test#failwith_5233 = fun v -> (FAILWITH(v))[@inline] in
    let poly_#Test#failwith_5232 = fun v -> (FAILWITH(v))[@inline] in
    let poly_#Test#failwith_5231 = fun v -> (FAILWITH(v))[@inline] in
    let poly_#Test#failwith_5230 = fun v -> (FAILWITH(v))[@inline] in
    let poly_#Test#failwith_5229 = fun v -> (FAILWITH(v))[@inline] in
    let poly_#Test#failwith_5228 = fun v -> (FAILWITH(v))[@inline] in
    let poly_#Test#failwith_5227 = fun v -> (FAILWITH(v))[@inline] in
    let poly_#Test#failwith_5226 = fun v -> (FAILWITH(v))[@inline] in
    let poly_#Test#failwith_5225 = fun v -> (FAILWITH(v))[@inline] in
    let poly_#Test#failwith_5224 = fun v -> (FAILWITH(v))[@inline] in
    let #Test#originate_from_file#159 =
      fun gen#149 ->
      (let (gen#233, gen#234) = gen#149 in
       let (gen#235, gen#236) = gen#233 in
       let (gen#239, gen#240) = gen#235 in
       let _fn = gen#239 in
       let _e = gen#240 in
       let (gen#237, gen#238) = gen#236 in
       let _v = gen#237 in
       let _s = gen#238 in
       let _t = gen#234 in (poly_#Test#failwith_5235)@(L("TEST MODE")))[@inline] in
    let #Test#set_source#161 =
      fun _a -> ((poly_#Test#failwith_5224)@(L("TEST MODE")))[@inline] in
    let #Test#set_baker#162 =
      fun _a -> ((poly_#Test#failwith_5224)@(L("TEST MODE")))[@inline] in
    let #Test#transfer#163 =
      fun gen#151 ->
      (let (gen#241, gen#242) = gen#151 in
       let (gen#243, gen#244) = gen#241 in
       let _a = gen#243 in
       let _s = gen#244 in
       let _t = gen#242 in (poly_#Test#failwith_5224)@(L("TEST MODE")))[@inline] in
    let #Test#transfer_exn#164 =
      fun gen#152 ->
      (let (gen#245, gen#246) = gen#152 in
       let (gen#247, gen#248) = gen#245 in
       let _a = gen#247 in
       let _s = gen#248 in
       let _t = gen#246 in (poly_#Test#failwith_5232)@(L("TEST MODE")))[@inline] in
    let #Test#get_storage_of_address#168 =
      fun _a -> ((poly_#Test#failwith_5224)@(L("TEST MODE")))[@inline] in
    let #Test#get_balance#169 =
      fun _a -> ((poly_#Test#failwith_5234)@(L("TEST MODE")))[@inline] in
    let #Test#michelson_equal#170 =
      fun gen#155 ->
      (let (gen#249, gen#250) = gen#155 in
       let _m1 = gen#249 in
       let _m2 = gen#250 in (poly_#Test#failwith_5233)@(L("TEST MODE")))[@inline] in
    let #Test#reset_state#172 =
      fun gen#156 ->
      (let (gen#251, gen#252) = gen#156 in
       let _n = gen#251 in
       let _l = gen#252 in (poly_#Test#failwith_5224)@(L("TEST MODE")))[@inline] in
    let #Test#get_voting_power#173 =
      fun _kh -> ((poly_#Test#failwith_5232)@(L("TEST MODE")))[@inline] in
    let #Test#get_total_voting_power#174 =
      (poly_#Test#failwith_5232)@(L("TEST MODE"))[@inline] in
    let #Test#nth_bootstrap_contract#176 =
      fun _i -> ((poly_#Test#failwith_5231)@(L("TEST MODE")))[@inline] in
    let #Test#nth_bootstrap_account#177 =
      fun _i -> ((poly_#Test#failwith_5231)@(L("TEST MODE")))[@inline] in
    let #Test#last_originations#179 =
      fun _u -> ((poly_#Test#failwith_5230)@(L("TEST MODE")))[@inline] in
    let #Test#save_mutation#182 =
      fun gen#159 ->
      (let (gen#253, gen#254) = gen#159 in
       let _s = gen#253 in
       let _m = gen#254 in (poly_#Test#failwith_5229)@(L("TEST MODE")))[@inline] in
    let #Test#add_account#189 =
      fun gen#163 ->
      (let (gen#255, gen#256) = gen#163 in
       let _s = gen#255 in
       let _k = gen#256 in (poly_#Test#failwith_5224)@(L("TEST MODE")))[@inline] in
    let #Test#new_account#190 =
      fun _u -> ((poly_#Test#failwith_5228)@(L("TEST MODE")))[@inline] in
    let #Test#baker_account#191 =
      fun gen#164 ->
      (let (gen#257, gen#258) = gen#164 in
       let _p = gen#257 in
       let _o = gen#258 in (poly_#Test#failwith_5224)@(L("TEST MODE")))[@inline] in
    let #Test#bake_until_n_cycle_end#192 =
      fun _n -> ((poly_#Test#failwith_5224)@(L("TEST MODE")))[@inline] in
    let #Test#register_delegate#193 =
      fun _kh -> ((poly_#Test#failwith_5224)@(L("TEST MODE")))[@inline] in
    let #Test#register_constant#194 =
      fun _m -> ((poly_#Test#failwith_5227)@(L("TEST MODE")))[@inline] in
    let #Test#create_chest#199 =
      fun gen#167 ->
      (let (gen#259, gen#260) = gen#167 in
       let _b = gen#259 in
       let _n = gen#260 in (poly_#Test#failwith_5226)@(L("TEST MODE")))[@inline] in
    let #Test#create_chest_key#200 =
      fun gen#168 ->
      (let (gen#261, gen#262) = gen#168 in
       let _c = gen#261 in
       let _n = gen#262 in (poly_#Test#failwith_5225)@(L("TEST MODE")))[@inline] in
    let #Test#constant_to_michelson_program#201 =
      fun _s -> ((poly_#Test#failwith_5224)@(L("TEST MODE")))[@inline] in
    let #Test#restore_context#202 =
      fun _u -> ((poly_#Test#failwith_5224)@(L("TEST_POP_CONTEXT")))[@inline] in
    let #Test#save_context#203 =
      fun _u -> ((poly_#Test#failwith_5224)@(L("TEST_PUSH_CONTEXT")))[@inline] in
    let v = PAIR(L(1) , L("b")) in
    let #A#y#204 = v in let tm = #A#y#204 in L(unit) |}]
