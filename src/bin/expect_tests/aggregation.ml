open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #A#a#132 = 42 in
    let #B#b#133 = 1 in
    let x = #A#a#132 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #A#a#132 = 40 in
    let #B#b#135 = let #LOCAL#inA#ba#133 = 1 in
    let #LOCAL#inA#baa#134 = #LOCAL#inA#ba#133 in
    ADD(#LOCAL#inA#ba#133 ,
    #LOCAL#inA#baa#134) in
    let x = ADD(#A#a#132 , #B#b#135) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #A#a#132 = 1 in
    let #A_s#as#133 = 42 in
    let #B#x#134 = #A#a#132 in
    let #B#b#135 = #A_s#as#133 in
    let x = #A_s#as#133 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #A_s#as#132 = 20 in
  let #A#s_as#133 = 22 in
  let x = ADD(#A_s#as#132 , #A#s_as#133) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #A#a#132 = 1 in
  let #A#A_s#as#133 = 42 in
  let #A#A_s#as#134 = 3 in
  let x = #A#A_s#as#133 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#132 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#133 = x in
  let #LOCAL#inFoo#y#134 = #Foo#x#132 in
  let #LOCAL#inFoo#z#135 = #LOCAL#inFoo#y#134 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#133 , #LOCAL#inFoo#y#134) , x) ,
  #LOCAL#inFoo#z#135) in
  let x = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #A#v#132 = 40 in
  let #A#B#v#133 = ADD(#A#v#132 , 1) in
  let #A#B#C#v#134 = ADD(#A#B#v#133 , 1) in
  let x = #A#B#C#v#134 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#132 = 41 in
  let x = 1 in
  let #TFoo#x#133 = x in
  let #TFoo#y#134 = #Foo#x#132 in
  let u = ADD(#TFoo#x#133 , #TFoo#y#134) in
  let x = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#132 = 41 in
  let #A#B#x#133 = ADD(#A#B#x#132 , 1) in
  let x = #A#B#x#133 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#132 = 42 in
  let #A#B#x#133 = 2 in
  let #A#y#134 = #A#B#x#132 in
  let x = #A#y#134 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#132 = 19 in
  let #Foo#y#133 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#132 in
  let v = #Foo#y#133 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #F#F#a#132 = 42 in
  let #F#F#x#133 = #F#F#a#132 in
  let x = #F#F#x#133 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #A#current_turn#135 = lambda (i) return ADD(i , +1) in
  let #A#other#136 = lambda (n) return let current_turn = (#A#current_turn#135)@(+1) in
  (assert)@(EQ(n ,
  current_turn)) in
  let main = lambda (gen#2) return  match gen#2 with
                                     | ( _p , _s ) ->
                                     ( LIST_EMPTY() , (#A#other#136)@(+2) ) in
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
  [%expect {|
    let #Tezos#balance#71 = BALANCE()[@inline] in
    let #Tezos#amount#72 = AMOUNT()[@inline] in
    let #Tezos#now#73 = NOW()[@inline] in
    let #Tezos#sender#74 = SENDER()[@inline] in
    let #Tezos#source#75 = SOURCE()[@inline] in
    let #Tezos#level#76 = LEVEL()[@inline] in
    let #Tezos#self_address#77 = SELF_ADDRESS()[@inline] in
    let #Tezos#chain_id#78 = CHAIN_ID()[@inline] in
    let #Tezos#total_voting_power#79 = TOTAL_VOTING_POWER()[@inline] in
    let #Tezos#voting_power#80 = fun kh -> (VOTING_POWER(kh))[@inline] in
    let #Tezos#implicit_account#82 = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
    let #Tezos#pairing_check#88 = fun l -> (PAIRING_CHECK(l))[@inline] in
    let #Tezos#open_chest#89 =
      fun gen#5 ->
      (let (gen#200, gen#201) = gen#5 in
       let (gen#202, gen#203) = gen#200 in
       let ck = gen#202 in
       let c = gen#203 in let n = gen#201 in OPEN_CHEST(ck , c , n))[@inline] in
    let #Tezos#set_delegate#93 = fun o -> (SET_DELEGATE(o))[@inline] in
    let #Bitwise#xor#94 =
      fun gen#8 ->
      (let (gen#204, gen#205) = gen#8 in
       let l = gen#204 in let r = gen#205 in XOR(l , r))[@inline] in
    let #Bitwise#shift_left#95 =
      fun gen#9 ->
      (let (gen#206, gen#207) = gen#9 in
       let l = gen#206 in let r = gen#207 in LSL(l , r))[@inline] in
    let #Bitwise#shift_right#96 =
      fun gen#10 ->
      (let (gen#208, gen#209) = gen#10 in
       let l = gen#208 in let r = gen#209 in LSR(l , r))[@inline] in
    let #String#concat#135 =
      fun gen#39 ->
      (let (gen#210, gen#211) = gen#39 in
       let b1 = gen#210 in let b2 = gen#211 in CONCAT(b1 , b2))[@inline] in
    let #String#sub#136 =
      fun gen#40 ->
      (let (gen#212, gen#213) = gen#40 in
       let (gen#214, gen#215) = gen#212 in
       let s = gen#214 in let l = gen#215 in let b = gen#213 in SLICE(s , l , b))[@inline] in
    let #String#length#137 = fun b -> (SIZE(b))[@inline] in
    let #Bytes#concat#140 =
      fun gen#42 ->
      (let (gen#216, gen#217) = gen#42 in
       let b1 = gen#216 in let b2 = gen#217 in CONCAT(b1 , b2))[@inline] in
    let #Bytes#sub#141 =
      fun gen#43 ->
      (let (gen#218, gen#219) = gen#43 in
       let (gen#220, gen#221) = gen#218 in
       let s = gen#220 in let l = gen#221 in let b = gen#219 in SLICE(s , l , b))[@inline] in
    let #Bytes#length#144 = fun b -> (SIZE(b))[@inline] in
    let #Crypto#blake2b#145 = fun b -> (BLAKE2b(b))[@inline] in
    let #Crypto#sha256#146 = fun b -> (SHA256(b))[@inline] in
    let #Crypto#sha512#147 = fun b -> (SHA512(b))[@inline] in
    let #Crypto#sha3#148 = fun b -> (SHA3(b))[@inline] in
    let #Crypto#keccak#149 = fun b -> (KECCAK(b))[@inline] in
    let #Crypto#hash_key#150 = fun k -> (HASH_KEY(k))[@inline] in
    let #Crypto#check#151 =
      fun gen#44 ->
      (let (gen#222, gen#223) = gen#44 in
       let (gen#224, gen#225) = gen#222 in
       let k = gen#224 in
       let s = gen#225 in let b = gen#223 in CHECK_SIGNATURE(k , s , b))[@inline] in
    let assert = fun b -> (ASSERTION(b))[@inline] in
    let assert_with_error =
      fun gen#45 ->
      (let (gen#226, gen#227) = gen#45 in
       let b = gen#226 in let s = gen#227 in ASSERTION_WITH_ERROR(b , s))[@inline] in
    let abs = fun i -> (ABS(i))[@inline] in
    let is_nat = fun i -> (IS_NAT(i))[@inline] in
    let true = TRUE()[@inline] in
    let false = FALSE()[@inline] in
    let unit = UNIT()[@inline] in
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
    let poly_#Test#failwith_5223 = fun v -> (FAILWITH(v))[@inline] in
    let poly_#Test#failwith_5222 = fun v -> (FAILWITH(v))[@inline] in
    let #Test#originate_from_file#154 =
      fun gen#143 ->
      (let (gen#228, gen#229) = gen#143 in
       let (gen#230, gen#231) = gen#228 in
       let (gen#234, gen#235) = gen#230 in
       let _fn = gen#234 in
       let _e = gen#235 in
       let (gen#232, gen#233) = gen#231 in
       let _v = gen#232 in
       let _s = gen#233 in
       let _t = gen#229 in (poly_#Test#failwith_5233)@(L("TEST MODE")))[@inline] in
    let #Test#set_source#156 =
      fun _a -> ((poly_#Test#failwith_5222)@(L("TEST MODE")))[@inline] in
    let #Test#set_baker#157 =
      fun _a -> ((poly_#Test#failwith_5222)@(L("TEST MODE")))[@inline] in
    let #Test#transfer#158 =
      fun gen#145 ->
      (let (gen#236, gen#237) = gen#145 in
       let (gen#238, gen#239) = gen#236 in
       let _a = gen#238 in
       let _s = gen#239 in
       let _t = gen#237 in (poly_#Test#failwith_5222)@(L("TEST MODE")))[@inline] in
    let #Test#transfer_exn#159 =
      fun gen#146 ->
      (let (gen#240, gen#241) = gen#146 in
       let (gen#242, gen#243) = gen#240 in
       let _a = gen#242 in
       let _s = gen#243 in
       let _t = gen#241 in (poly_#Test#failwith_5230)@(L("TEST MODE")))[@inline] in
    let #Test#get_storage_of_address#163 =
      fun _a -> ((poly_#Test#failwith_5222)@(L("TEST MODE")))[@inline] in
    let #Test#get_balance#164 =
      fun _a -> ((poly_#Test#failwith_5232)@(L("TEST MODE")))[@inline] in
    let #Test#michelson_equal#165 =
      fun gen#149 ->
      (let (gen#244, gen#245) = gen#149 in
       let _m1 = gen#244 in
       let _m2 = gen#245 in (poly_#Test#failwith_5231)@(L("TEST MODE")))[@inline] in
    let #Test#reset_state#167 =
      fun gen#150 ->
      (let (gen#246, gen#247) = gen#150 in
       let _n = gen#246 in
       let _l = gen#247 in (poly_#Test#failwith_5222)@(L("TEST MODE")))[@inline] in
    let #Test#get_voting_power#168 =
      fun _kh -> ((poly_#Test#failwith_5230)@(L("TEST MODE")))[@inline] in
    let #Test#get_total_voting_power#169 =
      (poly_#Test#failwith_5230)@(L("TEST MODE"))[@inline] in
    let #Test#nth_bootstrap_contract#171 =
      fun _i -> ((poly_#Test#failwith_5229)@(L("TEST MODE")))[@inline] in
    let #Test#nth_bootstrap_account#172 =
      fun _i -> ((poly_#Test#failwith_5229)@(L("TEST MODE")))[@inline] in
    let #Test#last_originations#174 =
      fun _u -> ((poly_#Test#failwith_5228)@(L("TEST MODE")))[@inline] in
    let #Test#save_mutation#177 =
      fun gen#153 ->
      (let (gen#248, gen#249) = gen#153 in
       let _s = gen#248 in
       let _m = gen#249 in (poly_#Test#failwith_5227)@(L("TEST MODE")))[@inline] in
    let #Test#add_account#184 =
      fun gen#157 ->
      (let (gen#250, gen#251) = gen#157 in
       let _s = gen#250 in
       let _k = gen#251 in (poly_#Test#failwith_5222)@(L("TEST MODE")))[@inline] in
    let #Test#new_account#185 =
      fun _u -> ((poly_#Test#failwith_5226)@(L("TEST MODE")))[@inline] in
    let #Test#baker_account#186 =
      fun gen#158 ->
      (let (gen#252, gen#253) = gen#158 in
       let _p = gen#252 in
       let _o = gen#253 in (poly_#Test#failwith_5222)@(L("TEST MODE")))[@inline] in
    let #Test#bake_until_n_cycle_end#187 =
      fun _n -> ((poly_#Test#failwith_5222)@(L("TEST MODE")))[@inline] in
    let #Test#register_delegate#188 =
      fun _kh -> ((poly_#Test#failwith_5222)@(L("TEST MODE")))[@inline] in
    let #Test#register_constant#189 =
      fun _m -> ((poly_#Test#failwith_5225)@(L("TEST MODE")))[@inline] in
    let #Test#create_chest#194 =
      fun gen#161 ->
      (let (gen#254, gen#255) = gen#161 in
       let _b = gen#254 in
       let _n = gen#255 in (poly_#Test#failwith_5224)@(L("TEST MODE")))[@inline] in
    let #Test#create_chest_key#195 =
      fun gen#162 ->
      (let (gen#256, gen#257) = gen#162 in
       let _c = gen#256 in
       let _n = gen#257 in (poly_#Test#failwith_5223)@(L("TEST MODE")))[@inline] in
    let #Test#constant_to_michelson_program#196 =
      fun _s -> ((poly_#Test#failwith_5222)@(L("TEST MODE")))[@inline] in
    let #Test#restore_context#197 =
      fun _u -> ((poly_#Test#failwith_5222)@(L("TEST_POP_CONTEXT")))[@inline] in
    let #Test#save_context#198 =
      fun _u -> ((poly_#Test#failwith_5222)@(L("TEST_PUSH_CONTEXT")))[@inline] in
    let v = PAIR(L(1) , L("b")) in
    let #A#y#199 = v in let tm = #A#y#199 in L(unit) |}]
