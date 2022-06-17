open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #A#a#263 : int = 42 in
    let #B#b#264 : int = 1 in
    let x : int = #A#a#263 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #A#a#263 : int = 40 in
    let #B#b#266 : int = let #LOCAL#inA#ba#264 : int = 1 in
    let #LOCAL#inA#baa#265 : int = #LOCAL#inA#ba#264 in
    ADD(#LOCAL#inA#ba#264 ,
    #LOCAL#inA#baa#265) in
    let x : int = ADD(#A#a#263 , #B#b#266) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #A#a#263 : int = 1 in
    let #A_s#as#264 : int = 42 in
    let #B#x#265 : int = #A#a#263 in
    let #B#b#266 : int = #A_s#as#264 in
    let x : int = #A_s#as#264 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #A_s#as#263 : int = 20 in
  let #A#s_as#264 : int = 22 in
  let x : int = ADD(#A_s#as#263 , #A#s_as#264) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #A#a#263 : int = 1 in
  let #A#A_s#as#264 : int = 42 in
  let #A#A_s#as#265 : int = 3 in
  let x : int = #A#A_s#as#264 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#263 : int = 1 in
  let foo : int = let x = 20 in
  let #LOCAL#inFoo#x#264 : int = x in
  let #LOCAL#inFoo#y#265 : int = #Foo#x#263 in
  let #LOCAL#inFoo#z#266 : int = #LOCAL#inFoo#y#265 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#264 , #LOCAL#inFoo#y#265) , x) ,
  #LOCAL#inFoo#z#266) in
  let x : int = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #A#v#263 : int = 40 in
  let #A#B#v#264 : int = ADD(#A#v#263 , 1) in
  let #A#B#C#v#265 : int = ADD(#A#B#v#264 , 1) in
  let x : int = #A#B#C#v#265 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#263 : int = 41 in
  let x : int = 1 in
  let #TFoo#x#264 : int = x in
  let #TFoo#y#265 : int = #Foo#x#263 in
  let u : int = ADD(#TFoo#x#264 , #TFoo#y#265) in
  let x : int = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#263 : int = 41 in
  let #A#B#x#264 : int = ADD(#A#B#x#263 , 1) in
  let x : int = #A#B#x#264 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#263 : int = 42 in
  let #A#B#x#264 : int = 2 in
  let #A#y#265 : int = #A#B#x#263 in
  let x : int = #A#y#265 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#263 : int = 19 in
  let #Foo#y#264 : int = 22 in
  let x : int = let x = 1 in
  let u = #Foo#x#263 in
  let v = #Foo#y#264 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #F#F#a#263 : int = 42 in
  let #F#F#x#264 : int = #F#F#a#263 in
  let x : int = #F#F#x#264 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #A#current_turn#265 : nat -> nat = lambda (i : nat) return ADD(i , +1) in
  let #A#other#266 : nat -> unit = lambda (n : nat) return let current_turn = (#A#current_turn#265)@(+1) in
  (assert)@(EQ(n ,
  current_turn)) in
  let main : ( unit * unit ) -> ( list (operation) * unit ) = lambda (gen#2 : ( unit * unit )) return  match
                                                                      gen#2 with
                                                                      | ( _p , _s ) ->
                                                                      ( LIST_EMPTY() , (#A#other#266)@(+2) ) in
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
    let #Tezos#balance#90 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
    let #Tezos#amount#91 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
    let #Tezos#now#92 = ({ DROP ; NOW })@(L(unit))[@inline] in
    let #Tezos#sender#93 = ({ DROP ; SENDER })@(L(unit))[@inline] in
    let #Tezos#source#94 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
    let #Tezos#level#95 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
    let #Tezos#self_address#96 = SELF_ADDRESS()[@inline] in
    let #Tezos#chain_id#97 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
    let #Tezos#total_voting_power#98 =
      ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
    let #Tezos#get_balance#99 =
      fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
    let #Tezos#get_amount#100 =
      fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
    let #Tezos#get_now#101 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
    let #Tezos#get_sender#102 =
      fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
    let #Tezos#get_source#103 =
      fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
    let #Tezos#get_level#104 =
      fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
    let #Tezos#get_self_address#105 = fun _u -> (SELF_ADDRESS())[@inline] in
    let #Tezos#get_chain_id#106 =
      fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
    let #Tezos#get_total_voting_power#107 =
      fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
    let #Tezos#min_block_time#108 = { DROP ; MIN_BLOCK_TIME }[@inline] in
    let #Tezos#get_min_block_time#109 = { DROP ; MIN_BLOCK_TIME }[@inline] in
    let #Tezos#voting_power#110 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
    let #Tezos#implicit_account#112 =
      fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
    let #Tezos#pairing_check#118 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
    let #Tezos#open_chest#119 =
      fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
    let #Tezos#set_delegate#123 = fun o -> (SET_DELEGATE(o))[@inline] in
    let #Bitwise#xor#124 = fun l -> (fun r -> (XOR(l , r)))[@inline] in
    let #Bitwise#shift_left#125 = fun l -> (fun r -> (LSL(l , r)))[@inline] in
    let #Bitwise#shift_right#126 = fun l -> (fun r -> (LSR(l , r)))[@inline] in
    let #String#concat#167 =
      fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
    let #String#sub#168 =
      fun s ->
      (fun l ->
       (fun b ->
        (({ UNPAIR ;
           UNPAIR ;
           SLICE ;
           IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                                  b)))))[@inline] in
    let #String#length#169 = fun b -> (({ SIZE })@(b))[@inline] in
    let #Bytes#concat#172 =
      fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
    let #Bytes#sub#173 =
      fun s ->
      (fun l ->
       (fun b ->
        (({ UNPAIR ;
           UNPAIR ;
           SLICE ;
           IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                                  b)))))[@inline] in
    let #Bytes#length#176 = fun b -> (({ SIZE })@(b))[@inline] in
    let #Crypto#blake2b#177 = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let #Crypto#sha256#178 = fun b -> (({ SHA256 })@(b))[@inline] in
    let #Crypto#sha512#179 = fun b -> (({ SHA512 })@(b))[@inline] in
    let #Crypto#sha3#180 = fun b -> (({ SHA3 })@(b))[@inline] in
    let #Crypto#keccak#181 = fun b -> (({ KECCAK })@(b))[@inline] in
    let #Crypto#hash_key#182 = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let #Crypto#check#183 =
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
    let #Test#originate_from_file#186 =
      fun gen#621 ->
      (let (gen#1119, gen#1120) = gen#621 in
       let (gen#1121, gen#1122) = gen#1119 in
       let (gen#1125, gen#1126) = gen#1121 in
       let _fn = gen#1125 in
       let _e = gen#1126 in
       let (gen#1123, gen#1124) = gen#1122 in
       let _v = gen#1123 in
       let _s = gen#1124 in
       let _t = gen#1120 in (poly_#Test#failwith_15)@(L("TEST MODE")))[@inline] in
    let #Test#set_source#188 =
      fun _a -> ((poly_#Test#failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#set_baker#189 =
      fun _a -> ((poly_#Test#failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#set_baker_policy#190 =
      fun _bp -> ((poly_#Test#failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#transfer#191 =
      fun gen#639 ->
      (let (gen#1127, gen#1128) = gen#639 in
       let (gen#1129, gen#1130) = gen#1127 in
       let _a = gen#1129 in
       let _s = gen#1130 in
       let _t = gen#1128 in (poly_#Test#failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#transfer_exn#192 =
      fun gen#644 ->
      (let (gen#1131, gen#1132) = gen#644 in
       let (gen#1133, gen#1134) = gen#1131 in
       let _a = gen#1133 in
       let _s = gen#1134 in
       let _t = gen#1132 in (poly_#Test#failwith_12)@(L("TEST MODE")))[@inline] in
    let #Test#get_storage_of_address#196 =
      fun _a -> ((poly_#Test#failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#get_balance#197 =
      fun _a -> ((poly_#Test#failwith_14)@(L("TEST MODE")))[@inline] in
    let #Test#michelson_equal#198 =
      fun gen#665 ->
      (let (gen#1135, gen#1136) = gen#665 in
       let _m1 = gen#1135 in
       let _m2 = gen#1136 in (poly_#Test#failwith_13)@(L("TEST MODE")))[@inline] in
    let #Test#reset_state#200 =
      fun gen#671 ->
      (let (gen#1137, gen#1138) = gen#671 in
       let _n = gen#1137 in
       let _l = gen#1138 in (poly_#Test#failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#reset_state_at#201 =
      fun gen#675 ->
      (let (gen#1139, gen#1140) = gen#675 in
       let (gen#1141, gen#1142) = gen#1139 in
       let _t = gen#1141 in
       let _n = gen#1142 in
       let _l = gen#1140 in (poly_#Test#failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#get_voting_power#202 =
      fun _kh -> ((poly_#Test#failwith_12)@(L("TEST MODE")))[@inline] in
    let #Test#get_total_voting_power#203 =
      (poly_#Test#failwith_12)@(L("TEST MODE"))[@inline] in
    let #Test#nth_bootstrap_contract#205 =
      fun _i -> ((poly_#Test#failwith_4)@(L("TEST MODE")))[@inline] in
    let #Test#nth_bootstrap_account#206 =
      fun _i -> ((poly_#Test#failwith_4)@(L("TEST MODE")))[@inline] in
    let #Test#last_originations#208 =
      fun _u -> ((poly_#Test#failwith_11)@(L("TEST MODE")))[@inline] in
    let #Test#save_mutation#211 =
      fun gen#702 ->
      (let (gen#1143, gen#1144) = gen#702 in
       let _s = gen#1143 in
       let _m = gen#1144 in (poly_#Test#failwith_10)@(L("TEST MODE")))[@inline] in
    let #Test#add_account#218 =
      fun gen#724 ->
      (let (gen#1145, gen#1146) = gen#724 in
       let _s = gen#1145 in
       let _k = gen#1146 in (poly_#Test#failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#new_account#219 =
      fun _u -> ((poly_#Test#failwith_9)@(L("TEST MODE")))[@inline] in
    let #Test#baker_account#220 =
      fun gen#730 ->
      (let (gen#1147, gen#1148) = gen#730 in
       let _p = gen#1147 in
       let _o = gen#1148 in (poly_#Test#failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#bake_until_n_cycle_end#221 =
      fun _n -> ((poly_#Test#failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#register_delegate#222 =
      fun _kh -> ((poly_#Test#failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#register_constant#223 =
      fun _m -> ((poly_#Test#failwith_8)@(L("TEST MODE")))[@inline] in
    let #Test#create_chest#228 =
      fun gen#752 ->
      (let (gen#1149, gen#1150) = gen#752 in
       let _b = gen#1149 in
       let _n = gen#1150 in (poly_#Test#failwith_7)@(L("TEST MODE")))[@inline] in
    let #Test#create_chest_key#229 =
      fun gen#756 ->
      (let (gen#1151, gen#1152) = gen#756 in
       let _c = gen#1151 in
       let _n = gen#1152 in (poly_#Test#failwith_6)@(L("TEST MODE")))[@inline] in
    let #Test#constant_to_michelson_program#230 =
      fun _s -> ((poly_#Test#failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#restore_context#231 =
      fun _u -> ((poly_#Test#failwith_5)@(L("TEST_POP_CONTEXT")))[@inline] in
    let #Test#save_context#232 =
      fun _u -> ((poly_#Test#failwith_5)@(L("TEST_PUSH_CONTEXT")))[@inline] in
    let #Test#drop_context#233 =
      fun _u -> ((poly_#Test#failwith_5)@(L("TEST_DROP_CONTEXT")))[@inline] in
    let #Test#read_contract_from_file#234 =
      fun _fn -> ((poly_#Test#failwith_5)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
    let #Test#compile_contract_from_file#235 =
      fun gen#770 ->
      (let (gen#1153, gen#1154) = gen#770 in
       let (gen#1155, gen#1156) = gen#1153 in
       let _fn = gen#1155 in
       let _e = gen#1156 in
       let _v = gen#1154 in
       (poly_#Test#failwith_5)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))[@inline] in
    let #Test#originate_contract#237 =
      fun gen#777 ->
      (let (gen#1157, gen#1158) = gen#777 in
       let (gen#1159, gen#1160) = gen#1157 in
       let _c = gen#1159 in
       let _s = gen#1160 in
       let _t = gen#1158 in (poly_#Test#failwith_4)@(L("TEST_ORIGINATE")))[@inline] in
    let #Test#size#238 =
      fun _c -> ((poly_#Test#failwith_3)@(L("TEST_SIZE")))[@inline] in
    let #Test#get_bootstrap_account#239 =
      fun _n -> ((poly_#Test#failwith_2)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
    let #Test#sign#240 =
      fun gen#786 ->
      (let (gen#1161, gen#1162) = gen#786 in
       let _sk = gen#1161 in
       let _d = gen#1162 in (poly_#Test#failwith_1)@(L("TEST_SIGN")))[@inline] in
    let #Tezos#balance#244 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
    let #Tezos#amount#245 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
    let #Tezos#now#246 = ({ DROP ; NOW })@(L(unit))[@inline] in
    let #Tezos#sender#247 = ({ DROP ; SENDER })@(L(unit))[@inline] in
    let #Tezos#source#248 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
    let #Tezos#level#249 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
    let #Tezos#self_address#250 = SELF_ADDRESS()[@inline] in
    let #Tezos#chain_id#251 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
    let #Tezos#total_voting_power#252 =
      ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
    let #Tezos#get_balance#253 =
      fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
    let #Tezos#get_amount#254 =
      fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
    let #Tezos#get_now#255 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
    let #Tezos#get_sender#256 =
      fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
    let #Tezos#get_source#257 =
      fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
    let #Tezos#get_level#258 =
      fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
    let #Tezos#get_self_address#259 = fun _u -> (SELF_ADDRESS())[@inline] in
    let #Tezos#get_chain_id#260 =
      fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
    let #Tezos#get_total_voting_power#261 =
      fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
    let #Tezos#voting_power#262 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
    let #Tezos#implicit_account#264 =
      fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
    let #Tezos#pairing_check#270 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
    let #Tezos#open_chest#271 =
      fun gen#847 ->
      (let (gen#1163, gen#1164) = gen#847 in
       let (gen#1165, gen#1166) = gen#1163 in
       let ck = gen#1165 in
       let c = gen#1166 in let n = gen#1164 in OPEN_CHEST(ck , c , n))[@inline] in
    let #Tezos#set_delegate#275 = fun o -> (SET_DELEGATE(o))[@inline] in
    let #Bitwise#xor#276 =
      fun gen#866 ->
      (let (gen#1167, gen#1168) = gen#866 in
       let l = gen#1167 in let r = gen#1168 in XOR(l , r))[@inline] in
    let #Bitwise#shift_left#277 =
      fun gen#870 ->
      (let (gen#1169, gen#1170) = gen#870 in
       let l = gen#1169 in let r = gen#1170 in LSL(l , r))[@inline] in
    let #Bitwise#shift_right#278 =
      fun gen#874 ->
      (let (gen#1171, gen#1172) = gen#874 in
       let l = gen#1171 in let r = gen#1172 in LSR(l , r))[@inline] in
    let #String#concat#319 =
      fun gen#1036 ->
      (let (gen#1173, gen#1174) = gen#1036 in
       let b1 = gen#1173 in
       let b2 = gen#1174 in ({ UNPAIR ; CONCAT })@(PAIR(b1 , b2)))[@inline] in
    let #String#sub#320 =
      fun gen#1040 ->
      (let (gen#1175, gen#1176) = gen#1040 in
       let (gen#1177, gen#1178) = gen#1175 in
       let s = gen#1177 in
       let l = gen#1178 in
       let b = gen#1176 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
    let #String#length#321 = fun b -> (({ SIZE })@(b))[@inline] in
    let #Bytes#concat#324 =
      fun gen#1053 ->
      (let (gen#1179, gen#1180) = gen#1053 in
       let b1 = gen#1179 in
       let b2 = gen#1180 in ({ UNPAIR ; CONCAT })@(PAIR(b1 , b2)))[@inline] in
    let #Bytes#sub#325 =
      fun gen#1057 ->
      (let (gen#1181, gen#1182) = gen#1057 in
       let (gen#1183, gen#1184) = gen#1181 in
       let s = gen#1183 in
       let l = gen#1184 in
       let b = gen#1182 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
    let #Bytes#length#328 = fun b -> (({ SIZE })@(b))[@inline] in
    let #Crypto#blake2b#329 = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let #Crypto#sha256#330 = fun b -> (({ SHA256 })@(b))[@inline] in
    let #Crypto#sha512#331 = fun b -> (({ SHA512 })@(b))[@inline] in
    let #Crypto#sha3#332 = fun b -> (({ SHA3 })@(b))[@inline] in
    let #Crypto#keccak#333 = fun b -> (({ KECCAK })@(b))[@inline] in
    let #Crypto#hash_key#334 = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let #Crypto#check#335 =
      fun gen#1080 ->
      (let (gen#1185, gen#1186) = gen#1080 in
       let (gen#1187, gen#1188) = gen#1185 in
       let k = gen#1187 in
       let s = gen#1188 in
       let b = gen#1186 in
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))[@inline] in
    let assert =
      fun b ->
      (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
    let assert_with_error =
      fun gen#1087 ->
      (let (gen#1189, gen#1190) = gen#1087 in
       let b = gen#1189 in
       let s = gen#1190 in
       ({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s)))[@inline] in
    let abs = fun i -> (({ ABS })@(i))[@inline] in
    let is_nat = fun i -> (({ ISNAT })@(i))[@inline] in
    let true = TRUE()[@inline] in
    let false = FALSE()[@inline] in
    let unit = UNIT()[@inline] in
    let v = PAIR(L(1) , L("b")) in
    let #A#y#336 = v in let tm = #A#y#336 in L(unit) |}]
