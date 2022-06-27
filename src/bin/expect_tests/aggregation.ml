open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #A#a#165 : int = 42 in
    let #B#b#166 : int = 1 in
    let x : int = #A#a#165 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #A#a#165 : int = 40 in
    let #B#b#168 : int = let #LOCAL#inA#ba#166 : int = 1 in
    let #LOCAL#inA#baa#167 : int = #LOCAL#inA#ba#166 in
    ADD(#LOCAL#inA#ba#166 ,
    #LOCAL#inA#baa#167) in
    let x : int = ADD(#A#a#165 , #B#b#168) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #A#a#165 : int = 1 in
    let #A_s#as#166 : int = 42 in
    let #B#x#167 : int = #A#a#165 in
    let #B#b#168 : int = #A_s#as#166 in
    let x : int = #A_s#as#166 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #A_s#as#165 : int = 20 in
  let #A#s_as#166 : int = 22 in
  let x : int = ADD(#A_s#as#165 , #A#s_as#166) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #A#a#165 : int = 1 in
  let #A#A_s#as#166 : int = 42 in
  let #A#A_s#as#167 : int = 3 in
  let x : int = #A#A_s#as#166 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#165 : int = 1 in
  let foo : int = let x = 20 in
  let #LOCAL#inFoo#x#166 : int = x in
  let #LOCAL#inFoo#y#167 : int = #Foo#x#165 in
  let #LOCAL#inFoo#z#168 : int = #LOCAL#inFoo#y#167 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#166 , #LOCAL#inFoo#y#167) , x) ,
  #LOCAL#inFoo#z#168) in
  let x : int = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #A#v#165 : int = 40 in
  let #A#B#v#166 : int = ADD(#A#v#165 , 1) in
  let #A#B#C#v#167 : int = ADD(#A#B#v#166 , 1) in
  let x : int = #A#B#C#v#167 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#165 : int = 41 in
  let x : int = 1 in
  let #TFoo#x#166 : int = x in
  let #TFoo#y#167 : int = #Foo#x#165 in
  let u : int = ADD(#TFoo#x#166 , #TFoo#y#167) in
  let x : int = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#165 : int = 41 in
  let #A#B#x#166 : int = ADD(#A#B#x#165 , 1) in
  let x : int = #A#B#x#166 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#165 : int = 42 in
  let #A#B#x#166 : int = 2 in
  let #A#y#167 : int = #A#B#x#165 in
  let x : int = #A#y#167 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#165 : int = 19 in
  let #Foo#y#166 : int = 22 in
  let x : int = let x = 1 in
  let u = #Foo#x#165 in
  let v = #Foo#y#166 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #F#F#a#165 : int = 42 in
  let #F#F#x#166 : int = #F#F#a#165 in
  let x : int = #F#F#x#166 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #A#current_turn#167 : nat -> nat = lambda (i : nat) return ADD(i , +1) in
  let #A#other#168 : nat -> unit = lambda (n : nat) return let current_turn = (#A#current_turn#167)@(+1) in
  (assert)@(EQ(n ,
  current_turn)) in
  let main : ( unit * unit ) -> ( list (operation) * unit ) = lambda (gen#2 : ( unit * unit )) return  match
                                                                      gen#2 with
                                                                      | ( _p , _s ) ->
                                                                      ( LIST_EMPTY() , (#A#other#168)@(+2) ) in
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
    let #Tezos#balance#84 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
    let #Tezos#amount#85 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
    let #Tezos#now#86 = ({ DROP ; NOW })@(L(unit))[@inline] in
    let #Tezos#sender#87 = ({ DROP ; SENDER })@(L(unit))[@inline] in
    let #Tezos#source#88 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
    let #Tezos#level#89 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
    let #Tezos#self_address#90 = SELF_ADDRESS()[@inline] in
    let #Tezos#chain_id#91 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
    let #Tezos#total_voting_power#92 =
      ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
    let #Tezos#get_balance#93 =
      fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
    let #Tezos#get_amount#94 =
      fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
    let #Tezos#get_now#95 = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
    let #Tezos#get_sender#96 =
      fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
    let #Tezos#get_source#97 =
      fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
    let #Tezos#get_level#98 =
      fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
    let #Tezos#get_self_address#99 = fun _u -> (SELF_ADDRESS())[@inline] in
    let #Tezos#get_chain_id#100 =
      fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
    let #Tezos#get_total_voting_power#101 =
      fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
    let #Tezos#voting_power#102 = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
    let #Tezos#implicit_account#104 =
      fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
    let #Tezos#pairing_check#110 = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
    let #Tezos#open_chest#111 =
      fun gen#297 ->
      (let (gen#752, gen#753) = gen#297 in
       let (gen#754, gen#755) = gen#752 in
       let ck = gen#754 in
       let c = gen#755 in let n = gen#753 in OPEN_CHEST(ck , c , n))[@inline] in
    let #Tezos#set_delegate#115 = fun o -> (SET_DELEGATE(o))[@inline] in
    let #Bitwise#xor#116 =
      fun gen#316 ->
      (let (gen#756, gen#757) = gen#316 in
       let l = gen#756 in let r = gen#757 in XOR(l , r))[@inline] in
    let #Bitwise#shift_left#117 =
      fun gen#320 ->
      (let (gen#758, gen#759) = gen#320 in
       let l = gen#758 in let r = gen#759 in LSL(l , r))[@inline] in
    let #Bitwise#shift_right#118 =
      fun gen#324 ->
      (let (gen#760, gen#761) = gen#324 in
       let l = gen#760 in let r = gen#761 in LSR(l , r))[@inline] in
    let #String#concat#159 =
      fun gen#486 ->
      (let (gen#762, gen#763) = gen#486 in
       let b1 = gen#762 in
       let b2 = gen#763 in ({ UNPAIR ; CONCAT })@(PAIR(b1 , b2)))[@inline] in
    let #String#sub#160 =
      fun gen#490 ->
      (let (gen#764, gen#765) = gen#490 in
       let (gen#766, gen#767) = gen#764 in
       let s = gen#766 in
       let l = gen#767 in
       let b = gen#765 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
    let #String#length#161 = fun b -> (({ SIZE })@(b))[@inline] in
    let #Bytes#concat#164 =
      fun gen#503 ->
      (let (gen#768, gen#769) = gen#503 in
       let b1 = gen#768 in
       let b2 = gen#769 in ({ UNPAIR ; CONCAT })@(PAIR(b1 , b2)))[@inline] in
    let #Bytes#sub#165 =
      fun gen#507 ->
      (let (gen#770, gen#771) = gen#507 in
       let (gen#772, gen#773) = gen#770 in
       let s = gen#772 in
       let l = gen#773 in
       let b = gen#771 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
    let #Bytes#length#168 = fun b -> (({ SIZE })@(b))[@inline] in
    let #Crypto#blake2b#169 = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let #Crypto#sha256#170 = fun b -> (({ SHA256 })@(b))[@inline] in
    let #Crypto#sha512#171 = fun b -> (({ SHA512 })@(b))[@inline] in
    let #Crypto#sha3#172 = fun b -> (({ SHA3 })@(b))[@inline] in
    let #Crypto#keccak#173 = fun b -> (({ KECCAK })@(b))[@inline] in
    let #Crypto#hash_key#174 = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let #Crypto#check#175 =
      fun gen#530 ->
      (let (gen#774, gen#775) = gen#530 in
       let (gen#776, gen#777) = gen#774 in
       let k = gen#776 in
       let s = gen#777 in
       let b = gen#775 in
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))[@inline] in
    let assert =
      fun b ->
      (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
    let assert_with_error =
      fun gen#537 ->
      (let (gen#778, gen#779) = gen#537 in
       let b = gen#778 in
       let s = gen#779 in
       ({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s)))[@inline] in
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
    let #Test#originate_from_file#177 =
      fun gen#570 ->
      (let (gen#780, gen#781) = gen#570 in
       let (gen#782, gen#783) = gen#780 in
       let (gen#786, gen#787) = gen#782 in
       let _fn = gen#786 in
       let _e = gen#787 in
       let (gen#784, gen#785) = gen#783 in
       let _v = gen#784 in
       let _s = gen#785 in
       let _t = gen#781 in (poly_failwith_15)@(L("TEST MODE")))[@inline] in
    let #Test#set_source#179 =
      fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let #Test#set_baker#180 =
      fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let #Test#set_baker_policy#181 =
      fun _bp -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let #Test#transfer#182 =
      fun gen#588 ->
      (let (gen#788, gen#789) = gen#588 in
       let (gen#790, gen#791) = gen#788 in
       let _a = gen#790 in
       let _s = gen#791 in let _t = gen#789 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let #Test#transfer_exn#183 =
      fun gen#593 ->
      (let (gen#792, gen#793) = gen#593 in
       let (gen#794, gen#795) = gen#792 in
       let _a = gen#794 in
       let _s = gen#795 in
       let _t = gen#793 in (poly_failwith_12)@(L("TEST MODE")))[@inline] in
    let #Test#get_storage_of_address#187 =
      fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let #Test#get_balance#188 =
      fun _a -> ((poly_failwith_14)@(L("TEST MODE")))[@inline] in
    let #Test#michelson_equal#189 =
      fun gen#614 ->
      (let (gen#796, gen#797) = gen#614 in
       let _m1 = gen#796 in
       let _m2 = gen#797 in (poly_failwith_13)@(L("TEST MODE")))[@inline] in
    let #Test#reset_state#191 =
      fun gen#620 ->
      (let (gen#798, gen#799) = gen#620 in
       let _n = gen#798 in let _l = gen#799 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let #Test#reset_state_at#192 =
      fun gen#624 ->
      (let (gen#800, gen#801) = gen#624 in
       let (gen#802, gen#803) = gen#800 in
       let _t = gen#802 in
       let _n = gen#803 in let _l = gen#801 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let #Test#get_voting_power#193 =
      fun _kh -> ((poly_failwith_12)@(L("TEST MODE")))[@inline] in
    let #Test#get_total_voting_power#194 =
      (poly_failwith_12)@(L("TEST MODE"))[@inline] in
    let #Test#nth_bootstrap_contract#196 =
      fun _i -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
    let #Test#nth_bootstrap_account#197 =
      fun _i -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
    let #Test#last_originations#199 =
      fun _u -> ((poly_failwith_11)@(L("TEST MODE")))[@inline] in
    let #Test#save_mutation#202 =
      fun gen#651 ->
      (let (gen#804, gen#805) = gen#651 in
       let _s = gen#804 in let _m = gen#805 in (poly_failwith_2)@(L("TEST MODE")))[@inline] in
    let #Test#add_account#209 =
      fun gen#673 ->
      (let (gen#806, gen#807) = gen#673 in
       let _s = gen#806 in let _k = gen#807 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let #Test#new_account#210 =
      fun _u -> ((poly_failwith_10)@(L("TEST MODE")))[@inline] in
    let #Test#baker_account#211 =
      fun gen#679 ->
      (let (gen#808, gen#809) = gen#679 in
       let _p = gen#808 in let _o = gen#809 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let #Test#bake_until_n_cycle_end#212 =
      fun _n -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let #Test#register_delegate#213 =
      fun _kh -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let #Test#register_constant#214 =
      fun _m -> ((poly_failwith_9)@(L("TEST MODE")))[@inline] in
    let #Test#create_chest#219 =
      fun gen#701 ->
      (let (gen#810, gen#811) = gen#701 in
       let _b = gen#810 in let _n = gen#811 in (poly_failwith_8)@(L("TEST MODE")))[@inline] in
    let #Test#create_chest_key#220 =
      fun gen#705 ->
      (let (gen#812, gen#813) = gen#705 in
       let _c = gen#812 in let _n = gen#813 in (poly_failwith_7)@(L("TEST MODE")))[@inline] in
    let #Test#constant_to_michelson_program#221 =
      fun _s -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let #Test#restore_context#222 =
      fun _u -> ((poly_failwith_1)@(L("TEST_POP_CONTEXT")))[@inline] in
    let #Test#save_context#223 =
      fun _u -> ((poly_failwith_1)@(L("TEST_PUSH_CONTEXT")))[@inline] in
    let #Test#drop_context#224 =
      fun _u -> ((poly_failwith_1)@(L("TEST_DROP_CONTEXT")))[@inline] in
    let #Test#read_contract_from_file#225 =
      fun _fn -> ((poly_failwith_1)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
    let #Test#compile_contract_from_file#226 =
      fun gen#719 ->
      (let (gen#814, gen#815) = gen#719 in
       let (gen#816, gen#817) = gen#814 in
       let _fn = gen#816 in
       let _e = gen#817 in
       let _v = gen#815 in
       (poly_failwith_1)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))[@inline] in
    let #Test#originate_contract#228 =
      fun gen#726 ->
      (let (gen#818, gen#819) = gen#726 in
       let (gen#820, gen#821) = gen#818 in
       let _c = gen#820 in
       let _s = gen#821 in
       let _t = gen#819 in (poly_failwith_6)@(L("TEST_ORIGINATE")))[@inline] in
    let #Test#size#229 =
      fun _c -> ((poly_failwith_5)@(L("TEST_SIZE")))[@inline] in
    let #Test#get_bootstrap_account#230 =
      fun _n -> ((poly_failwith_4)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
    let #Test#sign#231 =
      fun gen#735 ->
      (let (gen#822, gen#823) = gen#735 in
       let _sk = gen#822 in
       let _d = gen#823 in (poly_failwith_3)@(L("TEST_SIGN")))[@inline] in
    let #Test#chr#232 = fun _n -> ((poly_failwith_2)@(L("TEST_CHR")))[@inline] in
    let #Test#nl#233 = L("NEWLINE")[@inline] in
    let #Test#println#234 =
      fun _v -> ((poly_failwith_1)@(L("TEST_PRINTLN")))[@inline] in
    let #Test#print#235 =
      fun _v -> ((poly_failwith_1)@(L("TEST_PRINT")))[@inline] in
    let #Test#eprint#236 =
      fun _v -> ((poly_failwith_1)@(L("TEST_EPRINTL")))[@inline] in
    let v = PAIR(L(1) , L("b")) in
    let #A#y#238 = v in let tm = #A#y#238 in L(unit) |}]
