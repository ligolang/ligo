open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #A#a#159 : int = 42 in
    let #B#b#160 : int = 1 in
    let x : int = #A#a#159 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #A#a#159 : int = 40 in
    let #B#b#162 : int = let #LOCAL#inA#ba#160 : int = 1 in
    let #LOCAL#inA#baa#161 : int = #LOCAL#inA#ba#160 in
    ADD(#LOCAL#inA#ba#160 ,
    #LOCAL#inA#baa#161) in
    let x : int = ADD(#A#a#159 , #B#b#162) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #A#a#159 : int = 1 in
    let #A_s#as#160 : int = 42 in
    let #B#x#161 : int = #A#a#159 in
    let #B#b#162 : int = #A_s#as#160 in
    let x : int = #A_s#as#160 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #A_s#as#159 : int = 20 in
  let #A#s_as#160 : int = 22 in
  let x : int = ADD(#A_s#as#159 , #A#s_as#160) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #A#a#159 : int = 1 in
  let #A#A_s#as#160 : int = 42 in
  let #A#A_s#as#161 : int = 3 in
  let x : int = #A#A_s#as#160 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#159 : int = 1 in
  let foo : int = let x = 20 in
  let #LOCAL#inFoo#x#160 : int = x in
  let #LOCAL#inFoo#y#161 : int = #Foo#x#159 in
  let #LOCAL#inFoo#z#162 : int = #LOCAL#inFoo#y#161 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#160 , #LOCAL#inFoo#y#161) , x) ,
  #LOCAL#inFoo#z#162) in
  let x : int = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #A#v#159 : int = 40 in
  let #A#B#v#160 : int = ADD(#A#v#159 , 1) in
  let #A#B#C#v#161 : int = ADD(#A#B#v#160 , 1) in
  let x : int = #A#B#C#v#161 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#159 : int = 41 in
  let x : int = 1 in
  let #TFoo#x#160 : int = x in
  let #TFoo#y#161 : int = #Foo#x#159 in
  let u : int = ADD(#TFoo#x#160 , #TFoo#y#161) in
  let x : int = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#159 : int = 41 in
  let #A#B#x#160 : int = ADD(#A#B#x#159 , 1) in
  let x : int = #A#B#x#160 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#159 : int = 42 in
  let #A#B#x#160 : int = 2 in
  let #A#y#161 : int = #A#B#x#159 in
  let x : int = #A#y#161 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#159 : int = 19 in
  let #Foo#y#160 : int = 22 in
  let x : int = let x = 1 in
  let u = #Foo#x#159 in
  let v = #Foo#y#160 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #F#F#a#159 : int = 42 in
  let #F#F#x#160 : int = #F#F#a#159 in
  let x : int = #F#F#x#160 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #A#current_turn#161 : nat -> nat = lambda (i : nat) return ADD(i , +1) in
  let #A#other#162 : nat -> unit = lambda (n : nat) return let current_turn = (#A#current_turn#161)@(+1) in
  (assert)@(EQ(n ,
  current_turn)) in
  let main : ( unit * unit ) -> ( list (operation) * unit ) = lambda (gen#2 : ( unit * unit )) return  match
                                                                      gen#2 with
                                                                      | ( _p , _s ) ->
                                                                      ( LIST_EMPTY() , (#A#other#162)@(+2) ) in
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
      fun gen#291 ->
      (let (gen#735, gen#736) = gen#291 in
       let (gen#737, gen#738) = gen#735 in
       let ck = gen#737 in
       let c = gen#738 in let n = gen#736 in OPEN_CHEST(ck , c , n))[@inline] in
    let #Tezos#set_delegate#115 = fun o -> (SET_DELEGATE(o))[@inline] in
    let #Bitwise#xor#116 =
      fun gen#310 ->
      (let (gen#739, gen#740) = gen#310 in
       let l = gen#739 in let r = gen#740 in XOR(l , r))[@inline] in
    let #Bitwise#shift_left#117 =
      fun gen#314 ->
      (let (gen#741, gen#742) = gen#314 in
       let l = gen#741 in let r = gen#742 in LSL(l , r))[@inline] in
    let #Bitwise#shift_right#118 =
      fun gen#318 ->
      (let (gen#743, gen#744) = gen#318 in
       let l = gen#743 in let r = gen#744 in LSR(l , r))[@inline] in
    let #String#concat#159 =
      fun gen#480 ->
      (let (gen#745, gen#746) = gen#480 in
       let b1 = gen#745 in
       let b2 = gen#746 in ({ UNPAIR ; CONCAT })@(PAIR(b1 , b2)))[@inline] in
    let #String#sub#160 =
      fun gen#484 ->
      (let (gen#747, gen#748) = gen#484 in
       let (gen#749, gen#750) = gen#747 in
       let s = gen#749 in
       let l = gen#750 in
       let b = gen#748 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
    let #String#length#161 = fun b -> (({ SIZE })@(b))[@inline] in
    let #Bytes#concat#164 =
      fun gen#497 ->
      (let (gen#751, gen#752) = gen#497 in
       let b1 = gen#751 in
       let b2 = gen#752 in ({ UNPAIR ; CONCAT })@(PAIR(b1 , b2)))[@inline] in
    let #Bytes#sub#165 =
      fun gen#501 ->
      (let (gen#753, gen#754) = gen#501 in
       let (gen#755, gen#756) = gen#753 in
       let s = gen#755 in
       let l = gen#756 in
       let b = gen#754 in
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
      fun gen#524 ->
      (let (gen#757, gen#758) = gen#524 in
       let (gen#759, gen#760) = gen#757 in
       let k = gen#759 in
       let s = gen#760 in
       let b = gen#758 in
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))[@inline] in
    let assert =
      fun b ->
      (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
    let assert_with_error =
      fun gen#531 ->
      (let (gen#761, gen#762) = gen#531 in
       let b = gen#761 in
       let s = gen#762 in
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
      fun gen#564 ->
      (let (gen#763, gen#764) = gen#564 in
       let (gen#765, gen#766) = gen#763 in
       let (gen#769, gen#770) = gen#765 in
       let _fn = gen#769 in
       let _e = gen#770 in
       let (gen#767, gen#768) = gen#766 in
       let _v = gen#767 in
       let _s = gen#768 in
       let _t = gen#764 in (poly_failwith_15)@(L("TEST MODE")))[@inline] in
    let #Test#set_source#179 =
      fun _a -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#set_baker#180 =
      fun _a -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#set_baker_policy#181 =
      fun _bp -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#transfer#182 =
      fun gen#582 ->
      (let (gen#771, gen#772) = gen#582 in
       let (gen#773, gen#774) = gen#771 in
       let _a = gen#773 in
       let _s = gen#774 in let _t = gen#772 in (poly_failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#transfer_exn#183 =
      fun gen#587 ->
      (let (gen#775, gen#776) = gen#587 in
       let (gen#777, gen#778) = gen#775 in
       let _a = gen#777 in
       let _s = gen#778 in
       let _t = gen#776 in (poly_failwith_12)@(L("TEST MODE")))[@inline] in
    let #Test#get_storage_of_address#187 =
      fun _a -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#get_balance#188 =
      fun _a -> ((poly_failwith_14)@(L("TEST MODE")))[@inline] in
    let #Test#michelson_equal#189 =
      fun gen#608 ->
      (let (gen#779, gen#780) = gen#608 in
       let _m1 = gen#779 in
       let _m2 = gen#780 in (poly_failwith_13)@(L("TEST MODE")))[@inline] in
    let #Test#reset_state#191 =
      fun gen#614 ->
      (let (gen#781, gen#782) = gen#614 in
       let _n = gen#781 in let _l = gen#782 in (poly_failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#reset_state_at#192 =
      fun gen#618 ->
      (let (gen#783, gen#784) = gen#618 in
       let (gen#785, gen#786) = gen#783 in
       let _t = gen#785 in
       let _n = gen#786 in let _l = gen#784 in (poly_failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#get_voting_power#193 =
      fun _kh -> ((poly_failwith_12)@(L("TEST MODE")))[@inline] in
    let #Test#get_total_voting_power#194 =
      (poly_failwith_12)@(L("TEST MODE"))[@inline] in
    let #Test#nth_bootstrap_contract#196 =
      fun _i -> ((poly_failwith_4)@(L("TEST MODE")))[@inline] in
    let #Test#nth_bootstrap_account#197 =
      fun _i -> ((poly_failwith_4)@(L("TEST MODE")))[@inline] in
    let #Test#last_originations#199 =
      fun _u -> ((poly_failwith_11)@(L("TEST MODE")))[@inline] in
    let #Test#save_mutation#202 =
      fun gen#645 ->
      (let (gen#787, gen#788) = gen#645 in
       let _s = gen#787 in
       let _m = gen#788 in (poly_failwith_10)@(L("TEST MODE")))[@inline] in
    let #Test#add_account#209 =
      fun gen#667 ->
      (let (gen#789, gen#790) = gen#667 in
       let _s = gen#789 in let _k = gen#790 in (poly_failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#new_account#210 =
      fun _u -> ((poly_failwith_9)@(L("TEST MODE")))[@inline] in
    let #Test#baker_account#211 =
      fun gen#673 ->
      (let (gen#791, gen#792) = gen#673 in
       let _p = gen#791 in let _o = gen#792 in (poly_failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#bake_until_n_cycle_end#212 =
      fun _n -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#register_delegate#213 =
      fun _kh -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#register_constant#214 =
      fun _m -> ((poly_failwith_8)@(L("TEST MODE")))[@inline] in
    let #Test#create_chest#219 =
      fun gen#695 ->
      (let (gen#793, gen#794) = gen#695 in
       let _b = gen#793 in let _n = gen#794 in (poly_failwith_7)@(L("TEST MODE")))[@inline] in
    let #Test#create_chest_key#220 =
      fun gen#699 ->
      (let (gen#795, gen#796) = gen#699 in
       let _c = gen#795 in let _n = gen#796 in (poly_failwith_6)@(L("TEST MODE")))[@inline] in
    let #Test#constant_to_michelson_program#221 =
      fun _s -> ((poly_failwith_5)@(L("TEST MODE")))[@inline] in
    let #Test#restore_context#222 =
      fun _u -> ((poly_failwith_5)@(L("TEST_POP_CONTEXT")))[@inline] in
    let #Test#save_context#223 =
      fun _u -> ((poly_failwith_5)@(L("TEST_PUSH_CONTEXT")))[@inline] in
    let #Test#drop_context#224 =
      fun _u -> ((poly_failwith_5)@(L("TEST_DROP_CONTEXT")))[@inline] in
    let #Test#read_contract_from_file#225 =
      fun _fn -> ((poly_failwith_5)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
    let #Test#compile_contract_from_file#226 =
      fun gen#713 ->
      (let (gen#797, gen#798) = gen#713 in
       let (gen#799, gen#800) = gen#797 in
       let _fn = gen#799 in
       let _e = gen#800 in
       let _v = gen#798 in
       (poly_failwith_5)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))[@inline] in
    let #Test#originate_contract#228 =
      fun gen#720 ->
      (let (gen#801, gen#802) = gen#720 in
       let (gen#803, gen#804) = gen#801 in
       let _c = gen#803 in
       let _s = gen#804 in
       let _t = gen#802 in (poly_failwith_4)@(L("TEST_ORIGINATE")))[@inline] in
    let #Test#size#229 =
      fun _c -> ((poly_failwith_3)@(L("TEST_SIZE")))[@inline] in
    let #Test#get_bootstrap_account#230 =
      fun _n -> ((poly_failwith_2)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
    let #Test#sign#231 =
      fun gen#729 ->
      (let (gen#805, gen#806) = gen#729 in
       let _sk = gen#805 in
       let _d = gen#806 in (poly_failwith_1)@(L("TEST_SIGN")))[@inline] in
    let v = PAIR(L(1) , L("b")) in
    let #A#y#232 = v in let tm = #A#y#232 in L(unit) |}]
