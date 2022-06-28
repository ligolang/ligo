open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
<<<<<<< HEAD
    let #A#a#165 : int = 42 in
    let #B#b#166 : int = 1 in
    let x : int = #A#a#165 in
    unit |xxx}]
=======
let <A#0>a = 42 in
let <B#0>b = 1 in
let x = <A#0>a in
unit |xxx}]
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
<<<<<<< HEAD
    let #A#a#165 : int = 40 in
    let #B#b#168 : int = let #LOCAL#inA#ba#166 : int = 1 in
    let #LOCAL#inA#baa#167 : int = #LOCAL#inA#ba#166 in
    ADD(#LOCAL#inA#ba#166 ,
    #LOCAL#inA#baa#167) in
    let x : int = ADD(#A#a#165 , #B#b#168) in
=======
    let <A#0>a = 40 in
    let <B#0>b = let <B#0><A#12>ba = 1 in
    let <B#0><A#12>baa = <B#0><A#12>ba in
    ADD(<B#0><A#12>ba ,
    <B#0><A#12>baa) in
    let x = ADD(<A#0>a , <B#0>b) in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
<<<<<<< HEAD
    let #A#a#165 : int = 1 in
    let #A_s#as#166 : int = 42 in
    let #B#x#167 : int = #A#a#165 in
    let #B#b#168 : int = #A_s#as#166 in
    let x : int = #A_s#as#166 in
=======
    let <A#0>a = 1 in
    let <A_s#0>as = 42 in
    let <B#0>x = <A#0>a in
    let <B#0>b = <A_s#0>as in
    let x = <A_s#0>as in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
<<<<<<< HEAD
  let #A_s#as#165 : int = 20 in
  let #A#s_as#166 : int = 22 in
  let x : int = ADD(#A_s#as#165 , #A#s_as#166) in
=======
  let <A_s#0>as = 20 in
  let <A#0>s_as = 22 in
  let x = ADD(<A_s#0>as , <A#0>s_as) in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
<<<<<<< HEAD
  [%expect {xxx|
  let #A#a#165 : int = 1 in
  let #A#A_s#as#166 : int = 42 in
  let #A#A_s#as#167 : int = 3 in
  let x : int = #A#A_s#as#166 in
  unit |xxx}]
=======
  [%expect{|
    let <A#0>a = 1 in
    let <A#0><A_s#0>as = 42 in
    let <A#13><A_s#0>as = 3 in
    let x = <A#0><A_s#0>as in
    unit |}]
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
<<<<<<< HEAD
  let #Foo#x#165 : int = 1 in
  let foo : int = let x = 20 in
  let #LOCAL#inFoo#x#166 : int = x in
  let #LOCAL#inFoo#y#167 : int = #Foo#x#165 in
  let #LOCAL#inFoo#z#168 : int = #LOCAL#inFoo#y#167 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#166 , #LOCAL#inFoo#y#167) , x) ,
  #LOCAL#inFoo#z#168) in
  let x : int = foo in
=======
  let <Foo#0>x = 1 in
  let foo = let x = 20 in
  let <Foo#12>x = x in
  let <Foo#12>y = <Foo#0>x in
  let <Foo#12>z = <Foo#12>y in
  ADD(ADD(ADD(<Foo#12>x , <Foo#12>y) , x) ,
  <Foo#12>z) in
  let x = foo in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
<<<<<<< HEAD
  let #A#v#165 : int = 40 in
  let #A#B#v#166 : int = ADD(#A#v#165 , 1) in
  let #A#B#C#v#167 : int = ADD(#A#B#v#166 , 1) in
  let x : int = #A#B#C#v#167 in
=======
  let <A#0>v = 40 in
  let <A#0><B#0>v = ADD(<A#0>v , 1) in
  let <A#0><B#0><C#0>v = ADD(<A#0><B#0>v , 1) in
  let x = <A#0><B#0><C#0>v in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
<<<<<<< HEAD
  let #Foo#x#165 : int = 41 in
  let x : int = 1 in
  let #TFoo#x#166 : int = x in
  let #TFoo#y#167 : int = #Foo#x#165 in
  let u : int = ADD(#TFoo#x#166 , #TFoo#y#167) in
  let x : int = u in
=======
  let <Foo#0>x = 41 in
  let x = 1 in
  let <TFoo#0>x = x in
  let <TFoo#0>y = <Foo#0>x in
  let u = ADD(<TFoo#0>x , <TFoo#0>y) in
  let x = u in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
<<<<<<< HEAD
  let #A#B#x#165 : int = 41 in
  let #A#B#x#166 : int = ADD(#A#B#x#165 , 1) in
  let x : int = #A#B#x#166 in
=======
  let <A#0><B#0>x = 41 in
  let <A#12><B#0>x = ADD(<A#0><B#0>x , 1) in
  let x = <A#12><B#0>x in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
<<<<<<< HEAD
  let #A#B#x#165 : int = 42 in
  let #A#B#x#166 : int = 2 in
  let #A#y#167 : int = #A#B#x#165 in
  let x : int = #A#y#167 in
=======
  let <A#0><B#0>x = 42 in
  let <A#12><B#0>x = 2 in
  let <A#12>y = <A#0><B#0>x in
  let x = <A#12>y in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
<<<<<<< HEAD
  let #Foo#x#165 : int = 19 in
  let #Foo#y#166 : int = 22 in
  let x : int = let x = 1 in
  let u = #Foo#x#165 in
  let v = #Foo#y#166 in
=======
  let <Foo#0>x = 19 in
  let <Foo#0>y = 22 in
  let x = let x = 1 in
  let u = <Foo#0>x in
  let v = <Foo#0>y in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
<<<<<<< HEAD
  let #F#F#a#165 : int = 42 in
  let #F#F#x#166 : int = #F#F#a#165 in
  let x : int = #F#F#x#166 in
=======
  let <F#0><F#0>a = 42 in
  let <F#0><F#12>x = <F#0><F#0>a in
  let x = <F#0><F#12>x in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
<<<<<<< HEAD
  let #A#current_turn#167 : nat -> nat = lambda (i : nat) return ADD(i , +1) in
  let #A#other#168 : nat -> unit = lambda (n : nat) return let current_turn = (#A#current_turn#167)@(+1) in
=======
  let <A#0>current_turn = lambda (i : nat) return ADD(i , +1) in
  let <A#0>other = lambda (n : nat) return let current_turn = (<A#0>current_turn)@(+1) in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
  (assert)@(EQ(n ,
  current_turn)) in
  let main : ( unit * unit ) -> ( list (operation) * unit ) = lambda (gen#2 : ( unit * unit )) return  match
                                                                      gen#2 with
<<<<<<< HEAD
                                                                      | ( _p , _s ) ->
                                                                      ( LIST_EMPTY() , (#A#other#168)@(+2) ) in
=======
                                                                      | ( _p ,
                                                                      _s ) ->
                                                                      ( LIST_EMPTY() ,
                                                                      (<A#0>other)@(+2) ) in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
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
<<<<<<< HEAD
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
=======
    let <Tezos#0>balance = ({ DROP ; BALANCE })@(L(unit))[@inline] in
    let <Tezos#0>amount = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
    let <Tezos#0>now = ({ DROP ; NOW })@(L(unit))[@inline] in
    let <Tezos#0>sender = ({ DROP ; SENDER })@(L(unit))[@inline] in
    let <Tezos#0>source = ({ DROP ; SOURCE })@(L(unit))[@inline] in
    let <Tezos#0>level = ({ DROP ; LEVEL })@(L(unit))[@inline] in
    let <Tezos#0>self_address = SELF_ADDRESS()[@inline] in
    let <Tezos#0>chain_id = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
    let <Tezos#0>total_voting_power =
      ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
    let <Tezos#0>get_balance =
      fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
    let <Tezos#0>get_amount =
      fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
    let <Tezos#0>get_now = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
    let <Tezos#0>get_sender =
      fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
    let <Tezos#0>get_source =
      fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
    let <Tezos#0>get_level = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
    let <Tezos#0>get_self_address = fun _u -> (SELF_ADDRESS())[@inline] in
    let <Tezos#0>get_chain_id =
      fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
    let <Tezos#0>get_total_voting_power =
      fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
    let <Tezos#0>min_block_time = { DROP ; MIN_BLOCK_TIME }[@inline] in
    let <Tezos#0>get_min_block_time = { DROP ; MIN_BLOCK_TIME }[@inline] in
    let <Tezos#0>voting_power = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
    let <Tezos#0>implicit_account = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
    let <Tezos#0>pairing_check = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
    let <Tezos#0>open_chest =
      fun ck -> (fun c -> (fun n -> (OPEN_CHEST(ck , c , n))))[@inline] in
    let <Tezos#0>set_delegate = fun o -> (SET_DELEGATE(o))[@inline] in
    let <Bitwise#0>xor = fun l -> (fun r -> (XOR(l , r)))[@inline] in
    let <Bitwise#0>shift_left = fun l -> (fun r -> (LSL(l , r)))[@inline] in
    let <Bitwise#0>shift_right = fun l -> (fun r -> (LSR(l , r)))[@inline] in
    let <String#0>concat =
      fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
    let <String#0>sub =
      fun s ->
      (fun l ->
       (fun b ->
        (({ UNPAIR ;
           UNPAIR ;
           SLICE ;
           IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                                  b)))))[@inline] in
    let <String#0>length = fun b -> (({ SIZE })@(b))[@inline] in
    let <Bytes#0>concat =
      fun b1 -> (fun b2 -> (({ UNPAIR ; CONCAT })@(PAIR(b1 , b2))))[@inline] in
    let <Bytes#0>sub =
      fun s ->
      (fun l ->
       (fun b ->
        (({ UNPAIR ;
           UNPAIR ;
           SLICE ;
           IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) ,
                                                                  b)))))[@inline] in
    let <Bytes#0>length = fun b -> (({ SIZE })@(b))[@inline] in
    let <Crypto#0>blake2b = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let <Crypto#0>sha256 = fun b -> (({ SHA256 })@(b))[@inline] in
    let <Crypto#0>sha512 = fun b -> (({ SHA512 })@(b))[@inline] in
    let <Crypto#0>sha3 = fun b -> (({ SHA3 })@(b))[@inline] in
    let <Crypto#0>keccak = fun b -> (({ KECCAK })@(b))[@inline] in
    let <Crypto#0>hash_key = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let <Crypto#0>check =
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
    let <Test#0>originate_from_file =
      fun gen#371 ->
      (let (gen#880, gen#881) = gen#371 in
       let (gen#882, gen#883) = gen#880 in
       let (gen#886, gen#887) = gen#882 in
       let _fn = gen#886 in
       let _e = gen#887 in
       let (gen#884, gen#885) = gen#883 in
       let _v = gen#884 in
       let _s = gen#885 in
       let _t = gen#881 in (poly_failwith_15)@(L("TEST MODE")))[@inline] in
    let <Test#0>set_source =
      fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let <Test#0>set_baker =
      fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let <Test#0>set_baker_policy =
      fun _bp -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let <Test#0>transfer =
      fun gen#389 ->
      (let (gen#888, gen#889) = gen#389 in
       let (gen#890, gen#891) = gen#888 in
       let _a = gen#890 in
       let _s = gen#891 in let _t = gen#889 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let <Test#0>transfer_exn =
      fun gen#394 ->
      (let (gen#892, gen#893) = gen#394 in
       let (gen#894, gen#895) = gen#892 in
       let _a = gen#894 in
       let _s = gen#895 in
       let _t = gen#893 in (poly_failwith_12)@(L("TEST MODE")))[@inline] in
    let <Test#0>get_storage_of_address =
      fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let <Test#0>get_balance =
      fun _a -> ((poly_failwith_14)@(L("TEST MODE")))[@inline] in
    let <Test#0>michelson_equal =
      fun gen#415 ->
      (let (gen#896, gen#897) = gen#415 in
       let _m1 = gen#896 in
       let _m2 = gen#897 in (poly_failwith_13)@(L("TEST MODE")))[@inline] in
    let <Test#0>reset_state =
      fun gen#421 ->
      (let (gen#898, gen#899) = gen#421 in
       let _n = gen#898 in let _l = gen#899 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let <Test#0>reset_state_at =
      fun gen#425 ->
      (let (gen#900, gen#901) = gen#425 in
       let (gen#902, gen#903) = gen#900 in
       let _t = gen#902 in
       let _n = gen#903 in let _l = gen#901 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let <Test#0>get_voting_power =
      fun _kh -> ((poly_failwith_12)@(L("TEST MODE")))[@inline] in
    let <Test#0>get_total_voting_power =
      (poly_failwith_12)@(L("TEST MODE"))[@inline] in
    let <Test#0>nth_bootstrap_contract =
      fun _i -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
    let <Test#0>nth_bootstrap_account =
      fun _i -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
    let <Test#0>last_originations =
      fun _u -> ((poly_failwith_11)@(L("TEST MODE")))[@inline] in
    let <Test#0>save_mutation =
      fun gen#452 ->
      (let (gen#904, gen#905) = gen#452 in
       let _s = gen#904 in let _m = gen#905 in (poly_failwith_2)@(L("TEST MODE")))[@inline] in
    let <Test#0>add_account =
      fun gen#474 ->
      (let (gen#906, gen#907) = gen#474 in
       let _s = gen#906 in let _k = gen#907 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let <Test#0>new_account =
      fun _u -> ((poly_failwith_10)@(L("TEST MODE")))[@inline] in
    let <Test#0>baker_account =
      fun gen#480 ->
      (let (gen#908, gen#909) = gen#480 in
       let _p = gen#908 in let _o = gen#909 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let <Test#0>bake_until_n_cycle_end =
      fun _n -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let <Test#0>register_delegate =
      fun _kh -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let <Test#0>register_constant =
      fun _m -> ((poly_failwith_9)@(L("TEST MODE")))[@inline] in
    let <Test#0>create_chest =
      fun gen#502 ->
      (let (gen#910, gen#911) = gen#502 in
       let _b = gen#910 in let _n = gen#911 in (poly_failwith_8)@(L("TEST MODE")))[@inline] in
    let <Test#0>create_chest_key =
      fun gen#506 ->
      (let (gen#912, gen#913) = gen#506 in
       let _c = gen#912 in let _n = gen#913 in (poly_failwith_7)@(L("TEST MODE")))[@inline] in
    let <Test#0>constant_to_michelson_program =
      fun _s -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let <Test#0>restore_context =
      fun _u -> ((poly_failwith_1)@(L("TEST_POP_CONTEXT")))[@inline] in
    let <Test#0>save_context =
      fun _u -> ((poly_failwith_1)@(L("TEST_PUSH_CONTEXT")))[@inline] in
    let <Test#0>drop_context =
      fun _u -> ((poly_failwith_1)@(L("TEST_DROP_CONTEXT")))[@inline] in
    let <Test#0>read_contract_from_file =
      fun _fn -> ((poly_failwith_1)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
    let <Test#0>compile_contract_from_file =
      fun gen#520 ->
      (let (gen#914, gen#915) = gen#520 in
       let (gen#916, gen#917) = gen#914 in
       let _fn = gen#916 in
       let _e = gen#917 in
       let _v = gen#915 in
       (poly_failwith_1)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))[@inline] in
    let <Test#0>originate_contract =
      fun gen#527 ->
      (let (gen#918, gen#919) = gen#527 in
       let (gen#920, gen#921) = gen#918 in
       let _c = gen#920 in
       let _s = gen#921 in
       let _t = gen#919 in (poly_failwith_6)@(L("TEST_ORIGINATE")))[@inline] in
    let <Test#0>size = fun _c -> ((poly_failwith_5)@(L("TEST_SIZE")))[@inline] in
    let <Test#0>get_bootstrap_account =
      fun _n -> ((poly_failwith_4)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
    let <Test#0>sign =
      fun gen#536 ->
      (let (gen#922, gen#923) = gen#536 in
       let _sk = gen#922 in
       let _d = gen#923 in (poly_failwith_3)@(L("TEST_SIGN")))[@inline] in
    let <Test#0>chr = fun _n -> ((poly_failwith_2)@(L("TEST_CHR")))[@inline] in
    let <Test#0>nl = L("NEWLINE")[@inline] in
    let <Test#0>println =
      fun _v -> ((poly_failwith_1)@(L("TEST_PRINTLN")))[@inline] in
    let <Test#0>print =
      fun _v -> ((poly_failwith_1)@(L("TEST_PRINT")))[@inline] in
    let <Test#0>eprint =
      fun _v -> ((poly_failwith_1)@(L("TEST_EPRINTL")))[@inline] in
    let <Tezos#2>balance = ({ DROP ; BALANCE })@(L(unit))[@inline] in
    let <Tezos#2>amount = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
    let <Tezos#2>now = ({ DROP ; NOW })@(L(unit))[@inline] in
    let <Tezos#2>sender = ({ DROP ; SENDER })@(L(unit))[@inline] in
    let <Tezos#2>source = ({ DROP ; SOURCE })@(L(unit))[@inline] in
    let <Tezos#2>level = ({ DROP ; LEVEL })@(L(unit))[@inline] in
    let <Tezos#2>self_address = SELF_ADDRESS()[@inline] in
    let <Tezos#2>chain_id = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
    let <Tezos#2>total_voting_power =
      ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
    let <Tezos#2>get_balance =
      fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
    let <Tezos#2>get_amount =
      fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
    let <Tezos#2>get_now = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
    let <Tezos#2>get_sender =
      fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
    let <Tezos#2>get_source =
      fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
    let <Tezos#2>get_level = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
    let <Tezos#2>get_self_address = fun _u -> (SELF_ADDRESS())[@inline] in
    let <Tezos#2>get_chain_id =
      fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
    let <Tezos#2>get_total_voting_power =
      fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
    let <Tezos#2>voting_power = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
    let <Tezos#2>implicit_account = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
    let <Tezos#2>pairing_check = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
    let <Tezos#2>open_chest =
      fun gen#608 ->
      (let (gen#924, gen#925) = gen#608 in
       let (gen#926, gen#927) = gen#924 in
       let ck = gen#926 in
       let c = gen#927 in let n = gen#925 in OPEN_CHEST(ck , c , n))[@inline] in
    let <Tezos#2>set_delegate = fun o -> (SET_DELEGATE(o))[@inline] in
    let <Bitwise#3>xor =
      fun gen#627 ->
      (let (gen#928, gen#929) = gen#627 in
       let l = gen#928 in let r = gen#929 in XOR(l , r))[@inline] in
    let <Bitwise#3>shift_left =
      fun gen#631 ->
      (let (gen#930, gen#931) = gen#631 in
       let l = gen#930 in let r = gen#931 in LSL(l , r))[@inline] in
    let <Bitwise#3>shift_right =
      fun gen#635 ->
      (let (gen#932, gen#933) = gen#635 in
       let l = gen#932 in let r = gen#933 in LSR(l , r))[@inline] in
    let <String#8>concat =
      fun gen#797 ->
      (let (gen#934, gen#935) = gen#797 in
       let b1 = gen#934 in
       let b2 = gen#935 in ({ UNPAIR ; CONCAT })@(PAIR(b1 , b2)))[@inline] in
    let <String#8>sub =
      fun gen#801 ->
      (let (gen#936, gen#937) = gen#801 in
       let (gen#938, gen#939) = gen#936 in
       let s = gen#938 in
       let l = gen#939 in
       let b = gen#937 in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
<<<<<<< HEAD
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
=======
    let <String#8>length = fun b -> (({ SIZE })@(b))[@inline] in
    let <Bytes#10>concat =
      fun gen#814 ->
      (let (gen#940, gen#941) = gen#814 in
       let b1 = gen#940 in
       let b2 = gen#941 in ({ UNPAIR ; CONCAT })@(PAIR(b1 , b2)))[@inline] in
    let <Bytes#10>sub =
      fun gen#818 ->
      (let (gen#942, gen#943) = gen#818 in
       let (gen#944, gen#945) = gen#942 in
       let s = gen#944 in
       let l = gen#945 in
       let b = gen#943 in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
<<<<<<< HEAD
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
=======
    let <Bytes#10>length = fun b -> (({ SIZE })@(b))[@inline] in
    let <Crypto#11>blake2b = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let <Crypto#11>sha256 = fun b -> (({ SHA256 })@(b))[@inline] in
    let <Crypto#11>sha512 = fun b -> (({ SHA512 })@(b))[@inline] in
    let <Crypto#11>sha3 = fun b -> (({ SHA3 })@(b))[@inline] in
    let <Crypto#11>keccak = fun b -> (({ KECCAK })@(b))[@inline] in
    let <Crypto#11>hash_key = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let <Crypto#11>check =
      fun gen#841 ->
      (let (gen#946, gen#947) = gen#841 in
       let (gen#948, gen#949) = gen#946 in
       let k = gen#948 in
       let s = gen#949 in
       let b = gen#947 in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))[@inline] in
    let assert =
      fun b ->
      (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
    let assert_with_error =
<<<<<<< HEAD
      fun gen#537 ->
      (let (gen#778, gen#779) = gen#537 in
       let b = gen#778 in
       let s = gen#779 in
=======
      fun gen#848 ->
      (let (gen#950, gen#951) = gen#848 in
       let b = gen#950 in
       let s = gen#951 in
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
       ({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s)))[@inline] in
    let abs = fun i -> (({ ABS })@(i))[@inline] in
    let is_nat = fun i -> (({ ISNAT })@(i))[@inline] in
    let true = TRUE()[@inline] in
    let false = FALSE()[@inline] in
    let unit = UNIT()[@inline] in
<<<<<<< HEAD
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
=======
    let v = PAIR(L(1) , L("b")) in let <A#0>y = v in let tm = <A#0>y in L(unit) |}]
>>>>>>> cc9bbfcb0a6ec18c4f9549fd5948bf67e761df0d
