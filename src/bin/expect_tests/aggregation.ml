open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
let <A#0>a = 42 in
let <B#0>b = 1 in
let x = <A#0>a in
unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let <A#0>a = 40 in
    let <B#0>b = let <B#0><A#12>ba = 1 in
    let <B#0><A#12>baa = <B#0><A#12>ba in
    ADD(<B#0><A#12>ba ,
    <B#0><A#12>baa) in
    let x = ADD(<A#0>a , <B#0>b) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let <A#0>a = 1 in
    let <A_s#0>as = 42 in
    let <B#0>x = <A#0>a in
    let <B#0>b = <A_s#0>as in
    let x = <A_s#0>as in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let <A_s#0>as = 20 in
  let <A#0>s_as = 22 in
  let x = ADD(<A_s#0>as , <A#0>s_as) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect{|
    let <A#0>a = 1 in
    let <A#0><A_s#0>as = 42 in
    let <A#13><A_s#0>as = 3 in
    let x = <A#0><A_s#0>as in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let <Foo#0>x = 1 in
  let foo = let x = 20 in
  let <Foo#12>x = x in
  let <Foo#12>y = <Foo#0>x in
  let <Foo#12>z = <Foo#12>y in
  ADD(ADD(ADD(<Foo#12>x , <Foo#12>y) , x) ,
  <Foo#12>z) in
  let x = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let <A#0>v = 40 in
  let <A#0><B#0>v = ADD(<A#0>v , 1) in
  let <A#0><B#0><C#0>v = ADD(<A#0><B#0>v , 1) in
  let x = <A#0><B#0><C#0>v in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let <Foo#0>x = 41 in
  let x = 1 in
  let <TFoo#0>x = x in
  let <TFoo#0>y = <Foo#0>x in
  let u = ADD(<TFoo#0>x , <TFoo#0>y) in
  let x = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let <A#0><B#0>x = 41 in
  let <A#12><B#0>x = ADD(<A#0><B#0>x , 1) in
  let x = <A#12><B#0>x in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let <A#0><B#0>x = 42 in
  let <A#12><B#0>x = 2 in
  let <A#12>y = <A#0><B#0>x in
  let x = <A#12>y in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let <Foo#0>x = 19 in
  let <Foo#0>y = 22 in
  let x = let x = 1 in
  let u = <Foo#0>x in
  let v = <Foo#0>y in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let <F#0><F#0>a = 42 in
  let <F#0><F#12>x = <F#0><F#0>a in
  let x = <F#0><F#12>x in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let <A#0>current_turn = lambda (i : nat) return ADD(i , +1) in
  let <A#0>other = lambda (n : nat) return let current_turn = (<A#0>current_turn)@(+1) in
  (assert)@(EQ(n ,
  current_turn)) in
  let main : ( unit * unit ) -> ( list (operation) * unit ) = lambda (gen#2 : ( unit * unit )) return  match
                                                                      gen#2 with
                                                                      | ( _p ,
                                                                      _s ) ->
                                                                      ( LIST_EMPTY() ,
                                                                      (<A#0>other)@(+2) ) in
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
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
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
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
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
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))[@inline] in
    let assert =
      fun b ->
      (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
    let assert_with_error =
      fun gen#848 ->
      (let (gen#950, gen#951) = gen#848 in
       let b = gen#950 in
       let s = gen#951 in
       ({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b , s)))[@inline] in
    let abs = fun i -> (({ ABS })@(i))[@inline] in
    let is_nat = fun i -> (({ ISNAT })@(i))[@inline] in
    let true = TRUE()[@inline] in
    let false = FALSE()[@inline] in
    let unit = UNIT()[@inline] in
    let v = PAIR(L(1) , L("b")) in let <A#0>y = v in let tm = <A#0>y in L(unit) |}]
