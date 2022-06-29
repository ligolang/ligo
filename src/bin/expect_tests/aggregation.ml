open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
let a = 42 in
let b = 1 in
let x = a in
unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let a = 40 in
    let b = let ba = 1 in
    let baa = ba in
    ADD(ba ,
    baa) in
    let x = ADD(a , b) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let a = 1 in
    let as = 42 in
    let x = a in
    let b = as in
    let x = as in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let as = 20 in
  let s_as = 22 in
  let x = ADD(as , s_as) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect{|
    let a = 1 in
    let as = 42 in
    let as = 3 in
    let x = as in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 3 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let x = 1 in
  let foo = let x = 20 in
  let x = x in
  let y = x in
  let z = y in
  ADD(ADD(ADD(x , y) , x) ,
  z) in
  let x = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 80 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let v = 40 in
  let v = ADD(v , 1) in
  let v = ADD(v , 1) in
  let x = v in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let x = 41 in
  let x = 1 in
  let x = x in
  let y = x in
  let u = ADD(x , y) in
  let x = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let x = 41 in
  let x = ADD(x , 1) in
  let x = x in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let x = 42 in
  let x = 2 in
  let y = x in
  let x = y in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 2 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let x = 19 in
  let y = 22 in
  let x = let x = 1 in
  let u = x in
  let v = y in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let a = 42 in
  let x = a in
  let x = x in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let current_turn = lambda (i : nat) return ADD(i , +1) in
  let other = lambda (n : nat) return let current_turn = (current_turn)@(+1) in
  (assert)@(EQ(n ,
  current_turn)) in
  let main : ( unit * unit ) -> ( list (operation) * unit ) = lambda (gen#2 : ( unit * unit )) return  match
                                                                      gen#2 with
                                                                      | ( _p ,
                                                                      _s ) ->
                                                                      ( LIST_EMPTY() ,
                                                                      (other)@(+2) ) in
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
    let balance = ({ DROP ; BALANCE })@(L(unit))[@inline] in
    let amount = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
    let now = ({ DROP ; NOW })@(L(unit))[@inline] in
    let sender = ({ DROP ; SENDER })@(L(unit))[@inline] in
    let source = ({ DROP ; SOURCE })@(L(unit))[@inline] in
    let level = ({ DROP ; LEVEL })@(L(unit))[@inline] in
    let self_address = SELF_ADDRESS()[@inline] in
    let chain_id = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
    let total_voting_power =
      ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
    let get_balance = fun _u -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
    let get_amount = fun _u -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
    let get_now = fun _u -> (({ DROP ; NOW })@(L(unit)))[@inline] in
    let get_sender = fun _u -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
    let get_source = fun _u -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
    let get_level = fun _u -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
    let get_self_address = fun _u -> (SELF_ADDRESS())[@inline] in
    let get_chain_id = fun _u -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
    let get_total_voting_power =
      fun _u -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
    let voting_power = fun kh -> (({ VOTING_POWER })@(kh))[@inline] in
    let implicit_account = fun kh -> (IMPLICIT_ACCOUNT(kh))[@inline] in
    let pairing_check = fun l -> (({ PAIRING_CHECK })@(l))[@inline] in
    let open_chest =
      fun gen#139 ->
      (let (gen#594, gen#595) = gen#139 in
       let (gen#596, gen#597) = gen#594 in
       let ck = gen#596 in
       let c = gen#597 in let n = gen#595 in OPEN_CHEST(ck , c , n))[@inline] in
    let set_delegate = fun o -> (SET_DELEGATE(o))[@inline] in
    let xor =
      fun gen#158 ->
      (let (gen#598, gen#599) = gen#158 in
       let l = gen#598 in let r = gen#599 in XOR(l , r))[@inline] in
    let shift_left =
      fun gen#162 ->
      (let (gen#600, gen#601) = gen#162 in
       let l = gen#600 in let r = gen#601 in LSL(l , r))[@inline] in
    let shift_right =
      fun gen#166 ->
      (let (gen#602, gen#603) = gen#166 in
       let l = gen#602 in let r = gen#603 in LSR(l , r))[@inline] in
    let concat =
      fun gen#328 ->
      (let (gen#604, gen#605) = gen#328 in
       let b1 = gen#604 in
       let b2 = gen#605 in ({ UNPAIR ; CONCAT })@(PAIR(b1 , b2)))[@inline] in
    let sub =
      fun gen#332 ->
      (let (gen#606, gen#607) = gen#332 in
       let (gen#608, gen#609) = gen#606 in
       let s = gen#608 in
       let l = gen#609 in
       let b = gen#607 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
    let length = fun b -> (({ SIZE })@(b))[@inline] in
    let concat =
      fun gen#345 ->
      (let (gen#610, gen#611) = gen#345 in
       let b1 = gen#610 in
       let b2 = gen#611 in ({ UNPAIR ; CONCAT })@(PAIR(b1 , b2)))[@inline] in
    let sub =
      fun gen#349 ->
      (let (gen#612, gen#613) = gen#349 in
       let (gen#614, gen#615) = gen#612 in
       let s = gen#614 in
       let l = gen#615 in
       let b = gen#613 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s , l) , b)))[@inline] in
    let length = fun b -> (({ SIZE })@(b))[@inline] in
    let blake2b = fun b -> (({ BLAKE2B })@(b))[@inline] in
    let sha256 = fun b -> (({ SHA256 })@(b))[@inline] in
    let sha512 = fun b -> (({ SHA512 })@(b))[@inline] in
    let sha3 = fun b -> (({ SHA3 })@(b))[@inline] in
    let keccak = fun b -> (({ KECCAK })@(b))[@inline] in
    let hash_key = fun k -> (({ HASH_KEY })@(k))[@inline] in
    let check =
      fun gen#372 ->
      (let (gen#616, gen#617) = gen#372 in
       let (gen#618, gen#619) = gen#616 in
       let k = gen#618 in
       let s = gen#619 in
       let b = gen#617 in
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k , s) , b)))[@inline] in
    let assert =
      fun b ->
      (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b))[@inline] in
    let assert_with_error =
      fun gen#379 ->
      (let (gen#620, gen#621) = gen#379 in
       let b = gen#620 in
       let s = gen#621 in
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
    let originate_from_file =
      fun gen#412 ->
      (let (gen#622, gen#623) = gen#412 in
       let (gen#624, gen#625) = gen#622 in
       let (gen#628, gen#629) = gen#624 in
       let _fn = gen#628 in
       let _e = gen#629 in
       let (gen#626, gen#627) = gen#625 in
       let _v = gen#626 in
       let _s = gen#627 in
       let _t = gen#623 in (poly_failwith_15)@(L("TEST MODE")))[@inline] in
    let set_source = fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let set_baker = fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let set_baker_policy =
      fun _bp -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let transfer =
      fun gen#430 ->
      (let (gen#630, gen#631) = gen#430 in
       let (gen#632, gen#633) = gen#630 in
       let _a = gen#632 in
       let _s = gen#633 in let _t = gen#631 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let transfer_exn =
      fun gen#435 ->
      (let (gen#634, gen#635) = gen#435 in
       let (gen#636, gen#637) = gen#634 in
       let _a = gen#636 in
       let _s = gen#637 in
       let _t = gen#635 in (poly_failwith_12)@(L("TEST MODE")))[@inline] in
    let get_storage_of_address =
      fun _a -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let get_balance = fun _a -> ((poly_failwith_14)@(L("TEST MODE")))[@inline] in
    let michelson_equal =
      fun gen#456 ->
      (let (gen#638, gen#639) = gen#456 in
       let _m1 = gen#638 in
       let _m2 = gen#639 in (poly_failwith_13)@(L("TEST MODE")))[@inline] in
    let reset_state =
      fun gen#462 ->
      (let (gen#640, gen#641) = gen#462 in
       let _n = gen#640 in let _l = gen#641 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let reset_state_at =
      fun gen#466 ->
      (let (gen#642, gen#643) = gen#466 in
       let (gen#644, gen#645) = gen#642 in
       let _t = gen#644 in
       let _n = gen#645 in let _l = gen#643 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let get_voting_power =
      fun _kh -> ((poly_failwith_12)@(L("TEST MODE")))[@inline] in
    let get_total_voting_power = (poly_failwith_12)@(L("TEST MODE"))[@inline] in
    let nth_bootstrap_contract =
      fun _i -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
    let nth_bootstrap_account =
      fun _i -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
    let last_originations =
      fun _u -> ((poly_failwith_11)@(L("TEST MODE")))[@inline] in
    let save_mutation =
      fun gen#493 ->
      (let (gen#646, gen#647) = gen#493 in
       let _s = gen#646 in let _m = gen#647 in (poly_failwith_2)@(L("TEST MODE")))[@inline] in
    let add_account =
      fun gen#515 ->
      (let (gen#648, gen#649) = gen#515 in
       let _s = gen#648 in let _k = gen#649 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let new_account = fun _u -> ((poly_failwith_10)@(L("TEST MODE")))[@inline] in
    let baker_account =
      fun gen#521 ->
      (let (gen#650, gen#651) = gen#521 in
       let _p = gen#650 in let _o = gen#651 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let bake_until_n_cycle_end =
      fun _n -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let register_delegate =
      fun _kh -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let register_constant =
      fun _m -> ((poly_failwith_9)@(L("TEST MODE")))[@inline] in
    let create_chest =
      fun gen#543 ->
      (let (gen#652, gen#653) = gen#543 in
       let _b = gen#652 in let _n = gen#653 in (poly_failwith_8)@(L("TEST MODE")))[@inline] in
    let create_chest_key =
      fun gen#547 ->
      (let (gen#654, gen#655) = gen#547 in
       let _c = gen#654 in let _n = gen#655 in (poly_failwith_7)@(L("TEST MODE")))[@inline] in
    let constant_to_michelson_program =
      fun _s -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let restore_context =
      fun _u -> ((poly_failwith_1)@(L("TEST_POP_CONTEXT")))[@inline] in
    let save_context =
      fun _u -> ((poly_failwith_1)@(L("TEST_PUSH_CONTEXT")))[@inline] in
    let drop_context =
      fun _u -> ((poly_failwith_1)@(L("TEST_DROP_CONTEXT")))[@inline] in
    let read_contract_from_file =
      fun _fn -> ((poly_failwith_1)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
    let compile_contract_from_file =
      fun gen#561 ->
      (let (gen#656, gen#657) = gen#561 in
       let (gen#658, gen#659) = gen#656 in
       let _fn = gen#658 in
       let _e = gen#659 in
       let _v = gen#657 in
       (poly_failwith_1)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))[@inline] in
    let originate_contract =
      fun gen#568 ->
      (let (gen#660, gen#661) = gen#568 in
       let (gen#662, gen#663) = gen#660 in
       let _c = gen#662 in
       let _s = gen#663 in
       let _t = gen#661 in (poly_failwith_6)@(L("TEST_ORIGINATE")))[@inline] in
    let size = fun _c -> ((poly_failwith_5)@(L("TEST_SIZE")))[@inline] in
    let get_bootstrap_account =
      fun _n -> ((poly_failwith_4)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
    let sign =
      fun gen#577 ->
      (let (gen#664, gen#665) = gen#577 in
       let _sk = gen#664 in
       let _d = gen#665 in (poly_failwith_3)@(L("TEST_SIGN")))[@inline] in
    let chr = fun _n -> ((poly_failwith_2)@(L("TEST_CHR")))[@inline] in
    let nl = L("NEWLINE")[@inline] in
    let println = fun _v -> ((poly_failwith_1)@(L("TEST_PRINTLN")))[@inline] in
    let print = fun _v -> ((poly_failwith_1)@(L("TEST_PRINT")))[@inline] in
    let eprint = fun _v -> ((poly_failwith_1)@(L("TEST_EPRINTL")))[@inline] in
    let v = PAIR(L(1) , L("b")) in let y = v in let tm = y in L(unit) |}]
