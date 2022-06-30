open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
let a#8 = 42 in
let b#9 = 1 in
let x = a#8 in
unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let a#8 = 40 in
    let b#9 = let ba#8 = 1 in
    let baa#9 = ba#8 in
    ADD(ba#8 ,
    baa#9) in
    let x = ADD(a#8 , b#9) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let a#8 = 1 in
    let as#8 = 42 in
    let x#9 = a#8 in
    let b#9 = as#8 in
    let x = as#8 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let as#8 = 20 in
  let s_as#8 = 22 in
  let x = ADD(as#8 , s_as#8) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect{|
    let a#8 = 1 in
    let as#8 = 42 in
    let as#9 = 3 in
    let x = as#8 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let x#165 = 1 in
  let foo = let x = 20 in
  let x#8 = x in
  let y#8 = x#165 in
  let z#9 = y#8 in
  ADD(ADD(ADD(x#8 , y#8) , x) ,
  z#9) in
  let x = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let v#8 = 40 in
  let v#9 = ADD(v#8 , 1) in
  let v#10 = ADD(v#9 , 1) in
  let x = v#10 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let x#165 = 41 in
  let x = 1 in
  let x#8 = x in
  let y#9 = x#165 in
  let u = ADD(x#8 , y#9) in
  let x = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let x#8 = 41 in
  let x#9 = ADD(x#8 , 1) in
  let x = x#9 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let x#8 = 42 in
  let x#9 = 2 in
  let y#8 = x#8 in
  let x = y#8 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let x#165 = 19 in
  let y#8 = 22 in
  let x = let x = 1 in
  let u = x#165 in
  let v = y#8 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let a#8 = 42 in
  let x#9 = a#8 in
  let x = x#9 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let current_turn#10 = lambda (i : nat) return ADD(i , +1) in
  let other#11 = lambda (n : nat) return let current_turn = (current_turn#10)@(+1) in
  (assert)@(EQ(n ,
  current_turn)) in
  let main : ( unit * unit ) -> ( list (operation) * unit ) = lambda (gen#2 : ( unit * unit )) return  match
                                                                      gen#2 with
                                                                      | ( _p ,
                                                                      _s ) ->
                                                                      ( LIST_EMPTY() ,
                                                                      (other#11)@(+2) ) in
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
    let balance#11 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
    let amount#12 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
    let now#13 = ({ DROP ; NOW })@(L(unit))[@inline] in
    let sender#14 = ({ DROP ; SENDER })@(L(unit))[@inline] in
    let source#15 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
    let level#16 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
    let self_address#17 = SELF_ADDRESS()[@inline] in
    let chain_id#18 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
    let total_voting_power#19 =
      ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
    let get_balance#20 =
      fun _u#100 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
    let get_amount#21 = fun _u#102 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
    let get_now#22 = fun _u#104 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
    let get_sender#23 = fun _u#106 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
    let get_source#24 = fun _u#108 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
    let get_level#25 = fun _u#110 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
    let get_self_address#26 = fun _u#112 -> (SELF_ADDRESS())[@inline] in
    let get_chain_id#27 =
      fun _u#114 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
    let get_total_voting_power#28 =
      fun _u#116 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
    let voting_power#29 = fun kh#118 -> (({ VOTING_POWER })@(kh#118))[@inline] in
    let implicit_account#31 =
      fun kh#122 -> (IMPLICIT_ACCOUNT(kh#122))[@inline] in
    let pairing_check#37 = fun l#138 -> (({ PAIRING_CHECK })@(l#138))[@inline] in
    let open_chest#38 =
      fun gen#140 ->
      (let (gen#595, gen#596) = gen#140 in
       let (gen#597, gen#598) = gen#595 in
       let ck#141 = gen#597 in
       let c#142 = gen#598 in
       let n#143 = gen#596 in OPEN_CHEST(ck#141 , c#142 , n#143))[@inline] in
    let set_delegate#42 = fun o#157 -> (SET_DELEGATE(o#157))[@inline] in
    let xor#43 =
      fun gen#159 ->
      (let (gen#599, gen#600) = gen#159 in
       let l#160 = gen#599 in let r#161 = gen#600 in XOR(l#160 , r#161))[@inline] in
    let shift_left#44 =
      fun gen#163 ->
      (let (gen#601, gen#602) = gen#163 in
       let l#164 = gen#601 in let r#165 = gen#602 in LSL(l#164 , r#165))[@inline] in
    let shift_right#45 =
      fun gen#167 ->
      (let (gen#603, gen#604) = gen#167 in
       let l#168 = gen#603 in let r#169 = gen#604 in LSR(l#168 , r#169))[@inline] in
    let concat#86 =
      fun gen#329 ->
      (let (gen#605, gen#606) = gen#329 in
       let b1#330 = gen#605 in
       let b2#331 = gen#606 in ({ UNPAIR ; CONCAT })@(PAIR(b1#330 , b2#331)))[@inline] in
    let sub#87 =
      fun gen#333 ->
      (let (gen#607, gen#608) = gen#333 in
       let (gen#609, gen#610) = gen#607 in
       let s#334 = gen#609 in
       let l#335 = gen#610 in
       let b#336 = gen#608 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#334 ,
                                                                     l#335) ,
                                                                b#336)))[@inline] in
    let length#88 = fun b#338 -> (({ SIZE })@(b#338))[@inline] in
    let concat#91 =
      fun gen#346 ->
      (let (gen#611, gen#612) = gen#346 in
       let b1#347 = gen#611 in
       let b2#348 = gen#612 in ({ UNPAIR ; CONCAT })@(PAIR(b1#347 , b2#348)))[@inline] in
    let sub#92 =
      fun gen#350 ->
      (let (gen#613, gen#614) = gen#350 in
       let (gen#615, gen#616) = gen#613 in
       let s#351 = gen#615 in
       let l#352 = gen#616 in
       let b#353 = gen#614 in
       ({ UNPAIR ;
         UNPAIR ;
         SLICE ;
         IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#351 ,
                                                                     l#352) ,
                                                                b#353)))[@inline] in
    let length#95 = fun b#359 -> (({ SIZE })@(b#359))[@inline] in
    let blake2b#96 = fun b#361 -> (({ BLAKE2B })@(b#361))[@inline] in
    let sha256#97 = fun b#363 -> (({ SHA256 })@(b#363))[@inline] in
    let sha512#98 = fun b#365 -> (({ SHA512 })@(b#365))[@inline] in
    let sha3#99 = fun b#367 -> (({ SHA3 })@(b#367))[@inline] in
    let keccak#100 = fun b#369 -> (({ KECCAK })@(b#369))[@inline] in
    let hash_key#101 = fun k#371 -> (({ HASH_KEY })@(k#371))[@inline] in
    let check#102 =
      fun gen#373 ->
      (let (gen#617, gen#618) = gen#373 in
       let (gen#619, gen#620) = gen#617 in
       let k#374 = gen#619 in
       let s#375 = gen#620 in
       let b#376 = gen#618 in
       ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#374 , s#375) , b#376)))[@inline] in
    let assert =
      fun b#378 ->
      (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#378))[@inline] in
    let assert_with_error =
      fun gen#380 ->
      (let (gen#621, gen#622) = gen#380 in
       let b#381 = gen#621 in
       let s#382 = gen#622 in
       ({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#381 , s#382)))[@inline] in
    let abs = fun i#396 -> (({ ABS })@(i#396))[@inline] in
    let is_nat = fun i#398 -> (({ ISNAT })@(i#398))[@inline] in
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
    let originate_from_file#104 =
      fun gen#413 ->
      (let (gen#623, gen#624) = gen#413 in
       let (gen#625, gen#626) = gen#623 in
       let (gen#629, gen#630) = gen#625 in
       let _fn#414 = gen#629 in
       let _e#415 = gen#630 in
       let (gen#627, gen#628) = gen#626 in
       let _v#416 = gen#627 in
       let _s#417 = gen#628 in
       let _t#418 = gen#624 in (poly_failwith_15)@(L("TEST MODE")))[@inline] in
    let set_source#106 =
      fun _a#425 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let set_baker#107 =
      fun _a#427 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let set_baker_policy#108 =
      fun _bp#429 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let transfer#109 =
      fun gen#431 ->
      (let (gen#631, gen#632) = gen#431 in
       let (gen#633, gen#634) = gen#631 in
       let _a#432 = gen#633 in
       let _s#433 = gen#634 in
       let _t#434 = gen#632 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let transfer_exn#110 =
      fun gen#436 ->
      (let (gen#635, gen#636) = gen#436 in
       let (gen#637, gen#638) = gen#635 in
       let _a#437 = gen#637 in
       let _s#438 = gen#638 in
       let _t#439 = gen#636 in (poly_failwith_12)@(L("TEST MODE")))[@inline] in
    let get_storage_of_address#114 =
      fun _a#453 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let get_balance#115 =
      fun _a#455 -> ((poly_failwith_14)@(L("TEST MODE")))[@inline] in
    let michelson_equal#116 =
      fun gen#457 ->
      (let (gen#639, gen#640) = gen#457 in
       let _m1#458 = gen#639 in
       let _m2#459 = gen#640 in (poly_failwith_13)@(L("TEST MODE")))[@inline] in
    let reset_state#118 =
      fun gen#463 ->
      (let (gen#641, gen#642) = gen#463 in
       let _n#464 = gen#641 in
       let _l#465 = gen#642 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let reset_state_at#119 =
      fun gen#467 ->
      (let (gen#643, gen#644) = gen#467 in
       let (gen#645, gen#646) = gen#643 in
       let _t#468 = gen#645 in
       let _n#469 = gen#646 in
       let _l#470 = gen#644 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let get_voting_power#120 =
      fun _kh#472 -> ((poly_failwith_12)@(L("TEST MODE")))[@inline] in
    let get_total_voting_power#121 =
      (poly_failwith_12)@(L("TEST MODE"))[@inline] in
    let nth_bootstrap_contract#123 =
      fun _i#480 -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
    let nth_bootstrap_account#124 =
      fun _i#482 -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
    let last_originations#126 =
      fun _u#486 -> ((poly_failwith_11)@(L("TEST MODE")))[@inline] in
    let save_mutation#129 =
      fun gen#494 ->
      (let (gen#647, gen#648) = gen#494 in
       let _s#495 = gen#647 in
       let _m#496 = gen#648 in (poly_failwith_2)@(L("TEST MODE")))[@inline] in
    let add_account#136 =
      fun gen#516 ->
      (let (gen#649, gen#650) = gen#516 in
       let _s#517 = gen#649 in
       let _k#518 = gen#650 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let new_account#137 =
      fun _u#520 -> ((poly_failwith_10)@(L("TEST MODE")))[@inline] in
    let baker_account#138 =
      fun gen#522 ->
      (let (gen#651, gen#652) = gen#522 in
       let _p#523 = gen#651 in
       let _o#524 = gen#652 in (poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let bake_until_n_cycle_end#139 =
      fun _n#526 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let register_delegate#140 =
      fun _kh#528 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let register_constant#141 =
      fun _m#530 -> ((poly_failwith_9)@(L("TEST MODE")))[@inline] in
    let create_chest#146 =
      fun gen#544 ->
      (let (gen#653, gen#654) = gen#544 in
       let _b#545 = gen#653 in
       let _n#546 = gen#654 in (poly_failwith_8)@(L("TEST MODE")))[@inline] in
    let create_chest_key#147 =
      fun gen#548 ->
      (let (gen#655, gen#656) = gen#548 in
       let _c#549 = gen#655 in
       let _n#550 = gen#656 in (poly_failwith_7)@(L("TEST MODE")))[@inline] in
    let constant_to_michelson_program#148 =
      fun _s#552 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
    let restore_context#149 =
      fun _u#554 -> ((poly_failwith_1)@(L("TEST_POP_CONTEXT")))[@inline] in
    let save_context#150 =
      fun _u#556 -> ((poly_failwith_1)@(L("TEST_PUSH_CONTEXT")))[@inline] in
    let drop_context#151 =
      fun _u#558 -> ((poly_failwith_1)@(L("TEST_DROP_CONTEXT")))[@inline] in
    let read_contract_from_file#152 =
      fun _fn#560 -> ((poly_failwith_1)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
    let compile_contract_from_file#153 =
      fun gen#562 ->
      (let (gen#657, gen#658) = gen#562 in
       let (gen#659, gen#660) = gen#657 in
       let _fn#563 = gen#659 in
       let _e#564 = gen#660 in
       let _v#565 = gen#658 in
       (poly_failwith_1)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))[@inline] in
    let originate_contract#155 =
      fun gen#569 ->
      (let (gen#661, gen#662) = gen#569 in
       let (gen#663, gen#664) = gen#661 in
       let _c#570 = gen#663 in
       let _s#571 = gen#664 in
       let _t#572 = gen#662 in (poly_failwith_6)@(L("TEST_ORIGINATE")))[@inline] in
    let size#156 = fun _c#574 -> ((poly_failwith_5)@(L("TEST_SIZE")))[@inline] in
    let get_bootstrap_account#157 =
      fun _n#576 -> ((poly_failwith_4)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
    let sign#158 =
      fun gen#578 ->
      (let (gen#665, gen#666) = gen#578 in
       let _sk#579 = gen#665 in
       let _d#580 = gen#666 in (poly_failwith_3)@(L("TEST_SIGN")))[@inline] in
    let chr#159 = fun _n#582 -> ((poly_failwith_2)@(L("TEST_CHR")))[@inline] in
    let nl#160 = L("NEWLINE")[@inline] in
    let println#161 =
      fun _v#585 -> ((poly_failwith_1)@(L("TEST_PRINTLN")))[@inline] in
    let print#162 =
      fun _v#587 -> ((poly_failwith_1)@(L("TEST_PRINT")))[@inline] in
    let eprint#163 =
      fun _v#589 -> ((poly_failwith_1)@(L("TEST_EPRINTL")))[@inline] in
    let v = PAIR(L(1) , L("b")) in let y#81 = v in let tm = y#81 in L(unit) |}]
