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
             DUP 2 ;
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
    module C =
      Mangled_module_____________________test__contracts__build__C____mligo.
    module E =
      Mangled_module_____________________test__contracts__build__E____mligo.
    const toto = ADD(E.toto , C.B.A.toto)
    const fb = record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main =
      lambda (gen#5 : ( int * int )) return  match gen#5 with
                                              | ( p , s ) ->
                                              let s = ADD(ADD(p , s) ,
                                              toto) in ( LIST_EMPTY() , s ) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "instance/main.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage string ;
      code { DROP ;
             PUSH string "FA2_TOKEN_UNDEFINED" ;
             PUSH string "AAAA" ;
             DUP 2 ;
             CONCAT ;
             SWAP ;
             CONCAT ;
             NIL operation ;
             PAIR } } |}]
                         
let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "instance/main.mligo" ] ;
  [%expect {|
    module Errors =
      Mangled_module_____________________test__contracts__build__instance____________common__errors____mligo.
    module Storage =
      Mangled_module_____________________test__contracts__build__instance____________common__storage____mligo.
    const main =
      lambda (gen#2 : ( unit * string )) return  match gen#2 with
                                                  | ( _#4 , _#3 ) ->
                                                  ( LIST_EMPTY() , CONCAT(Errors.undefined_token , Storage.s) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let get_balance#49 =
  fun _u#4260 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#50 =
  fun _u#4262 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#51 = fun _u#4264 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#52 =
  fun _u#4266 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#53 =
  fun _u#4268 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#54 = fun _u#4270 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#55 = fun _u#4272 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#56 =
  fun _u#4274 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#57 =
  fun _u#4276 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#58 =
  fun _u#4278 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#59 =
  fun kh#4280 -> (({ VOTING_POWER })@(kh#4280))[@inline] in
let implicit_account#61 =
  fun kh#4284 -> (IMPLICIT_ACCOUNT(kh#4284))[@inline] in
let pairing_check#65 =
  fun l#4292 -> (({ PAIRING_CHECK })@(l#4292))[@inline] in
let set_delegate#67 = fun o#4296 -> (SET_DELEGATE(o#4296))[@inline] in
let open_chest#73 =
  fun ck#4312 ->
  (fun c#4313 -> (fun n#4314 -> (OPEN_CHEST(ck#4312 , c#4313 , n#4314))))[@inline] in
let xor#76 = fun l#4323 -> (fun r#4324 -> (XOR(l#4323 , r#4324)))[@inline] in
let shift_left#77 =
  fun l#4326 -> (fun r#4327 -> (LSL(l#4326 , r#4327)))[@inline] in
let shift_right#78 =
  fun l#4329 -> (fun r#4330 -> (LSR(l#4329 , r#4330)))[@inline] in
let length#119 = fun b#4460 -> (({ SIZE })@(b#4460))[@inline] in
let concat#120 =
  fun b1#4462 ->
  (fun b2#4463 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4462 , b2#4463))))[@inline] in
let sub#121 =
  fun s#4465 ->
  (fun l#4466 ->
   (fun b#4467 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4465 ,
                                                                   l#4466) ,
                                                              b#4467)))))[@inline] in
let length#126 = fun b#4478 -> (({ SIZE })@(b#4478))[@inline] in
let concat#127 =
  fun b1#4480 ->
  (fun b2#4481 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4480 , b2#4481))))[@inline] in
let sub#128 =
  fun s#4483 ->
  (fun l#4484 ->
   (fun b#4485 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4483 ,
                                                                   l#4484) ,
                                                              b#4485)))))[@inline] in
let blake2b#129 = fun b#4487 -> (({ BLAKE2B })@(b#4487))[@inline] in
let sha256#130 = fun b#4489 -> (({ SHA256 })@(b#4489))[@inline] in
let sha512#131 = fun b#4491 -> (({ SHA512 })@(b#4491))[@inline] in
let sha3#132 = fun b#4493 -> (({ SHA3 })@(b#4493))[@inline] in
let keccak#133 = fun b#4495 -> (({ KECCAK })@(b#4495))[@inline] in
let hash_key#134 = fun k#4497 -> (({ HASH_KEY })@(k#4497))[@inline] in
let check#135 =
  fun k#4499 ->
  (fun s#4500 ->
   (fun b#4501 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4499 , s#4500) ,
                                                   b#4501)))))[@inline] in
let assert#136 =
  fun b#4503 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4503))[@inline] in
let abs#139 = fun i#4509 -> (({ ABS })@(i#4509))[@inline] in
let is_nat#140 = fun i#4511 -> (({ ISNAT })@(i#4511))[@inline] in
let true#141 = TRUE()[@inline] in
let false#142 = FALSE()[@inline] in
let unit#143 = UNIT()[@inline] in
let assert_with_error#146 =
  fun b#4519 ->
  (fun s#4520 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4519 , s#4520))))[@inline] in
let get_total_voting_power#154 =
  fun _u#4540 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let set_source#157 =
  fun _a#4546 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let get_storage_of_address#158 =
  fun _a#4548 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let get_balance#159 =
  fun _a#4550 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let print#160 =
  fun _v#4552 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let eprint#161 =
  fun _v#4554 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let get_voting_power#162 =
  fun _kh#4556 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let nth_bootstrap_contract#163 =
  fun _i#4558 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let nth_bootstrap_account#164 =
  fun _i#4560 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let get_bootstrap_account#165 =
  fun _n#4562 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let last_originations#167 =
  fun _u#4566 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let new_account#169 =
  fun _u#4570 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let bake_until_n_cycle_end#171 =
  fun _n#4574 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let register_delegate#173 =
  fun _kh#4578 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let register_constant#174 =
  fun _m#4580 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let constant_to_michelson_program#176 =
  fun _s#4584 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let restore_context#177 =
  fun _u#4586 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let save_context#178 =
  fun _u#4588 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let drop_context#179 =
  fun _u#4590 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let set_baker_policy#182 =
  fun _bp#4596 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let set_baker#183 =
  fun _a#4598 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let size#184 =
  fun _c#4600 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let read_contract_from_file#186 =
  fun _fn#4604 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let chr#187 =
  fun _n#4606 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let nl#188 = L("NEWLINE")[@inline] in
let println#189 =
  fun _v#4609 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))[@inline] in
let transfer#190 =
  fun _a#4611 ->
  (fun _s#4612 ->
   (fun _t#4613 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))))[@inline] in
let transfer_exn#191 =
  fun _a#4615 ->
  (fun _s#4616 ->
   (fun _t#4617 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))))[@inline] in
let reset_state#193 =
  fun _n#4621 ->
  (fun _l#4622 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit))))[@inline] in
let reset_state_at#194 =
  fun _t#4624 ->
  (fun _n#4625 ->
   (fun _l#4626 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))))[@inline] in
let save_mutation#197 =
  fun _s#4635 ->
  (fun _m#4636 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit))))[@inline] in
let sign#200 =
  fun _sk#4644 ->
  (fun _d#4645 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit))))[@inline] in
let add_account#201 =
  fun _s#4647 ->
  (fun _k#4648 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit))))[@inline] in
let baker_account#202 =
  fun _p#4650 ->
  (fun _o#4651 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit))))[@inline] in
let create_chest#204 =
  fun _b#4656 ->
  (fun _n#4657 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit))))[@inline] in
let create_chest_key#205 =
  fun _c#4659 ->
  (fun _n#4660 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit))))[@inline] in
let michelson_equal#208 =
  fun _m1#4670 ->
  (fun _m2#4671 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit))))[@inline] in
let originate_contract#210 =
  fun _c#4676 ->
  (fun _s#4677 ->
   (fun _t#4678 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))))[@inline] in
let compile_contract_from_file#212 =
  fun _fn#4684 ->
  (fun _e#4685 ->
   (fun _v#4686 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))))[@inline] in
let originate_from_file#213 =
  fun _fn#4688 ->
  (fun _e#4689 ->
   (fun _v#4690 ->
    (fun _s#4691 ->
     (fun _t#4692 -> ((fun x#4531 -> (({ FAILWITH })@(x#4531)))@(L(unit)))))))[@inline] in
let toto#214 = L(1) in
let get_balance#215 =
  fun _u#4695 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#216 =
  fun _u#4697 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#217 = fun _u#4699 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#218 =
  fun _u#4701 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#219 =
  fun _u#4703 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#220 = fun _u#4705 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#221 = fun _u#4707 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#222 =
  fun _u#4709 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#223 =
  fun _u#4711 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#224 =
  fun _u#4713 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#225 =
  fun kh#4715 -> (({ VOTING_POWER })@(kh#4715))[@inline] in
let implicit_account#227 =
  fun kh#4719 -> (IMPLICIT_ACCOUNT(kh#4719))[@inline] in
let pairing_check#231 =
  fun l#4727 -> (({ PAIRING_CHECK })@(l#4727))[@inline] in
let set_delegate#233 = fun o#4731 -> (SET_DELEGATE(o#4731))[@inline] in
let open_chest#239 =
  fun ck#4747 ->
  (fun c#4748 -> (fun n#4749 -> (OPEN_CHEST(ck#4747 , c#4748 , n#4749))))[@inline] in
let xor#242 =
  fun l#4758 -> (fun r#4759 -> (XOR(l#4758 , r#4759)))[@inline] in
let shift_left#243 =
  fun l#4761 -> (fun r#4762 -> (LSL(l#4761 , r#4762)))[@inline] in
let shift_right#244 =
  fun l#4764 -> (fun r#4765 -> (LSR(l#4764 , r#4765)))[@inline] in
let length#285 = fun b#4895 -> (({ SIZE })@(b#4895))[@inline] in
let concat#286 =
  fun b1#4897 ->
  (fun b2#4898 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4897 , b2#4898))))[@inline] in
let sub#287 =
  fun s#4900 ->
  (fun l#4901 ->
   (fun b#4902 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4900 ,
                                                                   l#4901) ,
                                                              b#4902)))))[@inline] in
let length#292 = fun b#4913 -> (({ SIZE })@(b#4913))[@inline] in
let concat#293 =
  fun b1#4915 ->
  (fun b2#4916 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4915 , b2#4916))))[@inline] in
let sub#294 =
  fun s#4918 ->
  (fun l#4919 ->
   (fun b#4920 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4918 ,
                                                                   l#4919) ,
                                                              b#4920)))))[@inline] in
let blake2b#295 = fun b#4922 -> (({ BLAKE2B })@(b#4922))[@inline] in
let sha256#296 = fun b#4924 -> (({ SHA256 })@(b#4924))[@inline] in
let sha512#297 = fun b#4926 -> (({ SHA512 })@(b#4926))[@inline] in
let sha3#298 = fun b#4928 -> (({ SHA3 })@(b#4928))[@inline] in
let keccak#299 = fun b#4930 -> (({ KECCAK })@(b#4930))[@inline] in
let hash_key#300 = fun k#4932 -> (({ HASH_KEY })@(k#4932))[@inline] in
let check#301 =
  fun k#4934 ->
  (fun s#4935 ->
   (fun b#4936 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4934 , s#4935) ,
                                                   b#4936)))))[@inline] in
let assert#302 =
  fun b#4938 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4938))[@inline] in
let abs#305 = fun i#4944 -> (({ ABS })@(i#4944))[@inline] in
let is_nat#306 = fun i#4946 -> (({ ISNAT })@(i#4946))[@inline] in
let true#307 = TRUE()[@inline] in
let false#308 = FALSE()[@inline] in
let unit#309 = UNIT()[@inline] in
let assert_with_error#312 =
  fun b#4954 ->
  (fun s#4955 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4954 , s#4955))))[@inline] in
let get_total_voting_power#320 =
  fun _u#4975 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let set_source#323 =
  fun _a#4981 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let get_storage_of_address#324 =
  fun _a#4983 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let get_balance#325 =
  fun _a#4985 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let print#326 =
  fun _v#4987 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let eprint#327 =
  fun _v#4989 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let get_voting_power#328 =
  fun _kh#4991 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let nth_bootstrap_contract#329 =
  fun _i#4993 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let nth_bootstrap_account#330 =
  fun _i#4995 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let get_bootstrap_account#331 =
  fun _n#4997 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let last_originations#333 =
  fun _u#5001 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let new_account#335 =
  fun _u#5005 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let bake_until_n_cycle_end#337 =
  fun _n#5009 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let register_delegate#339 =
  fun _kh#5013 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let register_constant#340 =
  fun _m#5015 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let constant_to_michelson_program#342 =
  fun _s#5019 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let restore_context#343 =
  fun _u#5021 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let save_context#344 =
  fun _u#5023 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let drop_context#345 =
  fun _u#5025 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let set_baker_policy#348 =
  fun _bp#5031 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let set_baker#349 =
  fun _a#5033 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let size#350 =
  fun _c#5035 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let read_contract_from_file#352 =
  fun _fn#5039 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let chr#353 =
  fun _n#5041 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let nl#354 = L("NEWLINE")[@inline] in
let println#355 =
  fun _v#5044 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))[@inline] in
let transfer#356 =
  fun _a#5046 ->
  (fun _s#5047 ->
   (fun _t#5048 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))))[@inline] in
let transfer_exn#357 =
  fun _a#5050 ->
  (fun _s#5051 ->
   (fun _t#5052 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))))[@inline] in
let reset_state#359 =
  fun _n#5056 ->
  (fun _l#5057 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit))))[@inline] in
let reset_state_at#360 =
  fun _t#5059 ->
  (fun _n#5060 ->
   (fun _l#5061 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))))[@inline] in
let save_mutation#363 =
  fun _s#5070 ->
  (fun _m#5071 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit))))[@inline] in
let sign#366 =
  fun _sk#5079 ->
  (fun _d#5080 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit))))[@inline] in
let add_account#367 =
  fun _s#5082 ->
  (fun _k#5083 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit))))[@inline] in
let baker_account#368 =
  fun _p#5085 ->
  (fun _o#5086 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit))))[@inline] in
let create_chest#370 =
  fun _b#5091 ->
  (fun _n#5092 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit))))[@inline] in
let create_chest_key#371 =
  fun _c#5094 ->
  (fun _n#5095 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit))))[@inline] in
let michelson_equal#374 =
  fun _m1#5105 ->
  (fun _m2#5106 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit))))[@inline] in
let originate_contract#376 =
  fun _c#5111 ->
  (fun _s#5112 ->
   (fun _t#5113 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))))[@inline] in
let compile_contract_from_file#378 =
  fun _fn#5119 ->
  (fun _e#5120 ->
   (fun _v#5121 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))))[@inline] in
let originate_from_file#379 =
  fun _fn#5123 ->
  (fun _e#5124 ->
   (fun _v#5125 ->
    (fun _s#5126 ->
     (fun _t#5127 -> ((fun x#4966 -> (({ FAILWITH })@(x#4966)))@(L(unit)))))))[@inline] in
let toto#380 = L(32) in
let titi#381 = ADD(toto#214 , L(42)) in
let f#382 =
  fun gen#5131 ->
  (let (gen#7318, gen#7319) = gen#5131 in
   let gen#5132 = gen#7318 in
   let x#5133 = gen#7319 in
   let x#5134 = ADD(ADD(x#5133 , toto#214) , titi#381) in
   PAIR(LIST_EMPTY() , x#5134)) in
let get_balance#383 =
  fun _u#5136 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#384 =
  fun _u#5138 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#385 = fun _u#5140 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#386 =
  fun _u#5142 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#387 =
  fun _u#5144 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#388 = fun _u#5146 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#389 = fun _u#5148 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#390 =
  fun _u#5150 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#391 =
  fun _u#5152 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#392 =
  fun _u#5154 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#393 =
  fun kh#5156 -> (({ VOTING_POWER })@(kh#5156))[@inline] in
let implicit_account#395 =
  fun kh#5160 -> (IMPLICIT_ACCOUNT(kh#5160))[@inline] in
let pairing_check#399 =
  fun l#5168 -> (({ PAIRING_CHECK })@(l#5168))[@inline] in
let set_delegate#401 = fun o#5172 -> (SET_DELEGATE(o#5172))[@inline] in
let open_chest#407 =
  fun ck#5188 ->
  (fun c#5189 -> (fun n#5190 -> (OPEN_CHEST(ck#5188 , c#5189 , n#5190))))[@inline] in
let xor#410 =
  fun l#5199 -> (fun r#5200 -> (XOR(l#5199 , r#5200)))[@inline] in
let shift_left#411 =
  fun l#5202 -> (fun r#5203 -> (LSL(l#5202 , r#5203)))[@inline] in
let shift_right#412 =
  fun l#5205 -> (fun r#5206 -> (LSR(l#5205 , r#5206)))[@inline] in
let length#453 = fun b#5336 -> (({ SIZE })@(b#5336))[@inline] in
let concat#454 =
  fun b1#5338 ->
  (fun b2#5339 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5338 , b2#5339))))[@inline] in
let sub#455 =
  fun s#5341 ->
  (fun l#5342 ->
   (fun b#5343 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5341 ,
                                                                   l#5342) ,
                                                              b#5343)))))[@inline] in
let length#460 = fun b#5354 -> (({ SIZE })@(b#5354))[@inline] in
let concat#461 =
  fun b1#5356 ->
  (fun b2#5357 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5356 , b2#5357))))[@inline] in
let sub#462 =
  fun s#5359 ->
  (fun l#5360 ->
   (fun b#5361 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5359 ,
                                                                   l#5360) ,
                                                              b#5361)))))[@inline] in
let blake2b#463 = fun b#5363 -> (({ BLAKE2B })@(b#5363))[@inline] in
let sha256#464 = fun b#5365 -> (({ SHA256 })@(b#5365))[@inline] in
let sha512#465 = fun b#5367 -> (({ SHA512 })@(b#5367))[@inline] in
let sha3#466 = fun b#5369 -> (({ SHA3 })@(b#5369))[@inline] in
let keccak#467 = fun b#5371 -> (({ KECCAK })@(b#5371))[@inline] in
let hash_key#468 = fun k#5373 -> (({ HASH_KEY })@(k#5373))[@inline] in
let check#469 =
  fun k#5375 ->
  (fun s#5376 ->
   (fun b#5377 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5375 , s#5376) ,
                                                   b#5377)))))[@inline] in
let assert#470 =
  fun b#5379 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5379))[@inline] in
let abs#473 = fun i#5385 -> (({ ABS })@(i#5385))[@inline] in
let is_nat#474 = fun i#5387 -> (({ ISNAT })@(i#5387))[@inline] in
let true#475 = TRUE()[@inline] in
let false#476 = FALSE()[@inline] in
let unit#477 = UNIT()[@inline] in
let assert_with_error#480 =
  fun b#5395 ->
  (fun s#5396 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5395 , s#5396))))[@inline] in
let get_total_voting_power#488 =
  fun _u#5416 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let set_source#491 =
  fun _a#5422 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let get_storage_of_address#492 =
  fun _a#5424 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let get_balance#493 =
  fun _a#5426 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let print#494 =
  fun _v#5428 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let eprint#495 =
  fun _v#5430 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let get_voting_power#496 =
  fun _kh#5432 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let nth_bootstrap_contract#497 =
  fun _i#5434 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let nth_bootstrap_account#498 =
  fun _i#5436 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let get_bootstrap_account#499 =
  fun _n#5438 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let last_originations#501 =
  fun _u#5442 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let new_account#503 =
  fun _u#5446 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let bake_until_n_cycle_end#505 =
  fun _n#5450 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let register_delegate#507 =
  fun _kh#5454 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let register_constant#508 =
  fun _m#5456 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let constant_to_michelson_program#510 =
  fun _s#5460 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let restore_context#511 =
  fun _u#5462 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let save_context#512 =
  fun _u#5464 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let drop_context#513 =
  fun _u#5466 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let set_baker_policy#516 =
  fun _bp#5472 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let set_baker#517 =
  fun _a#5474 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let size#518 =
  fun _c#5476 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let read_contract_from_file#520 =
  fun _fn#5480 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let chr#521 =
  fun _n#5482 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let nl#522 = L("NEWLINE")[@inline] in
let println#523 =
  fun _v#5485 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))[@inline] in
let transfer#524 =
  fun _a#5487 ->
  (fun _s#5488 ->
   (fun _t#5489 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))))[@inline] in
let transfer_exn#525 =
  fun _a#5491 ->
  (fun _s#5492 ->
   (fun _t#5493 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))))[@inline] in
let reset_state#527 =
  fun _n#5497 ->
  (fun _l#5498 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit))))[@inline] in
let reset_state_at#528 =
  fun _t#5500 ->
  (fun _n#5501 ->
   (fun _l#5502 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))))[@inline] in
let save_mutation#531 =
  fun _s#5511 ->
  (fun _m#5512 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit))))[@inline] in
let sign#534 =
  fun _sk#5520 ->
  (fun _d#5521 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit))))[@inline] in
let add_account#535 =
  fun _s#5523 ->
  (fun _k#5524 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit))))[@inline] in
let baker_account#536 =
  fun _p#5526 ->
  (fun _o#5527 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit))))[@inline] in
let create_chest#538 =
  fun _b#5532 ->
  (fun _n#5533 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit))))[@inline] in
let create_chest_key#539 =
  fun _c#5535 ->
  (fun _n#5536 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit))))[@inline] in
let michelson_equal#542 =
  fun _m1#5546 ->
  (fun _m2#5547 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit))))[@inline] in
let originate_contract#544 =
  fun _c#5552 ->
  (fun _s#5553 ->
   (fun _t#5554 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))))[@inline] in
let compile_contract_from_file#546 =
  fun _fn#5560 ->
  (fun _e#5561 ->
   (fun _v#5562 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))))[@inline] in
let originate_from_file#547 =
  fun _fn#5564 ->
  (fun _e#5565 ->
   (fun _v#5566 ->
    (fun _s#5567 ->
     (fun _t#5568 -> ((fun x#5407 -> (({ FAILWITH })@(x#5407)))@(L(unit)))))))[@inline] in
let toto#548 = L(44) in
let get_balance#549 =
  fun _u#5571 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#550 =
  fun _u#5573 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#551 = fun _u#5575 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#552 =
  fun _u#5577 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#553 =
  fun _u#5579 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#554 = fun _u#5581 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#555 = fun _u#5583 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#556 =
  fun _u#5585 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#557 =
  fun _u#5587 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#558 =
  fun _u#5589 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#559 =
  fun kh#5591 -> (({ VOTING_POWER })@(kh#5591))[@inline] in
let implicit_account#561 =
  fun kh#5595 -> (IMPLICIT_ACCOUNT(kh#5595))[@inline] in
let pairing_check#565 =
  fun l#5603 -> (({ PAIRING_CHECK })@(l#5603))[@inline] in
let set_delegate#567 = fun o#5607 -> (SET_DELEGATE(o#5607))[@inline] in
let open_chest#573 =
  fun ck#5623 ->
  (fun c#5624 -> (fun n#5625 -> (OPEN_CHEST(ck#5623 , c#5624 , n#5625))))[@inline] in
let xor#576 =
  fun l#5634 -> (fun r#5635 -> (XOR(l#5634 , r#5635)))[@inline] in
let shift_left#577 =
  fun l#5637 -> (fun r#5638 -> (LSL(l#5637 , r#5638)))[@inline] in
let shift_right#578 =
  fun l#5640 -> (fun r#5641 -> (LSR(l#5640 , r#5641)))[@inline] in
let length#619 = fun b#5771 -> (({ SIZE })@(b#5771))[@inline] in
let concat#620 =
  fun b1#5773 ->
  (fun b2#5774 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5773 , b2#5774))))[@inline] in
let sub#621 =
  fun s#5776 ->
  (fun l#5777 ->
   (fun b#5778 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5776 ,
                                                                   l#5777) ,
                                                              b#5778)))))[@inline] in
let length#626 = fun b#5789 -> (({ SIZE })@(b#5789))[@inline] in
let concat#627 =
  fun b1#5791 ->
  (fun b2#5792 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5791 , b2#5792))))[@inline] in
let sub#628 =
  fun s#5794 ->
  (fun l#5795 ->
   (fun b#5796 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5794 ,
                                                                   l#5795) ,
                                                              b#5796)))))[@inline] in
let blake2b#629 = fun b#5798 -> (({ BLAKE2B })@(b#5798))[@inline] in
let sha256#630 = fun b#5800 -> (({ SHA256 })@(b#5800))[@inline] in
let sha512#631 = fun b#5802 -> (({ SHA512 })@(b#5802))[@inline] in
let sha3#632 = fun b#5804 -> (({ SHA3 })@(b#5804))[@inline] in
let keccak#633 = fun b#5806 -> (({ KECCAK })@(b#5806))[@inline] in
let hash_key#634 = fun k#5808 -> (({ HASH_KEY })@(k#5808))[@inline] in
let check#635 =
  fun k#5810 ->
  (fun s#5811 ->
   (fun b#5812 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5810 , s#5811) ,
                                                   b#5812)))))[@inline] in
let assert#636 =
  fun b#5814 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5814))[@inline] in
let abs#639 = fun i#5820 -> (({ ABS })@(i#5820))[@inline] in
let is_nat#640 = fun i#5822 -> (({ ISNAT })@(i#5822))[@inline] in
let true#641 = TRUE()[@inline] in
let false#642 = FALSE()[@inline] in
let unit#643 = UNIT()[@inline] in
let assert_with_error#646 =
  fun b#5830 ->
  (fun s#5831 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5830 , s#5831))))[@inline] in
let get_total_voting_power#654 =
  fun _u#5851 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let set_source#657 =
  fun _a#5857 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let get_storage_of_address#658 =
  fun _a#5859 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let get_balance#659 =
  fun _a#5861 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let print#660 =
  fun _v#5863 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let eprint#661 =
  fun _v#5865 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let get_voting_power#662 =
  fun _kh#5867 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let nth_bootstrap_contract#663 =
  fun _i#5869 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let nth_bootstrap_account#664 =
  fun _i#5871 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let get_bootstrap_account#665 =
  fun _n#5873 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let last_originations#667 =
  fun _u#5877 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let new_account#669 =
  fun _u#5881 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let bake_until_n_cycle_end#671 =
  fun _n#5885 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let register_delegate#673 =
  fun _kh#5889 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let register_constant#674 =
  fun _m#5891 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let constant_to_michelson_program#676 =
  fun _s#5895 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let restore_context#677 =
  fun _u#5897 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let save_context#678 =
  fun _u#5899 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let drop_context#679 =
  fun _u#5901 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let set_baker_policy#682 =
  fun _bp#5907 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let set_baker#683 =
  fun _a#5909 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let size#684 =
  fun _c#5911 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let read_contract_from_file#686 =
  fun _fn#5915 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let chr#687 =
  fun _n#5917 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let nl#688 = L("NEWLINE")[@inline] in
let println#689 =
  fun _v#5920 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))[@inline] in
let transfer#690 =
  fun _a#5922 ->
  (fun _s#5923 ->
   (fun _t#5924 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))))[@inline] in
let transfer_exn#691 =
  fun _a#5926 ->
  (fun _s#5927 ->
   (fun _t#5928 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))))[@inline] in
let reset_state#693 =
  fun _n#5932 ->
  (fun _l#5933 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit))))[@inline] in
let reset_state_at#694 =
  fun _t#5935 ->
  (fun _n#5936 ->
   (fun _l#5937 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))))[@inline] in
let save_mutation#697 =
  fun _s#5946 ->
  (fun _m#5947 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit))))[@inline] in
let sign#700 =
  fun _sk#5955 ->
  (fun _d#5956 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit))))[@inline] in
let add_account#701 =
  fun _s#5958 ->
  (fun _k#5959 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit))))[@inline] in
let baker_account#702 =
  fun _p#5961 ->
  (fun _o#5962 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit))))[@inline] in
let create_chest#704 =
  fun _b#5967 ->
  (fun _n#5968 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit))))[@inline] in
let create_chest_key#705 =
  fun _c#5970 ->
  (fun _n#5971 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit))))[@inline] in
let michelson_equal#708 =
  fun _m1#5981 ->
  (fun _m2#5982 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit))))[@inline] in
let originate_contract#710 =
  fun _c#5987 ->
  (fun _s#5988 ->
   (fun _t#5989 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))))[@inline] in
let compile_contract_from_file#712 =
  fun _fn#5995 ->
  (fun _e#5996 ->
   (fun _v#5997 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))))[@inline] in
let originate_from_file#713 =
  fun _fn#5999 ->
  (fun _e#6000 ->
   (fun _v#6001 ->
    (fun _s#6002 ->
     (fun _t#6003 -> ((fun x#5842 -> (({ FAILWITH })@(x#5842)))@(L(unit)))))))[@inline] in
let toto#714 = L(43) in
let get_balance#715 =
  fun _u#6006 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#716 =
  fun _u#6008 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#717 = fun _u#6010 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#718 =
  fun _u#6012 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#719 =
  fun _u#6014 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#720 = fun _u#6016 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#721 = fun _u#6018 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#722 =
  fun _u#6020 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#723 =
  fun _u#6022 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#724 =
  fun _u#6024 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#725 =
  fun kh#6026 -> (({ VOTING_POWER })@(kh#6026))[@inline] in
let implicit_account#727 =
  fun kh#6030 -> (IMPLICIT_ACCOUNT(kh#6030))[@inline] in
let pairing_check#731 =
  fun l#6038 -> (({ PAIRING_CHECK })@(l#6038))[@inline] in
let set_delegate#733 = fun o#6042 -> (SET_DELEGATE(o#6042))[@inline] in
let open_chest#739 =
  fun ck#6058 ->
  (fun c#6059 -> (fun n#6060 -> (OPEN_CHEST(ck#6058 , c#6059 , n#6060))))[@inline] in
let xor#742 =
  fun l#6069 -> (fun r#6070 -> (XOR(l#6069 , r#6070)))[@inline] in
let shift_left#743 =
  fun l#6072 -> (fun r#6073 -> (LSL(l#6072 , r#6073)))[@inline] in
let shift_right#744 =
  fun l#6075 -> (fun r#6076 -> (LSR(l#6075 , r#6076)))[@inline] in
let length#785 = fun b#6206 -> (({ SIZE })@(b#6206))[@inline] in
let concat#786 =
  fun b1#6208 ->
  (fun b2#6209 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6208 , b2#6209))))[@inline] in
let sub#787 =
  fun s#6211 ->
  (fun l#6212 ->
   (fun b#6213 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6211 ,
                                                                   l#6212) ,
                                                              b#6213)))))[@inline] in
let length#792 = fun b#6224 -> (({ SIZE })@(b#6224))[@inline] in
let concat#793 =
  fun b1#6226 ->
  (fun b2#6227 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6226 , b2#6227))))[@inline] in
let sub#794 =
  fun s#6229 ->
  (fun l#6230 ->
   (fun b#6231 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6229 ,
                                                                   l#6230) ,
                                                              b#6231)))))[@inline] in
let blake2b#795 = fun b#6233 -> (({ BLAKE2B })@(b#6233))[@inline] in
let sha256#796 = fun b#6235 -> (({ SHA256 })@(b#6235))[@inline] in
let sha512#797 = fun b#6237 -> (({ SHA512 })@(b#6237))[@inline] in
let sha3#798 = fun b#6239 -> (({ SHA3 })@(b#6239))[@inline] in
let keccak#799 = fun b#6241 -> (({ KECCAK })@(b#6241))[@inline] in
let hash_key#800 = fun k#6243 -> (({ HASH_KEY })@(k#6243))[@inline] in
let check#801 =
  fun k#6245 ->
  (fun s#6246 ->
   (fun b#6247 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6245 , s#6246) ,
                                                   b#6247)))))[@inline] in
let assert#802 =
  fun b#6249 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6249))[@inline] in
let abs#805 = fun i#6255 -> (({ ABS })@(i#6255))[@inline] in
let is_nat#806 = fun i#6257 -> (({ ISNAT })@(i#6257))[@inline] in
let true#807 = TRUE()[@inline] in
let false#808 = FALSE()[@inline] in
let unit#809 = UNIT()[@inline] in
let assert_with_error#812 =
  fun b#6265 ->
  (fun s#6266 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6265 , s#6266))))[@inline] in
let get_total_voting_power#820 =
  fun _u#6286 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let set_source#823 =
  fun _a#6292 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let get_storage_of_address#824 =
  fun _a#6294 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let get_balance#825 =
  fun _a#6296 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let print#826 =
  fun _v#6298 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let eprint#827 =
  fun _v#6300 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let get_voting_power#828 =
  fun _kh#6302 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let nth_bootstrap_contract#829 =
  fun _i#6304 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let nth_bootstrap_account#830 =
  fun _i#6306 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let get_bootstrap_account#831 =
  fun _n#6308 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let last_originations#833 =
  fun _u#6312 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let new_account#835 =
  fun _u#6316 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let bake_until_n_cycle_end#837 =
  fun _n#6320 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let register_delegate#839 =
  fun _kh#6324 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let register_constant#840 =
  fun _m#6326 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let constant_to_michelson_program#842 =
  fun _s#6330 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let restore_context#843 =
  fun _u#6332 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let save_context#844 =
  fun _u#6334 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let drop_context#845 =
  fun _u#6336 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let set_baker_policy#848 =
  fun _bp#6342 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let set_baker#849 =
  fun _a#6344 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let size#850 =
  fun _c#6346 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let read_contract_from_file#852 =
  fun _fn#6350 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let chr#853 =
  fun _n#6352 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let nl#854 = L("NEWLINE")[@inline] in
let println#855 =
  fun _v#6355 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))[@inline] in
let transfer#856 =
  fun _a#6357 ->
  (fun _s#6358 ->
   (fun _t#6359 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))))[@inline] in
let transfer_exn#857 =
  fun _a#6361 ->
  (fun _s#6362 ->
   (fun _t#6363 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))))[@inline] in
let reset_state#859 =
  fun _n#6367 ->
  (fun _l#6368 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit))))[@inline] in
let reset_state_at#860 =
  fun _t#6370 ->
  (fun _n#6371 ->
   (fun _l#6372 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))))[@inline] in
let save_mutation#863 =
  fun _s#6381 ->
  (fun _m#6382 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit))))[@inline] in
let sign#866 =
  fun _sk#6390 ->
  (fun _d#6391 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit))))[@inline] in
let add_account#867 =
  fun _s#6393 ->
  (fun _k#6394 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit))))[@inline] in
let baker_account#868 =
  fun _p#6396 ->
  (fun _o#6397 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit))))[@inline] in
let create_chest#870 =
  fun _b#6402 ->
  (fun _n#6403 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit))))[@inline] in
let create_chest_key#871 =
  fun _c#6405 ->
  (fun _n#6406 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit))))[@inline] in
let michelson_equal#874 =
  fun _m1#6416 ->
  (fun _m2#6417 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit))))[@inline] in
let originate_contract#876 =
  fun _c#6422 ->
  (fun _s#6423 ->
   (fun _t#6424 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))))[@inline] in
let compile_contract_from_file#878 =
  fun _fn#6430 ->
  (fun _e#6431 ->
   (fun _v#6432 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))))[@inline] in
let originate_from_file#879 =
  fun _fn#6434 ->
  (fun _e#6435 ->
   (fun _v#6436 ->
    (fun _s#6437 ->
     (fun _t#6438 -> ((fun x#6277 -> (({ FAILWITH })@(x#6277)))@(L(unit)))))))[@inline] in
let tata#880 = ADD(toto#214 , titi#381) in
let foo#881 = (f#382)@(PAIR(L(unit) , L(3))) in
let get_balance#882 =
  fun _u#6442 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#883 =
  fun _u#6444 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#884 = fun _u#6446 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#885 =
  fun _u#6448 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#886 =
  fun _u#6450 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#887 = fun _u#6452 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#888 = fun _u#6454 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#889 =
  fun _u#6456 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#890 =
  fun _u#6458 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#891 =
  fun _u#6460 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#892 =
  fun kh#6462 -> (({ VOTING_POWER })@(kh#6462))[@inline] in
let implicit_account#894 =
  fun kh#6466 -> (IMPLICIT_ACCOUNT(kh#6466))[@inline] in
let pairing_check#898 =
  fun l#6474 -> (({ PAIRING_CHECK })@(l#6474))[@inline] in
let set_delegate#900 = fun o#6478 -> (SET_DELEGATE(o#6478))[@inline] in
let open_chest#906 =
  fun ck#6494 ->
  (fun c#6495 -> (fun n#6496 -> (OPEN_CHEST(ck#6494 , c#6495 , n#6496))))[@inline] in
let xor#909 =
  fun l#6505 -> (fun r#6506 -> (XOR(l#6505 , r#6506)))[@inline] in
let shift_left#910 =
  fun l#6508 -> (fun r#6509 -> (LSL(l#6508 , r#6509)))[@inline] in
let shift_right#911 =
  fun l#6511 -> (fun r#6512 -> (LSR(l#6511 , r#6512)))[@inline] in
let length#952 = fun b#6642 -> (({ SIZE })@(b#6642))[@inline] in
let concat#953 =
  fun b1#6644 ->
  (fun b2#6645 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6644 , b2#6645))))[@inline] in
let sub#954 =
  fun s#6647 ->
  (fun l#6648 ->
   (fun b#6649 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6647 ,
                                                                   l#6648) ,
                                                              b#6649)))))[@inline] in
let length#959 = fun b#6660 -> (({ SIZE })@(b#6660))[@inline] in
let concat#960 =
  fun b1#6662 ->
  (fun b2#6663 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6662 , b2#6663))))[@inline] in
let sub#961 =
  fun s#6665 ->
  (fun l#6666 ->
   (fun b#6667 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6665 ,
                                                                   l#6666) ,
                                                              b#6667)))))[@inline] in
let blake2b#962 = fun b#6669 -> (({ BLAKE2B })@(b#6669))[@inline] in
let sha256#963 = fun b#6671 -> (({ SHA256 })@(b#6671))[@inline] in
let sha512#964 = fun b#6673 -> (({ SHA512 })@(b#6673))[@inline] in
let sha3#965 = fun b#6675 -> (({ SHA3 })@(b#6675))[@inline] in
let keccak#966 = fun b#6677 -> (({ KECCAK })@(b#6677))[@inline] in
let hash_key#967 = fun k#6679 -> (({ HASH_KEY })@(k#6679))[@inline] in
let check#968 =
  fun k#6681 ->
  (fun s#6682 ->
   (fun b#6683 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6681 , s#6682) ,
                                                   b#6683)))))[@inline] in
let assert#969 =
  fun b#6685 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6685))[@inline] in
let abs#972 = fun i#6691 -> (({ ABS })@(i#6691))[@inline] in
let is_nat#973 = fun i#6693 -> (({ ISNAT })@(i#6693))[@inline] in
let true#974 = TRUE()[@inline] in
let false#975 = FALSE()[@inline] in
let unit#976 = UNIT()[@inline] in
let assert_with_error#979 =
  fun b#6701 ->
  (fun s#6702 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6701 , s#6702))))[@inline] in
let get_total_voting_power#987 =
  fun _u#6722 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let set_source#990 =
  fun _a#6728 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let get_storage_of_address#991 =
  fun _a#6730 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let get_balance#992 =
  fun _a#6732 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let print#993 =
  fun _v#6734 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let eprint#994 =
  fun _v#6736 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let get_voting_power#995 =
  fun _kh#6738 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let nth_bootstrap_contract#996 =
  fun _i#6740 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let nth_bootstrap_account#997 =
  fun _i#6742 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let get_bootstrap_account#998 =
  fun _n#6744 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let last_originations#1000 =
  fun _u#6748 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let new_account#1002 =
  fun _u#6752 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1004 =
  fun _n#6756 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let register_delegate#1006 =
  fun _kh#6760 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let register_constant#1007 =
  fun _m#6762 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let constant_to_michelson_program#1009 =
  fun _s#6766 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let restore_context#1010 =
  fun _u#6768 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let save_context#1011 =
  fun _u#6770 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let drop_context#1012 =
  fun _u#6772 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let set_baker_policy#1015 =
  fun _bp#6778 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let set_baker#1016 =
  fun _a#6780 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let size#1017 =
  fun _c#6782 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let read_contract_from_file#1019 =
  fun _fn#6786 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let chr#1020 =
  fun _n#6788 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let nl#1021 = L("NEWLINE")[@inline] in
let println#1022 =
  fun _v#6791 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))[@inline] in
let transfer#1023 =
  fun _a#6793 ->
  (fun _s#6794 ->
   (fun _t#6795 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))))[@inline] in
let transfer_exn#1024 =
  fun _a#6797 ->
  (fun _s#6798 ->
   (fun _t#6799 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))))[@inline] in
let reset_state#1026 =
  fun _n#6803 ->
  (fun _l#6804 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit))))[@inline] in
let reset_state_at#1027 =
  fun _t#6806 ->
  (fun _n#6807 ->
   (fun _l#6808 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))))[@inline] in
let save_mutation#1030 =
  fun _s#6817 ->
  (fun _m#6818 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit))))[@inline] in
let sign#1033 =
  fun _sk#6826 ->
  (fun _d#6827 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit))))[@inline] in
let add_account#1034 =
  fun _s#6829 ->
  (fun _k#6830 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit))))[@inline] in
let baker_account#1035 =
  fun _p#6832 ->
  (fun _o#6833 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit))))[@inline] in
let create_chest#1037 =
  fun _b#6838 ->
  (fun _n#6839 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit))))[@inline] in
let create_chest_key#1038 =
  fun _c#6841 ->
  (fun _n#6842 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit))))[@inline] in
let michelson_equal#1041 =
  fun _m1#6852 ->
  (fun _m2#6853 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit))))[@inline] in
let originate_contract#1043 =
  fun _c#6858 ->
  (fun _s#6859 ->
   (fun _t#6860 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))))[@inline] in
let compile_contract_from_file#1045 =
  fun _fn#6866 ->
  (fun _e#6867 ->
   (fun _v#6868 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))))[@inline] in
let originate_from_file#1046 =
  fun _fn#6870 ->
  (fun _e#6871 ->
   (fun _v#6872 ->
    (fun _s#6873 ->
     (fun _t#6874 -> ((fun x#6713 -> (({ FAILWITH })@(x#6713)))@(L(unit)))))))[@inline] in
let toto#1047 = L(10) in
let foo#1048 = L("bar") in
let get_balance#1049 =
  fun _u#6878 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1050 =
  fun _u#6880 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1051 = fun _u#6882 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1052 =
  fun _u#6884 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1053 =
  fun _u#6886 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1054 =
  fun _u#6888 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1055 = fun _u#6890 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1056 =
  fun _u#6892 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1057 =
  fun _u#6894 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1058 =
  fun _u#6896 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1059 =
  fun kh#6898 -> (({ VOTING_POWER })@(kh#6898))[@inline] in
let implicit_account#1061 =
  fun kh#6902 -> (IMPLICIT_ACCOUNT(kh#6902))[@inline] in
let pairing_check#1065 =
  fun l#6910 -> (({ PAIRING_CHECK })@(l#6910))[@inline] in
let set_delegate#1067 = fun o#6914 -> (SET_DELEGATE(o#6914))[@inline] in
let open_chest#1073 =
  fun ck#6930 ->
  (fun c#6931 -> (fun n#6932 -> (OPEN_CHEST(ck#6930 , c#6931 , n#6932))))[@inline] in
let xor#1076 =
  fun l#6941 -> (fun r#6942 -> (XOR(l#6941 , r#6942)))[@inline] in
let shift_left#1077 =
  fun l#6944 -> (fun r#6945 -> (LSL(l#6944 , r#6945)))[@inline] in
let shift_right#1078 =
  fun l#6947 -> (fun r#6948 -> (LSR(l#6947 , r#6948)))[@inline] in
let length#1119 = fun b#7078 -> (({ SIZE })@(b#7078))[@inline] in
let concat#1120 =
  fun b1#7080 ->
  (fun b2#7081 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7080 , b2#7081))))[@inline] in
let sub#1121 =
  fun s#7083 ->
  (fun l#7084 ->
   (fun b#7085 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7083 ,
                                                                   l#7084) ,
                                                              b#7085)))))[@inline] in
let length#1126 = fun b#7096 -> (({ SIZE })@(b#7096))[@inline] in
let concat#1127 =
  fun b1#7098 ->
  (fun b2#7099 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7098 , b2#7099))))[@inline] in
let sub#1128 =
  fun s#7101 ->
  (fun l#7102 ->
   (fun b#7103 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7101 ,
                                                                   l#7102) ,
                                                              b#7103)))))[@inline] in
let blake2b#1129 = fun b#7105 -> (({ BLAKE2B })@(b#7105))[@inline] in
let sha256#1130 = fun b#7107 -> (({ SHA256 })@(b#7107))[@inline] in
let sha512#1131 = fun b#7109 -> (({ SHA512 })@(b#7109))[@inline] in
let sha3#1132 = fun b#7111 -> (({ SHA3 })@(b#7111))[@inline] in
let keccak#1133 = fun b#7113 -> (({ KECCAK })@(b#7113))[@inline] in
let hash_key#1134 = fun k#7115 -> (({ HASH_KEY })@(k#7115))[@inline] in
let check#1135 =
  fun k#7117 ->
  (fun s#7118 ->
   (fun b#7119 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7117 , s#7118) ,
                                                   b#7119)))))[@inline] in
let assert =
  fun b#7121 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7121))[@inline] in
let abs = fun i#7127 -> (({ ABS })@(i#7127))[@inline] in
let is_nat = fun i#7129 -> (({ ISNAT })@(i#7129))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#7137 ->
  (fun s#7138 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7137 , s#7138))))[@inline] in
let get_total_voting_power#1140 =
  fun _u#7158 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let set_source#1143 =
  fun _a#7164 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let get_storage_of_address#1144 =
  fun _a#7166 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let get_balance#1145 =
  fun _a#7168 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let print#1146 =
  fun _v#7170 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let eprint#1147 =
  fun _v#7172 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let get_voting_power#1148 =
  fun _kh#7174 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let nth_bootstrap_contract#1149 =
  fun _i#7176 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let nth_bootstrap_account#1150 =
  fun _i#7178 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let get_bootstrap_account#1151 =
  fun _n#7180 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let last_originations#1153 =
  fun _u#7184 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let new_account#1155 =
  fun _u#7188 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1157 =
  fun _n#7192 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let register_delegate#1159 =
  fun _kh#7196 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let register_constant#1160 =
  fun _m#7198 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let constant_to_michelson_program#1162 =
  fun _s#7202 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let restore_context#1163 =
  fun _u#7204 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let save_context#1164 =
  fun _u#7206 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let drop_context#1165 =
  fun _u#7208 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let set_baker_policy#1168 =
  fun _bp#7214 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let set_baker#1169 =
  fun _a#7216 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let size#1170 =
  fun _c#7218 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let read_contract_from_file#1172 =
  fun _fn#7222 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let chr#1173 =
  fun _n#7224 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let nl#1174 = L("NEWLINE")[@inline] in
let println#1175 =
  fun _v#7227 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))[@inline] in
let transfer#1176 =
  fun _a#7229 ->
  (fun _s#7230 ->
   (fun _t#7231 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))))[@inline] in
let transfer_exn#1177 =
  fun _a#7233 ->
  (fun _s#7234 ->
   (fun _t#7235 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))))[@inline] in
let reset_state#1179 =
  fun _n#7239 ->
  (fun _l#7240 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit))))[@inline] in
let reset_state_at#1180 =
  fun _t#7242 ->
  (fun _n#7243 ->
   (fun _l#7244 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))))[@inline] in
let save_mutation#1183 =
  fun _s#7253 ->
  (fun _m#7254 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit))))[@inline] in
let sign#1186 =
  fun _sk#7262 ->
  (fun _d#7263 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit))))[@inline] in
let add_account#1187 =
  fun _s#7265 ->
  (fun _k#7266 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit))))[@inline] in
let baker_account#1188 =
  fun _p#7268 ->
  (fun _o#7269 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit))))[@inline] in
let create_chest#1190 =
  fun _b#7274 ->
  (fun _n#7275 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit))))[@inline] in
let create_chest_key#1191 =
  fun _c#7277 ->
  (fun _n#7278 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit))))[@inline] in
let michelson_equal#1194 =
  fun _m1#7288 ->
  (fun _m2#7289 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit))))[@inline] in
let originate_contract#1196 =
  fun _c#7294 ->
  (fun _s#7295 ->
   (fun _t#7296 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))))[@inline] in
let compile_contract_from_file#1198 =
  fun _fn#7302 ->
  (fun _e#7303 ->
   (fun _v#7304 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))))[@inline] in
let originate_from_file#1199 =
  fun _fn#7306 ->
  (fun _e#7307 ->
   (fun _v#7308 ->
    (fun _s#7309 ->
     (fun _t#7310 -> ((fun x#7149 -> (({ FAILWITH })@(x#7149)))@(L(unit)))))))[@inline] in
let toto = ADD(toto#1047 , toto#214) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#7314 ->
  (let (gen#7320, gen#7321) = gen#7314 in
   let p#7315 = gen#7320 in
   let s#7316 = gen#7321 in
   let s#7317 = ADD(ADD(p#7315 , s#7316) , toto) in
   PAIR(LIST_EMPTY() , s#7317)) in
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
