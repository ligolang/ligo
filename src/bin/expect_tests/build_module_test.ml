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
  fun _u#4274 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#50 =
  fun _u#4276 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#51 = fun _u#4278 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#52 =
  fun _u#4280 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let sender#53 = (get_sender#52)@(L(unit))[@inline] in
let get_source#54 =
  fun _u#4283 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#55 = fun _u#4285 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#56 = fun _u#4287 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#57 =
  fun _u#4289 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#58 =
  fun _u#4291 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#59 =
  fun _u#4293 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#60 =
  fun kh#4295 -> (({ VOTING_POWER })@(kh#4295))[@inline] in
let implicit_account#62 =
  fun kh#4299 -> (IMPLICIT_ACCOUNT(kh#4299))[@inline] in
let pairing_check#66 =
  fun l#4307 -> (({ PAIRING_CHECK })@(l#4307))[@inline] in
let set_delegate#68 = fun o#4311 -> (SET_DELEGATE(o#4311))[@inline] in
let open_chest#74 =
  fun ck#4327 ->
  (fun c#4328 -> (fun n#4329 -> (OPEN_CHEST(ck#4327 , c#4328 , n#4329))))[@inline] in
let xor#77 = fun l#4338 -> (fun r#4339 -> (XOR(l#4338 , r#4339)))[@inline] in
let shift_left#78 =
  fun l#4341 -> (fun r#4342 -> (LSL(l#4341 , r#4342)))[@inline] in
let shift_right#79 =
  fun l#4344 -> (fun r#4345 -> (LSR(l#4344 , r#4345)))[@inline] in
let length#120 = fun b#4475 -> (({ SIZE })@(b#4475))[@inline] in
let concat#121 =
  fun b1#4477 ->
  (fun b2#4478 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4477 , b2#4478))))[@inline] in
let sub#122 =
  fun s#4480 ->
  (fun l#4481 ->
   (fun b#4482 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4480 ,
                                                                   l#4481) ,
                                                              b#4482)))))[@inline] in
let length#127 = fun b#4493 -> (({ SIZE })@(b#4493))[@inline] in
let concat#128 =
  fun b1#4495 ->
  (fun b2#4496 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4495 , b2#4496))))[@inline] in
let sub#129 =
  fun s#4498 ->
  (fun l#4499 ->
   (fun b#4500 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4498 ,
                                                                   l#4499) ,
                                                              b#4500)))))[@inline] in
let blake2b#130 = fun b#4502 -> (({ BLAKE2B })@(b#4502))[@inline] in
let sha256#131 = fun b#4504 -> (({ SHA256 })@(b#4504))[@inline] in
let sha512#132 = fun b#4506 -> (({ SHA512 })@(b#4506))[@inline] in
let sha3#133 = fun b#4508 -> (({ SHA3 })@(b#4508))[@inline] in
let keccak#134 = fun b#4510 -> (({ KECCAK })@(b#4510))[@inline] in
let hash_key#135 = fun k#4512 -> (({ HASH_KEY })@(k#4512))[@inline] in
let check#136 =
  fun k#4514 ->
  (fun s#4515 ->
   (fun b#4516 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4514 , s#4515) ,
                                                   b#4516)))))[@inline] in
let assert#137 =
  fun b#4518 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4518))[@inline] in
let abs#140 = fun i#4524 -> (({ ABS })@(i#4524))[@inline] in
let is_nat#141 = fun i#4526 -> (({ ISNAT })@(i#4526))[@inline] in
let true#142 = TRUE()[@inline] in
let false#143 = FALSE()[@inline] in
let unit#144 = UNIT()[@inline] in
let assert_with_error#147 =
  fun b#4534 ->
  (fun s#4535 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4534 , s#4535))))[@inline] in
let poly_stub_273 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_272 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_271 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_270 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_269 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_268 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_267 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_266 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_265 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_264 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_263 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_262 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_261 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_260 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_259 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_258 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_257 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_256 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_255 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_254 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_253 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_252 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_251 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_250 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_249 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_248 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_247 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_246 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_245 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_244 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_243 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_242 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_241 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_240 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_239 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_238 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_237 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_236 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let poly_stub_235 = fun x#4546 -> (({ FAILWITH })@(x#4546))[@inline] in
let get_total_voting_power#155 =
  fun _u#4555 -> ((poly_stub_273)@(L(unit)))[@inline] in
let set_source#158 = fun _a#4561 -> ((poly_stub_272)@(L(unit)))[@inline] in
let get_storage_of_address#159 =
  fun _a#4563 -> ((poly_stub_271)@(L(unit)))[@inline] in
let get_balance#160 = fun _a#4565 -> ((poly_stub_270)@(L(unit)))[@inline] in
let print#161 = fun _v#4567 -> ((poly_stub_269)@(L(unit)))[@inline] in
let eprint#162 = fun _v#4569 -> ((poly_stub_268)@(L(unit)))[@inline] in
let get_voting_power#163 =
  fun _kh#4571 -> ((poly_stub_267)@(L(unit)))[@inline] in
let nth_bootstrap_contract#164 =
  fun _i#4573 -> ((poly_stub_266)@(L(unit)))[@inline] in
let nth_bootstrap_account#165 =
  fun _i#4575 -> ((poly_stub_265)@(L(unit)))[@inline] in
let get_bootstrap_account#166 =
  fun _n#4577 -> ((poly_stub_264)@(L(unit)))[@inline] in
let last_originations#168 =
  fun _u#4581 -> ((poly_stub_263)@(L(unit)))[@inline] in
let new_account#170 = fun _u#4585 -> ((poly_stub_262)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#172 =
  fun _n#4589 -> ((poly_stub_261)@(L(unit)))[@inline] in
let register_delegate#174 =
  fun _kh#4593 -> ((poly_stub_260)@(L(unit)))[@inline] in
let register_constant#175 =
  fun _m#4595 -> ((poly_stub_259)@(L(unit)))[@inline] in
let constant_to_michelson_program#177 =
  fun _s#4599 -> ((poly_stub_258)@(L(unit)))[@inline] in
let restore_context#178 =
  fun _u#4601 -> ((poly_stub_257)@(L(unit)))[@inline] in
let save_context#179 = fun _u#4603 -> ((poly_stub_256)@(L(unit)))[@inline] in
let drop_context#180 = fun _u#4605 -> ((poly_stub_255)@(L(unit)))[@inline] in
let set_baker_policy#183 =
  fun _bp#4611 -> ((poly_stub_254)@(L(unit)))[@inline] in
let set_baker#184 = fun _a#4613 -> ((poly_stub_253)@(L(unit)))[@inline] in
let size#185 = fun _c#4615 -> ((poly_stub_252)@(L(unit)))[@inline] in
let read_contract_from_file#187 =
  fun _fn#4619 -> ((poly_stub_251)@(L(unit)))[@inline] in
let chr#188 = fun _n#4621 -> ((poly_stub_250)@(L(unit)))[@inline] in
let nl#189 = L("NEWLINE")[@inline] in
let println#190 = fun _v#4624 -> ((poly_stub_249)@(L(unit)))[@inline] in
let transfer#191 =
  fun _a#4626 ->
  (fun _s#4627 -> (fun _t#4628 -> ((poly_stub_248)@(L(unit)))))[@inline] in
let transfer_exn#192 =
  fun _a#4630 ->
  (fun _s#4631 -> (fun _t#4632 -> ((poly_stub_247)@(L(unit)))))[@inline] in
let reset_state#194 =
  fun _n#4636 -> (fun _l#4637 -> ((poly_stub_246)@(L(unit))))[@inline] in
let reset_state_at#195 =
  fun _t#4639 ->
  (fun _n#4640 -> (fun _l#4641 -> ((poly_stub_245)@(L(unit)))))[@inline] in
let save_mutation#198 =
  fun _s#4650 -> (fun _m#4651 -> ((poly_stub_244)@(L(unit))))[@inline] in
let sign#201 =
  fun _sk#4659 -> (fun _d#4660 -> ((poly_stub_243)@(L(unit))))[@inline] in
let add_account#202 =
  fun _s#4662 -> (fun _k#4663 -> ((poly_stub_242)@(L(unit))))[@inline] in
let baker_account#203 =
  fun _p#4665 -> (fun _o#4666 -> ((poly_stub_241)@(L(unit))))[@inline] in
let create_chest#205 =
  fun _b#4671 -> (fun _n#4672 -> ((poly_stub_240)@(L(unit))))[@inline] in
let create_chest_key#206 =
  fun _c#4674 -> (fun _n#4675 -> ((poly_stub_239)@(L(unit))))[@inline] in
let michelson_equal#209 =
  fun _m1#4685 -> (fun _m2#4686 -> ((poly_stub_238)@(L(unit))))[@inline] in
let originate_contract#211 =
  fun _c#4691 ->
  (fun _s#4692 -> (fun _t#4693 -> ((poly_stub_237)@(L(unit)))))[@inline] in
let compile_contract_from_file#213 =
  fun _fn#4699 ->
  (fun _e#4700 -> (fun _v#4701 -> ((poly_stub_236)@(L(unit)))))[@inline] in
let originate_from_file#214 =
  fun _fn#4703 ->
  (fun _e#4704 ->
   (fun _v#4705 ->
    (fun _s#4706 -> (fun _t#4707 -> ((poly_stub_235)@(L(unit)))))))[@inline] in
let toto#215 = L(1) in
let get_balance#216 =
  fun _u#4710 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#217 =
  fun _u#4712 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#218 = fun _u#4714 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#219 =
  fun _u#4716 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let sender#220 = (get_sender#219)@(L(unit))[@inline] in
let get_source#221 =
  fun _u#4719 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#222 = fun _u#4721 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#223 = fun _u#4723 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#224 =
  fun _u#4725 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#225 =
  fun _u#4727 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#226 =
  fun _u#4729 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#227 =
  fun kh#4731 -> (({ VOTING_POWER })@(kh#4731))[@inline] in
let implicit_account#229 =
  fun kh#4735 -> (IMPLICIT_ACCOUNT(kh#4735))[@inline] in
let pairing_check#233 =
  fun l#4743 -> (({ PAIRING_CHECK })@(l#4743))[@inline] in
let set_delegate#235 = fun o#4747 -> (SET_DELEGATE(o#4747))[@inline] in
let open_chest#241 =
  fun ck#4763 ->
  (fun c#4764 -> (fun n#4765 -> (OPEN_CHEST(ck#4763 , c#4764 , n#4765))))[@inline] in
let xor#244 =
  fun l#4774 -> (fun r#4775 -> (XOR(l#4774 , r#4775)))[@inline] in
let shift_left#245 =
  fun l#4777 -> (fun r#4778 -> (LSL(l#4777 , r#4778)))[@inline] in
let shift_right#246 =
  fun l#4780 -> (fun r#4781 -> (LSR(l#4780 , r#4781)))[@inline] in
let length#287 = fun b#4911 -> (({ SIZE })@(b#4911))[@inline] in
let concat#288 =
  fun b1#4913 ->
  (fun b2#4914 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4913 , b2#4914))))[@inline] in
let sub#289 =
  fun s#4916 ->
  (fun l#4917 ->
   (fun b#4918 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4916 ,
                                                                   l#4917) ,
                                                              b#4918)))))[@inline] in
let length#294 = fun b#4929 -> (({ SIZE })@(b#4929))[@inline] in
let concat#295 =
  fun b1#4931 ->
  (fun b2#4932 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4931 , b2#4932))))[@inline] in
let sub#296 =
  fun s#4934 ->
  (fun l#4935 ->
   (fun b#4936 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4934 ,
                                                                   l#4935) ,
                                                              b#4936)))))[@inline] in
let blake2b#297 = fun b#4938 -> (({ BLAKE2B })@(b#4938))[@inline] in
let sha256#298 = fun b#4940 -> (({ SHA256 })@(b#4940))[@inline] in
let sha512#299 = fun b#4942 -> (({ SHA512 })@(b#4942))[@inline] in
let sha3#300 = fun b#4944 -> (({ SHA3 })@(b#4944))[@inline] in
let keccak#301 = fun b#4946 -> (({ KECCAK })@(b#4946))[@inline] in
let hash_key#302 = fun k#4948 -> (({ HASH_KEY })@(k#4948))[@inline] in
let check#303 =
  fun k#4950 ->
  (fun s#4951 ->
   (fun b#4952 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4950 , s#4951) ,
                                                   b#4952)))))[@inline] in
let assert#304 =
  fun b#4954 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4954))[@inline] in
let abs#307 = fun i#4960 -> (({ ABS })@(i#4960))[@inline] in
let is_nat#308 = fun i#4962 -> (({ ISNAT })@(i#4962))[@inline] in
let true#309 = TRUE()[@inline] in
let false#310 = FALSE()[@inline] in
let unit#311 = UNIT()[@inline] in
let assert_with_error#314 =
  fun b#4970 ->
  (fun s#4971 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4970 , s#4971))))[@inline] in
let poly_stub_234 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_233 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_232 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_231 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_230 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_229 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_228 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_227 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_226 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_225 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_224 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_223 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_222 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_221 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_220 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_219 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_218 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_217 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_216 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_215 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_214 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_213 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_212 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_211 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_210 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_209 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_208 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_207 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_206 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_205 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_204 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_203 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_202 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_201 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_200 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_199 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_198 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_197 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let poly_stub_196 = fun x#4982 -> (({ FAILWITH })@(x#4982))[@inline] in
let get_total_voting_power#322 =
  fun _u#4991 -> ((poly_stub_234)@(L(unit)))[@inline] in
let set_source#325 = fun _a#4997 -> ((poly_stub_233)@(L(unit)))[@inline] in
let get_storage_of_address#326 =
  fun _a#4999 -> ((poly_stub_232)@(L(unit)))[@inline] in
let get_balance#327 = fun _a#5001 -> ((poly_stub_231)@(L(unit)))[@inline] in
let print#328 = fun _v#5003 -> ((poly_stub_230)@(L(unit)))[@inline] in
let eprint#329 = fun _v#5005 -> ((poly_stub_229)@(L(unit)))[@inline] in
let get_voting_power#330 =
  fun _kh#5007 -> ((poly_stub_228)@(L(unit)))[@inline] in
let nth_bootstrap_contract#331 =
  fun _i#5009 -> ((poly_stub_227)@(L(unit)))[@inline] in
let nth_bootstrap_account#332 =
  fun _i#5011 -> ((poly_stub_226)@(L(unit)))[@inline] in
let get_bootstrap_account#333 =
  fun _n#5013 -> ((poly_stub_225)@(L(unit)))[@inline] in
let last_originations#335 =
  fun _u#5017 -> ((poly_stub_224)@(L(unit)))[@inline] in
let new_account#337 = fun _u#5021 -> ((poly_stub_223)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#339 =
  fun _n#5025 -> ((poly_stub_222)@(L(unit)))[@inline] in
let register_delegate#341 =
  fun _kh#5029 -> ((poly_stub_221)@(L(unit)))[@inline] in
let register_constant#342 =
  fun _m#5031 -> ((poly_stub_220)@(L(unit)))[@inline] in
let constant_to_michelson_program#344 =
  fun _s#5035 -> ((poly_stub_219)@(L(unit)))[@inline] in
let restore_context#345 =
  fun _u#5037 -> ((poly_stub_218)@(L(unit)))[@inline] in
let save_context#346 = fun _u#5039 -> ((poly_stub_217)@(L(unit)))[@inline] in
let drop_context#347 = fun _u#5041 -> ((poly_stub_216)@(L(unit)))[@inline] in
let set_baker_policy#350 =
  fun _bp#5047 -> ((poly_stub_215)@(L(unit)))[@inline] in
let set_baker#351 = fun _a#5049 -> ((poly_stub_214)@(L(unit)))[@inline] in
let size#352 = fun _c#5051 -> ((poly_stub_213)@(L(unit)))[@inline] in
let read_contract_from_file#354 =
  fun _fn#5055 -> ((poly_stub_212)@(L(unit)))[@inline] in
let chr#355 = fun _n#5057 -> ((poly_stub_211)@(L(unit)))[@inline] in
let nl#356 = L("NEWLINE")[@inline] in
let println#357 = fun _v#5060 -> ((poly_stub_210)@(L(unit)))[@inline] in
let transfer#358 =
  fun _a#5062 ->
  (fun _s#5063 -> (fun _t#5064 -> ((poly_stub_209)@(L(unit)))))[@inline] in
let transfer_exn#359 =
  fun _a#5066 ->
  (fun _s#5067 -> (fun _t#5068 -> ((poly_stub_208)@(L(unit)))))[@inline] in
let reset_state#361 =
  fun _n#5072 -> (fun _l#5073 -> ((poly_stub_207)@(L(unit))))[@inline] in
let reset_state_at#362 =
  fun _t#5075 ->
  (fun _n#5076 -> (fun _l#5077 -> ((poly_stub_206)@(L(unit)))))[@inline] in
let save_mutation#365 =
  fun _s#5086 -> (fun _m#5087 -> ((poly_stub_205)@(L(unit))))[@inline] in
let sign#368 =
  fun _sk#5095 -> (fun _d#5096 -> ((poly_stub_204)@(L(unit))))[@inline] in
let add_account#369 =
  fun _s#5098 -> (fun _k#5099 -> ((poly_stub_203)@(L(unit))))[@inline] in
let baker_account#370 =
  fun _p#5101 -> (fun _o#5102 -> ((poly_stub_202)@(L(unit))))[@inline] in
let create_chest#372 =
  fun _b#5107 -> (fun _n#5108 -> ((poly_stub_201)@(L(unit))))[@inline] in
let create_chest_key#373 =
  fun _c#5110 -> (fun _n#5111 -> ((poly_stub_200)@(L(unit))))[@inline] in
let michelson_equal#376 =
  fun _m1#5121 -> (fun _m2#5122 -> ((poly_stub_199)@(L(unit))))[@inline] in
let originate_contract#378 =
  fun _c#5127 ->
  (fun _s#5128 -> (fun _t#5129 -> ((poly_stub_198)@(L(unit)))))[@inline] in
let compile_contract_from_file#380 =
  fun _fn#5135 ->
  (fun _e#5136 -> (fun _v#5137 -> ((poly_stub_197)@(L(unit)))))[@inline] in
let originate_from_file#381 =
  fun _fn#5139 ->
  (fun _e#5140 ->
   (fun _v#5141 ->
    (fun _s#5142 -> (fun _t#5143 -> ((poly_stub_196)@(L(unit)))))))[@inline] in
let toto#382 = L(32) in
let titi#383 = ADD(toto#215 , L(42)) in
let f#384 =
  fun gen#5147 ->
  (let (gen#7339, gen#7340) = gen#5147 in
   let gen#5148 = gen#7339 in
   let x#5149 = gen#7340 in
   let x#5150 = ADD(ADD(x#5149 , toto#215) , titi#383) in
   PAIR(LIST_EMPTY() , x#5150)) in
let get_balance#385 =
  fun _u#5152 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#386 =
  fun _u#5154 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#387 = fun _u#5156 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#388 =
  fun _u#5158 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let sender#389 = (get_sender#388)@(L(unit))[@inline] in
let get_source#390 =
  fun _u#5161 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#391 = fun _u#5163 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#392 = fun _u#5165 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#393 =
  fun _u#5167 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#394 =
  fun _u#5169 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#395 =
  fun _u#5171 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#396 =
  fun kh#5173 -> (({ VOTING_POWER })@(kh#5173))[@inline] in
let implicit_account#398 =
  fun kh#5177 -> (IMPLICIT_ACCOUNT(kh#5177))[@inline] in
let pairing_check#402 =
  fun l#5185 -> (({ PAIRING_CHECK })@(l#5185))[@inline] in
let set_delegate#404 = fun o#5189 -> (SET_DELEGATE(o#5189))[@inline] in
let open_chest#410 =
  fun ck#5205 ->
  (fun c#5206 -> (fun n#5207 -> (OPEN_CHEST(ck#5205 , c#5206 , n#5207))))[@inline] in
let xor#413 =
  fun l#5216 -> (fun r#5217 -> (XOR(l#5216 , r#5217)))[@inline] in
let shift_left#414 =
  fun l#5219 -> (fun r#5220 -> (LSL(l#5219 , r#5220)))[@inline] in
let shift_right#415 =
  fun l#5222 -> (fun r#5223 -> (LSR(l#5222 , r#5223)))[@inline] in
let length#456 = fun b#5353 -> (({ SIZE })@(b#5353))[@inline] in
let concat#457 =
  fun b1#5355 ->
  (fun b2#5356 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5355 , b2#5356))))[@inline] in
let sub#458 =
  fun s#5358 ->
  (fun l#5359 ->
   (fun b#5360 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5358 ,
                                                                   l#5359) ,
                                                              b#5360)))))[@inline] in
let length#463 = fun b#5371 -> (({ SIZE })@(b#5371))[@inline] in
let concat#464 =
  fun b1#5373 ->
  (fun b2#5374 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5373 , b2#5374))))[@inline] in
let sub#465 =
  fun s#5376 ->
  (fun l#5377 ->
   (fun b#5378 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5376 ,
                                                                   l#5377) ,
                                                              b#5378)))))[@inline] in
let blake2b#466 = fun b#5380 -> (({ BLAKE2B })@(b#5380))[@inline] in
let sha256#467 = fun b#5382 -> (({ SHA256 })@(b#5382))[@inline] in
let sha512#468 = fun b#5384 -> (({ SHA512 })@(b#5384))[@inline] in
let sha3#469 = fun b#5386 -> (({ SHA3 })@(b#5386))[@inline] in
let keccak#470 = fun b#5388 -> (({ KECCAK })@(b#5388))[@inline] in
let hash_key#471 = fun k#5390 -> (({ HASH_KEY })@(k#5390))[@inline] in
let check#472 =
  fun k#5392 ->
  (fun s#5393 ->
   (fun b#5394 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5392 , s#5393) ,
                                                   b#5394)))))[@inline] in
let assert#473 =
  fun b#5396 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5396))[@inline] in
let abs#476 = fun i#5402 -> (({ ABS })@(i#5402))[@inline] in
let is_nat#477 = fun i#5404 -> (({ ISNAT })@(i#5404))[@inline] in
let true#478 = TRUE()[@inline] in
let false#479 = FALSE()[@inline] in
let unit#480 = UNIT()[@inline] in
let assert_with_error#483 =
  fun b#5412 ->
  (fun s#5413 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5412 , s#5413))))[@inline] in
let poly_stub_195 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_194 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_193 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_192 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_191 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_190 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_189 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_188 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_187 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_186 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_185 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_184 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_183 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_182 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_181 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_180 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_179 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_178 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_177 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_176 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_175 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_174 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_173 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_172 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_171 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_170 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_169 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_168 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_167 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_166 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_165 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_164 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_163 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_162 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_161 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_160 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_159 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_158 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let poly_stub_157 = fun x#5424 -> (({ FAILWITH })@(x#5424))[@inline] in
let get_total_voting_power#491 =
  fun _u#5433 -> ((poly_stub_195)@(L(unit)))[@inline] in
let set_source#494 = fun _a#5439 -> ((poly_stub_194)@(L(unit)))[@inline] in
let get_storage_of_address#495 =
  fun _a#5441 -> ((poly_stub_193)@(L(unit)))[@inline] in
let get_balance#496 = fun _a#5443 -> ((poly_stub_192)@(L(unit)))[@inline] in
let print#497 = fun _v#5445 -> ((poly_stub_191)@(L(unit)))[@inline] in
let eprint#498 = fun _v#5447 -> ((poly_stub_190)@(L(unit)))[@inline] in
let get_voting_power#499 =
  fun _kh#5449 -> ((poly_stub_189)@(L(unit)))[@inline] in
let nth_bootstrap_contract#500 =
  fun _i#5451 -> ((poly_stub_188)@(L(unit)))[@inline] in
let nth_bootstrap_account#501 =
  fun _i#5453 -> ((poly_stub_187)@(L(unit)))[@inline] in
let get_bootstrap_account#502 =
  fun _n#5455 -> ((poly_stub_186)@(L(unit)))[@inline] in
let last_originations#504 =
  fun _u#5459 -> ((poly_stub_185)@(L(unit)))[@inline] in
let new_account#506 = fun _u#5463 -> ((poly_stub_184)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#508 =
  fun _n#5467 -> ((poly_stub_183)@(L(unit)))[@inline] in
let register_delegate#510 =
  fun _kh#5471 -> ((poly_stub_182)@(L(unit)))[@inline] in
let register_constant#511 =
  fun _m#5473 -> ((poly_stub_181)@(L(unit)))[@inline] in
let constant_to_michelson_program#513 =
  fun _s#5477 -> ((poly_stub_180)@(L(unit)))[@inline] in
let restore_context#514 =
  fun _u#5479 -> ((poly_stub_179)@(L(unit)))[@inline] in
let save_context#515 = fun _u#5481 -> ((poly_stub_178)@(L(unit)))[@inline] in
let drop_context#516 = fun _u#5483 -> ((poly_stub_177)@(L(unit)))[@inline] in
let set_baker_policy#519 =
  fun _bp#5489 -> ((poly_stub_176)@(L(unit)))[@inline] in
let set_baker#520 = fun _a#5491 -> ((poly_stub_175)@(L(unit)))[@inline] in
let size#521 = fun _c#5493 -> ((poly_stub_174)@(L(unit)))[@inline] in
let read_contract_from_file#523 =
  fun _fn#5497 -> ((poly_stub_173)@(L(unit)))[@inline] in
let chr#524 = fun _n#5499 -> ((poly_stub_172)@(L(unit)))[@inline] in
let nl#525 = L("NEWLINE")[@inline] in
let println#526 = fun _v#5502 -> ((poly_stub_171)@(L(unit)))[@inline] in
let transfer#527 =
  fun _a#5504 ->
  (fun _s#5505 -> (fun _t#5506 -> ((poly_stub_170)@(L(unit)))))[@inline] in
let transfer_exn#528 =
  fun _a#5508 ->
  (fun _s#5509 -> (fun _t#5510 -> ((poly_stub_169)@(L(unit)))))[@inline] in
let reset_state#530 =
  fun _n#5514 -> (fun _l#5515 -> ((poly_stub_168)@(L(unit))))[@inline] in
let reset_state_at#531 =
  fun _t#5517 ->
  (fun _n#5518 -> (fun _l#5519 -> ((poly_stub_167)@(L(unit)))))[@inline] in
let save_mutation#534 =
  fun _s#5528 -> (fun _m#5529 -> ((poly_stub_166)@(L(unit))))[@inline] in
let sign#537 =
  fun _sk#5537 -> (fun _d#5538 -> ((poly_stub_165)@(L(unit))))[@inline] in
let add_account#538 =
  fun _s#5540 -> (fun _k#5541 -> ((poly_stub_164)@(L(unit))))[@inline] in
let baker_account#539 =
  fun _p#5543 -> (fun _o#5544 -> ((poly_stub_163)@(L(unit))))[@inline] in
let create_chest#541 =
  fun _b#5549 -> (fun _n#5550 -> ((poly_stub_162)@(L(unit))))[@inline] in
let create_chest_key#542 =
  fun _c#5552 -> (fun _n#5553 -> ((poly_stub_161)@(L(unit))))[@inline] in
let michelson_equal#545 =
  fun _m1#5563 -> (fun _m2#5564 -> ((poly_stub_160)@(L(unit))))[@inline] in
let originate_contract#547 =
  fun _c#5569 ->
  (fun _s#5570 -> (fun _t#5571 -> ((poly_stub_159)@(L(unit)))))[@inline] in
let compile_contract_from_file#549 =
  fun _fn#5577 ->
  (fun _e#5578 -> (fun _v#5579 -> ((poly_stub_158)@(L(unit)))))[@inline] in
let originate_from_file#550 =
  fun _fn#5581 ->
  (fun _e#5582 ->
   (fun _v#5583 ->
    (fun _s#5584 -> (fun _t#5585 -> ((poly_stub_157)@(L(unit)))))))[@inline] in
let toto#551 = L(44) in
let get_balance#552 =
  fun _u#5588 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#553 =
  fun _u#5590 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#554 = fun _u#5592 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#555 =
  fun _u#5594 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let sender#556 = (get_sender#555)@(L(unit))[@inline] in
let get_source#557 =
  fun _u#5597 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#558 = fun _u#5599 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#559 = fun _u#5601 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#560 =
  fun _u#5603 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#561 =
  fun _u#5605 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#562 =
  fun _u#5607 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#563 =
  fun kh#5609 -> (({ VOTING_POWER })@(kh#5609))[@inline] in
let implicit_account#565 =
  fun kh#5613 -> (IMPLICIT_ACCOUNT(kh#5613))[@inline] in
let pairing_check#569 =
  fun l#5621 -> (({ PAIRING_CHECK })@(l#5621))[@inline] in
let set_delegate#571 = fun o#5625 -> (SET_DELEGATE(o#5625))[@inline] in
let open_chest#577 =
  fun ck#5641 ->
  (fun c#5642 -> (fun n#5643 -> (OPEN_CHEST(ck#5641 , c#5642 , n#5643))))[@inline] in
let xor#580 =
  fun l#5652 -> (fun r#5653 -> (XOR(l#5652 , r#5653)))[@inline] in
let shift_left#581 =
  fun l#5655 -> (fun r#5656 -> (LSL(l#5655 , r#5656)))[@inline] in
let shift_right#582 =
  fun l#5658 -> (fun r#5659 -> (LSR(l#5658 , r#5659)))[@inline] in
let length#623 = fun b#5789 -> (({ SIZE })@(b#5789))[@inline] in
let concat#624 =
  fun b1#5791 ->
  (fun b2#5792 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5791 , b2#5792))))[@inline] in
let sub#625 =
  fun s#5794 ->
  (fun l#5795 ->
   (fun b#5796 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5794 ,
                                                                   l#5795) ,
                                                              b#5796)))))[@inline] in
let length#630 = fun b#5807 -> (({ SIZE })@(b#5807))[@inline] in
let concat#631 =
  fun b1#5809 ->
  (fun b2#5810 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5809 , b2#5810))))[@inline] in
let sub#632 =
  fun s#5812 ->
  (fun l#5813 ->
   (fun b#5814 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5812 ,
                                                                   l#5813) ,
                                                              b#5814)))))[@inline] in
let blake2b#633 = fun b#5816 -> (({ BLAKE2B })@(b#5816))[@inline] in
let sha256#634 = fun b#5818 -> (({ SHA256 })@(b#5818))[@inline] in
let sha512#635 = fun b#5820 -> (({ SHA512 })@(b#5820))[@inline] in
let sha3#636 = fun b#5822 -> (({ SHA3 })@(b#5822))[@inline] in
let keccak#637 = fun b#5824 -> (({ KECCAK })@(b#5824))[@inline] in
let hash_key#638 = fun k#5826 -> (({ HASH_KEY })@(k#5826))[@inline] in
let check#639 =
  fun k#5828 ->
  (fun s#5829 ->
   (fun b#5830 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5828 , s#5829) ,
                                                   b#5830)))))[@inline] in
let assert#640 =
  fun b#5832 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5832))[@inline] in
let abs#643 = fun i#5838 -> (({ ABS })@(i#5838))[@inline] in
let is_nat#644 = fun i#5840 -> (({ ISNAT })@(i#5840))[@inline] in
let true#645 = TRUE()[@inline] in
let false#646 = FALSE()[@inline] in
let unit#647 = UNIT()[@inline] in
let assert_with_error#650 =
  fun b#5848 ->
  (fun s#5849 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5848 , s#5849))))[@inline] in
let poly_stub_156 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_155 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_154 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_153 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_152 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_151 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_150 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_149 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_148 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_147 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_146 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_145 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_144 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_143 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_142 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_141 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_140 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_139 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_138 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_137 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_136 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_135 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_134 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_133 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_132 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_131 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_130 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_129 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_128 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_127 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_126 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_125 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_124 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_123 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_122 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_121 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_120 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_119 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let poly_stub_118 = fun x#5860 -> (({ FAILWITH })@(x#5860))[@inline] in
let get_total_voting_power#658 =
  fun _u#5869 -> ((poly_stub_156)@(L(unit)))[@inline] in
let set_source#661 = fun _a#5875 -> ((poly_stub_155)@(L(unit)))[@inline] in
let get_storage_of_address#662 =
  fun _a#5877 -> ((poly_stub_154)@(L(unit)))[@inline] in
let get_balance#663 = fun _a#5879 -> ((poly_stub_153)@(L(unit)))[@inline] in
let print#664 = fun _v#5881 -> ((poly_stub_152)@(L(unit)))[@inline] in
let eprint#665 = fun _v#5883 -> ((poly_stub_151)@(L(unit)))[@inline] in
let get_voting_power#666 =
  fun _kh#5885 -> ((poly_stub_150)@(L(unit)))[@inline] in
let nth_bootstrap_contract#667 =
  fun _i#5887 -> ((poly_stub_149)@(L(unit)))[@inline] in
let nth_bootstrap_account#668 =
  fun _i#5889 -> ((poly_stub_148)@(L(unit)))[@inline] in
let get_bootstrap_account#669 =
  fun _n#5891 -> ((poly_stub_147)@(L(unit)))[@inline] in
let last_originations#671 =
  fun _u#5895 -> ((poly_stub_146)@(L(unit)))[@inline] in
let new_account#673 = fun _u#5899 -> ((poly_stub_145)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#675 =
  fun _n#5903 -> ((poly_stub_144)@(L(unit)))[@inline] in
let register_delegate#677 =
  fun _kh#5907 -> ((poly_stub_143)@(L(unit)))[@inline] in
let register_constant#678 =
  fun _m#5909 -> ((poly_stub_142)@(L(unit)))[@inline] in
let constant_to_michelson_program#680 =
  fun _s#5913 -> ((poly_stub_141)@(L(unit)))[@inline] in
let restore_context#681 =
  fun _u#5915 -> ((poly_stub_140)@(L(unit)))[@inline] in
let save_context#682 = fun _u#5917 -> ((poly_stub_139)@(L(unit)))[@inline] in
let drop_context#683 = fun _u#5919 -> ((poly_stub_138)@(L(unit)))[@inline] in
let set_baker_policy#686 =
  fun _bp#5925 -> ((poly_stub_137)@(L(unit)))[@inline] in
let set_baker#687 = fun _a#5927 -> ((poly_stub_136)@(L(unit)))[@inline] in
let size#688 = fun _c#5929 -> ((poly_stub_135)@(L(unit)))[@inline] in
let read_contract_from_file#690 =
  fun _fn#5933 -> ((poly_stub_134)@(L(unit)))[@inline] in
let chr#691 = fun _n#5935 -> ((poly_stub_133)@(L(unit)))[@inline] in
let nl#692 = L("NEWLINE")[@inline] in
let println#693 = fun _v#5938 -> ((poly_stub_132)@(L(unit)))[@inline] in
let transfer#694 =
  fun _a#5940 ->
  (fun _s#5941 -> (fun _t#5942 -> ((poly_stub_131)@(L(unit)))))[@inline] in
let transfer_exn#695 =
  fun _a#5944 ->
  (fun _s#5945 -> (fun _t#5946 -> ((poly_stub_130)@(L(unit)))))[@inline] in
let reset_state#697 =
  fun _n#5950 -> (fun _l#5951 -> ((poly_stub_129)@(L(unit))))[@inline] in
let reset_state_at#698 =
  fun _t#5953 ->
  (fun _n#5954 -> (fun _l#5955 -> ((poly_stub_128)@(L(unit)))))[@inline] in
let save_mutation#701 =
  fun _s#5964 -> (fun _m#5965 -> ((poly_stub_127)@(L(unit))))[@inline] in
let sign#704 =
  fun _sk#5973 -> (fun _d#5974 -> ((poly_stub_126)@(L(unit))))[@inline] in
let add_account#705 =
  fun _s#5976 -> (fun _k#5977 -> ((poly_stub_125)@(L(unit))))[@inline] in
let baker_account#706 =
  fun _p#5979 -> (fun _o#5980 -> ((poly_stub_124)@(L(unit))))[@inline] in
let create_chest#708 =
  fun _b#5985 -> (fun _n#5986 -> ((poly_stub_123)@(L(unit))))[@inline] in
let create_chest_key#709 =
  fun _c#5988 -> (fun _n#5989 -> ((poly_stub_122)@(L(unit))))[@inline] in
let michelson_equal#712 =
  fun _m1#5999 -> (fun _m2#6000 -> ((poly_stub_121)@(L(unit))))[@inline] in
let originate_contract#714 =
  fun _c#6005 ->
  (fun _s#6006 -> (fun _t#6007 -> ((poly_stub_120)@(L(unit)))))[@inline] in
let compile_contract_from_file#716 =
  fun _fn#6013 ->
  (fun _e#6014 -> (fun _v#6015 -> ((poly_stub_119)@(L(unit)))))[@inline] in
let originate_from_file#717 =
  fun _fn#6017 ->
  (fun _e#6018 ->
   (fun _v#6019 ->
    (fun _s#6020 -> (fun _t#6021 -> ((poly_stub_118)@(L(unit)))))))[@inline] in
let toto#718 = L(43) in
let get_balance#719 =
  fun _u#6024 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#720 =
  fun _u#6026 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#721 = fun _u#6028 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#722 =
  fun _u#6030 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let sender#723 = (get_sender#722)@(L(unit))[@inline] in
let get_source#724 =
  fun _u#6033 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#725 = fun _u#6035 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#726 = fun _u#6037 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#727 =
  fun _u#6039 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#728 =
  fun _u#6041 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#729 =
  fun _u#6043 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#730 =
  fun kh#6045 -> (({ VOTING_POWER })@(kh#6045))[@inline] in
let implicit_account#732 =
  fun kh#6049 -> (IMPLICIT_ACCOUNT(kh#6049))[@inline] in
let pairing_check#736 =
  fun l#6057 -> (({ PAIRING_CHECK })@(l#6057))[@inline] in
let set_delegate#738 = fun o#6061 -> (SET_DELEGATE(o#6061))[@inline] in
let open_chest#744 =
  fun ck#6077 ->
  (fun c#6078 -> (fun n#6079 -> (OPEN_CHEST(ck#6077 , c#6078 , n#6079))))[@inline] in
let xor#747 =
  fun l#6088 -> (fun r#6089 -> (XOR(l#6088 , r#6089)))[@inline] in
let shift_left#748 =
  fun l#6091 -> (fun r#6092 -> (LSL(l#6091 , r#6092)))[@inline] in
let shift_right#749 =
  fun l#6094 -> (fun r#6095 -> (LSR(l#6094 , r#6095)))[@inline] in
let length#790 = fun b#6225 -> (({ SIZE })@(b#6225))[@inline] in
let concat#791 =
  fun b1#6227 ->
  (fun b2#6228 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6227 , b2#6228))))[@inline] in
let sub#792 =
  fun s#6230 ->
  (fun l#6231 ->
   (fun b#6232 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6230 ,
                                                                   l#6231) ,
                                                              b#6232)))))[@inline] in
let length#797 = fun b#6243 -> (({ SIZE })@(b#6243))[@inline] in
let concat#798 =
  fun b1#6245 ->
  (fun b2#6246 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6245 , b2#6246))))[@inline] in
let sub#799 =
  fun s#6248 ->
  (fun l#6249 ->
   (fun b#6250 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6248 ,
                                                                   l#6249) ,
                                                              b#6250)))))[@inline] in
let blake2b#800 = fun b#6252 -> (({ BLAKE2B })@(b#6252))[@inline] in
let sha256#801 = fun b#6254 -> (({ SHA256 })@(b#6254))[@inline] in
let sha512#802 = fun b#6256 -> (({ SHA512 })@(b#6256))[@inline] in
let sha3#803 = fun b#6258 -> (({ SHA3 })@(b#6258))[@inline] in
let keccak#804 = fun b#6260 -> (({ KECCAK })@(b#6260))[@inline] in
let hash_key#805 = fun k#6262 -> (({ HASH_KEY })@(k#6262))[@inline] in
let check#806 =
  fun k#6264 ->
  (fun s#6265 ->
   (fun b#6266 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6264 , s#6265) ,
                                                   b#6266)))))[@inline] in
let assert#807 =
  fun b#6268 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6268))[@inline] in
let abs#810 = fun i#6274 -> (({ ABS })@(i#6274))[@inline] in
let is_nat#811 = fun i#6276 -> (({ ISNAT })@(i#6276))[@inline] in
let true#812 = TRUE()[@inline] in
let false#813 = FALSE()[@inline] in
let unit#814 = UNIT()[@inline] in
let assert_with_error#817 =
  fun b#6284 ->
  (fun s#6285 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6284 , s#6285))))[@inline] in
let poly_stub_117 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_116 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_115 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_114 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_113 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_112 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_111 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_110 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_109 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_108 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_107 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_106 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_105 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_104 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_103 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_102 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_101 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_100 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_99 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_98 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_97 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_96 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_95 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_94 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_93 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_92 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_91 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_90 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_89 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_88 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_87 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_86 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_85 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_84 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_83 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_82 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_81 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_80 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let poly_stub_79 = fun x#6296 -> (({ FAILWITH })@(x#6296))[@inline] in
let get_total_voting_power#825 =
  fun _u#6305 -> ((poly_stub_117)@(L(unit)))[@inline] in
let set_source#828 = fun _a#6311 -> ((poly_stub_116)@(L(unit)))[@inline] in
let get_storage_of_address#829 =
  fun _a#6313 -> ((poly_stub_115)@(L(unit)))[@inline] in
let get_balance#830 = fun _a#6315 -> ((poly_stub_114)@(L(unit)))[@inline] in
let print#831 = fun _v#6317 -> ((poly_stub_113)@(L(unit)))[@inline] in
let eprint#832 = fun _v#6319 -> ((poly_stub_112)@(L(unit)))[@inline] in
let get_voting_power#833 =
  fun _kh#6321 -> ((poly_stub_111)@(L(unit)))[@inline] in
let nth_bootstrap_contract#834 =
  fun _i#6323 -> ((poly_stub_110)@(L(unit)))[@inline] in
let nth_bootstrap_account#835 =
  fun _i#6325 -> ((poly_stub_109)@(L(unit)))[@inline] in
let get_bootstrap_account#836 =
  fun _n#6327 -> ((poly_stub_108)@(L(unit)))[@inline] in
let last_originations#838 =
  fun _u#6331 -> ((poly_stub_107)@(L(unit)))[@inline] in
let new_account#840 = fun _u#6335 -> ((poly_stub_106)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#842 =
  fun _n#6339 -> ((poly_stub_105)@(L(unit)))[@inline] in
let register_delegate#844 =
  fun _kh#6343 -> ((poly_stub_104)@(L(unit)))[@inline] in
let register_constant#845 =
  fun _m#6345 -> ((poly_stub_103)@(L(unit)))[@inline] in
let constant_to_michelson_program#847 =
  fun _s#6349 -> ((poly_stub_102)@(L(unit)))[@inline] in
let restore_context#848 =
  fun _u#6351 -> ((poly_stub_101)@(L(unit)))[@inline] in
let save_context#849 = fun _u#6353 -> ((poly_stub_100)@(L(unit)))[@inline] in
let drop_context#850 = fun _u#6355 -> ((poly_stub_99)@(L(unit)))[@inline] in
let set_baker_policy#853 =
  fun _bp#6361 -> ((poly_stub_98)@(L(unit)))[@inline] in
let set_baker#854 = fun _a#6363 -> ((poly_stub_97)@(L(unit)))[@inline] in
let size#855 = fun _c#6365 -> ((poly_stub_96)@(L(unit)))[@inline] in
let read_contract_from_file#857 =
  fun _fn#6369 -> ((poly_stub_95)@(L(unit)))[@inline] in
let chr#858 = fun _n#6371 -> ((poly_stub_94)@(L(unit)))[@inline] in
let nl#859 = L("NEWLINE")[@inline] in
let println#860 = fun _v#6374 -> ((poly_stub_93)@(L(unit)))[@inline] in
let transfer#861 =
  fun _a#6376 -> (fun _s#6377 -> (fun _t#6378 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#862 =
  fun _a#6380 -> (fun _s#6381 -> (fun _t#6382 -> ((poly_stub_91)@(L(unit)))))[@inline] in
let reset_state#864 =
  fun _n#6386 -> (fun _l#6387 -> ((poly_stub_90)@(L(unit))))[@inline] in
let reset_state_at#865 =
  fun _t#6389 -> (fun _n#6390 -> (fun _l#6391 -> ((poly_stub_89)@(L(unit)))))[@inline] in
let save_mutation#868 =
  fun _s#6400 -> (fun _m#6401 -> ((poly_stub_88)@(L(unit))))[@inline] in
let sign#871 =
  fun _sk#6409 -> (fun _d#6410 -> ((poly_stub_87)@(L(unit))))[@inline] in
let add_account#872 =
  fun _s#6412 -> (fun _k#6413 -> ((poly_stub_86)@(L(unit))))[@inline] in
let baker_account#873 =
  fun _p#6415 -> (fun _o#6416 -> ((poly_stub_85)@(L(unit))))[@inline] in
let create_chest#875 =
  fun _b#6421 -> (fun _n#6422 -> ((poly_stub_84)@(L(unit))))[@inline] in
let create_chest_key#876 =
  fun _c#6424 -> (fun _n#6425 -> ((poly_stub_83)@(L(unit))))[@inline] in
let michelson_equal#879 =
  fun _m1#6435 -> (fun _m2#6436 -> ((poly_stub_82)@(L(unit))))[@inline] in
let originate_contract#881 =
  fun _c#6441 -> (fun _s#6442 -> (fun _t#6443 -> ((poly_stub_81)@(L(unit)))))[@inline] in
let compile_contract_from_file#883 =
  fun _fn#6449 ->
  (fun _e#6450 -> (fun _v#6451 -> ((poly_stub_80)@(L(unit)))))[@inline] in
let originate_from_file#884 =
  fun _fn#6453 ->
  (fun _e#6454 ->
   (fun _v#6455 ->
    (fun _s#6456 -> (fun _t#6457 -> ((poly_stub_79)@(L(unit)))))))[@inline] in
let tata#885 = ADD(toto#215 , titi#383) in
let foo#886 = (f#384)@(PAIR(L(unit) , L(3))) in
let get_balance#887 =
  fun _u#6461 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#888 =
  fun _u#6463 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#889 = fun _u#6465 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#890 =
  fun _u#6467 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let sender#891 = (get_sender#890)@(L(unit))[@inline] in
let get_source#892 =
  fun _u#6470 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#893 = fun _u#6472 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#894 = fun _u#6474 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#895 =
  fun _u#6476 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#896 =
  fun _u#6478 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#897 =
  fun _u#6480 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#898 =
  fun kh#6482 -> (({ VOTING_POWER })@(kh#6482))[@inline] in
let implicit_account#900 =
  fun kh#6486 -> (IMPLICIT_ACCOUNT(kh#6486))[@inline] in
let pairing_check#904 =
  fun l#6494 -> (({ PAIRING_CHECK })@(l#6494))[@inline] in
let set_delegate#906 = fun o#6498 -> (SET_DELEGATE(o#6498))[@inline] in
let open_chest#912 =
  fun ck#6514 ->
  (fun c#6515 -> (fun n#6516 -> (OPEN_CHEST(ck#6514 , c#6515 , n#6516))))[@inline] in
let xor#915 =
  fun l#6525 -> (fun r#6526 -> (XOR(l#6525 , r#6526)))[@inline] in
let shift_left#916 =
  fun l#6528 -> (fun r#6529 -> (LSL(l#6528 , r#6529)))[@inline] in
let shift_right#917 =
  fun l#6531 -> (fun r#6532 -> (LSR(l#6531 , r#6532)))[@inline] in
let length#958 = fun b#6662 -> (({ SIZE })@(b#6662))[@inline] in
let concat#959 =
  fun b1#6664 ->
  (fun b2#6665 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6664 , b2#6665))))[@inline] in
let sub#960 =
  fun s#6667 ->
  (fun l#6668 ->
   (fun b#6669 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6667 ,
                                                                   l#6668) ,
                                                              b#6669)))))[@inline] in
let length#965 = fun b#6680 -> (({ SIZE })@(b#6680))[@inline] in
let concat#966 =
  fun b1#6682 ->
  (fun b2#6683 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6682 , b2#6683))))[@inline] in
let sub#967 =
  fun s#6685 ->
  (fun l#6686 ->
   (fun b#6687 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6685 ,
                                                                   l#6686) ,
                                                              b#6687)))))[@inline] in
let blake2b#968 = fun b#6689 -> (({ BLAKE2B })@(b#6689))[@inline] in
let sha256#969 = fun b#6691 -> (({ SHA256 })@(b#6691))[@inline] in
let sha512#970 = fun b#6693 -> (({ SHA512 })@(b#6693))[@inline] in
let sha3#971 = fun b#6695 -> (({ SHA3 })@(b#6695))[@inline] in
let keccak#972 = fun b#6697 -> (({ KECCAK })@(b#6697))[@inline] in
let hash_key#973 = fun k#6699 -> (({ HASH_KEY })@(k#6699))[@inline] in
let check#974 =
  fun k#6701 ->
  (fun s#6702 ->
   (fun b#6703 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6701 , s#6702) ,
                                                   b#6703)))))[@inline] in
let assert#975 =
  fun b#6705 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6705))[@inline] in
let abs#978 = fun i#6711 -> (({ ABS })@(i#6711))[@inline] in
let is_nat#979 = fun i#6713 -> (({ ISNAT })@(i#6713))[@inline] in
let true#980 = TRUE()[@inline] in
let false#981 = FALSE()[@inline] in
let unit#982 = UNIT()[@inline] in
let assert_with_error#985 =
  fun b#6721 ->
  (fun s#6722 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6721 , s#6722))))[@inline] in
let poly_stub_78 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_77 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_76 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_75 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_74 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_73 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_72 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_71 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_70 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_69 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_68 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_67 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_66 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_65 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_64 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_63 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_62 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_61 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_60 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_59 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_58 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_57 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_56 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_55 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_54 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_53 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_52 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_51 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_50 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_49 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_48 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_47 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_46 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_45 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_44 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_43 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_42 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_41 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let poly_stub_40 = fun x#6733 -> (({ FAILWITH })@(x#6733))[@inline] in
let get_total_voting_power#993 =
  fun _u#6742 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#996 = fun _a#6748 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#997 =
  fun _a#6750 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#998 = fun _a#6752 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#999 = fun _v#6754 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#1000 = fun _v#6756 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#1001 =
  fun _kh#6758 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1002 =
  fun _i#6760 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#1003 =
  fun _i#6762 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#1004 =
  fun _n#6764 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#1006 =
  fun _u#6768 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#1008 = fun _u#6772 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1010 =
  fun _n#6776 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#1012 =
  fun _kh#6780 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#1013 =
  fun _m#6782 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#1015 =
  fun _s#6786 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#1016 =
  fun _u#6788 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#1017 = fun _u#6790 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#1018 = fun _u#6792 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#1021 =
  fun _bp#6798 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#1022 = fun _a#6800 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#1023 = fun _c#6802 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#1025 =
  fun _fn#6806 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#1026 = fun _n#6808 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#1027 = L("NEWLINE")[@inline] in
let println#1028 = fun _v#6811 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#1029 =
  fun _a#6813 -> (fun _s#6814 -> (fun _t#6815 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#1030 =
  fun _a#6817 -> (fun _s#6818 -> (fun _t#6819 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#1032 =
  fun _n#6823 -> (fun _l#6824 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#1033 =
  fun _t#6826 -> (fun _n#6827 -> (fun _l#6828 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#1036 =
  fun _s#6837 -> (fun _m#6838 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#1039 =
  fun _sk#6846 -> (fun _d#6847 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#1040 =
  fun _s#6849 -> (fun _k#6850 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#1041 =
  fun _p#6852 -> (fun _o#6853 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#1043 =
  fun _b#6858 -> (fun _n#6859 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#1044 =
  fun _c#6861 -> (fun _n#6862 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#1047 =
  fun _m1#6872 -> (fun _m2#6873 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#1049 =
  fun _c#6878 -> (fun _s#6879 -> (fun _t#6880 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#1051 =
  fun _fn#6886 ->
  (fun _e#6887 -> (fun _v#6888 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#1052 =
  fun _fn#6890 ->
  (fun _e#6891 ->
   (fun _v#6892 ->
    (fun _s#6893 -> (fun _t#6894 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let toto#1053 = L(10) in
let foo#1054 = L("bar") in
let get_balance#1055 =
  fun _u#6898 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1056 =
  fun _u#6900 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1057 = fun _u#6902 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1058 =
  fun _u#6904 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let sender#1059 = (get_sender#1058)@(L(unit))[@inline] in
let get_source#1060 =
  fun _u#6907 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1061 =
  fun _u#6909 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1062 = fun _u#6911 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1063 =
  fun _u#6913 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1064 =
  fun _u#6915 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1065 =
  fun _u#6917 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1066 =
  fun kh#6919 -> (({ VOTING_POWER })@(kh#6919))[@inline] in
let implicit_account#1068 =
  fun kh#6923 -> (IMPLICIT_ACCOUNT(kh#6923))[@inline] in
let pairing_check#1072 =
  fun l#6931 -> (({ PAIRING_CHECK })@(l#6931))[@inline] in
let set_delegate#1074 = fun o#6935 -> (SET_DELEGATE(o#6935))[@inline] in
let open_chest#1080 =
  fun ck#6951 ->
  (fun c#6952 -> (fun n#6953 -> (OPEN_CHEST(ck#6951 , c#6952 , n#6953))))[@inline] in
let xor#1083 =
  fun l#6962 -> (fun r#6963 -> (XOR(l#6962 , r#6963)))[@inline] in
let shift_left#1084 =
  fun l#6965 -> (fun r#6966 -> (LSL(l#6965 , r#6966)))[@inline] in
let shift_right#1085 =
  fun l#6968 -> (fun r#6969 -> (LSR(l#6968 , r#6969)))[@inline] in
let length#1126 = fun b#7099 -> (({ SIZE })@(b#7099))[@inline] in
let concat#1127 =
  fun b1#7101 ->
  (fun b2#7102 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7101 , b2#7102))))[@inline] in
let sub#1128 =
  fun s#7104 ->
  (fun l#7105 ->
   (fun b#7106 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7104 ,
                                                                   l#7105) ,
                                                              b#7106)))))[@inline] in
let length#1133 = fun b#7117 -> (({ SIZE })@(b#7117))[@inline] in
let concat#1134 =
  fun b1#7119 ->
  (fun b2#7120 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7119 , b2#7120))))[@inline] in
let sub#1135 =
  fun s#7122 ->
  (fun l#7123 ->
   (fun b#7124 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7122 ,
                                                                   l#7123) ,
                                                              b#7124)))))[@inline] in
let blake2b#1136 = fun b#7126 -> (({ BLAKE2B })@(b#7126))[@inline] in
let sha256#1137 = fun b#7128 -> (({ SHA256 })@(b#7128))[@inline] in
let sha512#1138 = fun b#7130 -> (({ SHA512 })@(b#7130))[@inline] in
let sha3#1139 = fun b#7132 -> (({ SHA3 })@(b#7132))[@inline] in
let keccak#1140 = fun b#7134 -> (({ KECCAK })@(b#7134))[@inline] in
let hash_key#1141 = fun k#7136 -> (({ HASH_KEY })@(k#7136))[@inline] in
let check#1142 =
  fun k#7138 ->
  (fun s#7139 ->
   (fun b#7140 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7138 , s#7139) ,
                                                   b#7140)))))[@inline] in
let assert =
  fun b#7142 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7142))[@inline] in
let abs = fun i#7148 -> (({ ABS })@(i#7148))[@inline] in
let is_nat = fun i#7150 -> (({ ISNAT })@(i#7150))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#7158 ->
  (fun s#7159 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7158 , s#7159))))[@inline] in
let poly_stub_39 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_38 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_37 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_36 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_35 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_34 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_33 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_32 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_31 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_30 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_29 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_28 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_27 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_26 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_25 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_24 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_23 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_22 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_21 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_20 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_19 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_18 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_17 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_16 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_15 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_14 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_13 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_12 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_11 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_10 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_9 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_8 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_7 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_6 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_5 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_4 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_3 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_2 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let poly_stub_1 = fun x#7170 -> (({ FAILWITH })@(x#7170))[@inline] in
let get_total_voting_power#1147 =
  fun _u#7179 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#1150 = fun _a#7185 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#1151 =
  fun _a#7187 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#1152 = fun _a#7189 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#1153 = fun _v#7191 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#1154 = fun _v#7193 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#1155 =
  fun _kh#7195 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1156 =
  fun _i#7197 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#1157 =
  fun _i#7199 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#1158 =
  fun _n#7201 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#1160 =
  fun _u#7205 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#1162 = fun _u#7209 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1164 =
  fun _n#7213 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#1166 =
  fun _kh#7217 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#1167 =
  fun _m#7219 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#1169 =
  fun _s#7223 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#1170 =
  fun _u#7225 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#1171 = fun _u#7227 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#1172 = fun _u#7229 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#1175 =
  fun _bp#7235 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#1176 = fun _a#7237 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#1177 = fun _c#7239 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#1179 =
  fun _fn#7243 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1180 = fun _n#7245 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#1181 = L("NEWLINE")[@inline] in
let println#1182 = fun _v#7248 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#1183 =
  fun _a#7250 -> (fun _s#7251 -> (fun _t#7252 -> ((poly_stub_14)@(L(unit)))))[@inline] in
let transfer_exn#1184 =
  fun _a#7254 -> (fun _s#7255 -> (fun _t#7256 -> ((poly_stub_13)@(L(unit)))))[@inline] in
let reset_state#1186 =
  fun _n#7260 -> (fun _l#7261 -> ((poly_stub_12)@(L(unit))))[@inline] in
let reset_state_at#1187 =
  fun _t#7263 -> (fun _n#7264 -> (fun _l#7265 -> ((poly_stub_11)@(L(unit)))))[@inline] in
let save_mutation#1190 =
  fun _s#7274 -> (fun _m#7275 -> ((poly_stub_10)@(L(unit))))[@inline] in
let sign#1193 =
  fun _sk#7283 -> (fun _d#7284 -> ((poly_stub_9)@(L(unit))))[@inline] in
let add_account#1194 =
  fun _s#7286 -> (fun _k#7287 -> ((poly_stub_8)@(L(unit))))[@inline] in
let baker_account#1195 =
  fun _p#7289 -> (fun _o#7290 -> ((poly_stub_7)@(L(unit))))[@inline] in
let create_chest#1197 =
  fun _b#7295 -> (fun _n#7296 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#1198 =
  fun _c#7298 -> (fun _n#7299 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#1201 =
  fun _m1#7309 -> (fun _m2#7310 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#1203 =
  fun _c#7315 -> (fun _s#7316 -> (fun _t#7317 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#1205 =
  fun _fn#7323 -> (fun _e#7324 -> (fun _v#7325 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#1206 =
  fun _fn#7327 ->
  (fun _e#7328 ->
   (fun _v#7329 ->
    (fun _s#7330 -> (fun _t#7331 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1053 , toto#215) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#7335 ->
  (let (gen#7341, gen#7342) = gen#7335 in
   let p#7336 = gen#7341 in
   let s#7337 = gen#7342 in
   let s#7338 = ADD(ADD(p#7336 , s#7337) , toto) in
   PAIR(LIST_EMPTY() , s#7338)) in
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
