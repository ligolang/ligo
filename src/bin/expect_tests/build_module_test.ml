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
  fun _u#4288 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#50 =
  fun _u#4290 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#51 = fun _u#4292 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#52 =
  fun _u#4294 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#53 =
  fun _u#4296 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#54 = fun _u#4298 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#55 = fun _u#4300 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#56 =
  fun _u#4302 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#57 =
  fun _u#4304 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#58 =
  fun _u#4306 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#59 =
  fun kh#4308 -> (({ VOTING_POWER })@(kh#4308))[@inline] in
let implicit_account#61 =
  fun kh#4312 -> (IMPLICIT_ACCOUNT(kh#4312))[@inline] in
let pairing_check#65 =
  fun l#4320 -> (({ PAIRING_CHECK })@(l#4320))[@inline] in
let set_delegate#67 = fun o#4324 -> (SET_DELEGATE(o#4324))[@inline] in
let open_chest#73 =
  fun ck#4340 ->
  (fun c#4341 -> (fun n#4342 -> (OPEN_CHEST(ck#4340 , c#4341 , n#4342))))[@inline] in
let xor#76 = fun l#4351 -> (fun r#4352 -> (XOR(l#4351 , r#4352)))[@inline] in
let shift_left#77 =
  fun l#4354 -> (fun r#4355 -> (LSL(l#4354 , r#4355)))[@inline] in
let shift_right#78 =
  fun l#4357 -> (fun r#4358 -> (LSR(l#4357 , r#4358)))[@inline] in
let length#119 = fun b#4488 -> (({ SIZE })@(b#4488))[@inline] in
let concat#120 =
  fun b1#4490 ->
  (fun b2#4491 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4490 , b2#4491))))[@inline] in
let sub#121 =
  fun s#4493 ->
  (fun l#4494 ->
   (fun b#4495 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4493 ,
                                                                   l#4494) ,
                                                              b#4495)))))[@inline] in
let length#127 = fun b#4509 -> (({ SIZE })@(b#4509))[@inline] in
let concat#128 =
  fun b1#4511 ->
  (fun b2#4512 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4511 , b2#4512))))[@inline] in
let sub#129 =
  fun s#4514 ->
  (fun l#4515 ->
   (fun b#4516 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4514 ,
                                                                   l#4515) ,
                                                              b#4516)))))[@inline] in
let blake2b#130 = fun b#4518 -> (({ BLAKE2B })@(b#4518))[@inline] in
let sha256#131 = fun b#4520 -> (({ SHA256 })@(b#4520))[@inline] in
let sha512#132 = fun b#4522 -> (({ SHA512 })@(b#4522))[@inline] in
let sha3#133 = fun b#4524 -> (({ SHA3 })@(b#4524))[@inline] in
let keccak#134 = fun b#4526 -> (({ KECCAK })@(b#4526))[@inline] in
let hash_key#135 = fun k#4528 -> (({ HASH_KEY })@(k#4528))[@inline] in
let check#136 =
  fun k#4530 ->
  (fun s#4531 ->
   (fun b#4532 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4530 , s#4531) ,
                                                   b#4532)))))[@inline] in
let assert#137 =
  fun b#4534 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4534))[@inline] in
let abs#140 = fun i#4540 -> (({ ABS })@(i#4540))[@inline] in
let is_nat#141 = fun i#4542 -> (({ ISNAT })@(i#4542))[@inline] in
let true#142 = TRUE()[@inline] in
let false#143 = FALSE()[@inline] in
let unit#144 = UNIT()[@inline] in
let assert_with_error#147 =
  fun b#4550 ->
  (fun s#4551 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4550 , s#4551))))[@inline] in
let poly_stub_273 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_272 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_271 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_270 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_269 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_268 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_267 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_266 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_265 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_264 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_263 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_262 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_261 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_260 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_259 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_258 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_257 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_256 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_255 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_254 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_253 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_252 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_251 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_250 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_249 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_248 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_247 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_246 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_245 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_244 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_243 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_242 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_241 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_240 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_239 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_238 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_237 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_236 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let poly_stub_235 = fun x#4562 -> (({ FAILWITH })@(x#4562))[@inline] in
let get_total_voting_power#155 =
  fun _u#4571 -> ((poly_stub_273)@(L(unit)))[@inline] in
let set_source#158 = fun _a#4577 -> ((poly_stub_272)@(L(unit)))[@inline] in
let get_storage_of_address#159 =
  fun _a#4579 -> ((poly_stub_271)@(L(unit)))[@inline] in
let get_balance#160 = fun _a#4581 -> ((poly_stub_270)@(L(unit)))[@inline] in
let print#161 = fun _v#4583 -> ((poly_stub_269)@(L(unit)))[@inline] in
let eprint#162 = fun _v#4585 -> ((poly_stub_268)@(L(unit)))[@inline] in
let get_voting_power#163 =
  fun _kh#4587 -> ((poly_stub_267)@(L(unit)))[@inline] in
let nth_bootstrap_contract#164 =
  fun _i#4589 -> ((poly_stub_266)@(L(unit)))[@inline] in
let nth_bootstrap_account#165 =
  fun _i#4591 -> ((poly_stub_265)@(L(unit)))[@inline] in
let get_bootstrap_account#166 =
  fun _n#4593 -> ((poly_stub_264)@(L(unit)))[@inline] in
let last_originations#168 =
  fun _u#4597 -> ((poly_stub_263)@(L(unit)))[@inline] in
let new_account#170 = fun _u#4601 -> ((poly_stub_262)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#172 =
  fun _n#4605 -> ((poly_stub_261)@(L(unit)))[@inline] in
let register_delegate#174 =
  fun _kh#4609 -> ((poly_stub_260)@(L(unit)))[@inline] in
let register_constant#175 =
  fun _m#4611 -> ((poly_stub_259)@(L(unit)))[@inline] in
let constant_to_michelson_program#177 =
  fun _s#4615 -> ((poly_stub_258)@(L(unit)))[@inline] in
let restore_context#178 =
  fun _u#4617 -> ((poly_stub_257)@(L(unit)))[@inline] in
let save_context#179 = fun _u#4619 -> ((poly_stub_256)@(L(unit)))[@inline] in
let drop_context#180 = fun _u#4621 -> ((poly_stub_255)@(L(unit)))[@inline] in
let set_baker_policy#183 =
  fun _bp#4627 -> ((poly_stub_254)@(L(unit)))[@inline] in
let set_baker#184 = fun _a#4629 -> ((poly_stub_253)@(L(unit)))[@inline] in
let size#185 = fun _c#4631 -> ((poly_stub_252)@(L(unit)))[@inline] in
let read_contract_from_file#187 =
  fun _fn#4635 -> ((poly_stub_251)@(L(unit)))[@inline] in
let chr#188 = fun _n#4637 -> ((poly_stub_250)@(L(unit)))[@inline] in
let nl#189 = L("NEWLINE")[@inline] in
let println#190 = fun _v#4640 -> ((poly_stub_249)@(L(unit)))[@inline] in
let transfer#191 =
  fun _a#4642 ->
  (fun _s#4643 -> (fun _t#4644 -> ((poly_stub_248)@(L(unit)))))[@inline] in
let transfer_exn#192 =
  fun _a#4646 ->
  (fun _s#4647 -> (fun _t#4648 -> ((poly_stub_247)@(L(unit)))))[@inline] in
let reset_state#194 =
  fun _n#4652 -> (fun _l#4653 -> ((poly_stub_246)@(L(unit))))[@inline] in
let reset_state_at#195 =
  fun _t#4655 ->
  (fun _n#4656 -> (fun _l#4657 -> ((poly_stub_245)@(L(unit)))))[@inline] in
let save_mutation#198 =
  fun _s#4666 -> (fun _m#4667 -> ((poly_stub_244)@(L(unit))))[@inline] in
let sign#201 =
  fun _sk#4675 -> (fun _d#4676 -> ((poly_stub_243)@(L(unit))))[@inline] in
let add_account#202 =
  fun _s#4678 -> (fun _k#4679 -> ((poly_stub_242)@(L(unit))))[@inline] in
let baker_account#203 =
  fun _p#4681 -> (fun _o#4682 -> ((poly_stub_241)@(L(unit))))[@inline] in
let create_chest#205 =
  fun _b#4687 -> (fun _n#4688 -> ((poly_stub_240)@(L(unit))))[@inline] in
let create_chest_key#206 =
  fun _c#4690 -> (fun _n#4691 -> ((poly_stub_239)@(L(unit))))[@inline] in
let michelson_equal#209 =
  fun _m1#4701 -> (fun _m2#4702 -> ((poly_stub_238)@(L(unit))))[@inline] in
let originate_contract#211 =
  fun _c#4707 ->
  (fun _s#4708 -> (fun _t#4709 -> ((poly_stub_237)@(L(unit)))))[@inline] in
let compile_contract_from_file#213 =
  fun _fn#4715 ->
  (fun _e#4716 -> (fun _v#4717 -> ((poly_stub_236)@(L(unit)))))[@inline] in
let originate_from_file#214 =
  fun _fn#4719 ->
  (fun _e#4720 ->
   (fun _v#4721 ->
    (fun _s#4722 -> (fun _t#4723 -> ((poly_stub_235)@(L(unit)))))))[@inline] in
let toto#215 = L(1) in
let get_balance#216 =
  fun _u#4726 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#217 =
  fun _u#4728 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#218 = fun _u#4730 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#219 =
  fun _u#4732 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#220 =
  fun _u#4734 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#221 = fun _u#4736 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#222 = fun _u#4738 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#223 =
  fun _u#4740 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#224 =
  fun _u#4742 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#225 =
  fun _u#4744 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#226 =
  fun kh#4746 -> (({ VOTING_POWER })@(kh#4746))[@inline] in
let implicit_account#228 =
  fun kh#4750 -> (IMPLICIT_ACCOUNT(kh#4750))[@inline] in
let pairing_check#232 =
  fun l#4758 -> (({ PAIRING_CHECK })@(l#4758))[@inline] in
let set_delegate#234 = fun o#4762 -> (SET_DELEGATE(o#4762))[@inline] in
let open_chest#240 =
  fun ck#4778 ->
  (fun c#4779 -> (fun n#4780 -> (OPEN_CHEST(ck#4778 , c#4779 , n#4780))))[@inline] in
let xor#243 =
  fun l#4789 -> (fun r#4790 -> (XOR(l#4789 , r#4790)))[@inline] in
let shift_left#244 =
  fun l#4792 -> (fun r#4793 -> (LSL(l#4792 , r#4793)))[@inline] in
let shift_right#245 =
  fun l#4795 -> (fun r#4796 -> (LSR(l#4795 , r#4796)))[@inline] in
let length#286 = fun b#4926 -> (({ SIZE })@(b#4926))[@inline] in
let concat#287 =
  fun b1#4928 ->
  (fun b2#4929 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4928 , b2#4929))))[@inline] in
let sub#288 =
  fun s#4931 ->
  (fun l#4932 ->
   (fun b#4933 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4931 ,
                                                                   l#4932) ,
                                                              b#4933)))))[@inline] in
let length#294 = fun b#4947 -> (({ SIZE })@(b#4947))[@inline] in
let concat#295 =
  fun b1#4949 ->
  (fun b2#4950 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4949 , b2#4950))))[@inline] in
let sub#296 =
  fun s#4952 ->
  (fun l#4953 ->
   (fun b#4954 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4952 ,
                                                                   l#4953) ,
                                                              b#4954)))))[@inline] in
let blake2b#297 = fun b#4956 -> (({ BLAKE2B })@(b#4956))[@inline] in
let sha256#298 = fun b#4958 -> (({ SHA256 })@(b#4958))[@inline] in
let sha512#299 = fun b#4960 -> (({ SHA512 })@(b#4960))[@inline] in
let sha3#300 = fun b#4962 -> (({ SHA3 })@(b#4962))[@inline] in
let keccak#301 = fun b#4964 -> (({ KECCAK })@(b#4964))[@inline] in
let hash_key#302 = fun k#4966 -> (({ HASH_KEY })@(k#4966))[@inline] in
let check#303 =
  fun k#4968 ->
  (fun s#4969 ->
   (fun b#4970 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4968 , s#4969) ,
                                                   b#4970)))))[@inline] in
let assert#304 =
  fun b#4972 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4972))[@inline] in
let abs#307 = fun i#4978 -> (({ ABS })@(i#4978))[@inline] in
let is_nat#308 = fun i#4980 -> (({ ISNAT })@(i#4980))[@inline] in
let true#309 = TRUE()[@inline] in
let false#310 = FALSE()[@inline] in
let unit#311 = UNIT()[@inline] in
let assert_with_error#314 =
  fun b#4988 ->
  (fun s#4989 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4988 , s#4989))))[@inline] in
let poly_stub_234 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_233 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_232 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_231 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_230 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_229 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_228 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_227 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_226 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_225 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_224 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_223 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_222 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_221 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_220 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_219 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_218 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_217 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_216 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_215 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_214 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_213 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_212 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_211 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_210 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_209 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_208 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_207 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_206 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_205 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_204 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_203 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_202 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_201 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_200 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_199 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_198 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_197 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let poly_stub_196 = fun x#5000 -> (({ FAILWITH })@(x#5000))[@inline] in
let get_total_voting_power#322 =
  fun _u#5009 -> ((poly_stub_234)@(L(unit)))[@inline] in
let set_source#325 = fun _a#5015 -> ((poly_stub_233)@(L(unit)))[@inline] in
let get_storage_of_address#326 =
  fun _a#5017 -> ((poly_stub_232)@(L(unit)))[@inline] in
let get_balance#327 = fun _a#5019 -> ((poly_stub_231)@(L(unit)))[@inline] in
let print#328 = fun _v#5021 -> ((poly_stub_230)@(L(unit)))[@inline] in
let eprint#329 = fun _v#5023 -> ((poly_stub_229)@(L(unit)))[@inline] in
let get_voting_power#330 =
  fun _kh#5025 -> ((poly_stub_228)@(L(unit)))[@inline] in
let nth_bootstrap_contract#331 =
  fun _i#5027 -> ((poly_stub_227)@(L(unit)))[@inline] in
let nth_bootstrap_account#332 =
  fun _i#5029 -> ((poly_stub_226)@(L(unit)))[@inline] in
let get_bootstrap_account#333 =
  fun _n#5031 -> ((poly_stub_225)@(L(unit)))[@inline] in
let last_originations#335 =
  fun _u#5035 -> ((poly_stub_224)@(L(unit)))[@inline] in
let new_account#337 = fun _u#5039 -> ((poly_stub_223)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#339 =
  fun _n#5043 -> ((poly_stub_222)@(L(unit)))[@inline] in
let register_delegate#341 =
  fun _kh#5047 -> ((poly_stub_221)@(L(unit)))[@inline] in
let register_constant#342 =
  fun _m#5049 -> ((poly_stub_220)@(L(unit)))[@inline] in
let constant_to_michelson_program#344 =
  fun _s#5053 -> ((poly_stub_219)@(L(unit)))[@inline] in
let restore_context#345 =
  fun _u#5055 -> ((poly_stub_218)@(L(unit)))[@inline] in
let save_context#346 = fun _u#5057 -> ((poly_stub_217)@(L(unit)))[@inline] in
let drop_context#347 = fun _u#5059 -> ((poly_stub_216)@(L(unit)))[@inline] in
let set_baker_policy#350 =
  fun _bp#5065 -> ((poly_stub_215)@(L(unit)))[@inline] in
let set_baker#351 = fun _a#5067 -> ((poly_stub_214)@(L(unit)))[@inline] in
let size#352 = fun _c#5069 -> ((poly_stub_213)@(L(unit)))[@inline] in
let read_contract_from_file#354 =
  fun _fn#5073 -> ((poly_stub_212)@(L(unit)))[@inline] in
let chr#355 = fun _n#5075 -> ((poly_stub_211)@(L(unit)))[@inline] in
let nl#356 = L("NEWLINE")[@inline] in
let println#357 = fun _v#5078 -> ((poly_stub_210)@(L(unit)))[@inline] in
let transfer#358 =
  fun _a#5080 ->
  (fun _s#5081 -> (fun _t#5082 -> ((poly_stub_209)@(L(unit)))))[@inline] in
let transfer_exn#359 =
  fun _a#5084 ->
  (fun _s#5085 -> (fun _t#5086 -> ((poly_stub_208)@(L(unit)))))[@inline] in
let reset_state#361 =
  fun _n#5090 -> (fun _l#5091 -> ((poly_stub_207)@(L(unit))))[@inline] in
let reset_state_at#362 =
  fun _t#5093 ->
  (fun _n#5094 -> (fun _l#5095 -> ((poly_stub_206)@(L(unit)))))[@inline] in
let save_mutation#365 =
  fun _s#5104 -> (fun _m#5105 -> ((poly_stub_205)@(L(unit))))[@inline] in
let sign#368 =
  fun _sk#5113 -> (fun _d#5114 -> ((poly_stub_204)@(L(unit))))[@inline] in
let add_account#369 =
  fun _s#5116 -> (fun _k#5117 -> ((poly_stub_203)@(L(unit))))[@inline] in
let baker_account#370 =
  fun _p#5119 -> (fun _o#5120 -> ((poly_stub_202)@(L(unit))))[@inline] in
let create_chest#372 =
  fun _b#5125 -> (fun _n#5126 -> ((poly_stub_201)@(L(unit))))[@inline] in
let create_chest_key#373 =
  fun _c#5128 -> (fun _n#5129 -> ((poly_stub_200)@(L(unit))))[@inline] in
let michelson_equal#376 =
  fun _m1#5139 -> (fun _m2#5140 -> ((poly_stub_199)@(L(unit))))[@inline] in
let originate_contract#378 =
  fun _c#5145 ->
  (fun _s#5146 -> (fun _t#5147 -> ((poly_stub_198)@(L(unit)))))[@inline] in
let compile_contract_from_file#380 =
  fun _fn#5153 ->
  (fun _e#5154 -> (fun _v#5155 -> ((poly_stub_197)@(L(unit)))))[@inline] in
let originate_from_file#381 =
  fun _fn#5157 ->
  (fun _e#5158 ->
   (fun _v#5159 ->
    (fun _s#5160 -> (fun _t#5161 -> ((poly_stub_196)@(L(unit)))))))[@inline] in
let toto#382 = L(32) in
let titi#383 = ADD(toto#215 , L(42)) in
let f#384 =
  fun gen#5165 ->
  (let (gen#7367, gen#7368) = gen#5165 in
   let gen#5166 = gen#7367 in
   let x#5167 = gen#7368 in
   let x#5168 = ADD(ADD(x#5167 , toto#215) , titi#383) in
   PAIR(LIST_EMPTY() , x#5168)) in
let get_balance#385 =
  fun _u#5170 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#386 =
  fun _u#5172 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#387 = fun _u#5174 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#388 =
  fun _u#5176 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#389 =
  fun _u#5178 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#390 = fun _u#5180 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#391 = fun _u#5182 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#392 =
  fun _u#5184 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#393 =
  fun _u#5186 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#394 =
  fun _u#5188 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#395 =
  fun kh#5190 -> (({ VOTING_POWER })@(kh#5190))[@inline] in
let implicit_account#397 =
  fun kh#5194 -> (IMPLICIT_ACCOUNT(kh#5194))[@inline] in
let pairing_check#401 =
  fun l#5202 -> (({ PAIRING_CHECK })@(l#5202))[@inline] in
let set_delegate#403 = fun o#5206 -> (SET_DELEGATE(o#5206))[@inline] in
let open_chest#409 =
  fun ck#5222 ->
  (fun c#5223 -> (fun n#5224 -> (OPEN_CHEST(ck#5222 , c#5223 , n#5224))))[@inline] in
let xor#412 =
  fun l#5233 -> (fun r#5234 -> (XOR(l#5233 , r#5234)))[@inline] in
let shift_left#413 =
  fun l#5236 -> (fun r#5237 -> (LSL(l#5236 , r#5237)))[@inline] in
let shift_right#414 =
  fun l#5239 -> (fun r#5240 -> (LSR(l#5239 , r#5240)))[@inline] in
let length#455 = fun b#5370 -> (({ SIZE })@(b#5370))[@inline] in
let concat#456 =
  fun b1#5372 ->
  (fun b2#5373 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5372 , b2#5373))))[@inline] in
let sub#457 =
  fun s#5375 ->
  (fun l#5376 ->
   (fun b#5377 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5375 ,
                                                                   l#5376) ,
                                                              b#5377)))))[@inline] in
let length#463 = fun b#5391 -> (({ SIZE })@(b#5391))[@inline] in
let concat#464 =
  fun b1#5393 ->
  (fun b2#5394 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5393 , b2#5394))))[@inline] in
let sub#465 =
  fun s#5396 ->
  (fun l#5397 ->
   (fun b#5398 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5396 ,
                                                                   l#5397) ,
                                                              b#5398)))))[@inline] in
let blake2b#466 = fun b#5400 -> (({ BLAKE2B })@(b#5400))[@inline] in
let sha256#467 = fun b#5402 -> (({ SHA256 })@(b#5402))[@inline] in
let sha512#468 = fun b#5404 -> (({ SHA512 })@(b#5404))[@inline] in
let sha3#469 = fun b#5406 -> (({ SHA3 })@(b#5406))[@inline] in
let keccak#470 = fun b#5408 -> (({ KECCAK })@(b#5408))[@inline] in
let hash_key#471 = fun k#5410 -> (({ HASH_KEY })@(k#5410))[@inline] in
let check#472 =
  fun k#5412 ->
  (fun s#5413 ->
   (fun b#5414 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5412 , s#5413) ,
                                                   b#5414)))))[@inline] in
let assert#473 =
  fun b#5416 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5416))[@inline] in
let abs#476 = fun i#5422 -> (({ ABS })@(i#5422))[@inline] in
let is_nat#477 = fun i#5424 -> (({ ISNAT })@(i#5424))[@inline] in
let true#478 = TRUE()[@inline] in
let false#479 = FALSE()[@inline] in
let unit#480 = UNIT()[@inline] in
let assert_with_error#483 =
  fun b#5432 ->
  (fun s#5433 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5432 , s#5433))))[@inline] in
let poly_stub_195 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_194 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_193 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_192 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_191 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_190 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_189 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_188 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_187 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_186 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_185 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_184 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_183 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_182 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_181 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_180 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_179 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_178 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_177 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_176 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_175 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_174 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_173 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_172 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_171 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_170 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_169 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_168 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_167 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_166 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_165 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_164 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_163 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_162 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_161 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_160 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_159 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_158 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let poly_stub_157 = fun x#5444 -> (({ FAILWITH })@(x#5444))[@inline] in
let get_total_voting_power#491 =
  fun _u#5453 -> ((poly_stub_195)@(L(unit)))[@inline] in
let set_source#494 = fun _a#5459 -> ((poly_stub_194)@(L(unit)))[@inline] in
let get_storage_of_address#495 =
  fun _a#5461 -> ((poly_stub_193)@(L(unit)))[@inline] in
let get_balance#496 = fun _a#5463 -> ((poly_stub_192)@(L(unit)))[@inline] in
let print#497 = fun _v#5465 -> ((poly_stub_191)@(L(unit)))[@inline] in
let eprint#498 = fun _v#5467 -> ((poly_stub_190)@(L(unit)))[@inline] in
let get_voting_power#499 =
  fun _kh#5469 -> ((poly_stub_189)@(L(unit)))[@inline] in
let nth_bootstrap_contract#500 =
  fun _i#5471 -> ((poly_stub_188)@(L(unit)))[@inline] in
let nth_bootstrap_account#501 =
  fun _i#5473 -> ((poly_stub_187)@(L(unit)))[@inline] in
let get_bootstrap_account#502 =
  fun _n#5475 -> ((poly_stub_186)@(L(unit)))[@inline] in
let last_originations#504 =
  fun _u#5479 -> ((poly_stub_185)@(L(unit)))[@inline] in
let new_account#506 = fun _u#5483 -> ((poly_stub_184)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#508 =
  fun _n#5487 -> ((poly_stub_183)@(L(unit)))[@inline] in
let register_delegate#510 =
  fun _kh#5491 -> ((poly_stub_182)@(L(unit)))[@inline] in
let register_constant#511 =
  fun _m#5493 -> ((poly_stub_181)@(L(unit)))[@inline] in
let constant_to_michelson_program#513 =
  fun _s#5497 -> ((poly_stub_180)@(L(unit)))[@inline] in
let restore_context#514 =
  fun _u#5499 -> ((poly_stub_179)@(L(unit)))[@inline] in
let save_context#515 = fun _u#5501 -> ((poly_stub_178)@(L(unit)))[@inline] in
let drop_context#516 = fun _u#5503 -> ((poly_stub_177)@(L(unit)))[@inline] in
let set_baker_policy#519 =
  fun _bp#5509 -> ((poly_stub_176)@(L(unit)))[@inline] in
let set_baker#520 = fun _a#5511 -> ((poly_stub_175)@(L(unit)))[@inline] in
let size#521 = fun _c#5513 -> ((poly_stub_174)@(L(unit)))[@inline] in
let read_contract_from_file#523 =
  fun _fn#5517 -> ((poly_stub_173)@(L(unit)))[@inline] in
let chr#524 = fun _n#5519 -> ((poly_stub_172)@(L(unit)))[@inline] in
let nl#525 = L("NEWLINE")[@inline] in
let println#526 = fun _v#5522 -> ((poly_stub_171)@(L(unit)))[@inline] in
let transfer#527 =
  fun _a#5524 ->
  (fun _s#5525 -> (fun _t#5526 -> ((poly_stub_170)@(L(unit)))))[@inline] in
let transfer_exn#528 =
  fun _a#5528 ->
  (fun _s#5529 -> (fun _t#5530 -> ((poly_stub_169)@(L(unit)))))[@inline] in
let reset_state#530 =
  fun _n#5534 -> (fun _l#5535 -> ((poly_stub_168)@(L(unit))))[@inline] in
let reset_state_at#531 =
  fun _t#5537 ->
  (fun _n#5538 -> (fun _l#5539 -> ((poly_stub_167)@(L(unit)))))[@inline] in
let save_mutation#534 =
  fun _s#5548 -> (fun _m#5549 -> ((poly_stub_166)@(L(unit))))[@inline] in
let sign#537 =
  fun _sk#5557 -> (fun _d#5558 -> ((poly_stub_165)@(L(unit))))[@inline] in
let add_account#538 =
  fun _s#5560 -> (fun _k#5561 -> ((poly_stub_164)@(L(unit))))[@inline] in
let baker_account#539 =
  fun _p#5563 -> (fun _o#5564 -> ((poly_stub_163)@(L(unit))))[@inline] in
let create_chest#541 =
  fun _b#5569 -> (fun _n#5570 -> ((poly_stub_162)@(L(unit))))[@inline] in
let create_chest_key#542 =
  fun _c#5572 -> (fun _n#5573 -> ((poly_stub_161)@(L(unit))))[@inline] in
let michelson_equal#545 =
  fun _m1#5583 -> (fun _m2#5584 -> ((poly_stub_160)@(L(unit))))[@inline] in
let originate_contract#547 =
  fun _c#5589 ->
  (fun _s#5590 -> (fun _t#5591 -> ((poly_stub_159)@(L(unit)))))[@inline] in
let compile_contract_from_file#549 =
  fun _fn#5597 ->
  (fun _e#5598 -> (fun _v#5599 -> ((poly_stub_158)@(L(unit)))))[@inline] in
let originate_from_file#550 =
  fun _fn#5601 ->
  (fun _e#5602 ->
   (fun _v#5603 ->
    (fun _s#5604 -> (fun _t#5605 -> ((poly_stub_157)@(L(unit)))))))[@inline] in
let toto#551 = L(44) in
let get_balance#552 =
  fun _u#5608 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#553 =
  fun _u#5610 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#554 = fun _u#5612 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#555 =
  fun _u#5614 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#556 =
  fun _u#5616 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#557 = fun _u#5618 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#558 = fun _u#5620 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#559 =
  fun _u#5622 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#560 =
  fun _u#5624 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#561 =
  fun _u#5626 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#562 =
  fun kh#5628 -> (({ VOTING_POWER })@(kh#5628))[@inline] in
let implicit_account#564 =
  fun kh#5632 -> (IMPLICIT_ACCOUNT(kh#5632))[@inline] in
let pairing_check#568 =
  fun l#5640 -> (({ PAIRING_CHECK })@(l#5640))[@inline] in
let set_delegate#570 = fun o#5644 -> (SET_DELEGATE(o#5644))[@inline] in
let open_chest#576 =
  fun ck#5660 ->
  (fun c#5661 -> (fun n#5662 -> (OPEN_CHEST(ck#5660 , c#5661 , n#5662))))[@inline] in
let xor#579 =
  fun l#5671 -> (fun r#5672 -> (XOR(l#5671 , r#5672)))[@inline] in
let shift_left#580 =
  fun l#5674 -> (fun r#5675 -> (LSL(l#5674 , r#5675)))[@inline] in
let shift_right#581 =
  fun l#5677 -> (fun r#5678 -> (LSR(l#5677 , r#5678)))[@inline] in
let length#622 = fun b#5808 -> (({ SIZE })@(b#5808))[@inline] in
let concat#623 =
  fun b1#5810 ->
  (fun b2#5811 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5810 , b2#5811))))[@inline] in
let sub#624 =
  fun s#5813 ->
  (fun l#5814 ->
   (fun b#5815 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5813 ,
                                                                   l#5814) ,
                                                              b#5815)))))[@inline] in
let length#630 = fun b#5829 -> (({ SIZE })@(b#5829))[@inline] in
let concat#631 =
  fun b1#5831 ->
  (fun b2#5832 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5831 , b2#5832))))[@inline] in
let sub#632 =
  fun s#5834 ->
  (fun l#5835 ->
   (fun b#5836 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5834 ,
                                                                   l#5835) ,
                                                              b#5836)))))[@inline] in
let blake2b#633 = fun b#5838 -> (({ BLAKE2B })@(b#5838))[@inline] in
let sha256#634 = fun b#5840 -> (({ SHA256 })@(b#5840))[@inline] in
let sha512#635 = fun b#5842 -> (({ SHA512 })@(b#5842))[@inline] in
let sha3#636 = fun b#5844 -> (({ SHA3 })@(b#5844))[@inline] in
let keccak#637 = fun b#5846 -> (({ KECCAK })@(b#5846))[@inline] in
let hash_key#638 = fun k#5848 -> (({ HASH_KEY })@(k#5848))[@inline] in
let check#639 =
  fun k#5850 ->
  (fun s#5851 ->
   (fun b#5852 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5850 , s#5851) ,
                                                   b#5852)))))[@inline] in
let assert#640 =
  fun b#5854 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5854))[@inline] in
let abs#643 = fun i#5860 -> (({ ABS })@(i#5860))[@inline] in
let is_nat#644 = fun i#5862 -> (({ ISNAT })@(i#5862))[@inline] in
let true#645 = TRUE()[@inline] in
let false#646 = FALSE()[@inline] in
let unit#647 = UNIT()[@inline] in
let assert_with_error#650 =
  fun b#5870 ->
  (fun s#5871 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5870 , s#5871))))[@inline] in
let poly_stub_156 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_155 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_154 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_153 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_152 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_151 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_150 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_149 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_148 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_147 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_146 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_145 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_144 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_143 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_142 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_141 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_140 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_139 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_138 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_137 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_136 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_135 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_134 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_133 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_132 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_131 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_130 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_129 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_128 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_127 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_126 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_125 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_124 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_123 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_122 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_121 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_120 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_119 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let poly_stub_118 = fun x#5882 -> (({ FAILWITH })@(x#5882))[@inline] in
let get_total_voting_power#658 =
  fun _u#5891 -> ((poly_stub_156)@(L(unit)))[@inline] in
let set_source#661 = fun _a#5897 -> ((poly_stub_155)@(L(unit)))[@inline] in
let get_storage_of_address#662 =
  fun _a#5899 -> ((poly_stub_154)@(L(unit)))[@inline] in
let get_balance#663 = fun _a#5901 -> ((poly_stub_153)@(L(unit)))[@inline] in
let print#664 = fun _v#5903 -> ((poly_stub_152)@(L(unit)))[@inline] in
let eprint#665 = fun _v#5905 -> ((poly_stub_151)@(L(unit)))[@inline] in
let get_voting_power#666 =
  fun _kh#5907 -> ((poly_stub_150)@(L(unit)))[@inline] in
let nth_bootstrap_contract#667 =
  fun _i#5909 -> ((poly_stub_149)@(L(unit)))[@inline] in
let nth_bootstrap_account#668 =
  fun _i#5911 -> ((poly_stub_148)@(L(unit)))[@inline] in
let get_bootstrap_account#669 =
  fun _n#5913 -> ((poly_stub_147)@(L(unit)))[@inline] in
let last_originations#671 =
  fun _u#5917 -> ((poly_stub_146)@(L(unit)))[@inline] in
let new_account#673 = fun _u#5921 -> ((poly_stub_145)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#675 =
  fun _n#5925 -> ((poly_stub_144)@(L(unit)))[@inline] in
let register_delegate#677 =
  fun _kh#5929 -> ((poly_stub_143)@(L(unit)))[@inline] in
let register_constant#678 =
  fun _m#5931 -> ((poly_stub_142)@(L(unit)))[@inline] in
let constant_to_michelson_program#680 =
  fun _s#5935 -> ((poly_stub_141)@(L(unit)))[@inline] in
let restore_context#681 =
  fun _u#5937 -> ((poly_stub_140)@(L(unit)))[@inline] in
let save_context#682 = fun _u#5939 -> ((poly_stub_139)@(L(unit)))[@inline] in
let drop_context#683 = fun _u#5941 -> ((poly_stub_138)@(L(unit)))[@inline] in
let set_baker_policy#686 =
  fun _bp#5947 -> ((poly_stub_137)@(L(unit)))[@inline] in
let set_baker#687 = fun _a#5949 -> ((poly_stub_136)@(L(unit)))[@inline] in
let size#688 = fun _c#5951 -> ((poly_stub_135)@(L(unit)))[@inline] in
let read_contract_from_file#690 =
  fun _fn#5955 -> ((poly_stub_134)@(L(unit)))[@inline] in
let chr#691 = fun _n#5957 -> ((poly_stub_133)@(L(unit)))[@inline] in
let nl#692 = L("NEWLINE")[@inline] in
let println#693 = fun _v#5960 -> ((poly_stub_132)@(L(unit)))[@inline] in
let transfer#694 =
  fun _a#5962 ->
  (fun _s#5963 -> (fun _t#5964 -> ((poly_stub_131)@(L(unit)))))[@inline] in
let transfer_exn#695 =
  fun _a#5966 ->
  (fun _s#5967 -> (fun _t#5968 -> ((poly_stub_130)@(L(unit)))))[@inline] in
let reset_state#697 =
  fun _n#5972 -> (fun _l#5973 -> ((poly_stub_129)@(L(unit))))[@inline] in
let reset_state_at#698 =
  fun _t#5975 ->
  (fun _n#5976 -> (fun _l#5977 -> ((poly_stub_128)@(L(unit)))))[@inline] in
let save_mutation#701 =
  fun _s#5986 -> (fun _m#5987 -> ((poly_stub_127)@(L(unit))))[@inline] in
let sign#704 =
  fun _sk#5995 -> (fun _d#5996 -> ((poly_stub_126)@(L(unit))))[@inline] in
let add_account#705 =
  fun _s#5998 -> (fun _k#5999 -> ((poly_stub_125)@(L(unit))))[@inline] in
let baker_account#706 =
  fun _p#6001 -> (fun _o#6002 -> ((poly_stub_124)@(L(unit))))[@inline] in
let create_chest#708 =
  fun _b#6007 -> (fun _n#6008 -> ((poly_stub_123)@(L(unit))))[@inline] in
let create_chest_key#709 =
  fun _c#6010 -> (fun _n#6011 -> ((poly_stub_122)@(L(unit))))[@inline] in
let michelson_equal#712 =
  fun _m1#6021 -> (fun _m2#6022 -> ((poly_stub_121)@(L(unit))))[@inline] in
let originate_contract#714 =
  fun _c#6027 ->
  (fun _s#6028 -> (fun _t#6029 -> ((poly_stub_120)@(L(unit)))))[@inline] in
let compile_contract_from_file#716 =
  fun _fn#6035 ->
  (fun _e#6036 -> (fun _v#6037 -> ((poly_stub_119)@(L(unit)))))[@inline] in
let originate_from_file#717 =
  fun _fn#6039 ->
  (fun _e#6040 ->
   (fun _v#6041 ->
    (fun _s#6042 -> (fun _t#6043 -> ((poly_stub_118)@(L(unit)))))))[@inline] in
let toto#718 = L(43) in
let get_balance#719 =
  fun _u#6046 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#720 =
  fun _u#6048 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#721 = fun _u#6050 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#722 =
  fun _u#6052 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#723 =
  fun _u#6054 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#724 = fun _u#6056 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#725 = fun _u#6058 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#726 =
  fun _u#6060 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#727 =
  fun _u#6062 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#728 =
  fun _u#6064 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#729 =
  fun kh#6066 -> (({ VOTING_POWER })@(kh#6066))[@inline] in
let implicit_account#731 =
  fun kh#6070 -> (IMPLICIT_ACCOUNT(kh#6070))[@inline] in
let pairing_check#735 =
  fun l#6078 -> (({ PAIRING_CHECK })@(l#6078))[@inline] in
let set_delegate#737 = fun o#6082 -> (SET_DELEGATE(o#6082))[@inline] in
let open_chest#743 =
  fun ck#6098 ->
  (fun c#6099 -> (fun n#6100 -> (OPEN_CHEST(ck#6098 , c#6099 , n#6100))))[@inline] in
let xor#746 =
  fun l#6109 -> (fun r#6110 -> (XOR(l#6109 , r#6110)))[@inline] in
let shift_left#747 =
  fun l#6112 -> (fun r#6113 -> (LSL(l#6112 , r#6113)))[@inline] in
let shift_right#748 =
  fun l#6115 -> (fun r#6116 -> (LSR(l#6115 , r#6116)))[@inline] in
let length#789 = fun b#6246 -> (({ SIZE })@(b#6246))[@inline] in
let concat#790 =
  fun b1#6248 ->
  (fun b2#6249 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6248 , b2#6249))))[@inline] in
let sub#791 =
  fun s#6251 ->
  (fun l#6252 ->
   (fun b#6253 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6251 ,
                                                                   l#6252) ,
                                                              b#6253)))))[@inline] in
let length#797 = fun b#6267 -> (({ SIZE })@(b#6267))[@inline] in
let concat#798 =
  fun b1#6269 ->
  (fun b2#6270 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6269 , b2#6270))))[@inline] in
let sub#799 =
  fun s#6272 ->
  (fun l#6273 ->
   (fun b#6274 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6272 ,
                                                                   l#6273) ,
                                                              b#6274)))))[@inline] in
let blake2b#800 = fun b#6276 -> (({ BLAKE2B })@(b#6276))[@inline] in
let sha256#801 = fun b#6278 -> (({ SHA256 })@(b#6278))[@inline] in
let sha512#802 = fun b#6280 -> (({ SHA512 })@(b#6280))[@inline] in
let sha3#803 = fun b#6282 -> (({ SHA3 })@(b#6282))[@inline] in
let keccak#804 = fun b#6284 -> (({ KECCAK })@(b#6284))[@inline] in
let hash_key#805 = fun k#6286 -> (({ HASH_KEY })@(k#6286))[@inline] in
let check#806 =
  fun k#6288 ->
  (fun s#6289 ->
   (fun b#6290 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6288 , s#6289) ,
                                                   b#6290)))))[@inline] in
let assert#807 =
  fun b#6292 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6292))[@inline] in
let abs#810 = fun i#6298 -> (({ ABS })@(i#6298))[@inline] in
let is_nat#811 = fun i#6300 -> (({ ISNAT })@(i#6300))[@inline] in
let true#812 = TRUE()[@inline] in
let false#813 = FALSE()[@inline] in
let unit#814 = UNIT()[@inline] in
let assert_with_error#817 =
  fun b#6308 ->
  (fun s#6309 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6308 , s#6309))))[@inline] in
let poly_stub_117 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_116 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_115 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_114 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_113 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_112 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_111 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_110 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_109 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_108 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_107 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_106 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_105 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_104 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_103 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_102 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_101 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_100 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_99 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_98 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_97 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_96 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_95 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_94 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_93 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_92 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_91 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_90 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_89 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_88 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_87 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_86 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_85 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_84 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_83 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_82 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_81 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_80 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let poly_stub_79 = fun x#6320 -> (({ FAILWITH })@(x#6320))[@inline] in
let get_total_voting_power#825 =
  fun _u#6329 -> ((poly_stub_117)@(L(unit)))[@inline] in
let set_source#828 = fun _a#6335 -> ((poly_stub_116)@(L(unit)))[@inline] in
let get_storage_of_address#829 =
  fun _a#6337 -> ((poly_stub_115)@(L(unit)))[@inline] in
let get_balance#830 = fun _a#6339 -> ((poly_stub_114)@(L(unit)))[@inline] in
let print#831 = fun _v#6341 -> ((poly_stub_113)@(L(unit)))[@inline] in
let eprint#832 = fun _v#6343 -> ((poly_stub_112)@(L(unit)))[@inline] in
let get_voting_power#833 =
  fun _kh#6345 -> ((poly_stub_111)@(L(unit)))[@inline] in
let nth_bootstrap_contract#834 =
  fun _i#6347 -> ((poly_stub_110)@(L(unit)))[@inline] in
let nth_bootstrap_account#835 =
  fun _i#6349 -> ((poly_stub_109)@(L(unit)))[@inline] in
let get_bootstrap_account#836 =
  fun _n#6351 -> ((poly_stub_108)@(L(unit)))[@inline] in
let last_originations#838 =
  fun _u#6355 -> ((poly_stub_107)@(L(unit)))[@inline] in
let new_account#840 = fun _u#6359 -> ((poly_stub_106)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#842 =
  fun _n#6363 -> ((poly_stub_105)@(L(unit)))[@inline] in
let register_delegate#844 =
  fun _kh#6367 -> ((poly_stub_104)@(L(unit)))[@inline] in
let register_constant#845 =
  fun _m#6369 -> ((poly_stub_103)@(L(unit)))[@inline] in
let constant_to_michelson_program#847 =
  fun _s#6373 -> ((poly_stub_102)@(L(unit)))[@inline] in
let restore_context#848 =
  fun _u#6375 -> ((poly_stub_101)@(L(unit)))[@inline] in
let save_context#849 = fun _u#6377 -> ((poly_stub_100)@(L(unit)))[@inline] in
let drop_context#850 = fun _u#6379 -> ((poly_stub_99)@(L(unit)))[@inline] in
let set_baker_policy#853 =
  fun _bp#6385 -> ((poly_stub_98)@(L(unit)))[@inline] in
let set_baker#854 = fun _a#6387 -> ((poly_stub_97)@(L(unit)))[@inline] in
let size#855 = fun _c#6389 -> ((poly_stub_96)@(L(unit)))[@inline] in
let read_contract_from_file#857 =
  fun _fn#6393 -> ((poly_stub_95)@(L(unit)))[@inline] in
let chr#858 = fun _n#6395 -> ((poly_stub_94)@(L(unit)))[@inline] in
let nl#859 = L("NEWLINE")[@inline] in
let println#860 = fun _v#6398 -> ((poly_stub_93)@(L(unit)))[@inline] in
let transfer#861 =
  fun _a#6400 -> (fun _s#6401 -> (fun _t#6402 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#862 =
  fun _a#6404 -> (fun _s#6405 -> (fun _t#6406 -> ((poly_stub_91)@(L(unit)))))[@inline] in
let reset_state#864 =
  fun _n#6410 -> (fun _l#6411 -> ((poly_stub_90)@(L(unit))))[@inline] in
let reset_state_at#865 =
  fun _t#6413 -> (fun _n#6414 -> (fun _l#6415 -> ((poly_stub_89)@(L(unit)))))[@inline] in
let save_mutation#868 =
  fun _s#6424 -> (fun _m#6425 -> ((poly_stub_88)@(L(unit))))[@inline] in
let sign#871 =
  fun _sk#6433 -> (fun _d#6434 -> ((poly_stub_87)@(L(unit))))[@inline] in
let add_account#872 =
  fun _s#6436 -> (fun _k#6437 -> ((poly_stub_86)@(L(unit))))[@inline] in
let baker_account#873 =
  fun _p#6439 -> (fun _o#6440 -> ((poly_stub_85)@(L(unit))))[@inline] in
let create_chest#875 =
  fun _b#6445 -> (fun _n#6446 -> ((poly_stub_84)@(L(unit))))[@inline] in
let create_chest_key#876 =
  fun _c#6448 -> (fun _n#6449 -> ((poly_stub_83)@(L(unit))))[@inline] in
let michelson_equal#879 =
  fun _m1#6459 -> (fun _m2#6460 -> ((poly_stub_82)@(L(unit))))[@inline] in
let originate_contract#881 =
  fun _c#6465 -> (fun _s#6466 -> (fun _t#6467 -> ((poly_stub_81)@(L(unit)))))[@inline] in
let compile_contract_from_file#883 =
  fun _fn#6473 ->
  (fun _e#6474 -> (fun _v#6475 -> ((poly_stub_80)@(L(unit)))))[@inline] in
let originate_from_file#884 =
  fun _fn#6477 ->
  (fun _e#6478 ->
   (fun _v#6479 ->
    (fun _s#6480 -> (fun _t#6481 -> ((poly_stub_79)@(L(unit)))))))[@inline] in
let tata#885 = ADD(toto#215 , titi#383) in
let foo#886 = (f#384)@(PAIR(L(unit) , L(3))) in
let get_balance#887 =
  fun _u#6485 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#888 =
  fun _u#6487 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#889 = fun _u#6489 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#890 =
  fun _u#6491 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#891 =
  fun _u#6493 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#892 = fun _u#6495 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#893 = fun _u#6497 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#894 =
  fun _u#6499 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#895 =
  fun _u#6501 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#896 =
  fun _u#6503 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#897 =
  fun kh#6505 -> (({ VOTING_POWER })@(kh#6505))[@inline] in
let implicit_account#899 =
  fun kh#6509 -> (IMPLICIT_ACCOUNT(kh#6509))[@inline] in
let pairing_check#903 =
  fun l#6517 -> (({ PAIRING_CHECK })@(l#6517))[@inline] in
let set_delegate#905 = fun o#6521 -> (SET_DELEGATE(o#6521))[@inline] in
let open_chest#911 =
  fun ck#6537 ->
  (fun c#6538 -> (fun n#6539 -> (OPEN_CHEST(ck#6537 , c#6538 , n#6539))))[@inline] in
let xor#914 =
  fun l#6548 -> (fun r#6549 -> (XOR(l#6548 , r#6549)))[@inline] in
let shift_left#915 =
  fun l#6551 -> (fun r#6552 -> (LSL(l#6551 , r#6552)))[@inline] in
let shift_right#916 =
  fun l#6554 -> (fun r#6555 -> (LSR(l#6554 , r#6555)))[@inline] in
let length#957 = fun b#6685 -> (({ SIZE })@(b#6685))[@inline] in
let concat#958 =
  fun b1#6687 ->
  (fun b2#6688 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6687 , b2#6688))))[@inline] in
let sub#959 =
  fun s#6690 ->
  (fun l#6691 ->
   (fun b#6692 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6690 ,
                                                                   l#6691) ,
                                                              b#6692)))))[@inline] in
let length#965 = fun b#6706 -> (({ SIZE })@(b#6706))[@inline] in
let concat#966 =
  fun b1#6708 ->
  (fun b2#6709 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6708 , b2#6709))))[@inline] in
let sub#967 =
  fun s#6711 ->
  (fun l#6712 ->
   (fun b#6713 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6711 ,
                                                                   l#6712) ,
                                                              b#6713)))))[@inline] in
let blake2b#968 = fun b#6715 -> (({ BLAKE2B })@(b#6715))[@inline] in
let sha256#969 = fun b#6717 -> (({ SHA256 })@(b#6717))[@inline] in
let sha512#970 = fun b#6719 -> (({ SHA512 })@(b#6719))[@inline] in
let sha3#971 = fun b#6721 -> (({ SHA3 })@(b#6721))[@inline] in
let keccak#972 = fun b#6723 -> (({ KECCAK })@(b#6723))[@inline] in
let hash_key#973 = fun k#6725 -> (({ HASH_KEY })@(k#6725))[@inline] in
let check#974 =
  fun k#6727 ->
  (fun s#6728 ->
   (fun b#6729 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6727 , s#6728) ,
                                                   b#6729)))))[@inline] in
let assert#975 =
  fun b#6731 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6731))[@inline] in
let abs#978 = fun i#6737 -> (({ ABS })@(i#6737))[@inline] in
let is_nat#979 = fun i#6739 -> (({ ISNAT })@(i#6739))[@inline] in
let true#980 = TRUE()[@inline] in
let false#981 = FALSE()[@inline] in
let unit#982 = UNIT()[@inline] in
let assert_with_error#985 =
  fun b#6747 ->
  (fun s#6748 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6747 , s#6748))))[@inline] in
let poly_stub_78 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_77 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_76 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_75 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_74 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_73 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_72 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_71 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_70 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_69 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_68 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_67 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_66 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_65 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_64 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_63 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_62 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_61 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_60 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_59 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_58 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_57 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_56 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_55 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_54 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_53 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_52 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_51 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_50 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_49 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_48 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_47 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_46 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_45 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_44 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_43 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_42 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_41 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let poly_stub_40 = fun x#6759 -> (({ FAILWITH })@(x#6759))[@inline] in
let get_total_voting_power#993 =
  fun _u#6768 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#996 = fun _a#6774 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#997 =
  fun _a#6776 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#998 = fun _a#6778 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#999 = fun _v#6780 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#1000 = fun _v#6782 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#1001 =
  fun _kh#6784 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1002 =
  fun _i#6786 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#1003 =
  fun _i#6788 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#1004 =
  fun _n#6790 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#1006 =
  fun _u#6794 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#1008 = fun _u#6798 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1010 =
  fun _n#6802 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#1012 =
  fun _kh#6806 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#1013 =
  fun _m#6808 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#1015 =
  fun _s#6812 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#1016 =
  fun _u#6814 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#1017 = fun _u#6816 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#1018 = fun _u#6818 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#1021 =
  fun _bp#6824 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#1022 = fun _a#6826 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#1023 = fun _c#6828 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#1025 =
  fun _fn#6832 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#1026 = fun _n#6834 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#1027 = L("NEWLINE")[@inline] in
let println#1028 = fun _v#6837 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#1029 =
  fun _a#6839 -> (fun _s#6840 -> (fun _t#6841 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#1030 =
  fun _a#6843 -> (fun _s#6844 -> (fun _t#6845 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#1032 =
  fun _n#6849 -> (fun _l#6850 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#1033 =
  fun _t#6852 -> (fun _n#6853 -> (fun _l#6854 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#1036 =
  fun _s#6863 -> (fun _m#6864 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#1039 =
  fun _sk#6872 -> (fun _d#6873 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#1040 =
  fun _s#6875 -> (fun _k#6876 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#1041 =
  fun _p#6878 -> (fun _o#6879 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#1043 =
  fun _b#6884 -> (fun _n#6885 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#1044 =
  fun _c#6887 -> (fun _n#6888 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#1047 =
  fun _m1#6898 -> (fun _m2#6899 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#1049 =
  fun _c#6904 -> (fun _s#6905 -> (fun _t#6906 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#1051 =
  fun _fn#6912 ->
  (fun _e#6913 -> (fun _v#6914 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#1052 =
  fun _fn#6916 ->
  (fun _e#6917 ->
   (fun _v#6918 ->
    (fun _s#6919 -> (fun _t#6920 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let toto#1053 = L(10) in
let foo#1054 = L("bar") in
let get_balance#1055 =
  fun _u#6924 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1056 =
  fun _u#6926 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1057 = fun _u#6928 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1058 =
  fun _u#6930 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1059 =
  fun _u#6932 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1060 =
  fun _u#6934 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1061 = fun _u#6936 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1062 =
  fun _u#6938 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1063 =
  fun _u#6940 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1064 =
  fun _u#6942 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1065 =
  fun kh#6944 -> (({ VOTING_POWER })@(kh#6944))[@inline] in
let implicit_account#1067 =
  fun kh#6948 -> (IMPLICIT_ACCOUNT(kh#6948))[@inline] in
let pairing_check#1071 =
  fun l#6956 -> (({ PAIRING_CHECK })@(l#6956))[@inline] in
let set_delegate#1073 = fun o#6960 -> (SET_DELEGATE(o#6960))[@inline] in
let open_chest#1079 =
  fun ck#6976 ->
  (fun c#6977 -> (fun n#6978 -> (OPEN_CHEST(ck#6976 , c#6977 , n#6978))))[@inline] in
let xor#1082 =
  fun l#6987 -> (fun r#6988 -> (XOR(l#6987 , r#6988)))[@inline] in
let shift_left#1083 =
  fun l#6990 -> (fun r#6991 -> (LSL(l#6990 , r#6991)))[@inline] in
let shift_right#1084 =
  fun l#6993 -> (fun r#6994 -> (LSR(l#6993 , r#6994)))[@inline] in
let length#1125 = fun b#7124 -> (({ SIZE })@(b#7124))[@inline] in
let concat#1126 =
  fun b1#7126 ->
  (fun b2#7127 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7126 , b2#7127))))[@inline] in
let sub#1127 =
  fun s#7129 ->
  (fun l#7130 ->
   (fun b#7131 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7129 ,
                                                                   l#7130) ,
                                                              b#7131)))))[@inline] in
let length#1133 = fun b#7145 -> (({ SIZE })@(b#7145))[@inline] in
let concat#1134 =
  fun b1#7147 ->
  (fun b2#7148 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7147 , b2#7148))))[@inline] in
let sub#1135 =
  fun s#7150 ->
  (fun l#7151 ->
   (fun b#7152 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7150 ,
                                                                   l#7151) ,
                                                              b#7152)))))[@inline] in
let blake2b#1136 = fun b#7154 -> (({ BLAKE2B })@(b#7154))[@inline] in
let sha256#1137 = fun b#7156 -> (({ SHA256 })@(b#7156))[@inline] in
let sha512#1138 = fun b#7158 -> (({ SHA512 })@(b#7158))[@inline] in
let sha3#1139 = fun b#7160 -> (({ SHA3 })@(b#7160))[@inline] in
let keccak#1140 = fun b#7162 -> (({ KECCAK })@(b#7162))[@inline] in
let hash_key#1141 = fun k#7164 -> (({ HASH_KEY })@(k#7164))[@inline] in
let check#1142 =
  fun k#7166 ->
  (fun s#7167 ->
   (fun b#7168 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7166 , s#7167) ,
                                                   b#7168)))))[@inline] in
let assert =
  fun b#7170 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7170))[@inline] in
let abs = fun i#7176 -> (({ ABS })@(i#7176))[@inline] in
let is_nat = fun i#7178 -> (({ ISNAT })@(i#7178))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#7186 ->
  (fun s#7187 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7186 , s#7187))))[@inline] in
let poly_stub_39 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_38 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_37 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_36 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_35 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_34 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_33 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_32 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_31 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_30 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_29 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_28 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_27 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_26 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_25 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_24 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_23 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_22 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_21 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_20 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_19 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_18 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_17 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_16 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_15 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_14 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_13 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_12 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_11 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_10 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_9 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_8 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_7 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_6 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_5 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_4 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_3 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_2 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let poly_stub_1 = fun x#7198 -> (({ FAILWITH })@(x#7198))[@inline] in
let get_total_voting_power#1147 =
  fun _u#7207 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#1150 = fun _a#7213 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#1151 =
  fun _a#7215 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#1152 = fun _a#7217 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#1153 = fun _v#7219 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#1154 = fun _v#7221 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#1155 =
  fun _kh#7223 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1156 =
  fun _i#7225 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#1157 =
  fun _i#7227 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#1158 =
  fun _n#7229 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#1160 =
  fun _u#7233 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#1162 = fun _u#7237 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1164 =
  fun _n#7241 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#1166 =
  fun _kh#7245 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#1167 =
  fun _m#7247 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#1169 =
  fun _s#7251 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#1170 =
  fun _u#7253 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#1171 = fun _u#7255 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#1172 = fun _u#7257 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#1175 =
  fun _bp#7263 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#1176 = fun _a#7265 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#1177 = fun _c#7267 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#1179 =
  fun _fn#7271 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1180 = fun _n#7273 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#1181 = L("NEWLINE")[@inline] in
let println#1182 = fun _v#7276 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#1183 =
  fun _a#7278 -> (fun _s#7279 -> (fun _t#7280 -> ((poly_stub_14)@(L(unit)))))[@inline] in
let transfer_exn#1184 =
  fun _a#7282 -> (fun _s#7283 -> (fun _t#7284 -> ((poly_stub_13)@(L(unit)))))[@inline] in
let reset_state#1186 =
  fun _n#7288 -> (fun _l#7289 -> ((poly_stub_12)@(L(unit))))[@inline] in
let reset_state_at#1187 =
  fun _t#7291 -> (fun _n#7292 -> (fun _l#7293 -> ((poly_stub_11)@(L(unit)))))[@inline] in
let save_mutation#1190 =
  fun _s#7302 -> (fun _m#7303 -> ((poly_stub_10)@(L(unit))))[@inline] in
let sign#1193 =
  fun _sk#7311 -> (fun _d#7312 -> ((poly_stub_9)@(L(unit))))[@inline] in
let add_account#1194 =
  fun _s#7314 -> (fun _k#7315 -> ((poly_stub_8)@(L(unit))))[@inline] in
let baker_account#1195 =
  fun _p#7317 -> (fun _o#7318 -> ((poly_stub_7)@(L(unit))))[@inline] in
let create_chest#1197 =
  fun _b#7323 -> (fun _n#7324 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#1198 =
  fun _c#7326 -> (fun _n#7327 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#1201 =
  fun _m1#7337 -> (fun _m2#7338 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#1203 =
  fun _c#7343 -> (fun _s#7344 -> (fun _t#7345 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#1205 =
  fun _fn#7351 -> (fun _e#7352 -> (fun _v#7353 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#1206 =
  fun _fn#7355 ->
  (fun _e#7356 ->
   (fun _v#7357 ->
    (fun _s#7358 -> (fun _t#7359 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1053 , toto#215) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#7363 ->
  (let (gen#7369, gen#7370) = gen#7363 in
   let p#7364 = gen#7369 in
   let s#7365 = gen#7370 in
   let s#7366 = ADD(ADD(p#7364 , s#7365) , toto) in
   PAIR(LIST_EMPTY() , s#7366)) in
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
