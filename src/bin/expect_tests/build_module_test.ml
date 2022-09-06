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
    const fb: record[tata -> int , tete -> int , titi -> int , toto -> int] =
      record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main =
      lambda (gen#5( int * int ))( list (operation) * int ) return  match
                                                                     gen#5 with
                                                                     | ( p , s ) ->
                                                                     let sint =
                                                                       ADD
                                                                       (ADD
                                                                        (p ,
                                                                        s) ,
                                                                        toto) in
                                                                     ( LIST_EMPTY
                                                                       () ,
                                                                       s ) |}]

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
    const main: ( unit * string ) -> ( list (operation) * string ) =
      lambda (gen#2( unit * string ))( list (operation) * string ) return
       match gen#2 with
        | ( _#4 , _#3 ) ->
        ( LIST_EMPTY() , CONCAT(Errors.undefined_token , Storage.s) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let get_balance#49 =
  fun _u#4351 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#50 =
  fun _u#4353 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#51 = fun _u#4355 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#52 =
  fun _u#4357 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#53 =
  fun _u#4359 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#54 = fun _u#4361 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#55 = fun _u#4363 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#56 =
  fun _u#4365 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#57 =
  fun _u#4367 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#58 =
  fun _u#4369 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#59 =
  fun kh#4371 -> (({ VOTING_POWER })@(kh#4371))[@inline] in
let implicit_account#61 =
  fun kh#4375 -> (IMPLICIT_ACCOUNT(kh#4375))[@inline] in
let pairing_check#65 =
  fun l#4383 -> (({ PAIRING_CHECK })@(l#4383))[@inline] in
let set_delegate#67 = fun o#4387 -> (SET_DELEGATE(o#4387))[@inline] in
let open_chest#73 =
  fun ck#4403 ->
  (fun c#4404 -> (fun n#4405 -> (OPEN_CHEST(ck#4403 , c#4404 , n#4405))))[@inline] in
let xor#76 = fun l#4414 -> (fun r#4415 -> (XOR(l#4414 , r#4415)))[@inline] in
let shift_left#77 =
  fun l#4417 -> (fun r#4418 -> (LSL(l#4417 , r#4418)))[@inline] in
let shift_right#78 =
  fun l#4420 -> (fun r#4421 -> (LSR(l#4420 , r#4421)))[@inline] in
let length#122 = fun b#4557 -> (({ SIZE })@(b#4557))[@inline] in
let concat#123 =
  fun b1#4559 ->
  (fun b2#4560 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4559 , b2#4560))))[@inline] in
let sub#124 =
  fun s#4562 ->
  (fun l#4563 ->
   (fun b#4564 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4562 ,
                                                                   l#4563) ,
                                                              b#4564)))))[@inline] in
let length#130 = fun b#4578 -> (({ SIZE })@(b#4578))[@inline] in
let concat#131 =
  fun b1#4580 ->
  (fun b2#4581 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4580 , b2#4581))))[@inline] in
let sub#132 =
  fun s#4583 ->
  (fun l#4584 ->
   (fun b#4585 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4583 ,
                                                                   l#4584) ,
                                                              b#4585)))))[@inline] in
let blake2b#133 = fun b#4587 -> (({ BLAKE2B })@(b#4587))[@inline] in
let sha256#134 = fun b#4589 -> (({ SHA256 })@(b#4589))[@inline] in
let sha512#135 = fun b#4591 -> (({ SHA512 })@(b#4591))[@inline] in
let sha3#136 = fun b#4593 -> (({ SHA3 })@(b#4593))[@inline] in
let keccak#137 = fun b#4595 -> (({ KECCAK })@(b#4595))[@inline] in
let hash_key#138 = fun k#4597 -> (({ HASH_KEY })@(k#4597))[@inline] in
let check#139 =
  fun k#4599 ->
  (fun s#4600 ->
   (fun b#4601 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4599 , s#4600) ,
                                                   b#4601)))))[@inline] in
let assert#140 =
  fun b#4603 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4603))[@inline] in
let abs#143 = fun i#4609 -> (({ ABS })@(i#4609))[@inline] in
let is_nat#144 = fun i#4611 -> (({ ISNAT })@(i#4611))[@inline] in
let true#145 = TRUE()[@inline] in
let false#146 = FALSE()[@inline] in
let unit#147 = UNIT()[@inline] in
let assert_with_error#150 =
  fun b#4619 ->
  (fun s#4620 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4619 , s#4620))))[@inline] in
let poly_stub_273 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_272 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_271 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_270 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_269 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_268 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_267 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_266 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_265 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_264 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_263 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_262 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_261 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_260 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_259 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_258 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_257 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_256 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_255 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_254 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_253 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_252 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_251 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_250 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_249 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_248 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_247 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_246 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_245 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_244 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_243 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_242 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_241 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_240 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_239 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_238 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_237 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_236 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let poly_stub_235 = fun x#4631 -> (({ FAILWITH })@(x#4631))[@inline] in
let get_total_voting_power#158 =
  fun _u#4640 -> ((poly_stub_273)@(L(unit)))[@inline] in
let set_source#161 = fun _a#4646 -> ((poly_stub_272)@(L(unit)))[@inline] in
let get_storage_of_address#162 =
  fun _a#4648 -> ((poly_stub_271)@(L(unit)))[@inline] in
let get_balance#163 = fun _a#4650 -> ((poly_stub_270)@(L(unit)))[@inline] in
let print#164 = fun _v#4652 -> ((poly_stub_269)@(L(unit)))[@inline] in
let eprint#165 = fun _v#4654 -> ((poly_stub_268)@(L(unit)))[@inline] in
let get_voting_power#166 =
  fun _kh#4656 -> ((poly_stub_267)@(L(unit)))[@inline] in
let nth_bootstrap_contract#167 =
  fun _i#4658 -> ((poly_stub_266)@(L(unit)))[@inline] in
let nth_bootstrap_account#168 =
  fun _i#4660 -> ((poly_stub_265)@(L(unit)))[@inline] in
let get_bootstrap_account#169 =
  fun _n#4662 -> ((poly_stub_264)@(L(unit)))[@inline] in
let last_originations#171 =
  fun _u#4666 -> ((poly_stub_263)@(L(unit)))[@inline] in
let new_account#173 = fun _u#4670 -> ((poly_stub_262)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#175 =
  fun _n#4674 -> ((poly_stub_261)@(L(unit)))[@inline] in
let register_delegate#177 =
  fun _kh#4678 -> ((poly_stub_260)@(L(unit)))[@inline] in
let register_constant#178 =
  fun _m#4680 -> ((poly_stub_259)@(L(unit)))[@inline] in
let constant_to_michelson_program#180 =
  fun _s#4684 -> ((poly_stub_258)@(L(unit)))[@inline] in
let restore_context#181 =
  fun _u#4686 -> ((poly_stub_257)@(L(unit)))[@inline] in
let save_context#182 = fun _u#4688 -> ((poly_stub_256)@(L(unit)))[@inline] in
let drop_context#183 = fun _u#4690 -> ((poly_stub_255)@(L(unit)))[@inline] in
let set_baker_policy#186 =
  fun _bp#4696 -> ((poly_stub_254)@(L(unit)))[@inline] in
let set_baker#187 = fun _a#4698 -> ((poly_stub_253)@(L(unit)))[@inline] in
let size#188 = fun _c#4700 -> ((poly_stub_252)@(L(unit)))[@inline] in
let read_contract_from_file#190 =
  fun _fn#4704 -> ((poly_stub_251)@(L(unit)))[@inline] in
let chr#191 = fun _n#4706 -> ((poly_stub_250)@(L(unit)))[@inline] in
let nl#192 = L("NEWLINE")[@inline] in
let println#193 = fun _v#4709 -> ((poly_stub_249)@(L(unit)))[@inline] in
let transfer#194 =
  fun _a#4711 ->
  (fun _s#4712 -> (fun _t#4713 -> ((poly_stub_248)@(L(unit)))))[@inline] in
let transfer_exn#195 =
  fun _a#4715 ->
  (fun _s#4716 -> (fun _t#4717 -> ((poly_stub_247)@(L(unit)))))[@inline] in
let reset_state#197 =
  fun _n#4721 -> (fun _l#4722 -> ((poly_stub_246)@(L(unit))))[@inline] in
let reset_state_at#198 =
  fun _t#4724 ->
  (fun _n#4725 -> (fun _l#4726 -> ((poly_stub_245)@(L(unit)))))[@inline] in
let save_mutation#201 =
  fun _s#4735 -> (fun _m#4736 -> ((poly_stub_244)@(L(unit))))[@inline] in
let sign#204 =
  fun _sk#4744 -> (fun _d#4745 -> ((poly_stub_243)@(L(unit))))[@inline] in
let add_account#205 =
  fun _s#4747 -> (fun _k#4748 -> ((poly_stub_242)@(L(unit))))[@inline] in
let baker_account#206 =
  fun _p#4750 -> (fun _o#4751 -> ((poly_stub_241)@(L(unit))))[@inline] in
let create_chest#208 =
  fun _b#4756 -> (fun _n#4757 -> ((poly_stub_240)@(L(unit))))[@inline] in
let create_chest_key#209 =
  fun _c#4759 -> (fun _n#4760 -> ((poly_stub_239)@(L(unit))))[@inline] in
let michelson_equal#212 =
  fun _m1#4770 -> (fun _m2#4771 -> ((poly_stub_238)@(L(unit))))[@inline] in
let originate_contract#214 =
  fun _c#4776 ->
  (fun _s#4777 -> (fun _t#4778 -> ((poly_stub_237)@(L(unit)))))[@inline] in
let compile_contract_from_file#216 =
  fun _fn#4784 ->
  (fun _e#4785 -> (fun _v#4786 -> ((poly_stub_236)@(L(unit)))))[@inline] in
let originate_from_file#217 =
  fun _fn#4788 ->
  (fun _e#4789 ->
   (fun _v#4790 ->
    (fun _s#4791 -> (fun _t#4792 -> ((poly_stub_235)@(L(unit)))))))[@inline] in
let toto#218 = L(1) in
let get_balance#219 =
  fun _u#4795 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#220 =
  fun _u#4797 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#221 = fun _u#4799 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#222 =
  fun _u#4801 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#223 =
  fun _u#4803 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#224 = fun _u#4805 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#225 = fun _u#4807 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#226 =
  fun _u#4809 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#227 =
  fun _u#4811 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#228 =
  fun _u#4813 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#229 =
  fun kh#4815 -> (({ VOTING_POWER })@(kh#4815))[@inline] in
let implicit_account#231 =
  fun kh#4819 -> (IMPLICIT_ACCOUNT(kh#4819))[@inline] in
let pairing_check#235 =
  fun l#4827 -> (({ PAIRING_CHECK })@(l#4827))[@inline] in
let set_delegate#237 = fun o#4831 -> (SET_DELEGATE(o#4831))[@inline] in
let open_chest#243 =
  fun ck#4847 ->
  (fun c#4848 -> (fun n#4849 -> (OPEN_CHEST(ck#4847 , c#4848 , n#4849))))[@inline] in
let xor#246 =
  fun l#4858 -> (fun r#4859 -> (XOR(l#4858 , r#4859)))[@inline] in
let shift_left#247 =
  fun l#4861 -> (fun r#4862 -> (LSL(l#4861 , r#4862)))[@inline] in
let shift_right#248 =
  fun l#4864 -> (fun r#4865 -> (LSR(l#4864 , r#4865)))[@inline] in
let length#292 = fun b#5001 -> (({ SIZE })@(b#5001))[@inline] in
let concat#293 =
  fun b1#5003 ->
  (fun b2#5004 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5003 , b2#5004))))[@inline] in
let sub#294 =
  fun s#5006 ->
  (fun l#5007 ->
   (fun b#5008 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5006 ,
                                                                   l#5007) ,
                                                              b#5008)))))[@inline] in
let length#300 = fun b#5022 -> (({ SIZE })@(b#5022))[@inline] in
let concat#301 =
  fun b1#5024 ->
  (fun b2#5025 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5024 , b2#5025))))[@inline] in
let sub#302 =
  fun s#5027 ->
  (fun l#5028 ->
   (fun b#5029 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5027 ,
                                                                   l#5028) ,
                                                              b#5029)))))[@inline] in
let blake2b#303 = fun b#5031 -> (({ BLAKE2B })@(b#5031))[@inline] in
let sha256#304 = fun b#5033 -> (({ SHA256 })@(b#5033))[@inline] in
let sha512#305 = fun b#5035 -> (({ SHA512 })@(b#5035))[@inline] in
let sha3#306 = fun b#5037 -> (({ SHA3 })@(b#5037))[@inline] in
let keccak#307 = fun b#5039 -> (({ KECCAK })@(b#5039))[@inline] in
let hash_key#308 = fun k#5041 -> (({ HASH_KEY })@(k#5041))[@inline] in
let check#309 =
  fun k#5043 ->
  (fun s#5044 ->
   (fun b#5045 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5043 , s#5044) ,
                                                   b#5045)))))[@inline] in
let assert#310 =
  fun b#5047 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5047))[@inline] in
let abs#313 = fun i#5053 -> (({ ABS })@(i#5053))[@inline] in
let is_nat#314 = fun i#5055 -> (({ ISNAT })@(i#5055))[@inline] in
let true#315 = TRUE()[@inline] in
let false#316 = FALSE()[@inline] in
let unit#317 = UNIT()[@inline] in
let assert_with_error#320 =
  fun b#5063 ->
  (fun s#5064 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5063 , s#5064))))[@inline] in
let poly_stub_234 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_233 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_232 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_231 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_230 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_229 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_228 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_227 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_226 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_225 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_224 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_223 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_222 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_221 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_220 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_219 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_218 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_217 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_216 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_215 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_214 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_213 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_212 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_211 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_210 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_209 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_208 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_207 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_206 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_205 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_204 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_203 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_202 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_201 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_200 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_199 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_198 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_197 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let poly_stub_196 = fun x#5075 -> (({ FAILWITH })@(x#5075))[@inline] in
let get_total_voting_power#328 =
  fun _u#5084 -> ((poly_stub_234)@(L(unit)))[@inline] in
let set_source#331 = fun _a#5090 -> ((poly_stub_233)@(L(unit)))[@inline] in
let get_storage_of_address#332 =
  fun _a#5092 -> ((poly_stub_232)@(L(unit)))[@inline] in
let get_balance#333 = fun _a#5094 -> ((poly_stub_231)@(L(unit)))[@inline] in
let print#334 = fun _v#5096 -> ((poly_stub_230)@(L(unit)))[@inline] in
let eprint#335 = fun _v#5098 -> ((poly_stub_229)@(L(unit)))[@inline] in
let get_voting_power#336 =
  fun _kh#5100 -> ((poly_stub_228)@(L(unit)))[@inline] in
let nth_bootstrap_contract#337 =
  fun _i#5102 -> ((poly_stub_227)@(L(unit)))[@inline] in
let nth_bootstrap_account#338 =
  fun _i#5104 -> ((poly_stub_226)@(L(unit)))[@inline] in
let get_bootstrap_account#339 =
  fun _n#5106 -> ((poly_stub_225)@(L(unit)))[@inline] in
let last_originations#341 =
  fun _u#5110 -> ((poly_stub_224)@(L(unit)))[@inline] in
let new_account#343 = fun _u#5114 -> ((poly_stub_223)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#345 =
  fun _n#5118 -> ((poly_stub_222)@(L(unit)))[@inline] in
let register_delegate#347 =
  fun _kh#5122 -> ((poly_stub_221)@(L(unit)))[@inline] in
let register_constant#348 =
  fun _m#5124 -> ((poly_stub_220)@(L(unit)))[@inline] in
let constant_to_michelson_program#350 =
  fun _s#5128 -> ((poly_stub_219)@(L(unit)))[@inline] in
let restore_context#351 =
  fun _u#5130 -> ((poly_stub_218)@(L(unit)))[@inline] in
let save_context#352 = fun _u#5132 -> ((poly_stub_217)@(L(unit)))[@inline] in
let drop_context#353 = fun _u#5134 -> ((poly_stub_216)@(L(unit)))[@inline] in
let set_baker_policy#356 =
  fun _bp#5140 -> ((poly_stub_215)@(L(unit)))[@inline] in
let set_baker#357 = fun _a#5142 -> ((poly_stub_214)@(L(unit)))[@inline] in
let size#358 = fun _c#5144 -> ((poly_stub_213)@(L(unit)))[@inline] in
let read_contract_from_file#360 =
  fun _fn#5148 -> ((poly_stub_212)@(L(unit)))[@inline] in
let chr#361 = fun _n#5150 -> ((poly_stub_211)@(L(unit)))[@inline] in
let nl#362 = L("NEWLINE")[@inline] in
let println#363 = fun _v#5153 -> ((poly_stub_210)@(L(unit)))[@inline] in
let transfer#364 =
  fun _a#5155 ->
  (fun _s#5156 -> (fun _t#5157 -> ((poly_stub_209)@(L(unit)))))[@inline] in
let transfer_exn#365 =
  fun _a#5159 ->
  (fun _s#5160 -> (fun _t#5161 -> ((poly_stub_208)@(L(unit)))))[@inline] in
let reset_state#367 =
  fun _n#5165 -> (fun _l#5166 -> ((poly_stub_207)@(L(unit))))[@inline] in
let reset_state_at#368 =
  fun _t#5168 ->
  (fun _n#5169 -> (fun _l#5170 -> ((poly_stub_206)@(L(unit)))))[@inline] in
let save_mutation#371 =
  fun _s#5179 -> (fun _m#5180 -> ((poly_stub_205)@(L(unit))))[@inline] in
let sign#374 =
  fun _sk#5188 -> (fun _d#5189 -> ((poly_stub_204)@(L(unit))))[@inline] in
let add_account#375 =
  fun _s#5191 -> (fun _k#5192 -> ((poly_stub_203)@(L(unit))))[@inline] in
let baker_account#376 =
  fun _p#5194 -> (fun _o#5195 -> ((poly_stub_202)@(L(unit))))[@inline] in
let create_chest#378 =
  fun _b#5200 -> (fun _n#5201 -> ((poly_stub_201)@(L(unit))))[@inline] in
let create_chest_key#379 =
  fun _c#5203 -> (fun _n#5204 -> ((poly_stub_200)@(L(unit))))[@inline] in
let michelson_equal#382 =
  fun _m1#5214 -> (fun _m2#5215 -> ((poly_stub_199)@(L(unit))))[@inline] in
let originate_contract#384 =
  fun _c#5220 ->
  (fun _s#5221 -> (fun _t#5222 -> ((poly_stub_198)@(L(unit)))))[@inline] in
let compile_contract_from_file#386 =
  fun _fn#5228 ->
  (fun _e#5229 -> (fun _v#5230 -> ((poly_stub_197)@(L(unit)))))[@inline] in
let originate_from_file#387 =
  fun _fn#5232 ->
  (fun _e#5233 ->
   (fun _v#5234 ->
    (fun _s#5235 -> (fun _t#5236 -> ((poly_stub_196)@(L(unit)))))))[@inline] in
let toto#388 = L(32) in
let titi#389 = ADD(toto#218 , L(42)) in
let f#390 =
  fun gen#5240 ->
  (let (gen#7472, gen#7473) = gen#5240 in
   let gen#5241 = gen#7472 in
   let x#5242 = gen#7473 in
   let x#5243 = ADD(ADD(x#5242 , toto#218) , titi#389) in
   PAIR(LIST_EMPTY() , x#5243)) in
let get_balance#391 =
  fun _u#5245 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#392 =
  fun _u#5247 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#393 = fun _u#5249 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#394 =
  fun _u#5251 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#395 =
  fun _u#5253 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#396 = fun _u#5255 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#397 = fun _u#5257 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#398 =
  fun _u#5259 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#399 =
  fun _u#5261 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#400 =
  fun _u#5263 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#401 =
  fun kh#5265 -> (({ VOTING_POWER })@(kh#5265))[@inline] in
let implicit_account#403 =
  fun kh#5269 -> (IMPLICIT_ACCOUNT(kh#5269))[@inline] in
let pairing_check#407 =
  fun l#5277 -> (({ PAIRING_CHECK })@(l#5277))[@inline] in
let set_delegate#409 = fun o#5281 -> (SET_DELEGATE(o#5281))[@inline] in
let open_chest#415 =
  fun ck#5297 ->
  (fun c#5298 -> (fun n#5299 -> (OPEN_CHEST(ck#5297 , c#5298 , n#5299))))[@inline] in
let xor#418 =
  fun l#5308 -> (fun r#5309 -> (XOR(l#5308 , r#5309)))[@inline] in
let shift_left#419 =
  fun l#5311 -> (fun r#5312 -> (LSL(l#5311 , r#5312)))[@inline] in
let shift_right#420 =
  fun l#5314 -> (fun r#5315 -> (LSR(l#5314 , r#5315)))[@inline] in
let length#464 = fun b#5451 -> (({ SIZE })@(b#5451))[@inline] in
let concat#465 =
  fun b1#5453 ->
  (fun b2#5454 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5453 , b2#5454))))[@inline] in
let sub#466 =
  fun s#5456 ->
  (fun l#5457 ->
   (fun b#5458 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5456 ,
                                                                   l#5457) ,
                                                              b#5458)))))[@inline] in
let length#472 = fun b#5472 -> (({ SIZE })@(b#5472))[@inline] in
let concat#473 =
  fun b1#5474 ->
  (fun b2#5475 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5474 , b2#5475))))[@inline] in
let sub#474 =
  fun s#5477 ->
  (fun l#5478 ->
   (fun b#5479 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5477 ,
                                                                   l#5478) ,
                                                              b#5479)))))[@inline] in
let blake2b#475 = fun b#5481 -> (({ BLAKE2B })@(b#5481))[@inline] in
let sha256#476 = fun b#5483 -> (({ SHA256 })@(b#5483))[@inline] in
let sha512#477 = fun b#5485 -> (({ SHA512 })@(b#5485))[@inline] in
let sha3#478 = fun b#5487 -> (({ SHA3 })@(b#5487))[@inline] in
let keccak#479 = fun b#5489 -> (({ KECCAK })@(b#5489))[@inline] in
let hash_key#480 = fun k#5491 -> (({ HASH_KEY })@(k#5491))[@inline] in
let check#481 =
  fun k#5493 ->
  (fun s#5494 ->
   (fun b#5495 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5493 , s#5494) ,
                                                   b#5495)))))[@inline] in
let assert#482 =
  fun b#5497 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5497))[@inline] in
let abs#485 = fun i#5503 -> (({ ABS })@(i#5503))[@inline] in
let is_nat#486 = fun i#5505 -> (({ ISNAT })@(i#5505))[@inline] in
let true#487 = TRUE()[@inline] in
let false#488 = FALSE()[@inline] in
let unit#489 = UNIT()[@inline] in
let assert_with_error#492 =
  fun b#5513 ->
  (fun s#5514 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5513 , s#5514))))[@inline] in
let poly_stub_195 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_194 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_193 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_192 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_191 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_190 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_189 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_188 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_187 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_186 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_185 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_184 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_183 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_182 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_181 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_180 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_179 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_178 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_177 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_176 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_175 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_174 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_173 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_172 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_171 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_170 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_169 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_168 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_167 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_166 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_165 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_164 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_163 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_162 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_161 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_160 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_159 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_158 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let poly_stub_157 = fun x#5525 -> (({ FAILWITH })@(x#5525))[@inline] in
let get_total_voting_power#500 =
  fun _u#5534 -> ((poly_stub_195)@(L(unit)))[@inline] in
let set_source#503 = fun _a#5540 -> ((poly_stub_194)@(L(unit)))[@inline] in
let get_storage_of_address#504 =
  fun _a#5542 -> ((poly_stub_193)@(L(unit)))[@inline] in
let get_balance#505 = fun _a#5544 -> ((poly_stub_192)@(L(unit)))[@inline] in
let print#506 = fun _v#5546 -> ((poly_stub_191)@(L(unit)))[@inline] in
let eprint#507 = fun _v#5548 -> ((poly_stub_190)@(L(unit)))[@inline] in
let get_voting_power#508 =
  fun _kh#5550 -> ((poly_stub_189)@(L(unit)))[@inline] in
let nth_bootstrap_contract#509 =
  fun _i#5552 -> ((poly_stub_188)@(L(unit)))[@inline] in
let nth_bootstrap_account#510 =
  fun _i#5554 -> ((poly_stub_187)@(L(unit)))[@inline] in
let get_bootstrap_account#511 =
  fun _n#5556 -> ((poly_stub_186)@(L(unit)))[@inline] in
let last_originations#513 =
  fun _u#5560 -> ((poly_stub_185)@(L(unit)))[@inline] in
let new_account#515 = fun _u#5564 -> ((poly_stub_184)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#517 =
  fun _n#5568 -> ((poly_stub_183)@(L(unit)))[@inline] in
let register_delegate#519 =
  fun _kh#5572 -> ((poly_stub_182)@(L(unit)))[@inline] in
let register_constant#520 =
  fun _m#5574 -> ((poly_stub_181)@(L(unit)))[@inline] in
let constant_to_michelson_program#522 =
  fun _s#5578 -> ((poly_stub_180)@(L(unit)))[@inline] in
let restore_context#523 =
  fun _u#5580 -> ((poly_stub_179)@(L(unit)))[@inline] in
let save_context#524 = fun _u#5582 -> ((poly_stub_178)@(L(unit)))[@inline] in
let drop_context#525 = fun _u#5584 -> ((poly_stub_177)@(L(unit)))[@inline] in
let set_baker_policy#528 =
  fun _bp#5590 -> ((poly_stub_176)@(L(unit)))[@inline] in
let set_baker#529 = fun _a#5592 -> ((poly_stub_175)@(L(unit)))[@inline] in
let size#530 = fun _c#5594 -> ((poly_stub_174)@(L(unit)))[@inline] in
let read_contract_from_file#532 =
  fun _fn#5598 -> ((poly_stub_173)@(L(unit)))[@inline] in
let chr#533 = fun _n#5600 -> ((poly_stub_172)@(L(unit)))[@inline] in
let nl#534 = L("NEWLINE")[@inline] in
let println#535 = fun _v#5603 -> ((poly_stub_171)@(L(unit)))[@inline] in
let transfer#536 =
  fun _a#5605 ->
  (fun _s#5606 -> (fun _t#5607 -> ((poly_stub_170)@(L(unit)))))[@inline] in
let transfer_exn#537 =
  fun _a#5609 ->
  (fun _s#5610 -> (fun _t#5611 -> ((poly_stub_169)@(L(unit)))))[@inline] in
let reset_state#539 =
  fun _n#5615 -> (fun _l#5616 -> ((poly_stub_168)@(L(unit))))[@inline] in
let reset_state_at#540 =
  fun _t#5618 ->
  (fun _n#5619 -> (fun _l#5620 -> ((poly_stub_167)@(L(unit)))))[@inline] in
let save_mutation#543 =
  fun _s#5629 -> (fun _m#5630 -> ((poly_stub_166)@(L(unit))))[@inline] in
let sign#546 =
  fun _sk#5638 -> (fun _d#5639 -> ((poly_stub_165)@(L(unit))))[@inline] in
let add_account#547 =
  fun _s#5641 -> (fun _k#5642 -> ((poly_stub_164)@(L(unit))))[@inline] in
let baker_account#548 =
  fun _p#5644 -> (fun _o#5645 -> ((poly_stub_163)@(L(unit))))[@inline] in
let create_chest#550 =
  fun _b#5650 -> (fun _n#5651 -> ((poly_stub_162)@(L(unit))))[@inline] in
let create_chest_key#551 =
  fun _c#5653 -> (fun _n#5654 -> ((poly_stub_161)@(L(unit))))[@inline] in
let michelson_equal#554 =
  fun _m1#5664 -> (fun _m2#5665 -> ((poly_stub_160)@(L(unit))))[@inline] in
let originate_contract#556 =
  fun _c#5670 ->
  (fun _s#5671 -> (fun _t#5672 -> ((poly_stub_159)@(L(unit)))))[@inline] in
let compile_contract_from_file#558 =
  fun _fn#5678 ->
  (fun _e#5679 -> (fun _v#5680 -> ((poly_stub_158)@(L(unit)))))[@inline] in
let originate_from_file#559 =
  fun _fn#5682 ->
  (fun _e#5683 ->
   (fun _v#5684 ->
    (fun _s#5685 -> (fun _t#5686 -> ((poly_stub_157)@(L(unit)))))))[@inline] in
let toto#560 = L(44) in
let get_balance#561 =
  fun _u#5689 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#562 =
  fun _u#5691 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#563 = fun _u#5693 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#564 =
  fun _u#5695 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#565 =
  fun _u#5697 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#566 = fun _u#5699 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#567 = fun _u#5701 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#568 =
  fun _u#5703 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#569 =
  fun _u#5705 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#570 =
  fun _u#5707 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#571 =
  fun kh#5709 -> (({ VOTING_POWER })@(kh#5709))[@inline] in
let implicit_account#573 =
  fun kh#5713 -> (IMPLICIT_ACCOUNT(kh#5713))[@inline] in
let pairing_check#577 =
  fun l#5721 -> (({ PAIRING_CHECK })@(l#5721))[@inline] in
let set_delegate#579 = fun o#5725 -> (SET_DELEGATE(o#5725))[@inline] in
let open_chest#585 =
  fun ck#5741 ->
  (fun c#5742 -> (fun n#5743 -> (OPEN_CHEST(ck#5741 , c#5742 , n#5743))))[@inline] in
let xor#588 =
  fun l#5752 -> (fun r#5753 -> (XOR(l#5752 , r#5753)))[@inline] in
let shift_left#589 =
  fun l#5755 -> (fun r#5756 -> (LSL(l#5755 , r#5756)))[@inline] in
let shift_right#590 =
  fun l#5758 -> (fun r#5759 -> (LSR(l#5758 , r#5759)))[@inline] in
let length#634 = fun b#5895 -> (({ SIZE })@(b#5895))[@inline] in
let concat#635 =
  fun b1#5897 ->
  (fun b2#5898 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5897 , b2#5898))))[@inline] in
let sub#636 =
  fun s#5900 ->
  (fun l#5901 ->
   (fun b#5902 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5900 ,
                                                                   l#5901) ,
                                                              b#5902)))))[@inline] in
let length#642 = fun b#5916 -> (({ SIZE })@(b#5916))[@inline] in
let concat#643 =
  fun b1#5918 ->
  (fun b2#5919 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5918 , b2#5919))))[@inline] in
let sub#644 =
  fun s#5921 ->
  (fun l#5922 ->
   (fun b#5923 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5921 ,
                                                                   l#5922) ,
                                                              b#5923)))))[@inline] in
let blake2b#645 = fun b#5925 -> (({ BLAKE2B })@(b#5925))[@inline] in
let sha256#646 = fun b#5927 -> (({ SHA256 })@(b#5927))[@inline] in
let sha512#647 = fun b#5929 -> (({ SHA512 })@(b#5929))[@inline] in
let sha3#648 = fun b#5931 -> (({ SHA3 })@(b#5931))[@inline] in
let keccak#649 = fun b#5933 -> (({ KECCAK })@(b#5933))[@inline] in
let hash_key#650 = fun k#5935 -> (({ HASH_KEY })@(k#5935))[@inline] in
let check#651 =
  fun k#5937 ->
  (fun s#5938 ->
   (fun b#5939 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5937 , s#5938) ,
                                                   b#5939)))))[@inline] in
let assert#652 =
  fun b#5941 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5941))[@inline] in
let abs#655 = fun i#5947 -> (({ ABS })@(i#5947))[@inline] in
let is_nat#656 = fun i#5949 -> (({ ISNAT })@(i#5949))[@inline] in
let true#657 = TRUE()[@inline] in
let false#658 = FALSE()[@inline] in
let unit#659 = UNIT()[@inline] in
let assert_with_error#662 =
  fun b#5957 ->
  (fun s#5958 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5957 , s#5958))))[@inline] in
let poly_stub_156 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_155 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_154 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_153 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_152 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_151 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_150 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_149 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_148 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_147 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_146 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_145 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_144 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_143 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_142 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_141 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_140 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_139 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_138 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_137 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_136 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_135 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_134 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_133 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_132 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_131 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_130 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_129 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_128 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_127 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_126 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_125 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_124 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_123 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_122 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_121 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_120 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_119 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let poly_stub_118 = fun x#5969 -> (({ FAILWITH })@(x#5969))[@inline] in
let get_total_voting_power#670 =
  fun _u#5978 -> ((poly_stub_156)@(L(unit)))[@inline] in
let set_source#673 = fun _a#5984 -> ((poly_stub_155)@(L(unit)))[@inline] in
let get_storage_of_address#674 =
  fun _a#5986 -> ((poly_stub_154)@(L(unit)))[@inline] in
let get_balance#675 = fun _a#5988 -> ((poly_stub_153)@(L(unit)))[@inline] in
let print#676 = fun _v#5990 -> ((poly_stub_152)@(L(unit)))[@inline] in
let eprint#677 = fun _v#5992 -> ((poly_stub_151)@(L(unit)))[@inline] in
let get_voting_power#678 =
  fun _kh#5994 -> ((poly_stub_150)@(L(unit)))[@inline] in
let nth_bootstrap_contract#679 =
  fun _i#5996 -> ((poly_stub_149)@(L(unit)))[@inline] in
let nth_bootstrap_account#680 =
  fun _i#5998 -> ((poly_stub_148)@(L(unit)))[@inline] in
let get_bootstrap_account#681 =
  fun _n#6000 -> ((poly_stub_147)@(L(unit)))[@inline] in
let last_originations#683 =
  fun _u#6004 -> ((poly_stub_146)@(L(unit)))[@inline] in
let new_account#685 = fun _u#6008 -> ((poly_stub_145)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#687 =
  fun _n#6012 -> ((poly_stub_144)@(L(unit)))[@inline] in
let register_delegate#689 =
  fun _kh#6016 -> ((poly_stub_143)@(L(unit)))[@inline] in
let register_constant#690 =
  fun _m#6018 -> ((poly_stub_142)@(L(unit)))[@inline] in
let constant_to_michelson_program#692 =
  fun _s#6022 -> ((poly_stub_141)@(L(unit)))[@inline] in
let restore_context#693 =
  fun _u#6024 -> ((poly_stub_140)@(L(unit)))[@inline] in
let save_context#694 = fun _u#6026 -> ((poly_stub_139)@(L(unit)))[@inline] in
let drop_context#695 = fun _u#6028 -> ((poly_stub_138)@(L(unit)))[@inline] in
let set_baker_policy#698 =
  fun _bp#6034 -> ((poly_stub_137)@(L(unit)))[@inline] in
let set_baker#699 = fun _a#6036 -> ((poly_stub_136)@(L(unit)))[@inline] in
let size#700 = fun _c#6038 -> ((poly_stub_135)@(L(unit)))[@inline] in
let read_contract_from_file#702 =
  fun _fn#6042 -> ((poly_stub_134)@(L(unit)))[@inline] in
let chr#703 = fun _n#6044 -> ((poly_stub_133)@(L(unit)))[@inline] in
let nl#704 = L("NEWLINE")[@inline] in
let println#705 = fun _v#6047 -> ((poly_stub_132)@(L(unit)))[@inline] in
let transfer#706 =
  fun _a#6049 ->
  (fun _s#6050 -> (fun _t#6051 -> ((poly_stub_131)@(L(unit)))))[@inline] in
let transfer_exn#707 =
  fun _a#6053 ->
  (fun _s#6054 -> (fun _t#6055 -> ((poly_stub_130)@(L(unit)))))[@inline] in
let reset_state#709 =
  fun _n#6059 -> (fun _l#6060 -> ((poly_stub_129)@(L(unit))))[@inline] in
let reset_state_at#710 =
  fun _t#6062 ->
  (fun _n#6063 -> (fun _l#6064 -> ((poly_stub_128)@(L(unit)))))[@inline] in
let save_mutation#713 =
  fun _s#6073 -> (fun _m#6074 -> ((poly_stub_127)@(L(unit))))[@inline] in
let sign#716 =
  fun _sk#6082 -> (fun _d#6083 -> ((poly_stub_126)@(L(unit))))[@inline] in
let add_account#717 =
  fun _s#6085 -> (fun _k#6086 -> ((poly_stub_125)@(L(unit))))[@inline] in
let baker_account#718 =
  fun _p#6088 -> (fun _o#6089 -> ((poly_stub_124)@(L(unit))))[@inline] in
let create_chest#720 =
  fun _b#6094 -> (fun _n#6095 -> ((poly_stub_123)@(L(unit))))[@inline] in
let create_chest_key#721 =
  fun _c#6097 -> (fun _n#6098 -> ((poly_stub_122)@(L(unit))))[@inline] in
let michelson_equal#724 =
  fun _m1#6108 -> (fun _m2#6109 -> ((poly_stub_121)@(L(unit))))[@inline] in
let originate_contract#726 =
  fun _c#6114 ->
  (fun _s#6115 -> (fun _t#6116 -> ((poly_stub_120)@(L(unit)))))[@inline] in
let compile_contract_from_file#728 =
  fun _fn#6122 ->
  (fun _e#6123 -> (fun _v#6124 -> ((poly_stub_119)@(L(unit)))))[@inline] in
let originate_from_file#729 =
  fun _fn#6126 ->
  (fun _e#6127 ->
   (fun _v#6128 ->
    (fun _s#6129 -> (fun _t#6130 -> ((poly_stub_118)@(L(unit)))))))[@inline] in
let toto#730 = L(43) in
let get_balance#731 =
  fun _u#6133 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#732 =
  fun _u#6135 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#733 = fun _u#6137 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#734 =
  fun _u#6139 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#735 =
  fun _u#6141 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#736 = fun _u#6143 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#737 = fun _u#6145 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#738 =
  fun _u#6147 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#739 =
  fun _u#6149 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#740 =
  fun _u#6151 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#741 =
  fun kh#6153 -> (({ VOTING_POWER })@(kh#6153))[@inline] in
let implicit_account#743 =
  fun kh#6157 -> (IMPLICIT_ACCOUNT(kh#6157))[@inline] in
let pairing_check#747 =
  fun l#6165 -> (({ PAIRING_CHECK })@(l#6165))[@inline] in
let set_delegate#749 = fun o#6169 -> (SET_DELEGATE(o#6169))[@inline] in
let open_chest#755 =
  fun ck#6185 ->
  (fun c#6186 -> (fun n#6187 -> (OPEN_CHEST(ck#6185 , c#6186 , n#6187))))[@inline] in
let xor#758 =
  fun l#6196 -> (fun r#6197 -> (XOR(l#6196 , r#6197)))[@inline] in
let shift_left#759 =
  fun l#6199 -> (fun r#6200 -> (LSL(l#6199 , r#6200)))[@inline] in
let shift_right#760 =
  fun l#6202 -> (fun r#6203 -> (LSR(l#6202 , r#6203)))[@inline] in
let length#804 = fun b#6339 -> (({ SIZE })@(b#6339))[@inline] in
let concat#805 =
  fun b1#6341 ->
  (fun b2#6342 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6341 , b2#6342))))[@inline] in
let sub#806 =
  fun s#6344 ->
  (fun l#6345 ->
   (fun b#6346 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6344 ,
                                                                   l#6345) ,
                                                              b#6346)))))[@inline] in
let length#812 = fun b#6360 -> (({ SIZE })@(b#6360))[@inline] in
let concat#813 =
  fun b1#6362 ->
  (fun b2#6363 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6362 , b2#6363))))[@inline] in
let sub#814 =
  fun s#6365 ->
  (fun l#6366 ->
   (fun b#6367 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6365 ,
                                                                   l#6366) ,
                                                              b#6367)))))[@inline] in
let blake2b#815 = fun b#6369 -> (({ BLAKE2B })@(b#6369))[@inline] in
let sha256#816 = fun b#6371 -> (({ SHA256 })@(b#6371))[@inline] in
let sha512#817 = fun b#6373 -> (({ SHA512 })@(b#6373))[@inline] in
let sha3#818 = fun b#6375 -> (({ SHA3 })@(b#6375))[@inline] in
let keccak#819 = fun b#6377 -> (({ KECCAK })@(b#6377))[@inline] in
let hash_key#820 = fun k#6379 -> (({ HASH_KEY })@(k#6379))[@inline] in
let check#821 =
  fun k#6381 ->
  (fun s#6382 ->
   (fun b#6383 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6381 , s#6382) ,
                                                   b#6383)))))[@inline] in
let assert#822 =
  fun b#6385 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6385))[@inline] in
let abs#825 = fun i#6391 -> (({ ABS })@(i#6391))[@inline] in
let is_nat#826 = fun i#6393 -> (({ ISNAT })@(i#6393))[@inline] in
let true#827 = TRUE()[@inline] in
let false#828 = FALSE()[@inline] in
let unit#829 = UNIT()[@inline] in
let assert_with_error#832 =
  fun b#6401 ->
  (fun s#6402 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6401 , s#6402))))[@inline] in
let poly_stub_117 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_116 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_115 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_114 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_113 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_112 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_111 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_110 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_109 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_108 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_107 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_106 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_105 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_104 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_103 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_102 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_101 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_100 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_99 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_98 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_97 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_96 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_95 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_94 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_93 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_92 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_91 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_90 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_89 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_88 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_87 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_86 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_85 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_84 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_83 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_82 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_81 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_80 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let poly_stub_79 = fun x#6413 -> (({ FAILWITH })@(x#6413))[@inline] in
let get_total_voting_power#840 =
  fun _u#6422 -> ((poly_stub_117)@(L(unit)))[@inline] in
let set_source#843 = fun _a#6428 -> ((poly_stub_116)@(L(unit)))[@inline] in
let get_storage_of_address#844 =
  fun _a#6430 -> ((poly_stub_115)@(L(unit)))[@inline] in
let get_balance#845 = fun _a#6432 -> ((poly_stub_114)@(L(unit)))[@inline] in
let print#846 = fun _v#6434 -> ((poly_stub_113)@(L(unit)))[@inline] in
let eprint#847 = fun _v#6436 -> ((poly_stub_112)@(L(unit)))[@inline] in
let get_voting_power#848 =
  fun _kh#6438 -> ((poly_stub_111)@(L(unit)))[@inline] in
let nth_bootstrap_contract#849 =
  fun _i#6440 -> ((poly_stub_110)@(L(unit)))[@inline] in
let nth_bootstrap_account#850 =
  fun _i#6442 -> ((poly_stub_109)@(L(unit)))[@inline] in
let get_bootstrap_account#851 =
  fun _n#6444 -> ((poly_stub_108)@(L(unit)))[@inline] in
let last_originations#853 =
  fun _u#6448 -> ((poly_stub_107)@(L(unit)))[@inline] in
let new_account#855 = fun _u#6452 -> ((poly_stub_106)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#857 =
  fun _n#6456 -> ((poly_stub_105)@(L(unit)))[@inline] in
let register_delegate#859 =
  fun _kh#6460 -> ((poly_stub_104)@(L(unit)))[@inline] in
let register_constant#860 =
  fun _m#6462 -> ((poly_stub_103)@(L(unit)))[@inline] in
let constant_to_michelson_program#862 =
  fun _s#6466 -> ((poly_stub_102)@(L(unit)))[@inline] in
let restore_context#863 =
  fun _u#6468 -> ((poly_stub_101)@(L(unit)))[@inline] in
let save_context#864 = fun _u#6470 -> ((poly_stub_100)@(L(unit)))[@inline] in
let drop_context#865 = fun _u#6472 -> ((poly_stub_99)@(L(unit)))[@inline] in
let set_baker_policy#868 =
  fun _bp#6478 -> ((poly_stub_98)@(L(unit)))[@inline] in
let set_baker#869 = fun _a#6480 -> ((poly_stub_97)@(L(unit)))[@inline] in
let size#870 = fun _c#6482 -> ((poly_stub_96)@(L(unit)))[@inline] in
let read_contract_from_file#872 =
  fun _fn#6486 -> ((poly_stub_95)@(L(unit)))[@inline] in
let chr#873 = fun _n#6488 -> ((poly_stub_94)@(L(unit)))[@inline] in
let nl#874 = L("NEWLINE")[@inline] in
let println#875 = fun _v#6491 -> ((poly_stub_93)@(L(unit)))[@inline] in
let transfer#876 =
  fun _a#6493 -> (fun _s#6494 -> (fun _t#6495 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#877 =
  fun _a#6497 -> (fun _s#6498 -> (fun _t#6499 -> ((poly_stub_91)@(L(unit)))))[@inline] in
let reset_state#879 =
  fun _n#6503 -> (fun _l#6504 -> ((poly_stub_90)@(L(unit))))[@inline] in
let reset_state_at#880 =
  fun _t#6506 -> (fun _n#6507 -> (fun _l#6508 -> ((poly_stub_89)@(L(unit)))))[@inline] in
let save_mutation#883 =
  fun _s#6517 -> (fun _m#6518 -> ((poly_stub_88)@(L(unit))))[@inline] in
let sign#886 =
  fun _sk#6526 -> (fun _d#6527 -> ((poly_stub_87)@(L(unit))))[@inline] in
let add_account#887 =
  fun _s#6529 -> (fun _k#6530 -> ((poly_stub_86)@(L(unit))))[@inline] in
let baker_account#888 =
  fun _p#6532 -> (fun _o#6533 -> ((poly_stub_85)@(L(unit))))[@inline] in
let create_chest#890 =
  fun _b#6538 -> (fun _n#6539 -> ((poly_stub_84)@(L(unit))))[@inline] in
let create_chest_key#891 =
  fun _c#6541 -> (fun _n#6542 -> ((poly_stub_83)@(L(unit))))[@inline] in
let michelson_equal#894 =
  fun _m1#6552 -> (fun _m2#6553 -> ((poly_stub_82)@(L(unit))))[@inline] in
let originate_contract#896 =
  fun _c#6558 -> (fun _s#6559 -> (fun _t#6560 -> ((poly_stub_81)@(L(unit)))))[@inline] in
let compile_contract_from_file#898 =
  fun _fn#6566 ->
  (fun _e#6567 -> (fun _v#6568 -> ((poly_stub_80)@(L(unit)))))[@inline] in
let originate_from_file#899 =
  fun _fn#6570 ->
  (fun _e#6571 ->
   (fun _v#6572 ->
    (fun _s#6573 -> (fun _t#6574 -> ((poly_stub_79)@(L(unit)))))))[@inline] in
let tata#900 = ADD(toto#218 , titi#389) in
let foo#901 = (f#390)@(PAIR(L(unit) , L(3))) in
let get_balance#902 =
  fun _u#6578 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#903 =
  fun _u#6580 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#904 = fun _u#6582 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#905 =
  fun _u#6584 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#906 =
  fun _u#6586 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#907 = fun _u#6588 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#908 = fun _u#6590 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#909 =
  fun _u#6592 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#910 =
  fun _u#6594 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#911 =
  fun _u#6596 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#912 =
  fun kh#6598 -> (({ VOTING_POWER })@(kh#6598))[@inline] in
let implicit_account#914 =
  fun kh#6602 -> (IMPLICIT_ACCOUNT(kh#6602))[@inline] in
let pairing_check#918 =
  fun l#6610 -> (({ PAIRING_CHECK })@(l#6610))[@inline] in
let set_delegate#920 = fun o#6614 -> (SET_DELEGATE(o#6614))[@inline] in
let open_chest#926 =
  fun ck#6630 ->
  (fun c#6631 -> (fun n#6632 -> (OPEN_CHEST(ck#6630 , c#6631 , n#6632))))[@inline] in
let xor#929 =
  fun l#6641 -> (fun r#6642 -> (XOR(l#6641 , r#6642)))[@inline] in
let shift_left#930 =
  fun l#6644 -> (fun r#6645 -> (LSL(l#6644 , r#6645)))[@inline] in
let shift_right#931 =
  fun l#6647 -> (fun r#6648 -> (LSR(l#6647 , r#6648)))[@inline] in
let length#975 = fun b#6784 -> (({ SIZE })@(b#6784))[@inline] in
let concat#976 =
  fun b1#6786 ->
  (fun b2#6787 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6786 , b2#6787))))[@inline] in
let sub#977 =
  fun s#6789 ->
  (fun l#6790 ->
   (fun b#6791 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6789 ,
                                                                   l#6790) ,
                                                              b#6791)))))[@inline] in
let length#983 = fun b#6805 -> (({ SIZE })@(b#6805))[@inline] in
let concat#984 =
  fun b1#6807 ->
  (fun b2#6808 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6807 , b2#6808))))[@inline] in
let sub#985 =
  fun s#6810 ->
  (fun l#6811 ->
   (fun b#6812 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6810 ,
                                                                   l#6811) ,
                                                              b#6812)))))[@inline] in
let blake2b#986 = fun b#6814 -> (({ BLAKE2B })@(b#6814))[@inline] in
let sha256#987 = fun b#6816 -> (({ SHA256 })@(b#6816))[@inline] in
let sha512#988 = fun b#6818 -> (({ SHA512 })@(b#6818))[@inline] in
let sha3#989 = fun b#6820 -> (({ SHA3 })@(b#6820))[@inline] in
let keccak#990 = fun b#6822 -> (({ KECCAK })@(b#6822))[@inline] in
let hash_key#991 = fun k#6824 -> (({ HASH_KEY })@(k#6824))[@inline] in
let check#992 =
  fun k#6826 ->
  (fun s#6827 ->
   (fun b#6828 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6826 , s#6827) ,
                                                   b#6828)))))[@inline] in
let assert#993 =
  fun b#6830 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6830))[@inline] in
let abs#996 = fun i#6836 -> (({ ABS })@(i#6836))[@inline] in
let is_nat#997 = fun i#6838 -> (({ ISNAT })@(i#6838))[@inline] in
let true#998 = TRUE()[@inline] in
let false#999 = FALSE()[@inline] in
let unit#1000 = UNIT()[@inline] in
let assert_with_error#1003 =
  fun b#6846 ->
  (fun s#6847 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6846 , s#6847))))[@inline] in
let poly_stub_78 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_77 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_76 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_75 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_74 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_73 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_72 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_71 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_70 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_69 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_68 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_67 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_66 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_65 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_64 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_63 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_62 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_61 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_60 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_59 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_58 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_57 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_56 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_55 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_54 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_53 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_52 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_51 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_50 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_49 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_48 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_47 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_46 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_45 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_44 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_43 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_42 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_41 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let poly_stub_40 = fun x#6858 -> (({ FAILWITH })@(x#6858))[@inline] in
let get_total_voting_power#1011 =
  fun _u#6867 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#1014 = fun _a#6873 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#1015 =
  fun _a#6875 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#1016 = fun _a#6877 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#1017 = fun _v#6879 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#1018 = fun _v#6881 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#1019 =
  fun _kh#6883 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1020 =
  fun _i#6885 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#1021 =
  fun _i#6887 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#1022 =
  fun _n#6889 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#1024 =
  fun _u#6893 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#1026 = fun _u#6897 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1028 =
  fun _n#6901 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#1030 =
  fun _kh#6905 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#1031 =
  fun _m#6907 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#1033 =
  fun _s#6911 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#1034 =
  fun _u#6913 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#1035 = fun _u#6915 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#1036 = fun _u#6917 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#1039 =
  fun _bp#6923 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#1040 = fun _a#6925 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#1041 = fun _c#6927 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#1043 =
  fun _fn#6931 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#1044 = fun _n#6933 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#1045 = L("NEWLINE")[@inline] in
let println#1046 = fun _v#6936 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#1047 =
  fun _a#6938 -> (fun _s#6939 -> (fun _t#6940 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#1048 =
  fun _a#6942 -> (fun _s#6943 -> (fun _t#6944 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#1050 =
  fun _n#6948 -> (fun _l#6949 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#1051 =
  fun _t#6951 -> (fun _n#6952 -> (fun _l#6953 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#1054 =
  fun _s#6962 -> (fun _m#6963 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#1057 =
  fun _sk#6971 -> (fun _d#6972 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#1058 =
  fun _s#6974 -> (fun _k#6975 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#1059 =
  fun _p#6977 -> (fun _o#6978 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#1061 =
  fun _b#6983 -> (fun _n#6984 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#1062 =
  fun _c#6986 -> (fun _n#6987 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#1065 =
  fun _m1#6997 -> (fun _m2#6998 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#1067 =
  fun _c#7003 -> (fun _s#7004 -> (fun _t#7005 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#1069 =
  fun _fn#7011 ->
  (fun _e#7012 -> (fun _v#7013 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#1070 =
  fun _fn#7015 ->
  (fun _e#7016 ->
   (fun _v#7017 ->
    (fun _s#7018 -> (fun _t#7019 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let toto#1071 = L(10) in
let foo#1072 = L("bar") in
let get_balance#1073 =
  fun _u#7023 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1074 =
  fun _u#7025 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1075 = fun _u#7027 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1076 =
  fun _u#7029 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1077 =
  fun _u#7031 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1078 =
  fun _u#7033 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1079 = fun _u#7035 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1080 =
  fun _u#7037 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1081 =
  fun _u#7039 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1082 =
  fun _u#7041 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1083 =
  fun kh#7043 -> (({ VOTING_POWER })@(kh#7043))[@inline] in
let implicit_account#1085 =
  fun kh#7047 -> (IMPLICIT_ACCOUNT(kh#7047))[@inline] in
let pairing_check#1089 =
  fun l#7055 -> (({ PAIRING_CHECK })@(l#7055))[@inline] in
let set_delegate#1091 = fun o#7059 -> (SET_DELEGATE(o#7059))[@inline] in
let open_chest#1097 =
  fun ck#7075 ->
  (fun c#7076 -> (fun n#7077 -> (OPEN_CHEST(ck#7075 , c#7076 , n#7077))))[@inline] in
let xor#1100 =
  fun l#7086 -> (fun r#7087 -> (XOR(l#7086 , r#7087)))[@inline] in
let shift_left#1101 =
  fun l#7089 -> (fun r#7090 -> (LSL(l#7089 , r#7090)))[@inline] in
let shift_right#1102 =
  fun l#7092 -> (fun r#7093 -> (LSR(l#7092 , r#7093)))[@inline] in
let length#1146 = fun b#7229 -> (({ SIZE })@(b#7229))[@inline] in
let concat#1147 =
  fun b1#7231 ->
  (fun b2#7232 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7231 , b2#7232))))[@inline] in
let sub#1148 =
  fun s#7234 ->
  (fun l#7235 ->
   (fun b#7236 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7234 ,
                                                                   l#7235) ,
                                                              b#7236)))))[@inline] in
let length#1154 = fun b#7250 -> (({ SIZE })@(b#7250))[@inline] in
let concat#1155 =
  fun b1#7252 ->
  (fun b2#7253 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7252 , b2#7253))))[@inline] in
let sub#1156 =
  fun s#7255 ->
  (fun l#7256 ->
   (fun b#7257 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7255 ,
                                                                   l#7256) ,
                                                              b#7257)))))[@inline] in
let blake2b#1157 = fun b#7259 -> (({ BLAKE2B })@(b#7259))[@inline] in
let sha256#1158 = fun b#7261 -> (({ SHA256 })@(b#7261))[@inline] in
let sha512#1159 = fun b#7263 -> (({ SHA512 })@(b#7263))[@inline] in
let sha3#1160 = fun b#7265 -> (({ SHA3 })@(b#7265))[@inline] in
let keccak#1161 = fun b#7267 -> (({ KECCAK })@(b#7267))[@inline] in
let hash_key#1162 = fun k#7269 -> (({ HASH_KEY })@(k#7269))[@inline] in
let check#1163 =
  fun k#7271 ->
  (fun s#7272 ->
   (fun b#7273 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7271 , s#7272) ,
                                                   b#7273)))))[@inline] in
let assert =
  fun b#7275 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7275))[@inline] in
let abs = fun i#7281 -> (({ ABS })@(i#7281))[@inline] in
let is_nat = fun i#7283 -> (({ ISNAT })@(i#7283))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#7291 ->
  (fun s#7292 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7291 , s#7292))))[@inline] in
let poly_stub_39 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_38 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_37 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_36 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_35 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_34 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_33 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_32 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_31 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_30 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_29 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_28 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_27 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_26 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_25 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_24 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_23 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_22 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_21 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_20 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_19 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_18 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_17 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_16 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_15 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_14 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_13 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_12 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_11 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_10 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_9 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_8 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_7 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_6 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_5 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_4 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_3 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_2 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let poly_stub_1 = fun x#7303 -> (({ FAILWITH })@(x#7303))[@inline] in
let get_total_voting_power#1168 =
  fun _u#7312 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#1171 = fun _a#7318 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#1172 =
  fun _a#7320 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#1173 = fun _a#7322 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#1174 = fun _v#7324 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#1175 = fun _v#7326 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#1176 =
  fun _kh#7328 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1177 =
  fun _i#7330 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#1178 =
  fun _i#7332 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#1179 =
  fun _n#7334 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#1181 =
  fun _u#7338 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#1183 = fun _u#7342 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1185 =
  fun _n#7346 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#1187 =
  fun _kh#7350 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#1188 =
  fun _m#7352 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#1190 =
  fun _s#7356 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#1191 =
  fun _u#7358 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#1192 = fun _u#7360 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#1193 = fun _u#7362 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#1196 =
  fun _bp#7368 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#1197 = fun _a#7370 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#1198 = fun _c#7372 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#1200 =
  fun _fn#7376 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1201 = fun _n#7378 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#1202 = L("NEWLINE")[@inline] in
let println#1203 = fun _v#7381 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#1204 =
  fun _a#7383 -> (fun _s#7384 -> (fun _t#7385 -> ((poly_stub_14)@(L(unit)))))[@inline] in
let transfer_exn#1205 =
  fun _a#7387 -> (fun _s#7388 -> (fun _t#7389 -> ((poly_stub_13)@(L(unit)))))[@inline] in
let reset_state#1207 =
  fun _n#7393 -> (fun _l#7394 -> ((poly_stub_12)@(L(unit))))[@inline] in
let reset_state_at#1208 =
  fun _t#7396 -> (fun _n#7397 -> (fun _l#7398 -> ((poly_stub_11)@(L(unit)))))[@inline] in
let save_mutation#1211 =
  fun _s#7407 -> (fun _m#7408 -> ((poly_stub_10)@(L(unit))))[@inline] in
let sign#1214 =
  fun _sk#7416 -> (fun _d#7417 -> ((poly_stub_9)@(L(unit))))[@inline] in
let add_account#1215 =
  fun _s#7419 -> (fun _k#7420 -> ((poly_stub_8)@(L(unit))))[@inline] in
let baker_account#1216 =
  fun _p#7422 -> (fun _o#7423 -> ((poly_stub_7)@(L(unit))))[@inline] in
let create_chest#1218 =
  fun _b#7428 -> (fun _n#7429 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#1219 =
  fun _c#7431 -> (fun _n#7432 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#1222 =
  fun _m1#7442 -> (fun _m2#7443 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#1224 =
  fun _c#7448 -> (fun _s#7449 -> (fun _t#7450 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#1226 =
  fun _fn#7456 -> (fun _e#7457 -> (fun _v#7458 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#1227 =
  fun _fn#7460 ->
  (fun _e#7461 ->
   (fun _v#7462 ->
    (fun _s#7463 -> (fun _t#7464 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1071 , toto#218) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#7468 ->
  (let (gen#7474, gen#7475) = gen#7468 in
   let p#7469 = gen#7474 in
   let s#7470 = gen#7475 in
   let s#7471 = ADD(ADD(p#7469 , s#7470) , toto) in
   PAIR(LIST_EMPTY() , s#7471)) in
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
