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
    const toto : int = ADD(E.toto , C.B.A.toto)
    const fb : record[tata -> int , tete -> int , titi -> int , toto -> int] =
      record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main : ( int * int ) -> ( list (operation) * int ) =
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
    const main : ( unit * string ) -> ( list (operation) * string ) =
      lambda (gen#2( unit * string ))( list (operation) * string ) return
       match gen#2 with
        | ( _#4 , _#3 ) ->
        ( LIST_EMPTY() , CONCAT(Errors.undefined_token , Storage.s) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let get_balance#70 =
  fun _u#4393 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#71 =
  fun _u#4395 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#72 = fun _u#4397 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#73 =
  fun _u#4399 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#74 =
  fun _u#4401 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#75 = fun _u#4403 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#76 = fun _u#4405 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#77 =
  fun _u#4407 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#78 =
  fun _u#4409 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#79 =
  fun _u#4411 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#80 =
  fun kh#4413 -> (({ VOTING_POWER })@(kh#4413))[@inline] in
let implicit_account#82 =
  fun kh#4417 -> (IMPLICIT_ACCOUNT(kh#4417))[@inline] in
let pairing_check#86 =
  fun l#4425 -> (({ PAIRING_CHECK })@(l#4425))[@inline] in
let set_delegate#88 = fun o#4429 -> (SET_DELEGATE(o#4429))[@inline] in
let open_chest#94 =
  fun ck#4445 ->
  (fun c#4446 -> (fun n#4447 -> (OPEN_CHEST(ck#4445 , c#4446 , n#4447))))[@inline] in
let xor#97 = fun l#4456 -> (fun r#4457 -> (XOR(l#4456 , r#4457)))[@inline] in
let shift_left#98 =
  fun l#4459 -> (fun r#4460 -> (LSL(l#4459 , r#4460)))[@inline] in
let shift_right#99 =
  fun l#4462 -> (fun r#4463 -> (LSR(l#4462 , r#4463)))[@inline] in
let length#143 = fun b#4601 -> (({ SIZE })@(b#4601))[@inline] in
let concat#144 =
  fun b1#4603 ->
  (fun b2#4604 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4603 , b2#4604))))[@inline] in
let sub#145 =
  fun s#4606 ->
  (fun l#4607 ->
   (fun b#4608 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4606 ,
                                                                   l#4607) ,
                                                              b#4608)))))[@inline] in
let length#151 = fun b#4623 -> (({ SIZE })@(b#4623))[@inline] in
let concat#152 =
  fun b1#4625 ->
  (fun b2#4626 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4625 , b2#4626))))[@inline] in
let sub#153 =
  fun s#4628 ->
  (fun l#4629 ->
   (fun b#4630 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4628 ,
                                                                   l#4629) ,
                                                              b#4630)))))[@inline] in
let blake2b#154 = fun b#4632 -> (({ BLAKE2B })@(b#4632))[@inline] in
let sha256#155 = fun b#4634 -> (({ SHA256 })@(b#4634))[@inline] in
let sha512#156 = fun b#4636 -> (({ SHA512 })@(b#4636))[@inline] in
let sha3#157 = fun b#4638 -> (({ SHA3 })@(b#4638))[@inline] in
let keccak#158 = fun b#4640 -> (({ KECCAK })@(b#4640))[@inline] in
let hash_key#159 = fun k#4642 -> (({ HASH_KEY })@(k#4642))[@inline] in
let check#160 =
  fun k#4644 ->
  (fun s#4645 ->
   (fun b#4646 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4644 , s#4645) ,
                                                   b#4646)))))[@inline] in
let assert#161 =
  fun b#4648 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4648))[@inline] in
let abs#164 = fun i#4654 -> (({ ABS })@(i#4654))[@inline] in
let is_nat#165 = fun i#4656 -> (({ ISNAT })@(i#4656))[@inline] in
let true#166 = TRUE()[@inline] in
let false#167 = FALSE()[@inline] in
let unit#168 = UNIT()[@inline] in
let assert_with_error#171 =
  fun b#4664 ->
  (fun s#4665 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4664 , s#4665))))[@inline] in
let poly_stub_273 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_272 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_271 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_270 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_269 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_268 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_267 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_266 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_265 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_264 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_263 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_262 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_261 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_260 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_259 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_258 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_257 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_256 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_255 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_254 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_253 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_252 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_251 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_250 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_249 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_248 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_247 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_246 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_245 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_244 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_243 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_242 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_241 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_240 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_239 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_238 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_237 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_236 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let poly_stub_235 = fun x#4676 -> (({ FAILWITH })@(x#4676))[@inline] in
let get_total_voting_power#179 =
  fun _u#4685 -> ((poly_stub_273)@(L(unit)))[@inline] in
let set_source#182 = fun _a#4691 -> ((poly_stub_272)@(L(unit)))[@inline] in
let get_storage_of_address#183 =
  fun _a#4693 -> ((poly_stub_271)@(L(unit)))[@inline] in
let get_balance#184 = fun _a#4695 -> ((poly_stub_270)@(L(unit)))[@inline] in
let print#185 = fun _v#4697 -> ((poly_stub_269)@(L(unit)))[@inline] in
let eprint#186 = fun _v#4699 -> ((poly_stub_268)@(L(unit)))[@inline] in
let get_voting_power#187 =
  fun _kh#4701 -> ((poly_stub_267)@(L(unit)))[@inline] in
let nth_bootstrap_contract#188 =
  fun _i#4703 -> ((poly_stub_266)@(L(unit)))[@inline] in
let nth_bootstrap_account#189 =
  fun _i#4705 -> ((poly_stub_265)@(L(unit)))[@inline] in
let get_bootstrap_account#190 =
  fun _n#4707 -> ((poly_stub_264)@(L(unit)))[@inline] in
let last_originations#192 =
  fun _u#4711 -> ((poly_stub_263)@(L(unit)))[@inline] in
let new_account#194 = fun _u#4715 -> ((poly_stub_262)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#196 =
  fun _n#4719 -> ((poly_stub_261)@(L(unit)))[@inline] in
let register_delegate#198 =
  fun _kh#4723 -> ((poly_stub_260)@(L(unit)))[@inline] in
let register_constant#199 =
  fun _m#4725 -> ((poly_stub_259)@(L(unit)))[@inline] in
let constant_to_michelson_program#201 =
  fun _s#4729 -> ((poly_stub_258)@(L(unit)))[@inline] in
let restore_context#202 =
  fun _u#4731 -> ((poly_stub_257)@(L(unit)))[@inline] in
let save_context#203 = fun _u#4733 -> ((poly_stub_256)@(L(unit)))[@inline] in
let drop_context#204 = fun _u#4735 -> ((poly_stub_255)@(L(unit)))[@inline] in
let set_baker_policy#207 =
  fun _bp#4741 -> ((poly_stub_254)@(L(unit)))[@inline] in
let set_baker#208 = fun _a#4743 -> ((poly_stub_253)@(L(unit)))[@inline] in
let size#209 = fun _c#4745 -> ((poly_stub_252)@(L(unit)))[@inline] in
let read_contract_from_file#211 =
  fun _fn#4749 -> ((poly_stub_251)@(L(unit)))[@inline] in
let chr#212 = fun _n#4751 -> ((poly_stub_250)@(L(unit)))[@inline] in
let nl#213 = L("NEWLINE")[@inline] in
let println#214 = fun _v#4754 -> ((poly_stub_249)@(L(unit)))[@inline] in
let transfer#215 =
  fun _a#4756 ->
  (fun _s#4757 -> (fun _t#4758 -> ((poly_stub_248)@(L(unit)))))[@inline] in
let transfer_exn#216 =
  fun _a#4760 ->
  (fun _s#4761 -> (fun _t#4762 -> ((poly_stub_247)@(L(unit)))))[@inline] in
let reset_state#218 =
  fun _n#4766 -> (fun _l#4767 -> ((poly_stub_246)@(L(unit))))[@inline] in
let reset_state_at#219 =
  fun _t#4769 ->
  (fun _n#4770 -> (fun _l#4771 -> ((poly_stub_245)@(L(unit)))))[@inline] in
let save_mutation#222 =
  fun _s#4780 -> (fun _m#4781 -> ((poly_stub_244)@(L(unit))))[@inline] in
let sign#225 =
  fun _sk#4789 -> (fun _d#4790 -> ((poly_stub_243)@(L(unit))))[@inline] in
let add_account#226 =
  fun _s#4792 -> (fun _k#4793 -> ((poly_stub_242)@(L(unit))))[@inline] in
let baker_account#227 =
  fun _p#4795 -> (fun _o#4796 -> ((poly_stub_241)@(L(unit))))[@inline] in
let create_chest#229 =
  fun _b#4801 -> (fun _n#4802 -> ((poly_stub_240)@(L(unit))))[@inline] in
let create_chest_key#230 =
  fun _c#4804 -> (fun _n#4805 -> ((poly_stub_239)@(L(unit))))[@inline] in
let michelson_equal#233 =
  fun _m1#4815 -> (fun _m2#4816 -> ((poly_stub_238)@(L(unit))))[@inline] in
let originate_contract#235 =
  fun _c#4821 ->
  (fun _s#4822 -> (fun _t#4823 -> ((poly_stub_237)@(L(unit)))))[@inline] in
let compile_contract_from_file#237 =
  fun _fn#4829 ->
  (fun _e#4830 -> (fun _v#4831 -> ((poly_stub_236)@(L(unit)))))[@inline] in
let originate_from_file#238 =
  fun _fn#4833 ->
  (fun _e#4834 ->
   (fun _v#4835 ->
    (fun _s#4836 -> (fun _t#4837 -> ((poly_stub_235)@(L(unit)))))))[@inline] in
let toto#239 = L(1) in
let get_balance#240 =
  fun _u#4840 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#241 =
  fun _u#4842 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#242 = fun _u#4844 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#243 =
  fun _u#4846 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#244 =
  fun _u#4848 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#245 = fun _u#4850 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#246 = fun _u#4852 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#247 =
  fun _u#4854 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#248 =
  fun _u#4856 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#249 =
  fun _u#4858 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#250 =
  fun kh#4860 -> (({ VOTING_POWER })@(kh#4860))[@inline] in
let implicit_account#252 =
  fun kh#4864 -> (IMPLICIT_ACCOUNT(kh#4864))[@inline] in
let pairing_check#256 =
  fun l#4872 -> (({ PAIRING_CHECK })@(l#4872))[@inline] in
let set_delegate#258 = fun o#4876 -> (SET_DELEGATE(o#4876))[@inline] in
let open_chest#264 =
  fun ck#4892 ->
  (fun c#4893 -> (fun n#4894 -> (OPEN_CHEST(ck#4892 , c#4893 , n#4894))))[@inline] in
let xor#267 =
  fun l#4903 -> (fun r#4904 -> (XOR(l#4903 , r#4904)))[@inline] in
let shift_left#268 =
  fun l#4906 -> (fun r#4907 -> (LSL(l#4906 , r#4907)))[@inline] in
let shift_right#269 =
  fun l#4909 -> (fun r#4910 -> (LSR(l#4909 , r#4910)))[@inline] in
let length#313 = fun b#5048 -> (({ SIZE })@(b#5048))[@inline] in
let concat#314 =
  fun b1#5050 ->
  (fun b2#5051 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5050 , b2#5051))))[@inline] in
let sub#315 =
  fun s#5053 ->
  (fun l#5054 ->
   (fun b#5055 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5053 ,
                                                                   l#5054) ,
                                                              b#5055)))))[@inline] in
let length#321 = fun b#5070 -> (({ SIZE })@(b#5070))[@inline] in
let concat#322 =
  fun b1#5072 ->
  (fun b2#5073 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5072 , b2#5073))))[@inline] in
let sub#323 =
  fun s#5075 ->
  (fun l#5076 ->
   (fun b#5077 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5075 ,
                                                                   l#5076) ,
                                                              b#5077)))))[@inline] in
let blake2b#324 = fun b#5079 -> (({ BLAKE2B })@(b#5079))[@inline] in
let sha256#325 = fun b#5081 -> (({ SHA256 })@(b#5081))[@inline] in
let sha512#326 = fun b#5083 -> (({ SHA512 })@(b#5083))[@inline] in
let sha3#327 = fun b#5085 -> (({ SHA3 })@(b#5085))[@inline] in
let keccak#328 = fun b#5087 -> (({ KECCAK })@(b#5087))[@inline] in
let hash_key#329 = fun k#5089 -> (({ HASH_KEY })@(k#5089))[@inline] in
let check#330 =
  fun k#5091 ->
  (fun s#5092 ->
   (fun b#5093 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5091 , s#5092) ,
                                                   b#5093)))))[@inline] in
let assert#331 =
  fun b#5095 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5095))[@inline] in
let abs#334 = fun i#5101 -> (({ ABS })@(i#5101))[@inline] in
let is_nat#335 = fun i#5103 -> (({ ISNAT })@(i#5103))[@inline] in
let true#336 = TRUE()[@inline] in
let false#337 = FALSE()[@inline] in
let unit#338 = UNIT()[@inline] in
let assert_with_error#341 =
  fun b#5111 ->
  (fun s#5112 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5111 , s#5112))))[@inline] in
let poly_stub_234 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_233 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_232 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_231 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_230 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_229 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_228 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_227 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_226 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_225 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_224 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_223 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_222 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_221 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_220 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_219 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_218 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_217 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_216 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_215 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_214 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_213 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_212 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_211 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_210 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_209 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_208 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_207 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_206 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_205 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_204 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_203 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_202 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_201 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_200 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_199 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_198 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_197 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let poly_stub_196 = fun x#5123 -> (({ FAILWITH })@(x#5123))[@inline] in
let get_total_voting_power#349 =
  fun _u#5132 -> ((poly_stub_234)@(L(unit)))[@inline] in
let set_source#352 = fun _a#5138 -> ((poly_stub_233)@(L(unit)))[@inline] in
let get_storage_of_address#353 =
  fun _a#5140 -> ((poly_stub_232)@(L(unit)))[@inline] in
let get_balance#354 = fun _a#5142 -> ((poly_stub_231)@(L(unit)))[@inline] in
let print#355 = fun _v#5144 -> ((poly_stub_230)@(L(unit)))[@inline] in
let eprint#356 = fun _v#5146 -> ((poly_stub_229)@(L(unit)))[@inline] in
let get_voting_power#357 =
  fun _kh#5148 -> ((poly_stub_228)@(L(unit)))[@inline] in
let nth_bootstrap_contract#358 =
  fun _i#5150 -> ((poly_stub_227)@(L(unit)))[@inline] in
let nth_bootstrap_account#359 =
  fun _i#5152 -> ((poly_stub_226)@(L(unit)))[@inline] in
let get_bootstrap_account#360 =
  fun _n#5154 -> ((poly_stub_225)@(L(unit)))[@inline] in
let last_originations#362 =
  fun _u#5158 -> ((poly_stub_224)@(L(unit)))[@inline] in
let new_account#364 = fun _u#5162 -> ((poly_stub_223)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#366 =
  fun _n#5166 -> ((poly_stub_222)@(L(unit)))[@inline] in
let register_delegate#368 =
  fun _kh#5170 -> ((poly_stub_221)@(L(unit)))[@inline] in
let register_constant#369 =
  fun _m#5172 -> ((poly_stub_220)@(L(unit)))[@inline] in
let constant_to_michelson_program#371 =
  fun _s#5176 -> ((poly_stub_219)@(L(unit)))[@inline] in
let restore_context#372 =
  fun _u#5178 -> ((poly_stub_218)@(L(unit)))[@inline] in
let save_context#373 = fun _u#5180 -> ((poly_stub_217)@(L(unit)))[@inline] in
let drop_context#374 = fun _u#5182 -> ((poly_stub_216)@(L(unit)))[@inline] in
let set_baker_policy#377 =
  fun _bp#5188 -> ((poly_stub_215)@(L(unit)))[@inline] in
let set_baker#378 = fun _a#5190 -> ((poly_stub_214)@(L(unit)))[@inline] in
let size#379 = fun _c#5192 -> ((poly_stub_213)@(L(unit)))[@inline] in
let read_contract_from_file#381 =
  fun _fn#5196 -> ((poly_stub_212)@(L(unit)))[@inline] in
let chr#382 = fun _n#5198 -> ((poly_stub_211)@(L(unit)))[@inline] in
let nl#383 = L("NEWLINE")[@inline] in
let println#384 = fun _v#5201 -> ((poly_stub_210)@(L(unit)))[@inline] in
let transfer#385 =
  fun _a#5203 ->
  (fun _s#5204 -> (fun _t#5205 -> ((poly_stub_209)@(L(unit)))))[@inline] in
let transfer_exn#386 =
  fun _a#5207 ->
  (fun _s#5208 -> (fun _t#5209 -> ((poly_stub_208)@(L(unit)))))[@inline] in
let reset_state#388 =
  fun _n#5213 -> (fun _l#5214 -> ((poly_stub_207)@(L(unit))))[@inline] in
let reset_state_at#389 =
  fun _t#5216 ->
  (fun _n#5217 -> (fun _l#5218 -> ((poly_stub_206)@(L(unit)))))[@inline] in
let save_mutation#392 =
  fun _s#5227 -> (fun _m#5228 -> ((poly_stub_205)@(L(unit))))[@inline] in
let sign#395 =
  fun _sk#5236 -> (fun _d#5237 -> ((poly_stub_204)@(L(unit))))[@inline] in
let add_account#396 =
  fun _s#5239 -> (fun _k#5240 -> ((poly_stub_203)@(L(unit))))[@inline] in
let baker_account#397 =
  fun _p#5242 -> (fun _o#5243 -> ((poly_stub_202)@(L(unit))))[@inline] in
let create_chest#399 =
  fun _b#5248 -> (fun _n#5249 -> ((poly_stub_201)@(L(unit))))[@inline] in
let create_chest_key#400 =
  fun _c#5251 -> (fun _n#5252 -> ((poly_stub_200)@(L(unit))))[@inline] in
let michelson_equal#403 =
  fun _m1#5262 -> (fun _m2#5263 -> ((poly_stub_199)@(L(unit))))[@inline] in
let originate_contract#405 =
  fun _c#5268 ->
  (fun _s#5269 -> (fun _t#5270 -> ((poly_stub_198)@(L(unit)))))[@inline] in
let compile_contract_from_file#407 =
  fun _fn#5276 ->
  (fun _e#5277 -> (fun _v#5278 -> ((poly_stub_197)@(L(unit)))))[@inline] in
let originate_from_file#408 =
  fun _fn#5280 ->
  (fun _e#5281 ->
   (fun _v#5282 ->
    (fun _s#5283 -> (fun _t#5284 -> ((poly_stub_196)@(L(unit)))))))[@inline] in
let toto#409 = L(32) in
let titi#410 = ADD(toto#239 , L(42)) in
let f#411 =
  fun gen#5288 ->
  (let (gen#7535, gen#7536) = gen#5288 in
   let gen#5289 = gen#7535 in
   let x#5290 = gen#7536 in
   let x#5291 = ADD(ADD(x#5290 , toto#239) , titi#410) in
   PAIR(LIST_EMPTY() , x#5291)) in
let get_balance#412 =
  fun _u#5293 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#413 =
  fun _u#5295 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#414 = fun _u#5297 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#415 =
  fun _u#5299 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#416 =
  fun _u#5301 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#417 = fun _u#5303 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#418 = fun _u#5305 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#419 =
  fun _u#5307 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#420 =
  fun _u#5309 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#421 =
  fun _u#5311 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#422 =
  fun kh#5313 -> (({ VOTING_POWER })@(kh#5313))[@inline] in
let implicit_account#424 =
  fun kh#5317 -> (IMPLICIT_ACCOUNT(kh#5317))[@inline] in
let pairing_check#428 =
  fun l#5325 -> (({ PAIRING_CHECK })@(l#5325))[@inline] in
let set_delegate#430 = fun o#5329 -> (SET_DELEGATE(o#5329))[@inline] in
let open_chest#436 =
  fun ck#5345 ->
  (fun c#5346 -> (fun n#5347 -> (OPEN_CHEST(ck#5345 , c#5346 , n#5347))))[@inline] in
let xor#439 =
  fun l#5356 -> (fun r#5357 -> (XOR(l#5356 , r#5357)))[@inline] in
let shift_left#440 =
  fun l#5359 -> (fun r#5360 -> (LSL(l#5359 , r#5360)))[@inline] in
let shift_right#441 =
  fun l#5362 -> (fun r#5363 -> (LSR(l#5362 , r#5363)))[@inline] in
let length#485 = fun b#5501 -> (({ SIZE })@(b#5501))[@inline] in
let concat#486 =
  fun b1#5503 ->
  (fun b2#5504 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5503 , b2#5504))))[@inline] in
let sub#487 =
  fun s#5506 ->
  (fun l#5507 ->
   (fun b#5508 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5506 ,
                                                                   l#5507) ,
                                                              b#5508)))))[@inline] in
let length#493 = fun b#5523 -> (({ SIZE })@(b#5523))[@inline] in
let concat#494 =
  fun b1#5525 ->
  (fun b2#5526 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5525 , b2#5526))))[@inline] in
let sub#495 =
  fun s#5528 ->
  (fun l#5529 ->
   (fun b#5530 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5528 ,
                                                                   l#5529) ,
                                                              b#5530)))))[@inline] in
let blake2b#496 = fun b#5532 -> (({ BLAKE2B })@(b#5532))[@inline] in
let sha256#497 = fun b#5534 -> (({ SHA256 })@(b#5534))[@inline] in
let sha512#498 = fun b#5536 -> (({ SHA512 })@(b#5536))[@inline] in
let sha3#499 = fun b#5538 -> (({ SHA3 })@(b#5538))[@inline] in
let keccak#500 = fun b#5540 -> (({ KECCAK })@(b#5540))[@inline] in
let hash_key#501 = fun k#5542 -> (({ HASH_KEY })@(k#5542))[@inline] in
let check#502 =
  fun k#5544 ->
  (fun s#5545 ->
   (fun b#5546 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5544 , s#5545) ,
                                                   b#5546)))))[@inline] in
let assert#503 =
  fun b#5548 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5548))[@inline] in
let abs#506 = fun i#5554 -> (({ ABS })@(i#5554))[@inline] in
let is_nat#507 = fun i#5556 -> (({ ISNAT })@(i#5556))[@inline] in
let true#508 = TRUE()[@inline] in
let false#509 = FALSE()[@inline] in
let unit#510 = UNIT()[@inline] in
let assert_with_error#513 =
  fun b#5564 ->
  (fun s#5565 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5564 , s#5565))))[@inline] in
let poly_stub_195 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_194 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_193 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_192 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_191 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_190 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_189 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_188 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_187 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_186 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_185 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_184 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_183 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_182 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_181 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_180 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_179 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_178 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_177 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_176 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_175 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_174 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_173 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_172 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_171 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_170 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_169 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_168 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_167 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_166 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_165 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_164 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_163 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_162 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_161 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_160 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_159 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_158 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let poly_stub_157 = fun x#5576 -> (({ FAILWITH })@(x#5576))[@inline] in
let get_total_voting_power#521 =
  fun _u#5585 -> ((poly_stub_195)@(L(unit)))[@inline] in
let set_source#524 = fun _a#5591 -> ((poly_stub_194)@(L(unit)))[@inline] in
let get_storage_of_address#525 =
  fun _a#5593 -> ((poly_stub_193)@(L(unit)))[@inline] in
let get_balance#526 = fun _a#5595 -> ((poly_stub_192)@(L(unit)))[@inline] in
let print#527 = fun _v#5597 -> ((poly_stub_191)@(L(unit)))[@inline] in
let eprint#528 = fun _v#5599 -> ((poly_stub_190)@(L(unit)))[@inline] in
let get_voting_power#529 =
  fun _kh#5601 -> ((poly_stub_189)@(L(unit)))[@inline] in
let nth_bootstrap_contract#530 =
  fun _i#5603 -> ((poly_stub_188)@(L(unit)))[@inline] in
let nth_bootstrap_account#531 =
  fun _i#5605 -> ((poly_stub_187)@(L(unit)))[@inline] in
let get_bootstrap_account#532 =
  fun _n#5607 -> ((poly_stub_186)@(L(unit)))[@inline] in
let last_originations#534 =
  fun _u#5611 -> ((poly_stub_185)@(L(unit)))[@inline] in
let new_account#536 = fun _u#5615 -> ((poly_stub_184)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#538 =
  fun _n#5619 -> ((poly_stub_183)@(L(unit)))[@inline] in
let register_delegate#540 =
  fun _kh#5623 -> ((poly_stub_182)@(L(unit)))[@inline] in
let register_constant#541 =
  fun _m#5625 -> ((poly_stub_181)@(L(unit)))[@inline] in
let constant_to_michelson_program#543 =
  fun _s#5629 -> ((poly_stub_180)@(L(unit)))[@inline] in
let restore_context#544 =
  fun _u#5631 -> ((poly_stub_179)@(L(unit)))[@inline] in
let save_context#545 = fun _u#5633 -> ((poly_stub_178)@(L(unit)))[@inline] in
let drop_context#546 = fun _u#5635 -> ((poly_stub_177)@(L(unit)))[@inline] in
let set_baker_policy#549 =
  fun _bp#5641 -> ((poly_stub_176)@(L(unit)))[@inline] in
let set_baker#550 = fun _a#5643 -> ((poly_stub_175)@(L(unit)))[@inline] in
let size#551 = fun _c#5645 -> ((poly_stub_174)@(L(unit)))[@inline] in
let read_contract_from_file#553 =
  fun _fn#5649 -> ((poly_stub_173)@(L(unit)))[@inline] in
let chr#554 = fun _n#5651 -> ((poly_stub_172)@(L(unit)))[@inline] in
let nl#555 = L("NEWLINE")[@inline] in
let println#556 = fun _v#5654 -> ((poly_stub_171)@(L(unit)))[@inline] in
let transfer#557 =
  fun _a#5656 ->
  (fun _s#5657 -> (fun _t#5658 -> ((poly_stub_170)@(L(unit)))))[@inline] in
let transfer_exn#558 =
  fun _a#5660 ->
  (fun _s#5661 -> (fun _t#5662 -> ((poly_stub_169)@(L(unit)))))[@inline] in
let reset_state#560 =
  fun _n#5666 -> (fun _l#5667 -> ((poly_stub_168)@(L(unit))))[@inline] in
let reset_state_at#561 =
  fun _t#5669 ->
  (fun _n#5670 -> (fun _l#5671 -> ((poly_stub_167)@(L(unit)))))[@inline] in
let save_mutation#564 =
  fun _s#5680 -> (fun _m#5681 -> ((poly_stub_166)@(L(unit))))[@inline] in
let sign#567 =
  fun _sk#5689 -> (fun _d#5690 -> ((poly_stub_165)@(L(unit))))[@inline] in
let add_account#568 =
  fun _s#5692 -> (fun _k#5693 -> ((poly_stub_164)@(L(unit))))[@inline] in
let baker_account#569 =
  fun _p#5695 -> (fun _o#5696 -> ((poly_stub_163)@(L(unit))))[@inline] in
let create_chest#571 =
  fun _b#5701 -> (fun _n#5702 -> ((poly_stub_162)@(L(unit))))[@inline] in
let create_chest_key#572 =
  fun _c#5704 -> (fun _n#5705 -> ((poly_stub_161)@(L(unit))))[@inline] in
let michelson_equal#575 =
  fun _m1#5715 -> (fun _m2#5716 -> ((poly_stub_160)@(L(unit))))[@inline] in
let originate_contract#577 =
  fun _c#5721 ->
  (fun _s#5722 -> (fun _t#5723 -> ((poly_stub_159)@(L(unit)))))[@inline] in
let compile_contract_from_file#579 =
  fun _fn#5729 ->
  (fun _e#5730 -> (fun _v#5731 -> ((poly_stub_158)@(L(unit)))))[@inline] in
let originate_from_file#580 =
  fun _fn#5733 ->
  (fun _e#5734 ->
   (fun _v#5735 ->
    (fun _s#5736 -> (fun _t#5737 -> ((poly_stub_157)@(L(unit)))))))[@inline] in
let toto#581 = L(44) in
let get_balance#582 =
  fun _u#5740 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#583 =
  fun _u#5742 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#584 = fun _u#5744 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#585 =
  fun _u#5746 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#586 =
  fun _u#5748 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#587 = fun _u#5750 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#588 = fun _u#5752 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#589 =
  fun _u#5754 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#590 =
  fun _u#5756 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#591 =
  fun _u#5758 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#592 =
  fun kh#5760 -> (({ VOTING_POWER })@(kh#5760))[@inline] in
let implicit_account#594 =
  fun kh#5764 -> (IMPLICIT_ACCOUNT(kh#5764))[@inline] in
let pairing_check#598 =
  fun l#5772 -> (({ PAIRING_CHECK })@(l#5772))[@inline] in
let set_delegate#600 = fun o#5776 -> (SET_DELEGATE(o#5776))[@inline] in
let open_chest#606 =
  fun ck#5792 ->
  (fun c#5793 -> (fun n#5794 -> (OPEN_CHEST(ck#5792 , c#5793 , n#5794))))[@inline] in
let xor#609 =
  fun l#5803 -> (fun r#5804 -> (XOR(l#5803 , r#5804)))[@inline] in
let shift_left#610 =
  fun l#5806 -> (fun r#5807 -> (LSL(l#5806 , r#5807)))[@inline] in
let shift_right#611 =
  fun l#5809 -> (fun r#5810 -> (LSR(l#5809 , r#5810)))[@inline] in
let length#655 = fun b#5948 -> (({ SIZE })@(b#5948))[@inline] in
let concat#656 =
  fun b1#5950 ->
  (fun b2#5951 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5950 , b2#5951))))[@inline] in
let sub#657 =
  fun s#5953 ->
  (fun l#5954 ->
   (fun b#5955 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5953 ,
                                                                   l#5954) ,
                                                              b#5955)))))[@inline] in
let length#663 = fun b#5970 -> (({ SIZE })@(b#5970))[@inline] in
let concat#664 =
  fun b1#5972 ->
  (fun b2#5973 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5972 , b2#5973))))[@inline] in
let sub#665 =
  fun s#5975 ->
  (fun l#5976 ->
   (fun b#5977 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5975 ,
                                                                   l#5976) ,
                                                              b#5977)))))[@inline] in
let blake2b#666 = fun b#5979 -> (({ BLAKE2B })@(b#5979))[@inline] in
let sha256#667 = fun b#5981 -> (({ SHA256 })@(b#5981))[@inline] in
let sha512#668 = fun b#5983 -> (({ SHA512 })@(b#5983))[@inline] in
let sha3#669 = fun b#5985 -> (({ SHA3 })@(b#5985))[@inline] in
let keccak#670 = fun b#5987 -> (({ KECCAK })@(b#5987))[@inline] in
let hash_key#671 = fun k#5989 -> (({ HASH_KEY })@(k#5989))[@inline] in
let check#672 =
  fun k#5991 ->
  (fun s#5992 ->
   (fun b#5993 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5991 , s#5992) ,
                                                   b#5993)))))[@inline] in
let assert#673 =
  fun b#5995 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5995))[@inline] in
let abs#676 = fun i#6001 -> (({ ABS })@(i#6001))[@inline] in
let is_nat#677 = fun i#6003 -> (({ ISNAT })@(i#6003))[@inline] in
let true#678 = TRUE()[@inline] in
let false#679 = FALSE()[@inline] in
let unit#680 = UNIT()[@inline] in
let assert_with_error#683 =
  fun b#6011 ->
  (fun s#6012 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6011 , s#6012))))[@inline] in
let poly_stub_156 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_155 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_154 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_153 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_152 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_151 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_150 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_149 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_148 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_147 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_146 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_145 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_144 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_143 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_142 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_141 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_140 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_139 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_138 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_137 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_136 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_135 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_134 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_133 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_132 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_131 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_130 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_129 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_128 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_127 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_126 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_125 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_124 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_123 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_122 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_121 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_120 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_119 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let poly_stub_118 = fun x#6023 -> (({ FAILWITH })@(x#6023))[@inline] in
let get_total_voting_power#691 =
  fun _u#6032 -> ((poly_stub_156)@(L(unit)))[@inline] in
let set_source#694 = fun _a#6038 -> ((poly_stub_155)@(L(unit)))[@inline] in
let get_storage_of_address#695 =
  fun _a#6040 -> ((poly_stub_154)@(L(unit)))[@inline] in
let get_balance#696 = fun _a#6042 -> ((poly_stub_153)@(L(unit)))[@inline] in
let print#697 = fun _v#6044 -> ((poly_stub_152)@(L(unit)))[@inline] in
let eprint#698 = fun _v#6046 -> ((poly_stub_151)@(L(unit)))[@inline] in
let get_voting_power#699 =
  fun _kh#6048 -> ((poly_stub_150)@(L(unit)))[@inline] in
let nth_bootstrap_contract#700 =
  fun _i#6050 -> ((poly_stub_149)@(L(unit)))[@inline] in
let nth_bootstrap_account#701 =
  fun _i#6052 -> ((poly_stub_148)@(L(unit)))[@inline] in
let get_bootstrap_account#702 =
  fun _n#6054 -> ((poly_stub_147)@(L(unit)))[@inline] in
let last_originations#704 =
  fun _u#6058 -> ((poly_stub_146)@(L(unit)))[@inline] in
let new_account#706 = fun _u#6062 -> ((poly_stub_145)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#708 =
  fun _n#6066 -> ((poly_stub_144)@(L(unit)))[@inline] in
let register_delegate#710 =
  fun _kh#6070 -> ((poly_stub_143)@(L(unit)))[@inline] in
let register_constant#711 =
  fun _m#6072 -> ((poly_stub_142)@(L(unit)))[@inline] in
let constant_to_michelson_program#713 =
  fun _s#6076 -> ((poly_stub_141)@(L(unit)))[@inline] in
let restore_context#714 =
  fun _u#6078 -> ((poly_stub_140)@(L(unit)))[@inline] in
let save_context#715 = fun _u#6080 -> ((poly_stub_139)@(L(unit)))[@inline] in
let drop_context#716 = fun _u#6082 -> ((poly_stub_138)@(L(unit)))[@inline] in
let set_baker_policy#719 =
  fun _bp#6088 -> ((poly_stub_137)@(L(unit)))[@inline] in
let set_baker#720 = fun _a#6090 -> ((poly_stub_136)@(L(unit)))[@inline] in
let size#721 = fun _c#6092 -> ((poly_stub_135)@(L(unit)))[@inline] in
let read_contract_from_file#723 =
  fun _fn#6096 -> ((poly_stub_134)@(L(unit)))[@inline] in
let chr#724 = fun _n#6098 -> ((poly_stub_133)@(L(unit)))[@inline] in
let nl#725 = L("NEWLINE")[@inline] in
let println#726 = fun _v#6101 -> ((poly_stub_132)@(L(unit)))[@inline] in
let transfer#727 =
  fun _a#6103 ->
  (fun _s#6104 -> (fun _t#6105 -> ((poly_stub_131)@(L(unit)))))[@inline] in
let transfer_exn#728 =
  fun _a#6107 ->
  (fun _s#6108 -> (fun _t#6109 -> ((poly_stub_130)@(L(unit)))))[@inline] in
let reset_state#730 =
  fun _n#6113 -> (fun _l#6114 -> ((poly_stub_129)@(L(unit))))[@inline] in
let reset_state_at#731 =
  fun _t#6116 ->
  (fun _n#6117 -> (fun _l#6118 -> ((poly_stub_128)@(L(unit)))))[@inline] in
let save_mutation#734 =
  fun _s#6127 -> (fun _m#6128 -> ((poly_stub_127)@(L(unit))))[@inline] in
let sign#737 =
  fun _sk#6136 -> (fun _d#6137 -> ((poly_stub_126)@(L(unit))))[@inline] in
let add_account#738 =
  fun _s#6139 -> (fun _k#6140 -> ((poly_stub_125)@(L(unit))))[@inline] in
let baker_account#739 =
  fun _p#6142 -> (fun _o#6143 -> ((poly_stub_124)@(L(unit))))[@inline] in
let create_chest#741 =
  fun _b#6148 -> (fun _n#6149 -> ((poly_stub_123)@(L(unit))))[@inline] in
let create_chest_key#742 =
  fun _c#6151 -> (fun _n#6152 -> ((poly_stub_122)@(L(unit))))[@inline] in
let michelson_equal#745 =
  fun _m1#6162 -> (fun _m2#6163 -> ((poly_stub_121)@(L(unit))))[@inline] in
let originate_contract#747 =
  fun _c#6168 ->
  (fun _s#6169 -> (fun _t#6170 -> ((poly_stub_120)@(L(unit)))))[@inline] in
let compile_contract_from_file#749 =
  fun _fn#6176 ->
  (fun _e#6177 -> (fun _v#6178 -> ((poly_stub_119)@(L(unit)))))[@inline] in
let originate_from_file#750 =
  fun _fn#6180 ->
  (fun _e#6181 ->
   (fun _v#6182 ->
    (fun _s#6183 -> (fun _t#6184 -> ((poly_stub_118)@(L(unit)))))))[@inline] in
let toto#751 = L(43) in
let get_balance#752 =
  fun _u#6187 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#753 =
  fun _u#6189 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#754 = fun _u#6191 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#755 =
  fun _u#6193 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#756 =
  fun _u#6195 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#757 = fun _u#6197 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#758 = fun _u#6199 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#759 =
  fun _u#6201 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#760 =
  fun _u#6203 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#761 =
  fun _u#6205 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#762 =
  fun kh#6207 -> (({ VOTING_POWER })@(kh#6207))[@inline] in
let implicit_account#764 =
  fun kh#6211 -> (IMPLICIT_ACCOUNT(kh#6211))[@inline] in
let pairing_check#768 =
  fun l#6219 -> (({ PAIRING_CHECK })@(l#6219))[@inline] in
let set_delegate#770 = fun o#6223 -> (SET_DELEGATE(o#6223))[@inline] in
let open_chest#776 =
  fun ck#6239 ->
  (fun c#6240 -> (fun n#6241 -> (OPEN_CHEST(ck#6239 , c#6240 , n#6241))))[@inline] in
let xor#779 =
  fun l#6250 -> (fun r#6251 -> (XOR(l#6250 , r#6251)))[@inline] in
let shift_left#780 =
  fun l#6253 -> (fun r#6254 -> (LSL(l#6253 , r#6254)))[@inline] in
let shift_right#781 =
  fun l#6256 -> (fun r#6257 -> (LSR(l#6256 , r#6257)))[@inline] in
let length#825 = fun b#6395 -> (({ SIZE })@(b#6395))[@inline] in
let concat#826 =
  fun b1#6397 ->
  (fun b2#6398 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6397 , b2#6398))))[@inline] in
let sub#827 =
  fun s#6400 ->
  (fun l#6401 ->
   (fun b#6402 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6400 ,
                                                                   l#6401) ,
                                                              b#6402)))))[@inline] in
let length#833 = fun b#6417 -> (({ SIZE })@(b#6417))[@inline] in
let concat#834 =
  fun b1#6419 ->
  (fun b2#6420 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6419 , b2#6420))))[@inline] in
let sub#835 =
  fun s#6422 ->
  (fun l#6423 ->
   (fun b#6424 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6422 ,
                                                                   l#6423) ,
                                                              b#6424)))))[@inline] in
let blake2b#836 = fun b#6426 -> (({ BLAKE2B })@(b#6426))[@inline] in
let sha256#837 = fun b#6428 -> (({ SHA256 })@(b#6428))[@inline] in
let sha512#838 = fun b#6430 -> (({ SHA512 })@(b#6430))[@inline] in
let sha3#839 = fun b#6432 -> (({ SHA3 })@(b#6432))[@inline] in
let keccak#840 = fun b#6434 -> (({ KECCAK })@(b#6434))[@inline] in
let hash_key#841 = fun k#6436 -> (({ HASH_KEY })@(k#6436))[@inline] in
let check#842 =
  fun k#6438 ->
  (fun s#6439 ->
   (fun b#6440 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6438 , s#6439) ,
                                                   b#6440)))))[@inline] in
let assert#843 =
  fun b#6442 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6442))[@inline] in
let abs#846 = fun i#6448 -> (({ ABS })@(i#6448))[@inline] in
let is_nat#847 = fun i#6450 -> (({ ISNAT })@(i#6450))[@inline] in
let true#848 = TRUE()[@inline] in
let false#849 = FALSE()[@inline] in
let unit#850 = UNIT()[@inline] in
let assert_with_error#853 =
  fun b#6458 ->
  (fun s#6459 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6458 , s#6459))))[@inline] in
let poly_stub_117 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_116 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_115 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_114 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_113 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_112 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_111 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_110 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_109 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_108 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_107 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_106 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_105 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_104 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_103 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_102 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_101 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_100 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_99 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_98 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_97 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_96 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_95 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_94 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_93 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_92 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_91 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_90 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_89 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_88 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_87 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_86 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_85 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_84 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_83 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_82 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_81 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_80 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let poly_stub_79 = fun x#6470 -> (({ FAILWITH })@(x#6470))[@inline] in
let get_total_voting_power#861 =
  fun _u#6479 -> ((poly_stub_117)@(L(unit)))[@inline] in
let set_source#864 = fun _a#6485 -> ((poly_stub_116)@(L(unit)))[@inline] in
let get_storage_of_address#865 =
  fun _a#6487 -> ((poly_stub_115)@(L(unit)))[@inline] in
let get_balance#866 = fun _a#6489 -> ((poly_stub_114)@(L(unit)))[@inline] in
let print#867 = fun _v#6491 -> ((poly_stub_113)@(L(unit)))[@inline] in
let eprint#868 = fun _v#6493 -> ((poly_stub_112)@(L(unit)))[@inline] in
let get_voting_power#869 =
  fun _kh#6495 -> ((poly_stub_111)@(L(unit)))[@inline] in
let nth_bootstrap_contract#870 =
  fun _i#6497 -> ((poly_stub_110)@(L(unit)))[@inline] in
let nth_bootstrap_account#871 =
  fun _i#6499 -> ((poly_stub_109)@(L(unit)))[@inline] in
let get_bootstrap_account#872 =
  fun _n#6501 -> ((poly_stub_108)@(L(unit)))[@inline] in
let last_originations#874 =
  fun _u#6505 -> ((poly_stub_107)@(L(unit)))[@inline] in
let new_account#876 = fun _u#6509 -> ((poly_stub_106)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#878 =
  fun _n#6513 -> ((poly_stub_105)@(L(unit)))[@inline] in
let register_delegate#880 =
  fun _kh#6517 -> ((poly_stub_104)@(L(unit)))[@inline] in
let register_constant#881 =
  fun _m#6519 -> ((poly_stub_103)@(L(unit)))[@inline] in
let constant_to_michelson_program#883 =
  fun _s#6523 -> ((poly_stub_102)@(L(unit)))[@inline] in
let restore_context#884 =
  fun _u#6525 -> ((poly_stub_101)@(L(unit)))[@inline] in
let save_context#885 = fun _u#6527 -> ((poly_stub_100)@(L(unit)))[@inline] in
let drop_context#886 = fun _u#6529 -> ((poly_stub_99)@(L(unit)))[@inline] in
let set_baker_policy#889 =
  fun _bp#6535 -> ((poly_stub_98)@(L(unit)))[@inline] in
let set_baker#890 = fun _a#6537 -> ((poly_stub_97)@(L(unit)))[@inline] in
let size#891 = fun _c#6539 -> ((poly_stub_96)@(L(unit)))[@inline] in
let read_contract_from_file#893 =
  fun _fn#6543 -> ((poly_stub_95)@(L(unit)))[@inline] in
let chr#894 = fun _n#6545 -> ((poly_stub_94)@(L(unit)))[@inline] in
let nl#895 = L("NEWLINE")[@inline] in
let println#896 = fun _v#6548 -> ((poly_stub_93)@(L(unit)))[@inline] in
let transfer#897 =
  fun _a#6550 -> (fun _s#6551 -> (fun _t#6552 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#898 =
  fun _a#6554 -> (fun _s#6555 -> (fun _t#6556 -> ((poly_stub_91)@(L(unit)))))[@inline] in
let reset_state#900 =
  fun _n#6560 -> (fun _l#6561 -> ((poly_stub_90)@(L(unit))))[@inline] in
let reset_state_at#901 =
  fun _t#6563 -> (fun _n#6564 -> (fun _l#6565 -> ((poly_stub_89)@(L(unit)))))[@inline] in
let save_mutation#904 =
  fun _s#6574 -> (fun _m#6575 -> ((poly_stub_88)@(L(unit))))[@inline] in
let sign#907 =
  fun _sk#6583 -> (fun _d#6584 -> ((poly_stub_87)@(L(unit))))[@inline] in
let add_account#908 =
  fun _s#6586 -> (fun _k#6587 -> ((poly_stub_86)@(L(unit))))[@inline] in
let baker_account#909 =
  fun _p#6589 -> (fun _o#6590 -> ((poly_stub_85)@(L(unit))))[@inline] in
let create_chest#911 =
  fun _b#6595 -> (fun _n#6596 -> ((poly_stub_84)@(L(unit))))[@inline] in
let create_chest_key#912 =
  fun _c#6598 -> (fun _n#6599 -> ((poly_stub_83)@(L(unit))))[@inline] in
let michelson_equal#915 =
  fun _m1#6609 -> (fun _m2#6610 -> ((poly_stub_82)@(L(unit))))[@inline] in
let originate_contract#917 =
  fun _c#6615 -> (fun _s#6616 -> (fun _t#6617 -> ((poly_stub_81)@(L(unit)))))[@inline] in
let compile_contract_from_file#919 =
  fun _fn#6623 ->
  (fun _e#6624 -> (fun _v#6625 -> ((poly_stub_80)@(L(unit)))))[@inline] in
let originate_from_file#920 =
  fun _fn#6627 ->
  (fun _e#6628 ->
   (fun _v#6629 ->
    (fun _s#6630 -> (fun _t#6631 -> ((poly_stub_79)@(L(unit)))))))[@inline] in
let tata#921 = ADD(toto#239 , titi#410) in
let foo#922 = (f#411)@(PAIR(L(unit) , L(3))) in
let get_balance#923 =
  fun _u#6635 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#924 =
  fun _u#6637 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#925 = fun _u#6639 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#926 =
  fun _u#6641 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#927 =
  fun _u#6643 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#928 = fun _u#6645 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#929 = fun _u#6647 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#930 =
  fun _u#6649 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#931 =
  fun _u#6651 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#932 =
  fun _u#6653 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#933 =
  fun kh#6655 -> (({ VOTING_POWER })@(kh#6655))[@inline] in
let implicit_account#935 =
  fun kh#6659 -> (IMPLICIT_ACCOUNT(kh#6659))[@inline] in
let pairing_check#939 =
  fun l#6667 -> (({ PAIRING_CHECK })@(l#6667))[@inline] in
let set_delegate#941 = fun o#6671 -> (SET_DELEGATE(o#6671))[@inline] in
let open_chest#947 =
  fun ck#6687 ->
  (fun c#6688 -> (fun n#6689 -> (OPEN_CHEST(ck#6687 , c#6688 , n#6689))))[@inline] in
let xor#950 =
  fun l#6698 -> (fun r#6699 -> (XOR(l#6698 , r#6699)))[@inline] in
let shift_left#951 =
  fun l#6701 -> (fun r#6702 -> (LSL(l#6701 , r#6702)))[@inline] in
let shift_right#952 =
  fun l#6704 -> (fun r#6705 -> (LSR(l#6704 , r#6705)))[@inline] in
let length#996 = fun b#6843 -> (({ SIZE })@(b#6843))[@inline] in
let concat#997 =
  fun b1#6845 ->
  (fun b2#6846 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6845 , b2#6846))))[@inline] in
let sub#998 =
  fun s#6848 ->
  (fun l#6849 ->
   (fun b#6850 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6848 ,
                                                                   l#6849) ,
                                                              b#6850)))))[@inline] in
let length#1004 = fun b#6865 -> (({ SIZE })@(b#6865))[@inline] in
let concat#1005 =
  fun b1#6867 ->
  (fun b2#6868 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6867 , b2#6868))))[@inline] in
let sub#1006 =
  fun s#6870 ->
  (fun l#6871 ->
   (fun b#6872 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6870 ,
                                                                   l#6871) ,
                                                              b#6872)))))[@inline] in
let blake2b#1007 = fun b#6874 -> (({ BLAKE2B })@(b#6874))[@inline] in
let sha256#1008 = fun b#6876 -> (({ SHA256 })@(b#6876))[@inline] in
let sha512#1009 = fun b#6878 -> (({ SHA512 })@(b#6878))[@inline] in
let sha3#1010 = fun b#6880 -> (({ SHA3 })@(b#6880))[@inline] in
let keccak#1011 = fun b#6882 -> (({ KECCAK })@(b#6882))[@inline] in
let hash_key#1012 = fun k#6884 -> (({ HASH_KEY })@(k#6884))[@inline] in
let check#1013 =
  fun k#6886 ->
  (fun s#6887 ->
   (fun b#6888 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6886 , s#6887) ,
                                                   b#6888)))))[@inline] in
let assert#1014 =
  fun b#6890 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6890))[@inline] in
let abs#1017 = fun i#6896 -> (({ ABS })@(i#6896))[@inline] in
let is_nat#1018 = fun i#6898 -> (({ ISNAT })@(i#6898))[@inline] in
let true#1019 = TRUE()[@inline] in
let false#1020 = FALSE()[@inline] in
let unit#1021 = UNIT()[@inline] in
let assert_with_error#1024 =
  fun b#6906 ->
  (fun s#6907 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6906 , s#6907))))[@inline] in
let poly_stub_78 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_77 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_76 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_75 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_74 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_73 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_72 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_71 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_70 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_69 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_68 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_67 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_66 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_65 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_64 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_63 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_62 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_61 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_60 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_59 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_58 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_57 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_56 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_55 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_54 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_53 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_52 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_51 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_50 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_49 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_48 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_47 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_46 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_45 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_44 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_43 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_42 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_41 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let poly_stub_40 = fun x#6918 -> (({ FAILWITH })@(x#6918))[@inline] in
let get_total_voting_power#1032 =
  fun _u#6927 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#1035 = fun _a#6933 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#1036 =
  fun _a#6935 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#1037 = fun _a#6937 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#1038 = fun _v#6939 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#1039 = fun _v#6941 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#1040 =
  fun _kh#6943 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1041 =
  fun _i#6945 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#1042 =
  fun _i#6947 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#1043 =
  fun _n#6949 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#1045 =
  fun _u#6953 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#1047 = fun _u#6957 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1049 =
  fun _n#6961 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#1051 =
  fun _kh#6965 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#1052 =
  fun _m#6967 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#1054 =
  fun _s#6971 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#1055 =
  fun _u#6973 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#1056 = fun _u#6975 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#1057 = fun _u#6977 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#1060 =
  fun _bp#6983 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#1061 = fun _a#6985 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#1062 = fun _c#6987 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#1064 =
  fun _fn#6991 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#1065 = fun _n#6993 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#1066 = L("NEWLINE")[@inline] in
let println#1067 = fun _v#6996 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#1068 =
  fun _a#6998 -> (fun _s#6999 -> (fun _t#7000 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#1069 =
  fun _a#7002 -> (fun _s#7003 -> (fun _t#7004 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#1071 =
  fun _n#7008 -> (fun _l#7009 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#1072 =
  fun _t#7011 -> (fun _n#7012 -> (fun _l#7013 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#1075 =
  fun _s#7022 -> (fun _m#7023 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#1078 =
  fun _sk#7031 -> (fun _d#7032 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#1079 =
  fun _s#7034 -> (fun _k#7035 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#1080 =
  fun _p#7037 -> (fun _o#7038 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#1082 =
  fun _b#7043 -> (fun _n#7044 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#1083 =
  fun _c#7046 -> (fun _n#7047 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#1086 =
  fun _m1#7057 -> (fun _m2#7058 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#1088 =
  fun _c#7063 -> (fun _s#7064 -> (fun _t#7065 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#1090 =
  fun _fn#7071 ->
  (fun _e#7072 -> (fun _v#7073 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#1091 =
  fun _fn#7075 ->
  (fun _e#7076 ->
   (fun _v#7077 ->
    (fun _s#7078 -> (fun _t#7079 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let toto#1092 = L(10) in
let foo#1093 = L("bar") in
let get_balance#1094 =
  fun _u#7083 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1095 =
  fun _u#7085 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1096 = fun _u#7087 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1097 =
  fun _u#7089 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1098 =
  fun _u#7091 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1099 =
  fun _u#7093 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1100 = fun _u#7095 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1101 =
  fun _u#7097 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1102 =
  fun _u#7099 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1103 =
  fun _u#7101 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1104 =
  fun kh#7103 -> (({ VOTING_POWER })@(kh#7103))[@inline] in
let implicit_account#1106 =
  fun kh#7107 -> (IMPLICIT_ACCOUNT(kh#7107))[@inline] in
let pairing_check#1110 =
  fun l#7115 -> (({ PAIRING_CHECK })@(l#7115))[@inline] in
let set_delegate#1112 = fun o#7119 -> (SET_DELEGATE(o#7119))[@inline] in
let open_chest#1118 =
  fun ck#7135 ->
  (fun c#7136 -> (fun n#7137 -> (OPEN_CHEST(ck#7135 , c#7136 , n#7137))))[@inline] in
let xor#1121 =
  fun l#7146 -> (fun r#7147 -> (XOR(l#7146 , r#7147)))[@inline] in
let shift_left#1122 =
  fun l#7149 -> (fun r#7150 -> (LSL(l#7149 , r#7150)))[@inline] in
let shift_right#1123 =
  fun l#7152 -> (fun r#7153 -> (LSR(l#7152 , r#7153)))[@inline] in
let length#1167 = fun b#7291 -> (({ SIZE })@(b#7291))[@inline] in
let concat#1168 =
  fun b1#7293 ->
  (fun b2#7294 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7293 , b2#7294))))[@inline] in
let sub#1169 =
  fun s#7296 ->
  (fun l#7297 ->
   (fun b#7298 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7296 ,
                                                                   l#7297) ,
                                                              b#7298)))))[@inline] in
let length#1175 = fun b#7313 -> (({ SIZE })@(b#7313))[@inline] in
let concat#1176 =
  fun b1#7315 ->
  (fun b2#7316 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7315 , b2#7316))))[@inline] in
let sub#1177 =
  fun s#7318 ->
  (fun l#7319 ->
   (fun b#7320 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7318 ,
                                                                   l#7319) ,
                                                              b#7320)))))[@inline] in
let blake2b#1178 = fun b#7322 -> (({ BLAKE2B })@(b#7322))[@inline] in
let sha256#1179 = fun b#7324 -> (({ SHA256 })@(b#7324))[@inline] in
let sha512#1180 = fun b#7326 -> (({ SHA512 })@(b#7326))[@inline] in
let sha3#1181 = fun b#7328 -> (({ SHA3 })@(b#7328))[@inline] in
let keccak#1182 = fun b#7330 -> (({ KECCAK })@(b#7330))[@inline] in
let hash_key#1183 = fun k#7332 -> (({ HASH_KEY })@(k#7332))[@inline] in
let check#1184 =
  fun k#7334 ->
  (fun s#7335 ->
   (fun b#7336 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7334 , s#7335) ,
                                                   b#7336)))))[@inline] in
let assert =
  fun b#7338 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7338))[@inline] in
let abs = fun i#7344 -> (({ ABS })@(i#7344))[@inline] in
let is_nat = fun i#7346 -> (({ ISNAT })@(i#7346))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#7354 ->
  (fun s#7355 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7354 , s#7355))))[@inline] in
let poly_stub_39 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_38 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_37 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_36 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_35 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_34 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_33 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_32 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_31 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_30 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_29 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_28 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_27 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_26 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_25 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_24 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_23 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_22 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_21 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_20 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_19 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_18 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_17 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_16 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_15 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_14 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_13 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_12 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_11 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_10 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_9 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_8 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_7 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_6 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_5 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_4 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_3 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_2 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let poly_stub_1 = fun x#7366 -> (({ FAILWITH })@(x#7366))[@inline] in
let get_total_voting_power#1189 =
  fun _u#7375 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#1192 = fun _a#7381 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#1193 =
  fun _a#7383 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#1194 = fun _a#7385 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#1195 = fun _v#7387 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#1196 = fun _v#7389 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#1197 =
  fun _kh#7391 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1198 =
  fun _i#7393 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#1199 =
  fun _i#7395 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#1200 =
  fun _n#7397 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#1202 =
  fun _u#7401 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#1204 = fun _u#7405 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1206 =
  fun _n#7409 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#1208 =
  fun _kh#7413 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#1209 =
  fun _m#7415 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#1211 =
  fun _s#7419 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#1212 =
  fun _u#7421 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#1213 = fun _u#7423 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#1214 = fun _u#7425 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#1217 =
  fun _bp#7431 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#1218 = fun _a#7433 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#1219 = fun _c#7435 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#1221 =
  fun _fn#7439 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1222 = fun _n#7441 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#1223 = L("NEWLINE")[@inline] in
let println#1224 = fun _v#7444 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#1225 =
  fun _a#7446 -> (fun _s#7447 -> (fun _t#7448 -> ((poly_stub_14)@(L(unit)))))[@inline] in
let transfer_exn#1226 =
  fun _a#7450 -> (fun _s#7451 -> (fun _t#7452 -> ((poly_stub_13)@(L(unit)))))[@inline] in
let reset_state#1228 =
  fun _n#7456 -> (fun _l#7457 -> ((poly_stub_12)@(L(unit))))[@inline] in
let reset_state_at#1229 =
  fun _t#7459 -> (fun _n#7460 -> (fun _l#7461 -> ((poly_stub_11)@(L(unit)))))[@inline] in
let save_mutation#1232 =
  fun _s#7470 -> (fun _m#7471 -> ((poly_stub_10)@(L(unit))))[@inline] in
let sign#1235 =
  fun _sk#7479 -> (fun _d#7480 -> ((poly_stub_9)@(L(unit))))[@inline] in
let add_account#1236 =
  fun _s#7482 -> (fun _k#7483 -> ((poly_stub_8)@(L(unit))))[@inline] in
let baker_account#1237 =
  fun _p#7485 -> (fun _o#7486 -> ((poly_stub_7)@(L(unit))))[@inline] in
let create_chest#1239 =
  fun _b#7491 -> (fun _n#7492 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#1240 =
  fun _c#7494 -> (fun _n#7495 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#1243 =
  fun _m1#7505 -> (fun _m2#7506 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#1245 =
  fun _c#7511 -> (fun _s#7512 -> (fun _t#7513 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#1247 =
  fun _fn#7519 -> (fun _e#7520 -> (fun _v#7521 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#1248 =
  fun _fn#7523 ->
  (fun _e#7524 ->
   (fun _v#7525 ->
    (fun _s#7526 -> (fun _t#7527 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1092 , toto#239) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#7531 ->
  (let (gen#7537, gen#7538) = gen#7531 in
   let p#7532 = gen#7537 in
   let s#7533 = gen#7538 in
   let s#7534 = ADD(ADD(p#7532 , s#7533) , toto) in
   PAIR(LIST_EMPTY() , s#7534)) in
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
    File "../../test/contracts/build/module_scoping_bug.mligo", line 24, characters 8-13:
     23 |
     24 | let x = B.A.a

    Module "A" not found. |}]
