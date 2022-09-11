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
let get_balance#98 =
  fun _u#4771 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#99 =
  fun _u#4773 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#100 = fun _u#4775 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#101 =
  fun _u#4777 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#102 =
  fun _u#4779 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#103 = fun _u#4781 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#104 = fun _u#4783 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#105 =
  fun _u#4785 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#106 =
  fun _u#4787 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#107 =
  fun _u#4789 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#108 =
  fun kh#4791 -> (({ VOTING_POWER })@(kh#4791))[@inline] in
let implicit_account#110 =
  fun kh#4795 -> (IMPLICIT_ACCOUNT(kh#4795))[@inline] in
let pairing_check#114 =
  fun l#4803 -> (({ PAIRING_CHECK })@(l#4803))[@inline] in
let set_delegate#116 = fun o#4807 -> (SET_DELEGATE(o#4807))[@inline] in
let open_chest#124 =
  fun ck#4828 ->
  (fun c#4829 -> (fun n#4830 -> (OPEN_CHEST(ck#4828 , c#4829 , n#4830))))[@inline] in
let xor#133 =
  fun l#4864 -> (fun r#4865 -> (XOR(l#4864 , r#4865)))[@inline] in
let or#134 = fun l#4867 -> (fun r#4868 -> (OR(l#4867 , r#4868)))[@inline] in
let shift_left#135 =
  fun l#4870 -> (fun r#4871 -> (LSL(l#4870 , r#4871)))[@inline] in
let shift_right#136 =
  fun l#4873 -> (fun r#4874 -> (LSR(l#4873 , r#4874)))[@inline] in
let length#181 = fun b#5019 -> (({ SIZE })@(b#5019))[@inline] in
let concat#182 =
  fun b1#5021 ->
  (fun b2#5022 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5021 , b2#5022))))[@inline] in
let sub#183 =
  fun s#5024 ->
  (fun l#5025 ->
   (fun b#5026 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5024 ,
                                                                   l#5025) ,
                                                              b#5026)))))[@inline] in
let length#189 = fun b#5041 -> (({ SIZE })@(b#5041))[@inline] in
let concat#190 =
  fun b1#5043 ->
  (fun b2#5044 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5043 , b2#5044))))[@inline] in
let sub#191 =
  fun s#5046 ->
  (fun l#5047 ->
   (fun b#5048 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5046 ,
                                                                   l#5047) ,
                                                              b#5048)))))[@inline] in
let blake2b#192 = fun b#5050 -> (({ BLAKE2B })@(b#5050))[@inline] in
let sha256#193 = fun b#5052 -> (({ SHA256 })@(b#5052))[@inline] in
let sha512#194 = fun b#5054 -> (({ SHA512 })@(b#5054))[@inline] in
let sha3#195 = fun b#5056 -> (({ SHA3 })@(b#5056))[@inline] in
let keccak#196 = fun b#5058 -> (({ KECCAK })@(b#5058))[@inline] in
let hash_key#197 = fun k#5060 -> (({ HASH_KEY })@(k#5060))[@inline] in
let check#198 =
  fun k#5062 ->
  (fun s#5063 ->
   (fun b#5064 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5062 , s#5063) ,
                                                   b#5064)))))[@inline] in
let assert#199 =
  fun b#5066 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5066))[@inline] in
let abs#202 = fun i#5072 -> (({ ABS })@(i#5072))[@inline] in
let is_nat#203 = fun i#5074 -> (({ ISNAT })@(i#5074))[@inline] in
let true#204 = TRUE()[@inline] in
let false#205 = FALSE()[@inline] in
let unit#206 = UNIT()[@inline] in
let assert_with_error#209 =
  fun b#5082 ->
  (fun s#5083 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5082 , s#5083))))[@inline] in
let poly_stub_273 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_272 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_271 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_270 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_269 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_268 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_267 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_266 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_265 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_264 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_263 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_262 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_261 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_260 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_259 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_258 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_257 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_256 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_255 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_254 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_253 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_252 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_251 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_250 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_249 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_248 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_247 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_246 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_245 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_244 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_243 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_242 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_241 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_240 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_239 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_238 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_237 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_236 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let poly_stub_235 = fun x#5094 -> (({ FAILWITH })@(x#5094))[@inline] in
let get_total_voting_power#217 =
  fun _u#5103 -> ((poly_stub_273)@(L(unit)))[@inline] in
let set_source#220 = fun _a#5109 -> ((poly_stub_272)@(L(unit)))[@inline] in
let get_storage_of_address#221 =
  fun _a#5111 -> ((poly_stub_271)@(L(unit)))[@inline] in
let get_balance#222 = fun _a#5113 -> ((poly_stub_270)@(L(unit)))[@inline] in
let print#223 = fun _v#5115 -> ((poly_stub_269)@(L(unit)))[@inline] in
let eprint#224 = fun _v#5117 -> ((poly_stub_268)@(L(unit)))[@inline] in
let get_voting_power#225 =
  fun _kh#5119 -> ((poly_stub_267)@(L(unit)))[@inline] in
let nth_bootstrap_contract#226 =
  fun _i#5121 -> ((poly_stub_266)@(L(unit)))[@inline] in
let nth_bootstrap_account#227 =
  fun _i#5123 -> ((poly_stub_265)@(L(unit)))[@inline] in
let get_bootstrap_account#228 =
  fun _n#5125 -> ((poly_stub_264)@(L(unit)))[@inline] in
let last_originations#230 =
  fun _u#5129 -> ((poly_stub_263)@(L(unit)))[@inline] in
let new_account#232 = fun _u#5133 -> ((poly_stub_262)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#234 =
  fun _n#5137 -> ((poly_stub_261)@(L(unit)))[@inline] in
let register_delegate#236 =
  fun _kh#5141 -> ((poly_stub_260)@(L(unit)))[@inline] in
let register_constant#237 =
  fun _m#5143 -> ((poly_stub_259)@(L(unit)))[@inline] in
let constant_to_michelson_program#239 =
  fun _s#5147 -> ((poly_stub_258)@(L(unit)))[@inline] in
let restore_context#240 =
  fun _u#5149 -> ((poly_stub_257)@(L(unit)))[@inline] in
let save_context#241 = fun _u#5151 -> ((poly_stub_256)@(L(unit)))[@inline] in
let drop_context#242 = fun _u#5153 -> ((poly_stub_255)@(L(unit)))[@inline] in
let set_baker_policy#245 =
  fun _bp#5159 -> ((poly_stub_254)@(L(unit)))[@inline] in
let set_baker#246 = fun _a#5161 -> ((poly_stub_253)@(L(unit)))[@inline] in
let size#247 = fun _c#5163 -> ((poly_stub_252)@(L(unit)))[@inline] in
let read_contract_from_file#249 =
  fun _fn#5167 -> ((poly_stub_251)@(L(unit)))[@inline] in
let chr#250 = fun _n#5169 -> ((poly_stub_250)@(L(unit)))[@inline] in
let nl#251 = L("NEWLINE")[@inline] in
let println#252 = fun _v#5172 -> ((poly_stub_249)@(L(unit)))[@inline] in
let transfer#253 =
  fun _a#5174 ->
  (fun _s#5175 -> (fun _t#5176 -> ((poly_stub_248)@(L(unit)))))[@inline] in
let transfer_exn#254 =
  fun _a#5178 ->
  (fun _s#5179 -> (fun _t#5180 -> ((poly_stub_247)@(L(unit)))))[@inline] in
let reset_state#256 =
  fun _n#5184 -> (fun _l#5185 -> ((poly_stub_246)@(L(unit))))[@inline] in
let reset_state_at#257 =
  fun _t#5187 ->
  (fun _n#5188 -> (fun _l#5189 -> ((poly_stub_245)@(L(unit)))))[@inline] in
let save_mutation#260 =
  fun _s#5198 -> (fun _m#5199 -> ((poly_stub_244)@(L(unit))))[@inline] in
let sign#263 =
  fun _sk#5207 -> (fun _d#5208 -> ((poly_stub_243)@(L(unit))))[@inline] in
let add_account#264 =
  fun _s#5210 -> (fun _k#5211 -> ((poly_stub_242)@(L(unit))))[@inline] in
let baker_account#265 =
  fun _p#5213 -> (fun _o#5214 -> ((poly_stub_241)@(L(unit))))[@inline] in
let create_chest#267 =
  fun _b#5219 -> (fun _n#5220 -> ((poly_stub_240)@(L(unit))))[@inline] in
let create_chest_key#268 =
  fun _c#5222 -> (fun _n#5223 -> ((poly_stub_239)@(L(unit))))[@inline] in
let michelson_equal#271 =
  fun _m1#5233 -> (fun _m2#5234 -> ((poly_stub_238)@(L(unit))))[@inline] in
let originate_contract#273 =
  fun _c#5239 ->
  (fun _s#5240 -> (fun _t#5241 -> ((poly_stub_237)@(L(unit)))))[@inline] in
let compile_contract_from_file#275 =
  fun _fn#5247 ->
  (fun _e#5248 -> (fun _v#5249 -> ((poly_stub_236)@(L(unit)))))[@inline] in
let originate_from_file#276 =
  fun _fn#5251 ->
  (fun _e#5252 ->
   (fun _v#5253 ->
    (fun _s#5254 -> (fun _t#5255 -> ((poly_stub_235)@(L(unit)))))))[@inline] in
let toto#277 = L(1) in
let get_balance#278 =
  fun _u#5258 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#279 =
  fun _u#5260 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#280 = fun _u#5262 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#281 =
  fun _u#5264 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#282 =
  fun _u#5266 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#283 = fun _u#5268 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#284 = fun _u#5270 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#285 =
  fun _u#5272 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#286 =
  fun _u#5274 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#287 =
  fun _u#5276 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#288 =
  fun kh#5278 -> (({ VOTING_POWER })@(kh#5278))[@inline] in
let implicit_account#290 =
  fun kh#5282 -> (IMPLICIT_ACCOUNT(kh#5282))[@inline] in
let pairing_check#294 =
  fun l#5290 -> (({ PAIRING_CHECK })@(l#5290))[@inline] in
let set_delegate#296 = fun o#5294 -> (SET_DELEGATE(o#5294))[@inline] in
let open_chest#304 =
  fun ck#5315 ->
  (fun c#5316 -> (fun n#5317 -> (OPEN_CHEST(ck#5315 , c#5316 , n#5317))))[@inline] in
let xor#313 =
  fun l#5351 -> (fun r#5352 -> (XOR(l#5351 , r#5352)))[@inline] in
let or#314 = fun l#5354 -> (fun r#5355 -> (OR(l#5354 , r#5355)))[@inline] in
let shift_left#315 =
  fun l#5357 -> (fun r#5358 -> (LSL(l#5357 , r#5358)))[@inline] in
let shift_right#316 =
  fun l#5360 -> (fun r#5361 -> (LSR(l#5360 , r#5361)))[@inline] in
let length#361 = fun b#5506 -> (({ SIZE })@(b#5506))[@inline] in
let concat#362 =
  fun b1#5508 ->
  (fun b2#5509 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5508 , b2#5509))))[@inline] in
let sub#363 =
  fun s#5511 ->
  (fun l#5512 ->
   (fun b#5513 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5511 ,
                                                                   l#5512) ,
                                                              b#5513)))))[@inline] in
let length#369 = fun b#5528 -> (({ SIZE })@(b#5528))[@inline] in
let concat#370 =
  fun b1#5530 ->
  (fun b2#5531 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5530 , b2#5531))))[@inline] in
let sub#371 =
  fun s#5533 ->
  (fun l#5534 ->
   (fun b#5535 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5533 ,
                                                                   l#5534) ,
                                                              b#5535)))))[@inline] in
let blake2b#372 = fun b#5537 -> (({ BLAKE2B })@(b#5537))[@inline] in
let sha256#373 = fun b#5539 -> (({ SHA256 })@(b#5539))[@inline] in
let sha512#374 = fun b#5541 -> (({ SHA512 })@(b#5541))[@inline] in
let sha3#375 = fun b#5543 -> (({ SHA3 })@(b#5543))[@inline] in
let keccak#376 = fun b#5545 -> (({ KECCAK })@(b#5545))[@inline] in
let hash_key#377 = fun k#5547 -> (({ HASH_KEY })@(k#5547))[@inline] in
let check#378 =
  fun k#5549 ->
  (fun s#5550 ->
   (fun b#5551 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5549 , s#5550) ,
                                                   b#5551)))))[@inline] in
let assert#379 =
  fun b#5553 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5553))[@inline] in
let abs#382 = fun i#5559 -> (({ ABS })@(i#5559))[@inline] in
let is_nat#383 = fun i#5561 -> (({ ISNAT })@(i#5561))[@inline] in
let true#384 = TRUE()[@inline] in
let false#385 = FALSE()[@inline] in
let unit#386 = UNIT()[@inline] in
let assert_with_error#389 =
  fun b#5569 ->
  (fun s#5570 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5569 , s#5570))))[@inline] in
let poly_stub_234 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_233 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_232 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_231 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_230 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_229 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_228 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_227 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_226 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_225 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_224 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_223 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_222 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_221 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_220 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_219 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_218 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_217 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_216 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_215 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_214 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_213 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_212 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_211 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_210 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_209 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_208 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_207 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_206 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_205 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_204 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_203 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_202 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_201 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_200 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_199 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_198 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_197 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let poly_stub_196 = fun x#5581 -> (({ FAILWITH })@(x#5581))[@inline] in
let get_total_voting_power#397 =
  fun _u#5590 -> ((poly_stub_234)@(L(unit)))[@inline] in
let set_source#400 = fun _a#5596 -> ((poly_stub_233)@(L(unit)))[@inline] in
let get_storage_of_address#401 =
  fun _a#5598 -> ((poly_stub_232)@(L(unit)))[@inline] in
let get_balance#402 = fun _a#5600 -> ((poly_stub_231)@(L(unit)))[@inline] in
let print#403 = fun _v#5602 -> ((poly_stub_230)@(L(unit)))[@inline] in
let eprint#404 = fun _v#5604 -> ((poly_stub_229)@(L(unit)))[@inline] in
let get_voting_power#405 =
  fun _kh#5606 -> ((poly_stub_228)@(L(unit)))[@inline] in
let nth_bootstrap_contract#406 =
  fun _i#5608 -> ((poly_stub_227)@(L(unit)))[@inline] in
let nth_bootstrap_account#407 =
  fun _i#5610 -> ((poly_stub_226)@(L(unit)))[@inline] in
let get_bootstrap_account#408 =
  fun _n#5612 -> ((poly_stub_225)@(L(unit)))[@inline] in
let last_originations#410 =
  fun _u#5616 -> ((poly_stub_224)@(L(unit)))[@inline] in
let new_account#412 = fun _u#5620 -> ((poly_stub_223)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#414 =
  fun _n#5624 -> ((poly_stub_222)@(L(unit)))[@inline] in
let register_delegate#416 =
  fun _kh#5628 -> ((poly_stub_221)@(L(unit)))[@inline] in
let register_constant#417 =
  fun _m#5630 -> ((poly_stub_220)@(L(unit)))[@inline] in
let constant_to_michelson_program#419 =
  fun _s#5634 -> ((poly_stub_219)@(L(unit)))[@inline] in
let restore_context#420 =
  fun _u#5636 -> ((poly_stub_218)@(L(unit)))[@inline] in
let save_context#421 = fun _u#5638 -> ((poly_stub_217)@(L(unit)))[@inline] in
let drop_context#422 = fun _u#5640 -> ((poly_stub_216)@(L(unit)))[@inline] in
let set_baker_policy#425 =
  fun _bp#5646 -> ((poly_stub_215)@(L(unit)))[@inline] in
let set_baker#426 = fun _a#5648 -> ((poly_stub_214)@(L(unit)))[@inline] in
let size#427 = fun _c#5650 -> ((poly_stub_213)@(L(unit)))[@inline] in
let read_contract_from_file#429 =
  fun _fn#5654 -> ((poly_stub_212)@(L(unit)))[@inline] in
let chr#430 = fun _n#5656 -> ((poly_stub_211)@(L(unit)))[@inline] in
let nl#431 = L("NEWLINE")[@inline] in
let println#432 = fun _v#5659 -> ((poly_stub_210)@(L(unit)))[@inline] in
let transfer#433 =
  fun _a#5661 ->
  (fun _s#5662 -> (fun _t#5663 -> ((poly_stub_209)@(L(unit)))))[@inline] in
let transfer_exn#434 =
  fun _a#5665 ->
  (fun _s#5666 -> (fun _t#5667 -> ((poly_stub_208)@(L(unit)))))[@inline] in
let reset_state#436 =
  fun _n#5671 -> (fun _l#5672 -> ((poly_stub_207)@(L(unit))))[@inline] in
let reset_state_at#437 =
  fun _t#5674 ->
  (fun _n#5675 -> (fun _l#5676 -> ((poly_stub_206)@(L(unit)))))[@inline] in
let save_mutation#440 =
  fun _s#5685 -> (fun _m#5686 -> ((poly_stub_205)@(L(unit))))[@inline] in
let sign#443 =
  fun _sk#5694 -> (fun _d#5695 -> ((poly_stub_204)@(L(unit))))[@inline] in
let add_account#444 =
  fun _s#5697 -> (fun _k#5698 -> ((poly_stub_203)@(L(unit))))[@inline] in
let baker_account#445 =
  fun _p#5700 -> (fun _o#5701 -> ((poly_stub_202)@(L(unit))))[@inline] in
let create_chest#447 =
  fun _b#5706 -> (fun _n#5707 -> ((poly_stub_201)@(L(unit))))[@inline] in
let create_chest_key#448 =
  fun _c#5709 -> (fun _n#5710 -> ((poly_stub_200)@(L(unit))))[@inline] in
let michelson_equal#451 =
  fun _m1#5720 -> (fun _m2#5721 -> ((poly_stub_199)@(L(unit))))[@inline] in
let originate_contract#453 =
  fun _c#5726 ->
  (fun _s#5727 -> (fun _t#5728 -> ((poly_stub_198)@(L(unit)))))[@inline] in
let compile_contract_from_file#455 =
  fun _fn#5734 ->
  (fun _e#5735 -> (fun _v#5736 -> ((poly_stub_197)@(L(unit)))))[@inline] in
let originate_from_file#456 =
  fun _fn#5738 ->
  (fun _e#5739 ->
   (fun _v#5740 ->
    (fun _s#5741 -> (fun _t#5742 -> ((poly_stub_196)@(L(unit)))))))[@inline] in
let toto#457 = L(32) in
let titi#458 = ADD(toto#277 , L(42)) in
let f#459 =
  fun gen#5746 ->
  (let (gen#8193, gen#8194) = gen#5746 in
   let gen#5747 = gen#8193 in
   let x#5748 = gen#8194 in
   let x#5749 = ADD(ADD(x#5748 , toto#277) , titi#458) in
   PAIR(LIST_EMPTY() , x#5749)) in
let get_balance#460 =
  fun _u#5751 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#461 =
  fun _u#5753 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#462 = fun _u#5755 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#463 =
  fun _u#5757 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#464 =
  fun _u#5759 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#465 = fun _u#5761 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#466 = fun _u#5763 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#467 =
  fun _u#5765 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#468 =
  fun _u#5767 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#469 =
  fun _u#5769 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#470 =
  fun kh#5771 -> (({ VOTING_POWER })@(kh#5771))[@inline] in
let implicit_account#472 =
  fun kh#5775 -> (IMPLICIT_ACCOUNT(kh#5775))[@inline] in
let pairing_check#476 =
  fun l#5783 -> (({ PAIRING_CHECK })@(l#5783))[@inline] in
let set_delegate#478 = fun o#5787 -> (SET_DELEGATE(o#5787))[@inline] in
let open_chest#486 =
  fun ck#5808 ->
  (fun c#5809 -> (fun n#5810 -> (OPEN_CHEST(ck#5808 , c#5809 , n#5810))))[@inline] in
let xor#495 =
  fun l#5844 -> (fun r#5845 -> (XOR(l#5844 , r#5845)))[@inline] in
let or#496 = fun l#5847 -> (fun r#5848 -> (OR(l#5847 , r#5848)))[@inline] in
let shift_left#497 =
  fun l#5850 -> (fun r#5851 -> (LSL(l#5850 , r#5851)))[@inline] in
let shift_right#498 =
  fun l#5853 -> (fun r#5854 -> (LSR(l#5853 , r#5854)))[@inline] in
let length#543 = fun b#5999 -> (({ SIZE })@(b#5999))[@inline] in
let concat#544 =
  fun b1#6001 ->
  (fun b2#6002 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6001 , b2#6002))))[@inline] in
let sub#545 =
  fun s#6004 ->
  (fun l#6005 ->
   (fun b#6006 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6004 ,
                                                                   l#6005) ,
                                                              b#6006)))))[@inline] in
let length#551 = fun b#6021 -> (({ SIZE })@(b#6021))[@inline] in
let concat#552 =
  fun b1#6023 ->
  (fun b2#6024 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6023 , b2#6024))))[@inline] in
let sub#553 =
  fun s#6026 ->
  (fun l#6027 ->
   (fun b#6028 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6026 ,
                                                                   l#6027) ,
                                                              b#6028)))))[@inline] in
let blake2b#554 = fun b#6030 -> (({ BLAKE2B })@(b#6030))[@inline] in
let sha256#555 = fun b#6032 -> (({ SHA256 })@(b#6032))[@inline] in
let sha512#556 = fun b#6034 -> (({ SHA512 })@(b#6034))[@inline] in
let sha3#557 = fun b#6036 -> (({ SHA3 })@(b#6036))[@inline] in
let keccak#558 = fun b#6038 -> (({ KECCAK })@(b#6038))[@inline] in
let hash_key#559 = fun k#6040 -> (({ HASH_KEY })@(k#6040))[@inline] in
let check#560 =
  fun k#6042 ->
  (fun s#6043 ->
   (fun b#6044 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6042 , s#6043) ,
                                                   b#6044)))))[@inline] in
let assert#561 =
  fun b#6046 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6046))[@inline] in
let abs#564 = fun i#6052 -> (({ ABS })@(i#6052))[@inline] in
let is_nat#565 = fun i#6054 -> (({ ISNAT })@(i#6054))[@inline] in
let true#566 = TRUE()[@inline] in
let false#567 = FALSE()[@inline] in
let unit#568 = UNIT()[@inline] in
let assert_with_error#571 =
  fun b#6062 ->
  (fun s#6063 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6062 , s#6063))))[@inline] in
let poly_stub_195 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_194 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_193 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_192 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_191 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_190 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_189 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_188 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_187 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_186 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_185 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_184 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_183 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_182 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_181 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_180 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_179 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_178 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_177 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_176 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_175 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_174 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_173 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_172 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_171 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_170 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_169 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_168 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_167 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_166 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_165 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_164 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_163 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_162 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_161 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_160 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_159 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_158 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let poly_stub_157 = fun x#6074 -> (({ FAILWITH })@(x#6074))[@inline] in
let get_total_voting_power#579 =
  fun _u#6083 -> ((poly_stub_195)@(L(unit)))[@inline] in
let set_source#582 = fun _a#6089 -> ((poly_stub_194)@(L(unit)))[@inline] in
let get_storage_of_address#583 =
  fun _a#6091 -> ((poly_stub_193)@(L(unit)))[@inline] in
let get_balance#584 = fun _a#6093 -> ((poly_stub_192)@(L(unit)))[@inline] in
let print#585 = fun _v#6095 -> ((poly_stub_191)@(L(unit)))[@inline] in
let eprint#586 = fun _v#6097 -> ((poly_stub_190)@(L(unit)))[@inline] in
let get_voting_power#587 =
  fun _kh#6099 -> ((poly_stub_189)@(L(unit)))[@inline] in
let nth_bootstrap_contract#588 =
  fun _i#6101 -> ((poly_stub_188)@(L(unit)))[@inline] in
let nth_bootstrap_account#589 =
  fun _i#6103 -> ((poly_stub_187)@(L(unit)))[@inline] in
let get_bootstrap_account#590 =
  fun _n#6105 -> ((poly_stub_186)@(L(unit)))[@inline] in
let last_originations#592 =
  fun _u#6109 -> ((poly_stub_185)@(L(unit)))[@inline] in
let new_account#594 = fun _u#6113 -> ((poly_stub_184)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#596 =
  fun _n#6117 -> ((poly_stub_183)@(L(unit)))[@inline] in
let register_delegate#598 =
  fun _kh#6121 -> ((poly_stub_182)@(L(unit)))[@inline] in
let register_constant#599 =
  fun _m#6123 -> ((poly_stub_181)@(L(unit)))[@inline] in
let constant_to_michelson_program#601 =
  fun _s#6127 -> ((poly_stub_180)@(L(unit)))[@inline] in
let restore_context#602 =
  fun _u#6129 -> ((poly_stub_179)@(L(unit)))[@inline] in
let save_context#603 = fun _u#6131 -> ((poly_stub_178)@(L(unit)))[@inline] in
let drop_context#604 = fun _u#6133 -> ((poly_stub_177)@(L(unit)))[@inline] in
let set_baker_policy#607 =
  fun _bp#6139 -> ((poly_stub_176)@(L(unit)))[@inline] in
let set_baker#608 = fun _a#6141 -> ((poly_stub_175)@(L(unit)))[@inline] in
let size#609 = fun _c#6143 -> ((poly_stub_174)@(L(unit)))[@inline] in
let read_contract_from_file#611 =
  fun _fn#6147 -> ((poly_stub_173)@(L(unit)))[@inline] in
let chr#612 = fun _n#6149 -> ((poly_stub_172)@(L(unit)))[@inline] in
let nl#613 = L("NEWLINE")[@inline] in
let println#614 = fun _v#6152 -> ((poly_stub_171)@(L(unit)))[@inline] in
let transfer#615 =
  fun _a#6154 ->
  (fun _s#6155 -> (fun _t#6156 -> ((poly_stub_170)@(L(unit)))))[@inline] in
let transfer_exn#616 =
  fun _a#6158 ->
  (fun _s#6159 -> (fun _t#6160 -> ((poly_stub_169)@(L(unit)))))[@inline] in
let reset_state#618 =
  fun _n#6164 -> (fun _l#6165 -> ((poly_stub_168)@(L(unit))))[@inline] in
let reset_state_at#619 =
  fun _t#6167 ->
  (fun _n#6168 -> (fun _l#6169 -> ((poly_stub_167)@(L(unit)))))[@inline] in
let save_mutation#622 =
  fun _s#6178 -> (fun _m#6179 -> ((poly_stub_166)@(L(unit))))[@inline] in
let sign#625 =
  fun _sk#6187 -> (fun _d#6188 -> ((poly_stub_165)@(L(unit))))[@inline] in
let add_account#626 =
  fun _s#6190 -> (fun _k#6191 -> ((poly_stub_164)@(L(unit))))[@inline] in
let baker_account#627 =
  fun _p#6193 -> (fun _o#6194 -> ((poly_stub_163)@(L(unit))))[@inline] in
let create_chest#629 =
  fun _b#6199 -> (fun _n#6200 -> ((poly_stub_162)@(L(unit))))[@inline] in
let create_chest_key#630 =
  fun _c#6202 -> (fun _n#6203 -> ((poly_stub_161)@(L(unit))))[@inline] in
let michelson_equal#633 =
  fun _m1#6213 -> (fun _m2#6214 -> ((poly_stub_160)@(L(unit))))[@inline] in
let originate_contract#635 =
  fun _c#6219 ->
  (fun _s#6220 -> (fun _t#6221 -> ((poly_stub_159)@(L(unit)))))[@inline] in
let compile_contract_from_file#637 =
  fun _fn#6227 ->
  (fun _e#6228 -> (fun _v#6229 -> ((poly_stub_158)@(L(unit)))))[@inline] in
let originate_from_file#638 =
  fun _fn#6231 ->
  (fun _e#6232 ->
   (fun _v#6233 ->
    (fun _s#6234 -> (fun _t#6235 -> ((poly_stub_157)@(L(unit)))))))[@inline] in
let toto#639 = L(44) in
let get_balance#640 =
  fun _u#6238 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#641 =
  fun _u#6240 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#642 = fun _u#6242 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#643 =
  fun _u#6244 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#644 =
  fun _u#6246 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#645 = fun _u#6248 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#646 = fun _u#6250 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#647 =
  fun _u#6252 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#648 =
  fun _u#6254 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#649 =
  fun _u#6256 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#650 =
  fun kh#6258 -> (({ VOTING_POWER })@(kh#6258))[@inline] in
let implicit_account#652 =
  fun kh#6262 -> (IMPLICIT_ACCOUNT(kh#6262))[@inline] in
let pairing_check#656 =
  fun l#6270 -> (({ PAIRING_CHECK })@(l#6270))[@inline] in
let set_delegate#658 = fun o#6274 -> (SET_DELEGATE(o#6274))[@inline] in
let open_chest#666 =
  fun ck#6295 ->
  (fun c#6296 -> (fun n#6297 -> (OPEN_CHEST(ck#6295 , c#6296 , n#6297))))[@inline] in
let xor#675 =
  fun l#6331 -> (fun r#6332 -> (XOR(l#6331 , r#6332)))[@inline] in
let or#676 = fun l#6334 -> (fun r#6335 -> (OR(l#6334 , r#6335)))[@inline] in
let shift_left#677 =
  fun l#6337 -> (fun r#6338 -> (LSL(l#6337 , r#6338)))[@inline] in
let shift_right#678 =
  fun l#6340 -> (fun r#6341 -> (LSR(l#6340 , r#6341)))[@inline] in
let length#723 = fun b#6486 -> (({ SIZE })@(b#6486))[@inline] in
let concat#724 =
  fun b1#6488 ->
  (fun b2#6489 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6488 , b2#6489))))[@inline] in
let sub#725 =
  fun s#6491 ->
  (fun l#6492 ->
   (fun b#6493 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6491 ,
                                                                   l#6492) ,
                                                              b#6493)))))[@inline] in
let length#731 = fun b#6508 -> (({ SIZE })@(b#6508))[@inline] in
let concat#732 =
  fun b1#6510 ->
  (fun b2#6511 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6510 , b2#6511))))[@inline] in
let sub#733 =
  fun s#6513 ->
  (fun l#6514 ->
   (fun b#6515 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6513 ,
                                                                   l#6514) ,
                                                              b#6515)))))[@inline] in
let blake2b#734 = fun b#6517 -> (({ BLAKE2B })@(b#6517))[@inline] in
let sha256#735 = fun b#6519 -> (({ SHA256 })@(b#6519))[@inline] in
let sha512#736 = fun b#6521 -> (({ SHA512 })@(b#6521))[@inline] in
let sha3#737 = fun b#6523 -> (({ SHA3 })@(b#6523))[@inline] in
let keccak#738 = fun b#6525 -> (({ KECCAK })@(b#6525))[@inline] in
let hash_key#739 = fun k#6527 -> (({ HASH_KEY })@(k#6527))[@inline] in
let check#740 =
  fun k#6529 ->
  (fun s#6530 ->
   (fun b#6531 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6529 , s#6530) ,
                                                   b#6531)))))[@inline] in
let assert#741 =
  fun b#6533 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6533))[@inline] in
let abs#744 = fun i#6539 -> (({ ABS })@(i#6539))[@inline] in
let is_nat#745 = fun i#6541 -> (({ ISNAT })@(i#6541))[@inline] in
let true#746 = TRUE()[@inline] in
let false#747 = FALSE()[@inline] in
let unit#748 = UNIT()[@inline] in
let assert_with_error#751 =
  fun b#6549 ->
  (fun s#6550 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6549 , s#6550))))[@inline] in
let poly_stub_156 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_155 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_154 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_153 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_152 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_151 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_150 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_149 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_148 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_147 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_146 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_145 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_144 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_143 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_142 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_141 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_140 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_139 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_138 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_137 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_136 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_135 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_134 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_133 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_132 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_131 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_130 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_129 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_128 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_127 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_126 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_125 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_124 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_123 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_122 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_121 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_120 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_119 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let poly_stub_118 = fun x#6561 -> (({ FAILWITH })@(x#6561))[@inline] in
let get_total_voting_power#759 =
  fun _u#6570 -> ((poly_stub_156)@(L(unit)))[@inline] in
let set_source#762 = fun _a#6576 -> ((poly_stub_155)@(L(unit)))[@inline] in
let get_storage_of_address#763 =
  fun _a#6578 -> ((poly_stub_154)@(L(unit)))[@inline] in
let get_balance#764 = fun _a#6580 -> ((poly_stub_153)@(L(unit)))[@inline] in
let print#765 = fun _v#6582 -> ((poly_stub_152)@(L(unit)))[@inline] in
let eprint#766 = fun _v#6584 -> ((poly_stub_151)@(L(unit)))[@inline] in
let get_voting_power#767 =
  fun _kh#6586 -> ((poly_stub_150)@(L(unit)))[@inline] in
let nth_bootstrap_contract#768 =
  fun _i#6588 -> ((poly_stub_149)@(L(unit)))[@inline] in
let nth_bootstrap_account#769 =
  fun _i#6590 -> ((poly_stub_148)@(L(unit)))[@inline] in
let get_bootstrap_account#770 =
  fun _n#6592 -> ((poly_stub_147)@(L(unit)))[@inline] in
let last_originations#772 =
  fun _u#6596 -> ((poly_stub_146)@(L(unit)))[@inline] in
let new_account#774 = fun _u#6600 -> ((poly_stub_145)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#776 =
  fun _n#6604 -> ((poly_stub_144)@(L(unit)))[@inline] in
let register_delegate#778 =
  fun _kh#6608 -> ((poly_stub_143)@(L(unit)))[@inline] in
let register_constant#779 =
  fun _m#6610 -> ((poly_stub_142)@(L(unit)))[@inline] in
let constant_to_michelson_program#781 =
  fun _s#6614 -> ((poly_stub_141)@(L(unit)))[@inline] in
let restore_context#782 =
  fun _u#6616 -> ((poly_stub_140)@(L(unit)))[@inline] in
let save_context#783 = fun _u#6618 -> ((poly_stub_139)@(L(unit)))[@inline] in
let drop_context#784 = fun _u#6620 -> ((poly_stub_138)@(L(unit)))[@inline] in
let set_baker_policy#787 =
  fun _bp#6626 -> ((poly_stub_137)@(L(unit)))[@inline] in
let set_baker#788 = fun _a#6628 -> ((poly_stub_136)@(L(unit)))[@inline] in
let size#789 = fun _c#6630 -> ((poly_stub_135)@(L(unit)))[@inline] in
let read_contract_from_file#791 =
  fun _fn#6634 -> ((poly_stub_134)@(L(unit)))[@inline] in
let chr#792 = fun _n#6636 -> ((poly_stub_133)@(L(unit)))[@inline] in
let nl#793 = L("NEWLINE")[@inline] in
let println#794 = fun _v#6639 -> ((poly_stub_132)@(L(unit)))[@inline] in
let transfer#795 =
  fun _a#6641 ->
  (fun _s#6642 -> (fun _t#6643 -> ((poly_stub_131)@(L(unit)))))[@inline] in
let transfer_exn#796 =
  fun _a#6645 ->
  (fun _s#6646 -> (fun _t#6647 -> ((poly_stub_130)@(L(unit)))))[@inline] in
let reset_state#798 =
  fun _n#6651 -> (fun _l#6652 -> ((poly_stub_129)@(L(unit))))[@inline] in
let reset_state_at#799 =
  fun _t#6654 ->
  (fun _n#6655 -> (fun _l#6656 -> ((poly_stub_128)@(L(unit)))))[@inline] in
let save_mutation#802 =
  fun _s#6665 -> (fun _m#6666 -> ((poly_stub_127)@(L(unit))))[@inline] in
let sign#805 =
  fun _sk#6674 -> (fun _d#6675 -> ((poly_stub_126)@(L(unit))))[@inline] in
let add_account#806 =
  fun _s#6677 -> (fun _k#6678 -> ((poly_stub_125)@(L(unit))))[@inline] in
let baker_account#807 =
  fun _p#6680 -> (fun _o#6681 -> ((poly_stub_124)@(L(unit))))[@inline] in
let create_chest#809 =
  fun _b#6686 -> (fun _n#6687 -> ((poly_stub_123)@(L(unit))))[@inline] in
let create_chest_key#810 =
  fun _c#6689 -> (fun _n#6690 -> ((poly_stub_122)@(L(unit))))[@inline] in
let michelson_equal#813 =
  fun _m1#6700 -> (fun _m2#6701 -> ((poly_stub_121)@(L(unit))))[@inline] in
let originate_contract#815 =
  fun _c#6706 ->
  (fun _s#6707 -> (fun _t#6708 -> ((poly_stub_120)@(L(unit)))))[@inline] in
let compile_contract_from_file#817 =
  fun _fn#6714 ->
  (fun _e#6715 -> (fun _v#6716 -> ((poly_stub_119)@(L(unit)))))[@inline] in
let originate_from_file#818 =
  fun _fn#6718 ->
  (fun _e#6719 ->
   (fun _v#6720 ->
    (fun _s#6721 -> (fun _t#6722 -> ((poly_stub_118)@(L(unit)))))))[@inline] in
let toto#819 = L(43) in
let get_balance#820 =
  fun _u#6725 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#821 =
  fun _u#6727 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#822 = fun _u#6729 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#823 =
  fun _u#6731 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#824 =
  fun _u#6733 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#825 = fun _u#6735 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#826 = fun _u#6737 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#827 =
  fun _u#6739 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#828 =
  fun _u#6741 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#829 =
  fun _u#6743 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#830 =
  fun kh#6745 -> (({ VOTING_POWER })@(kh#6745))[@inline] in
let implicit_account#832 =
  fun kh#6749 -> (IMPLICIT_ACCOUNT(kh#6749))[@inline] in
let pairing_check#836 =
  fun l#6757 -> (({ PAIRING_CHECK })@(l#6757))[@inline] in
let set_delegate#838 = fun o#6761 -> (SET_DELEGATE(o#6761))[@inline] in
let open_chest#846 =
  fun ck#6782 ->
  (fun c#6783 -> (fun n#6784 -> (OPEN_CHEST(ck#6782 , c#6783 , n#6784))))[@inline] in
let xor#855 =
  fun l#6818 -> (fun r#6819 -> (XOR(l#6818 , r#6819)))[@inline] in
let or#856 = fun l#6821 -> (fun r#6822 -> (OR(l#6821 , r#6822)))[@inline] in
let shift_left#857 =
  fun l#6824 -> (fun r#6825 -> (LSL(l#6824 , r#6825)))[@inline] in
let shift_right#858 =
  fun l#6827 -> (fun r#6828 -> (LSR(l#6827 , r#6828)))[@inline] in
let length#903 = fun b#6973 -> (({ SIZE })@(b#6973))[@inline] in
let concat#904 =
  fun b1#6975 ->
  (fun b2#6976 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6975 , b2#6976))))[@inline] in
let sub#905 =
  fun s#6978 ->
  (fun l#6979 ->
   (fun b#6980 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6978 ,
                                                                   l#6979) ,
                                                              b#6980)))))[@inline] in
let length#911 = fun b#6995 -> (({ SIZE })@(b#6995))[@inline] in
let concat#912 =
  fun b1#6997 ->
  (fun b2#6998 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6997 , b2#6998))))[@inline] in
let sub#913 =
  fun s#7000 ->
  (fun l#7001 ->
   (fun b#7002 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7000 ,
                                                                   l#7001) ,
                                                              b#7002)))))[@inline] in
let blake2b#914 = fun b#7004 -> (({ BLAKE2B })@(b#7004))[@inline] in
let sha256#915 = fun b#7006 -> (({ SHA256 })@(b#7006))[@inline] in
let sha512#916 = fun b#7008 -> (({ SHA512 })@(b#7008))[@inline] in
let sha3#917 = fun b#7010 -> (({ SHA3 })@(b#7010))[@inline] in
let keccak#918 = fun b#7012 -> (({ KECCAK })@(b#7012))[@inline] in
let hash_key#919 = fun k#7014 -> (({ HASH_KEY })@(k#7014))[@inline] in
let check#920 =
  fun k#7016 ->
  (fun s#7017 ->
   (fun b#7018 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7016 , s#7017) ,
                                                   b#7018)))))[@inline] in
let assert#921 =
  fun b#7020 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7020))[@inline] in
let abs#924 = fun i#7026 -> (({ ABS })@(i#7026))[@inline] in
let is_nat#925 = fun i#7028 -> (({ ISNAT })@(i#7028))[@inline] in
let true#926 = TRUE()[@inline] in
let false#927 = FALSE()[@inline] in
let unit#928 = UNIT()[@inline] in
let assert_with_error#931 =
  fun b#7036 ->
  (fun s#7037 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7036 , s#7037))))[@inline] in
let poly_stub_117 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_116 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_115 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_114 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_113 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_112 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_111 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_110 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_109 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_108 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_107 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_106 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_105 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_104 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_103 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_102 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_101 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_100 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_99 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_98 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_97 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_96 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_95 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_94 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_93 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_92 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_91 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_90 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_89 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_88 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_87 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_86 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_85 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_84 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_83 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_82 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_81 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_80 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let poly_stub_79 = fun x#7048 -> (({ FAILWITH })@(x#7048))[@inline] in
let get_total_voting_power#939 =
  fun _u#7057 -> ((poly_stub_117)@(L(unit)))[@inline] in
let set_source#942 = fun _a#7063 -> ((poly_stub_116)@(L(unit)))[@inline] in
let get_storage_of_address#943 =
  fun _a#7065 -> ((poly_stub_115)@(L(unit)))[@inline] in
let get_balance#944 = fun _a#7067 -> ((poly_stub_114)@(L(unit)))[@inline] in
let print#945 = fun _v#7069 -> ((poly_stub_113)@(L(unit)))[@inline] in
let eprint#946 = fun _v#7071 -> ((poly_stub_112)@(L(unit)))[@inline] in
let get_voting_power#947 =
  fun _kh#7073 -> ((poly_stub_111)@(L(unit)))[@inline] in
let nth_bootstrap_contract#948 =
  fun _i#7075 -> ((poly_stub_110)@(L(unit)))[@inline] in
let nth_bootstrap_account#949 =
  fun _i#7077 -> ((poly_stub_109)@(L(unit)))[@inline] in
let get_bootstrap_account#950 =
  fun _n#7079 -> ((poly_stub_108)@(L(unit)))[@inline] in
let last_originations#952 =
  fun _u#7083 -> ((poly_stub_107)@(L(unit)))[@inline] in
let new_account#954 = fun _u#7087 -> ((poly_stub_106)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#956 =
  fun _n#7091 -> ((poly_stub_105)@(L(unit)))[@inline] in
let register_delegate#958 =
  fun _kh#7095 -> ((poly_stub_104)@(L(unit)))[@inline] in
let register_constant#959 =
  fun _m#7097 -> ((poly_stub_103)@(L(unit)))[@inline] in
let constant_to_michelson_program#961 =
  fun _s#7101 -> ((poly_stub_102)@(L(unit)))[@inline] in
let restore_context#962 =
  fun _u#7103 -> ((poly_stub_101)@(L(unit)))[@inline] in
let save_context#963 = fun _u#7105 -> ((poly_stub_100)@(L(unit)))[@inline] in
let drop_context#964 = fun _u#7107 -> ((poly_stub_99)@(L(unit)))[@inline] in
let set_baker_policy#967 =
  fun _bp#7113 -> ((poly_stub_98)@(L(unit)))[@inline] in
let set_baker#968 = fun _a#7115 -> ((poly_stub_97)@(L(unit)))[@inline] in
let size#969 = fun _c#7117 -> ((poly_stub_96)@(L(unit)))[@inline] in
let read_contract_from_file#971 =
  fun _fn#7121 -> ((poly_stub_95)@(L(unit)))[@inline] in
let chr#972 = fun _n#7123 -> ((poly_stub_94)@(L(unit)))[@inline] in
let nl#973 = L("NEWLINE")[@inline] in
let println#974 = fun _v#7126 -> ((poly_stub_93)@(L(unit)))[@inline] in
let transfer#975 =
  fun _a#7128 -> (fun _s#7129 -> (fun _t#7130 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#976 =
  fun _a#7132 -> (fun _s#7133 -> (fun _t#7134 -> ((poly_stub_91)@(L(unit)))))[@inline] in
let reset_state#978 =
  fun _n#7138 -> (fun _l#7139 -> ((poly_stub_90)@(L(unit))))[@inline] in
let reset_state_at#979 =
  fun _t#7141 -> (fun _n#7142 -> (fun _l#7143 -> ((poly_stub_89)@(L(unit)))))[@inline] in
let save_mutation#982 =
  fun _s#7152 -> (fun _m#7153 -> ((poly_stub_88)@(L(unit))))[@inline] in
let sign#985 =
  fun _sk#7161 -> (fun _d#7162 -> ((poly_stub_87)@(L(unit))))[@inline] in
let add_account#986 =
  fun _s#7164 -> (fun _k#7165 -> ((poly_stub_86)@(L(unit))))[@inline] in
let baker_account#987 =
  fun _p#7167 -> (fun _o#7168 -> ((poly_stub_85)@(L(unit))))[@inline] in
let create_chest#989 =
  fun _b#7173 -> (fun _n#7174 -> ((poly_stub_84)@(L(unit))))[@inline] in
let create_chest_key#990 =
  fun _c#7176 -> (fun _n#7177 -> ((poly_stub_83)@(L(unit))))[@inline] in
let michelson_equal#993 =
  fun _m1#7187 -> (fun _m2#7188 -> ((poly_stub_82)@(L(unit))))[@inline] in
let originate_contract#995 =
  fun _c#7193 -> (fun _s#7194 -> (fun _t#7195 -> ((poly_stub_81)@(L(unit)))))[@inline] in
let compile_contract_from_file#997 =
  fun _fn#7201 ->
  (fun _e#7202 -> (fun _v#7203 -> ((poly_stub_80)@(L(unit)))))[@inline] in
let originate_from_file#998 =
  fun _fn#7205 ->
  (fun _e#7206 ->
   (fun _v#7207 ->
    (fun _s#7208 -> (fun _t#7209 -> ((poly_stub_79)@(L(unit)))))))[@inline] in
let tata#999 = ADD(toto#277 , titi#458) in
let foo#1000 = (f#459)@(PAIR(L(unit) , L(3))) in
let get_balance#1001 =
  fun _u#7213 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1002 =
  fun _u#7215 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1003 = fun _u#7217 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1004 =
  fun _u#7219 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1005 =
  fun _u#7221 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1006 =
  fun _u#7223 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1007 = fun _u#7225 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1008 =
  fun _u#7227 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1009 =
  fun _u#7229 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1010 =
  fun _u#7231 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1011 =
  fun kh#7233 -> (({ VOTING_POWER })@(kh#7233))[@inline] in
let implicit_account#1013 =
  fun kh#7237 -> (IMPLICIT_ACCOUNT(kh#7237))[@inline] in
let pairing_check#1017 =
  fun l#7245 -> (({ PAIRING_CHECK })@(l#7245))[@inline] in
let set_delegate#1019 = fun o#7249 -> (SET_DELEGATE(o#7249))[@inline] in
let open_chest#1027 =
  fun ck#7270 ->
  (fun c#7271 -> (fun n#7272 -> (OPEN_CHEST(ck#7270 , c#7271 , n#7272))))[@inline] in
let xor#1036 =
  fun l#7306 -> (fun r#7307 -> (XOR(l#7306 , r#7307)))[@inline] in
let or#1037 = fun l#7309 -> (fun r#7310 -> (OR(l#7309 , r#7310)))[@inline] in
let shift_left#1038 =
  fun l#7312 -> (fun r#7313 -> (LSL(l#7312 , r#7313)))[@inline] in
let shift_right#1039 =
  fun l#7315 -> (fun r#7316 -> (LSR(l#7315 , r#7316)))[@inline] in
let length#1084 = fun b#7461 -> (({ SIZE })@(b#7461))[@inline] in
let concat#1085 =
  fun b1#7463 ->
  (fun b2#7464 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7463 , b2#7464))))[@inline] in
let sub#1086 =
  fun s#7466 ->
  (fun l#7467 ->
   (fun b#7468 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7466 ,
                                                                   l#7467) ,
                                                              b#7468)))))[@inline] in
let length#1092 = fun b#7483 -> (({ SIZE })@(b#7483))[@inline] in
let concat#1093 =
  fun b1#7485 ->
  (fun b2#7486 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7485 , b2#7486))))[@inline] in
let sub#1094 =
  fun s#7488 ->
  (fun l#7489 ->
   (fun b#7490 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7488 ,
                                                                   l#7489) ,
                                                              b#7490)))))[@inline] in
let blake2b#1095 = fun b#7492 -> (({ BLAKE2B })@(b#7492))[@inline] in
let sha256#1096 = fun b#7494 -> (({ SHA256 })@(b#7494))[@inline] in
let sha512#1097 = fun b#7496 -> (({ SHA512 })@(b#7496))[@inline] in
let sha3#1098 = fun b#7498 -> (({ SHA3 })@(b#7498))[@inline] in
let keccak#1099 = fun b#7500 -> (({ KECCAK })@(b#7500))[@inline] in
let hash_key#1100 = fun k#7502 -> (({ HASH_KEY })@(k#7502))[@inline] in
let check#1101 =
  fun k#7504 ->
  (fun s#7505 ->
   (fun b#7506 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7504 , s#7505) ,
                                                   b#7506)))))[@inline] in
let assert#1102 =
  fun b#7508 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7508))[@inline] in
let abs#1105 = fun i#7514 -> (({ ABS })@(i#7514))[@inline] in
let is_nat#1106 = fun i#7516 -> (({ ISNAT })@(i#7516))[@inline] in
let true#1107 = TRUE()[@inline] in
let false#1108 = FALSE()[@inline] in
let unit#1109 = UNIT()[@inline] in
let assert_with_error#1112 =
  fun b#7524 ->
  (fun s#7525 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7524 , s#7525))))[@inline] in
let poly_stub_78 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_77 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_76 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_75 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_74 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_73 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_72 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_71 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_70 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_69 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_68 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_67 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_66 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_65 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_64 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_63 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_62 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_61 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_60 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_59 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_58 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_57 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_56 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_55 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_54 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_53 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_52 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_51 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_50 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_49 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_48 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_47 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_46 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_45 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_44 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_43 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_42 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_41 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let poly_stub_40 = fun x#7536 -> (({ FAILWITH })@(x#7536))[@inline] in
let get_total_voting_power#1120 =
  fun _u#7545 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#1123 = fun _a#7551 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#1124 =
  fun _a#7553 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#1125 = fun _a#7555 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#1126 = fun _v#7557 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#1127 = fun _v#7559 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#1128 =
  fun _kh#7561 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1129 =
  fun _i#7563 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#1130 =
  fun _i#7565 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#1131 =
  fun _n#7567 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#1133 =
  fun _u#7571 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#1135 = fun _u#7575 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1137 =
  fun _n#7579 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#1139 =
  fun _kh#7583 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#1140 =
  fun _m#7585 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#1142 =
  fun _s#7589 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#1143 =
  fun _u#7591 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#1144 = fun _u#7593 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#1145 = fun _u#7595 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#1148 =
  fun _bp#7601 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#1149 = fun _a#7603 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#1150 = fun _c#7605 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#1152 =
  fun _fn#7609 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#1153 = fun _n#7611 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#1154 = L("NEWLINE")[@inline] in
let println#1155 = fun _v#7614 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#1156 =
  fun _a#7616 -> (fun _s#7617 -> (fun _t#7618 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#1157 =
  fun _a#7620 -> (fun _s#7621 -> (fun _t#7622 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#1159 =
  fun _n#7626 -> (fun _l#7627 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#1160 =
  fun _t#7629 -> (fun _n#7630 -> (fun _l#7631 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#1163 =
  fun _s#7640 -> (fun _m#7641 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#1166 =
  fun _sk#7649 -> (fun _d#7650 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#1167 =
  fun _s#7652 -> (fun _k#7653 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#1168 =
  fun _p#7655 -> (fun _o#7656 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#1170 =
  fun _b#7661 -> (fun _n#7662 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#1171 =
  fun _c#7664 -> (fun _n#7665 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#1174 =
  fun _m1#7675 -> (fun _m2#7676 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#1176 =
  fun _c#7681 -> (fun _s#7682 -> (fun _t#7683 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#1178 =
  fun _fn#7689 ->
  (fun _e#7690 -> (fun _v#7691 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#1179 =
  fun _fn#7693 ->
  (fun _e#7694 ->
   (fun _v#7695 ->
    (fun _s#7696 -> (fun _t#7697 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let toto#1180 = L(10) in
let foo#1181 = L("bar") in
let get_balance#1182 =
  fun _u#7701 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1183 =
  fun _u#7703 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1184 = fun _u#7705 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1185 =
  fun _u#7707 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1186 =
  fun _u#7709 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1187 =
  fun _u#7711 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1188 = fun _u#7713 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1189 =
  fun _u#7715 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1190 =
  fun _u#7717 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1191 =
  fun _u#7719 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1192 =
  fun kh#7721 -> (({ VOTING_POWER })@(kh#7721))[@inline] in
let implicit_account#1194 =
  fun kh#7725 -> (IMPLICIT_ACCOUNT(kh#7725))[@inline] in
let pairing_check#1198 =
  fun l#7733 -> (({ PAIRING_CHECK })@(l#7733))[@inline] in
let set_delegate#1200 = fun o#7737 -> (SET_DELEGATE(o#7737))[@inline] in
let open_chest#1208 =
  fun ck#7758 ->
  (fun c#7759 -> (fun n#7760 -> (OPEN_CHEST(ck#7758 , c#7759 , n#7760))))[@inline] in
let xor#1217 =
  fun l#7794 -> (fun r#7795 -> (XOR(l#7794 , r#7795)))[@inline] in
let or#1218 = fun l#7797 -> (fun r#7798 -> (OR(l#7797 , r#7798)))[@inline] in
let shift_left#1219 =
  fun l#7800 -> (fun r#7801 -> (LSL(l#7800 , r#7801)))[@inline] in
let shift_right#1220 =
  fun l#7803 -> (fun r#7804 -> (LSR(l#7803 , r#7804)))[@inline] in
let length#1265 = fun b#7949 -> (({ SIZE })@(b#7949))[@inline] in
let concat#1266 =
  fun b1#7951 ->
  (fun b2#7952 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7951 , b2#7952))))[@inline] in
let sub#1267 =
  fun s#7954 ->
  (fun l#7955 ->
   (fun b#7956 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7954 ,
                                                                   l#7955) ,
                                                              b#7956)))))[@inline] in
let length#1273 = fun b#7971 -> (({ SIZE })@(b#7971))[@inline] in
let concat#1274 =
  fun b1#7973 ->
  (fun b2#7974 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7973 , b2#7974))))[@inline] in
let sub#1275 =
  fun s#7976 ->
  (fun l#7977 ->
   (fun b#7978 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7976 ,
                                                                   l#7977) ,
                                                              b#7978)))))[@inline] in
let blake2b#1276 = fun b#7980 -> (({ BLAKE2B })@(b#7980))[@inline] in
let sha256#1277 = fun b#7982 -> (({ SHA256 })@(b#7982))[@inline] in
let sha512#1278 = fun b#7984 -> (({ SHA512 })@(b#7984))[@inline] in
let sha3#1279 = fun b#7986 -> (({ SHA3 })@(b#7986))[@inline] in
let keccak#1280 = fun b#7988 -> (({ KECCAK })@(b#7988))[@inline] in
let hash_key#1281 = fun k#7990 -> (({ HASH_KEY })@(k#7990))[@inline] in
let check#1282 =
  fun k#7992 ->
  (fun s#7993 ->
   (fun b#7994 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7992 , s#7993) ,
                                                   b#7994)))))[@inline] in
let assert =
  fun b#7996 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7996))[@inline] in
let abs = fun i#8002 -> (({ ABS })@(i#8002))[@inline] in
let is_nat = fun i#8004 -> (({ ISNAT })@(i#8004))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#8012 ->
  (fun s#8013 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#8012 , s#8013))))[@inline] in
let poly_stub_39 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_38 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_37 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_36 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_35 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_34 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_33 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_32 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_31 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_30 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_29 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_28 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_27 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_26 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_25 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_24 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_23 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_22 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_21 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_20 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_19 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_18 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_17 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_16 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_15 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_14 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_13 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_12 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_11 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_10 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_9 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_8 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_7 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_6 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_5 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_4 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_3 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_2 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let poly_stub_1 = fun x#8024 -> (({ FAILWITH })@(x#8024))[@inline] in
let get_total_voting_power#1287 =
  fun _u#8033 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#1290 = fun _a#8039 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#1291 =
  fun _a#8041 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#1292 = fun _a#8043 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#1293 = fun _v#8045 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#1294 = fun _v#8047 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#1295 =
  fun _kh#8049 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1296 =
  fun _i#8051 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#1297 =
  fun _i#8053 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#1298 =
  fun _n#8055 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#1300 =
  fun _u#8059 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#1302 = fun _u#8063 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1304 =
  fun _n#8067 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#1306 =
  fun _kh#8071 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#1307 =
  fun _m#8073 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#1309 =
  fun _s#8077 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#1310 =
  fun _u#8079 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#1311 = fun _u#8081 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#1312 = fun _u#8083 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#1315 =
  fun _bp#8089 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#1316 = fun _a#8091 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#1317 = fun _c#8093 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#1319 =
  fun _fn#8097 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1320 = fun _n#8099 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#1321 = L("NEWLINE")[@inline] in
let println#1322 = fun _v#8102 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#1323 =
  fun _a#8104 -> (fun _s#8105 -> (fun _t#8106 -> ((poly_stub_14)@(L(unit)))))[@inline] in
let transfer_exn#1324 =
  fun _a#8108 -> (fun _s#8109 -> (fun _t#8110 -> ((poly_stub_13)@(L(unit)))))[@inline] in
let reset_state#1326 =
  fun _n#8114 -> (fun _l#8115 -> ((poly_stub_12)@(L(unit))))[@inline] in
let reset_state_at#1327 =
  fun _t#8117 -> (fun _n#8118 -> (fun _l#8119 -> ((poly_stub_11)@(L(unit)))))[@inline] in
let save_mutation#1330 =
  fun _s#8128 -> (fun _m#8129 -> ((poly_stub_10)@(L(unit))))[@inline] in
let sign#1333 =
  fun _sk#8137 -> (fun _d#8138 -> ((poly_stub_9)@(L(unit))))[@inline] in
let add_account#1334 =
  fun _s#8140 -> (fun _k#8141 -> ((poly_stub_8)@(L(unit))))[@inline] in
let baker_account#1335 =
  fun _p#8143 -> (fun _o#8144 -> ((poly_stub_7)@(L(unit))))[@inline] in
let create_chest#1337 =
  fun _b#8149 -> (fun _n#8150 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#1338 =
  fun _c#8152 -> (fun _n#8153 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#1341 =
  fun _m1#8163 -> (fun _m2#8164 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#1343 =
  fun _c#8169 -> (fun _s#8170 -> (fun _t#8171 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#1345 =
  fun _fn#8177 -> (fun _e#8178 -> (fun _v#8179 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#1346 =
  fun _fn#8181 ->
  (fun _e#8182 ->
   (fun _v#8183 ->
    (fun _s#8184 -> (fun _t#8185 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1180 , toto#277) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#8189 ->
  (let (gen#8195, gen#8196) = gen#8189 in
   let p#8190 = gen#8195 in
   let s#8191 = gen#8196 in
   let s#8192 = ADD(ADD(p#8190 , s#8191) , toto) in
   PAIR(LIST_EMPTY() , s#8192)) in
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
