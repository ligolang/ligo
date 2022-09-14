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
                                                                     | ( p : int , s : int ) ->
                                                                     let s : int =
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
        | ( _#4 : unit , _#3 : string ) ->
        ( LIST_EMPTY() , CONCAT(Errors.undefined_token , Storage.s) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let get_balance#98 =
  fun _u#4791 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#99 =
  fun _u#4793 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#100 = fun _u#4795 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#101 =
  fun _u#4797 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#102 =
  fun _u#4799 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#103 = fun _u#4801 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#104 = fun _u#4803 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#105 =
  fun _u#4805 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#106 =
  fun _u#4807 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#107 =
  fun _u#4809 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#108 =
  fun kh#4811 -> (({ VOTING_POWER })@(kh#4811))[@inline] in
let implicit_account#110 =
  fun kh#4815 -> (IMPLICIT_ACCOUNT(kh#4815))[@inline] in
let pairing_check#114 =
  fun l#4823 -> (({ PAIRING_CHECK })@(l#4823))[@inline] in
let set_delegate#116 = fun o#4827 -> (SET_DELEGATE(o#4827))[@inline] in
let open_chest#124 =
  fun ck#4848 ->
  (fun c#4849 -> (fun n#4850 -> (OPEN_CHEST(ck#4848 , c#4849 , n#4850))))[@inline] in
let xor#133 =
  fun l#4884 -> (fun r#4885 -> (XOR(l#4884 , r#4885)))[@inline] in
let or#134 = fun l#4887 -> (fun r#4888 -> (OR(l#4887 , r#4888)))[@inline] in
let shift_left#135 =
  fun l#4890 -> (fun r#4891 -> (LSL(l#4890 , r#4891)))[@inline] in
let shift_right#136 =
  fun l#4893 -> (fun r#4894 -> (LSR(l#4893 , r#4894)))[@inline] in
let length#181 = fun b#5039 -> (({ SIZE })@(b#5039))[@inline] in
let concat#182 =
  fun b1#5041 ->
  (fun b2#5042 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5041 , b2#5042))))[@inline] in
let sub#183 =
  fun s#5044 ->
  (fun l#5045 ->
   (fun b#5046 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5044 ,
                                                                   l#5045) ,
                                                              b#5046)))))[@inline] in
let length#189 = fun b#5061 -> (({ SIZE })@(b#5061))[@inline] in
let concat#190 =
  fun b1#5063 ->
  (fun b2#5064 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5063 , b2#5064))))[@inline] in
let sub#191 =
  fun s#5066 ->
  (fun l#5067 ->
   (fun b#5068 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5066 ,
                                                                   l#5067) ,
                                                              b#5068)))))[@inline] in
let blake2b#192 = fun b#5070 -> (({ BLAKE2B })@(b#5070))[@inline] in
let sha256#193 = fun b#5072 -> (({ SHA256 })@(b#5072))[@inline] in
let sha512#194 = fun b#5074 -> (({ SHA512 })@(b#5074))[@inline] in
let sha3#195 = fun b#5076 -> (({ SHA3 })@(b#5076))[@inline] in
let keccak#196 = fun b#5078 -> (({ KECCAK })@(b#5078))[@inline] in
let hash_key#197 = fun k#5080 -> (({ HASH_KEY })@(k#5080))[@inline] in
let check#198 =
  fun k#5082 ->
  (fun s#5083 ->
   (fun b#5084 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5082 , s#5083) ,
                                                   b#5084)))))[@inline] in
let assert#199 =
  fun b#5086 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5086))[@inline] in
let abs#202 = fun i#5092 -> (({ ABS })@(i#5092))[@inline] in
let is_nat#203 = fun i#5094 -> (({ ISNAT })@(i#5094))[@inline] in
let true#204 = TRUE()[@inline] in
let false#205 = FALSE()[@inline] in
let unit#206 = UNIT()[@inline] in
let assert_with_error#210 =
  fun b#5104 ->
  (fun s#5105 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5104 , s#5105))))[@inline] in
let poly_stub_273 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_272 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_271 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_270 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_269 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_268 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_267 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_266 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_265 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_264 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_263 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_262 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_261 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_260 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_259 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_258 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_257 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_256 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_255 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_254 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_253 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_252 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_251 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_250 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_249 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_248 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_247 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_246 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_245 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_244 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_243 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_242 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_241 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_240 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_239 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_238 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_237 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_236 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let poly_stub_235 = fun x#5116 -> (({ FAILWITH })@(x#5116))[@inline] in
let get_total_voting_power#218 =
  fun _u#5125 -> ((poly_stub_273)@(L(unit)))[@inline] in
let set_source#221 = fun _a#5131 -> ((poly_stub_272)@(L(unit)))[@inline] in
let get_storage_of_address#222 =
  fun _a#5133 -> ((poly_stub_271)@(L(unit)))[@inline] in
let get_balance#223 = fun _a#5135 -> ((poly_stub_270)@(L(unit)))[@inline] in
let print#224 = fun _v#5137 -> ((poly_stub_269)@(L(unit)))[@inline] in
let eprint#225 = fun _v#5139 -> ((poly_stub_268)@(L(unit)))[@inline] in
let get_voting_power#226 =
  fun _kh#5141 -> ((poly_stub_267)@(L(unit)))[@inline] in
let nth_bootstrap_contract#227 =
  fun _i#5143 -> ((poly_stub_266)@(L(unit)))[@inline] in
let nth_bootstrap_account#228 =
  fun _i#5145 -> ((poly_stub_265)@(L(unit)))[@inline] in
let get_bootstrap_account#229 =
  fun _n#5147 -> ((poly_stub_264)@(L(unit)))[@inline] in
let last_originations#231 =
  fun _u#5151 -> ((poly_stub_263)@(L(unit)))[@inline] in
let new_account#233 = fun _u#5155 -> ((poly_stub_262)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#235 =
  fun _n#5159 -> ((poly_stub_261)@(L(unit)))[@inline] in
let register_delegate#237 =
  fun _kh#5163 -> ((poly_stub_260)@(L(unit)))[@inline] in
let register_constant#238 =
  fun _m#5165 -> ((poly_stub_259)@(L(unit)))[@inline] in
let constant_to_michelson_program#240 =
  fun _s#5169 -> ((poly_stub_258)@(L(unit)))[@inline] in
let restore_context#241 =
  fun _u#5171 -> ((poly_stub_257)@(L(unit)))[@inline] in
let save_context#242 = fun _u#5173 -> ((poly_stub_256)@(L(unit)))[@inline] in
let drop_context#243 = fun _u#5175 -> ((poly_stub_255)@(L(unit)))[@inline] in
let set_baker_policy#246 =
  fun _bp#5181 -> ((poly_stub_254)@(L(unit)))[@inline] in
let set_baker#247 = fun _a#5183 -> ((poly_stub_253)@(L(unit)))[@inline] in
let size#248 = fun _c#5185 -> ((poly_stub_252)@(L(unit)))[@inline] in
let read_contract_from_file#250 =
  fun _fn#5189 -> ((poly_stub_251)@(L(unit)))[@inline] in
let chr#251 = fun _n#5191 -> ((poly_stub_250)@(L(unit)))[@inline] in
let nl#252 = L("NEWLINE")[@inline] in
let println#253 = fun _v#5194 -> ((poly_stub_249)@(L(unit)))[@inline] in
let transfer#254 =
  fun _a#5196 ->
  (fun _s#5197 -> (fun _t#5198 -> ((poly_stub_248)@(L(unit)))))[@inline] in
let transfer_exn#255 =
  fun _a#5200 ->
  (fun _s#5201 -> (fun _t#5202 -> ((poly_stub_247)@(L(unit)))))[@inline] in
let reset_state#257 =
  fun _n#5206 -> (fun _l#5207 -> ((poly_stub_246)@(L(unit))))[@inline] in
let reset_state_at#258 =
  fun _t#5209 ->
  (fun _n#5210 -> (fun _l#5211 -> ((poly_stub_245)@(L(unit)))))[@inline] in
let save_mutation#261 =
  fun _s#5220 -> (fun _m#5221 -> ((poly_stub_244)@(L(unit))))[@inline] in
let sign#264 =
  fun _sk#5229 -> (fun _d#5230 -> ((poly_stub_243)@(L(unit))))[@inline] in
let add_account#265 =
  fun _s#5232 -> (fun _k#5233 -> ((poly_stub_242)@(L(unit))))[@inline] in
let baker_account#266 =
  fun _p#5235 -> (fun _o#5236 -> ((poly_stub_241)@(L(unit))))[@inline] in
let create_chest#268 =
  fun _b#5241 -> (fun _n#5242 -> ((poly_stub_240)@(L(unit))))[@inline] in
let create_chest_key#269 =
  fun _c#5244 -> (fun _n#5245 -> ((poly_stub_239)@(L(unit))))[@inline] in
let michelson_equal#272 =
  fun _m1#5255 -> (fun _m2#5256 -> ((poly_stub_238)@(L(unit))))[@inline] in
let originate_contract#274 =
  fun _c#5261 ->
  (fun _s#5262 -> (fun _t#5263 -> ((poly_stub_237)@(L(unit)))))[@inline] in
let compile_contract_from_file#276 =
  fun _fn#5269 ->
  (fun _e#5270 -> (fun _v#5271 -> ((poly_stub_236)@(L(unit)))))[@inline] in
let originate_from_file#277 =
  fun _fn#5273 ->
  (fun _e#5274 ->
   (fun _v#5275 ->
    (fun _s#5276 -> (fun _t#5277 -> ((poly_stub_235)@(L(unit)))))))[@inline] in
let toto#278 = L(1) in
let get_balance#279 =
  fun _u#5280 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#280 =
  fun _u#5282 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#281 = fun _u#5284 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#282 =
  fun _u#5286 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#283 =
  fun _u#5288 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#284 = fun _u#5290 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#285 = fun _u#5292 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#286 =
  fun _u#5294 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#287 =
  fun _u#5296 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#288 =
  fun _u#5298 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#289 =
  fun kh#5300 -> (({ VOTING_POWER })@(kh#5300))[@inline] in
let implicit_account#291 =
  fun kh#5304 -> (IMPLICIT_ACCOUNT(kh#5304))[@inline] in
let pairing_check#295 =
  fun l#5312 -> (({ PAIRING_CHECK })@(l#5312))[@inline] in
let set_delegate#297 = fun o#5316 -> (SET_DELEGATE(o#5316))[@inline] in
let open_chest#305 =
  fun ck#5337 ->
  (fun c#5338 -> (fun n#5339 -> (OPEN_CHEST(ck#5337 , c#5338 , n#5339))))[@inline] in
let xor#314 =
  fun l#5373 -> (fun r#5374 -> (XOR(l#5373 , r#5374)))[@inline] in
let or#315 = fun l#5376 -> (fun r#5377 -> (OR(l#5376 , r#5377)))[@inline] in
let shift_left#316 =
  fun l#5379 -> (fun r#5380 -> (LSL(l#5379 , r#5380)))[@inline] in
let shift_right#317 =
  fun l#5382 -> (fun r#5383 -> (LSR(l#5382 , r#5383)))[@inline] in
let length#362 = fun b#5528 -> (({ SIZE })@(b#5528))[@inline] in
let concat#363 =
  fun b1#5530 ->
  (fun b2#5531 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5530 , b2#5531))))[@inline] in
let sub#364 =
  fun s#5533 ->
  (fun l#5534 ->
   (fun b#5535 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5533 ,
                                                                   l#5534) ,
                                                              b#5535)))))[@inline] in
let length#370 = fun b#5550 -> (({ SIZE })@(b#5550))[@inline] in
let concat#371 =
  fun b1#5552 ->
  (fun b2#5553 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5552 , b2#5553))))[@inline] in
let sub#372 =
  fun s#5555 ->
  (fun l#5556 ->
   (fun b#5557 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5555 ,
                                                                   l#5556) ,
                                                              b#5557)))))[@inline] in
let blake2b#373 = fun b#5559 -> (({ BLAKE2B })@(b#5559))[@inline] in
let sha256#374 = fun b#5561 -> (({ SHA256 })@(b#5561))[@inline] in
let sha512#375 = fun b#5563 -> (({ SHA512 })@(b#5563))[@inline] in
let sha3#376 = fun b#5565 -> (({ SHA3 })@(b#5565))[@inline] in
let keccak#377 = fun b#5567 -> (({ KECCAK })@(b#5567))[@inline] in
let hash_key#378 = fun k#5569 -> (({ HASH_KEY })@(k#5569))[@inline] in
let check#379 =
  fun k#5571 ->
  (fun s#5572 ->
   (fun b#5573 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5571 , s#5572) ,
                                                   b#5573)))))[@inline] in
let assert#380 =
  fun b#5575 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5575))[@inline] in
let abs#383 = fun i#5581 -> (({ ABS })@(i#5581))[@inline] in
let is_nat#384 = fun i#5583 -> (({ ISNAT })@(i#5583))[@inline] in
let true#385 = TRUE()[@inline] in
let false#386 = FALSE()[@inline] in
let unit#387 = UNIT()[@inline] in
let assert_with_error#391 =
  fun b#5593 ->
  (fun s#5594 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5593 , s#5594))))[@inline] in
let poly_stub_234 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_233 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_232 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_231 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_230 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_229 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_228 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_227 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_226 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_225 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_224 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_223 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_222 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_221 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_220 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_219 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_218 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_217 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_216 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_215 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_214 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_213 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_212 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_211 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_210 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_209 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_208 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_207 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_206 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_205 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_204 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_203 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_202 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_201 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_200 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_199 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_198 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_197 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let poly_stub_196 = fun x#5605 -> (({ FAILWITH })@(x#5605))[@inline] in
let get_total_voting_power#399 =
  fun _u#5614 -> ((poly_stub_234)@(L(unit)))[@inline] in
let set_source#402 = fun _a#5620 -> ((poly_stub_233)@(L(unit)))[@inline] in
let get_storage_of_address#403 =
  fun _a#5622 -> ((poly_stub_232)@(L(unit)))[@inline] in
let get_balance#404 = fun _a#5624 -> ((poly_stub_231)@(L(unit)))[@inline] in
let print#405 = fun _v#5626 -> ((poly_stub_230)@(L(unit)))[@inline] in
let eprint#406 = fun _v#5628 -> ((poly_stub_229)@(L(unit)))[@inline] in
let get_voting_power#407 =
  fun _kh#5630 -> ((poly_stub_228)@(L(unit)))[@inline] in
let nth_bootstrap_contract#408 =
  fun _i#5632 -> ((poly_stub_227)@(L(unit)))[@inline] in
let nth_bootstrap_account#409 =
  fun _i#5634 -> ((poly_stub_226)@(L(unit)))[@inline] in
let get_bootstrap_account#410 =
  fun _n#5636 -> ((poly_stub_225)@(L(unit)))[@inline] in
let last_originations#412 =
  fun _u#5640 -> ((poly_stub_224)@(L(unit)))[@inline] in
let new_account#414 = fun _u#5644 -> ((poly_stub_223)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#416 =
  fun _n#5648 -> ((poly_stub_222)@(L(unit)))[@inline] in
let register_delegate#418 =
  fun _kh#5652 -> ((poly_stub_221)@(L(unit)))[@inline] in
let register_constant#419 =
  fun _m#5654 -> ((poly_stub_220)@(L(unit)))[@inline] in
let constant_to_michelson_program#421 =
  fun _s#5658 -> ((poly_stub_219)@(L(unit)))[@inline] in
let restore_context#422 =
  fun _u#5660 -> ((poly_stub_218)@(L(unit)))[@inline] in
let save_context#423 = fun _u#5662 -> ((poly_stub_217)@(L(unit)))[@inline] in
let drop_context#424 = fun _u#5664 -> ((poly_stub_216)@(L(unit)))[@inline] in
let set_baker_policy#427 =
  fun _bp#5670 -> ((poly_stub_215)@(L(unit)))[@inline] in
let set_baker#428 = fun _a#5672 -> ((poly_stub_214)@(L(unit)))[@inline] in
let size#429 = fun _c#5674 -> ((poly_stub_213)@(L(unit)))[@inline] in
let read_contract_from_file#431 =
  fun _fn#5678 -> ((poly_stub_212)@(L(unit)))[@inline] in
let chr#432 = fun _n#5680 -> ((poly_stub_211)@(L(unit)))[@inline] in
let nl#433 = L("NEWLINE")[@inline] in
let println#434 = fun _v#5683 -> ((poly_stub_210)@(L(unit)))[@inline] in
let transfer#435 =
  fun _a#5685 ->
  (fun _s#5686 -> (fun _t#5687 -> ((poly_stub_209)@(L(unit)))))[@inline] in
let transfer_exn#436 =
  fun _a#5689 ->
  (fun _s#5690 -> (fun _t#5691 -> ((poly_stub_208)@(L(unit)))))[@inline] in
let reset_state#438 =
  fun _n#5695 -> (fun _l#5696 -> ((poly_stub_207)@(L(unit))))[@inline] in
let reset_state_at#439 =
  fun _t#5698 ->
  (fun _n#5699 -> (fun _l#5700 -> ((poly_stub_206)@(L(unit)))))[@inline] in
let save_mutation#442 =
  fun _s#5709 -> (fun _m#5710 -> ((poly_stub_205)@(L(unit))))[@inline] in
let sign#445 =
  fun _sk#5718 -> (fun _d#5719 -> ((poly_stub_204)@(L(unit))))[@inline] in
let add_account#446 =
  fun _s#5721 -> (fun _k#5722 -> ((poly_stub_203)@(L(unit))))[@inline] in
let baker_account#447 =
  fun _p#5724 -> (fun _o#5725 -> ((poly_stub_202)@(L(unit))))[@inline] in
let create_chest#449 =
  fun _b#5730 -> (fun _n#5731 -> ((poly_stub_201)@(L(unit))))[@inline] in
let create_chest_key#450 =
  fun _c#5733 -> (fun _n#5734 -> ((poly_stub_200)@(L(unit))))[@inline] in
let michelson_equal#453 =
  fun _m1#5744 -> (fun _m2#5745 -> ((poly_stub_199)@(L(unit))))[@inline] in
let originate_contract#455 =
  fun _c#5750 ->
  (fun _s#5751 -> (fun _t#5752 -> ((poly_stub_198)@(L(unit)))))[@inline] in
let compile_contract_from_file#457 =
  fun _fn#5758 ->
  (fun _e#5759 -> (fun _v#5760 -> ((poly_stub_197)@(L(unit)))))[@inline] in
let originate_from_file#458 =
  fun _fn#5762 ->
  (fun _e#5763 ->
   (fun _v#5764 ->
    (fun _s#5765 -> (fun _t#5766 -> ((poly_stub_196)@(L(unit)))))))[@inline] in
let toto#459 = L(32) in
let titi#460 = ADD(toto#278 , L(42)) in
let f#461 =
  fun gen#5770 ->
  (let (gen#8227, gen#8228) = gen#5770 in
   let gen#5771 = gen#8227 in
   let x#5772 = gen#8228 in
   let x#5773 = ADD(ADD(x#5772 , toto#278) , titi#460) in
   PAIR(LIST_EMPTY() , x#5773)) in
let get_balance#462 =
  fun _u#5775 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#463 =
  fun _u#5777 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#464 = fun _u#5779 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#465 =
  fun _u#5781 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#466 =
  fun _u#5783 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#467 = fun _u#5785 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#468 = fun _u#5787 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#469 =
  fun _u#5789 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#470 =
  fun _u#5791 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#471 =
  fun _u#5793 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#472 =
  fun kh#5795 -> (({ VOTING_POWER })@(kh#5795))[@inline] in
let implicit_account#474 =
  fun kh#5799 -> (IMPLICIT_ACCOUNT(kh#5799))[@inline] in
let pairing_check#478 =
  fun l#5807 -> (({ PAIRING_CHECK })@(l#5807))[@inline] in
let set_delegate#480 = fun o#5811 -> (SET_DELEGATE(o#5811))[@inline] in
let open_chest#488 =
  fun ck#5832 ->
  (fun c#5833 -> (fun n#5834 -> (OPEN_CHEST(ck#5832 , c#5833 , n#5834))))[@inline] in
let xor#497 =
  fun l#5868 -> (fun r#5869 -> (XOR(l#5868 , r#5869)))[@inline] in
let or#498 = fun l#5871 -> (fun r#5872 -> (OR(l#5871 , r#5872)))[@inline] in
let shift_left#499 =
  fun l#5874 -> (fun r#5875 -> (LSL(l#5874 , r#5875)))[@inline] in
let shift_right#500 =
  fun l#5877 -> (fun r#5878 -> (LSR(l#5877 , r#5878)))[@inline] in
let length#545 = fun b#6023 -> (({ SIZE })@(b#6023))[@inline] in
let concat#546 =
  fun b1#6025 ->
  (fun b2#6026 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6025 , b2#6026))))[@inline] in
let sub#547 =
  fun s#6028 ->
  (fun l#6029 ->
   (fun b#6030 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6028 ,
                                                                   l#6029) ,
                                                              b#6030)))))[@inline] in
let length#553 = fun b#6045 -> (({ SIZE })@(b#6045))[@inline] in
let concat#554 =
  fun b1#6047 ->
  (fun b2#6048 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6047 , b2#6048))))[@inline] in
let sub#555 =
  fun s#6050 ->
  (fun l#6051 ->
   (fun b#6052 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6050 ,
                                                                   l#6051) ,
                                                              b#6052)))))[@inline] in
let blake2b#556 = fun b#6054 -> (({ BLAKE2B })@(b#6054))[@inline] in
let sha256#557 = fun b#6056 -> (({ SHA256 })@(b#6056))[@inline] in
let sha512#558 = fun b#6058 -> (({ SHA512 })@(b#6058))[@inline] in
let sha3#559 = fun b#6060 -> (({ SHA3 })@(b#6060))[@inline] in
let keccak#560 = fun b#6062 -> (({ KECCAK })@(b#6062))[@inline] in
let hash_key#561 = fun k#6064 -> (({ HASH_KEY })@(k#6064))[@inline] in
let check#562 =
  fun k#6066 ->
  (fun s#6067 ->
   (fun b#6068 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6066 , s#6067) ,
                                                   b#6068)))))[@inline] in
let assert#563 =
  fun b#6070 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6070))[@inline] in
let abs#566 = fun i#6076 -> (({ ABS })@(i#6076))[@inline] in
let is_nat#567 = fun i#6078 -> (({ ISNAT })@(i#6078))[@inline] in
let true#568 = TRUE()[@inline] in
let false#569 = FALSE()[@inline] in
let unit#570 = UNIT()[@inline] in
let assert_with_error#574 =
  fun b#6088 ->
  (fun s#6089 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6088 , s#6089))))[@inline] in
let poly_stub_195 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_194 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_193 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_192 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_191 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_190 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_189 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_188 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_187 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_186 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_185 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_184 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_183 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_182 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_181 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_180 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_179 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_178 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_177 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_176 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_175 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_174 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_173 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_172 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_171 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_170 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_169 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_168 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_167 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_166 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_165 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_164 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_163 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_162 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_161 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_160 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_159 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_158 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let poly_stub_157 = fun x#6100 -> (({ FAILWITH })@(x#6100))[@inline] in
let get_total_voting_power#582 =
  fun _u#6109 -> ((poly_stub_195)@(L(unit)))[@inline] in
let set_source#585 = fun _a#6115 -> ((poly_stub_194)@(L(unit)))[@inline] in
let get_storage_of_address#586 =
  fun _a#6117 -> ((poly_stub_193)@(L(unit)))[@inline] in
let get_balance#587 = fun _a#6119 -> ((poly_stub_192)@(L(unit)))[@inline] in
let print#588 = fun _v#6121 -> ((poly_stub_191)@(L(unit)))[@inline] in
let eprint#589 = fun _v#6123 -> ((poly_stub_190)@(L(unit)))[@inline] in
let get_voting_power#590 =
  fun _kh#6125 -> ((poly_stub_189)@(L(unit)))[@inline] in
let nth_bootstrap_contract#591 =
  fun _i#6127 -> ((poly_stub_188)@(L(unit)))[@inline] in
let nth_bootstrap_account#592 =
  fun _i#6129 -> ((poly_stub_187)@(L(unit)))[@inline] in
let get_bootstrap_account#593 =
  fun _n#6131 -> ((poly_stub_186)@(L(unit)))[@inline] in
let last_originations#595 =
  fun _u#6135 -> ((poly_stub_185)@(L(unit)))[@inline] in
let new_account#597 = fun _u#6139 -> ((poly_stub_184)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#599 =
  fun _n#6143 -> ((poly_stub_183)@(L(unit)))[@inline] in
let register_delegate#601 =
  fun _kh#6147 -> ((poly_stub_182)@(L(unit)))[@inline] in
let register_constant#602 =
  fun _m#6149 -> ((poly_stub_181)@(L(unit)))[@inline] in
let constant_to_michelson_program#604 =
  fun _s#6153 -> ((poly_stub_180)@(L(unit)))[@inline] in
let restore_context#605 =
  fun _u#6155 -> ((poly_stub_179)@(L(unit)))[@inline] in
let save_context#606 = fun _u#6157 -> ((poly_stub_178)@(L(unit)))[@inline] in
let drop_context#607 = fun _u#6159 -> ((poly_stub_177)@(L(unit)))[@inline] in
let set_baker_policy#610 =
  fun _bp#6165 -> ((poly_stub_176)@(L(unit)))[@inline] in
let set_baker#611 = fun _a#6167 -> ((poly_stub_175)@(L(unit)))[@inline] in
let size#612 = fun _c#6169 -> ((poly_stub_174)@(L(unit)))[@inline] in
let read_contract_from_file#614 =
  fun _fn#6173 -> ((poly_stub_173)@(L(unit)))[@inline] in
let chr#615 = fun _n#6175 -> ((poly_stub_172)@(L(unit)))[@inline] in
let nl#616 = L("NEWLINE")[@inline] in
let println#617 = fun _v#6178 -> ((poly_stub_171)@(L(unit)))[@inline] in
let transfer#618 =
  fun _a#6180 ->
  (fun _s#6181 -> (fun _t#6182 -> ((poly_stub_170)@(L(unit)))))[@inline] in
let transfer_exn#619 =
  fun _a#6184 ->
  (fun _s#6185 -> (fun _t#6186 -> ((poly_stub_169)@(L(unit)))))[@inline] in
let reset_state#621 =
  fun _n#6190 -> (fun _l#6191 -> ((poly_stub_168)@(L(unit))))[@inline] in
let reset_state_at#622 =
  fun _t#6193 ->
  (fun _n#6194 -> (fun _l#6195 -> ((poly_stub_167)@(L(unit)))))[@inline] in
let save_mutation#625 =
  fun _s#6204 -> (fun _m#6205 -> ((poly_stub_166)@(L(unit))))[@inline] in
let sign#628 =
  fun _sk#6213 -> (fun _d#6214 -> ((poly_stub_165)@(L(unit))))[@inline] in
let add_account#629 =
  fun _s#6216 -> (fun _k#6217 -> ((poly_stub_164)@(L(unit))))[@inline] in
let baker_account#630 =
  fun _p#6219 -> (fun _o#6220 -> ((poly_stub_163)@(L(unit))))[@inline] in
let create_chest#632 =
  fun _b#6225 -> (fun _n#6226 -> ((poly_stub_162)@(L(unit))))[@inline] in
let create_chest_key#633 =
  fun _c#6228 -> (fun _n#6229 -> ((poly_stub_161)@(L(unit))))[@inline] in
let michelson_equal#636 =
  fun _m1#6239 -> (fun _m2#6240 -> ((poly_stub_160)@(L(unit))))[@inline] in
let originate_contract#638 =
  fun _c#6245 ->
  (fun _s#6246 -> (fun _t#6247 -> ((poly_stub_159)@(L(unit)))))[@inline] in
let compile_contract_from_file#640 =
  fun _fn#6253 ->
  (fun _e#6254 -> (fun _v#6255 -> ((poly_stub_158)@(L(unit)))))[@inline] in
let originate_from_file#641 =
  fun _fn#6257 ->
  (fun _e#6258 ->
   (fun _v#6259 ->
    (fun _s#6260 -> (fun _t#6261 -> ((poly_stub_157)@(L(unit)))))))[@inline] in
let toto#642 = L(44) in
let get_balance#643 =
  fun _u#6264 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#644 =
  fun _u#6266 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#645 = fun _u#6268 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#646 =
  fun _u#6270 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#647 =
  fun _u#6272 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#648 = fun _u#6274 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#649 = fun _u#6276 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#650 =
  fun _u#6278 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#651 =
  fun _u#6280 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#652 =
  fun _u#6282 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#653 =
  fun kh#6284 -> (({ VOTING_POWER })@(kh#6284))[@inline] in
let implicit_account#655 =
  fun kh#6288 -> (IMPLICIT_ACCOUNT(kh#6288))[@inline] in
let pairing_check#659 =
  fun l#6296 -> (({ PAIRING_CHECK })@(l#6296))[@inline] in
let set_delegate#661 = fun o#6300 -> (SET_DELEGATE(o#6300))[@inline] in
let open_chest#669 =
  fun ck#6321 ->
  (fun c#6322 -> (fun n#6323 -> (OPEN_CHEST(ck#6321 , c#6322 , n#6323))))[@inline] in
let xor#678 =
  fun l#6357 -> (fun r#6358 -> (XOR(l#6357 , r#6358)))[@inline] in
let or#679 = fun l#6360 -> (fun r#6361 -> (OR(l#6360 , r#6361)))[@inline] in
let shift_left#680 =
  fun l#6363 -> (fun r#6364 -> (LSL(l#6363 , r#6364)))[@inline] in
let shift_right#681 =
  fun l#6366 -> (fun r#6367 -> (LSR(l#6366 , r#6367)))[@inline] in
let length#726 = fun b#6512 -> (({ SIZE })@(b#6512))[@inline] in
let concat#727 =
  fun b1#6514 ->
  (fun b2#6515 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6514 , b2#6515))))[@inline] in
let sub#728 =
  fun s#6517 ->
  (fun l#6518 ->
   (fun b#6519 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6517 ,
                                                                   l#6518) ,
                                                              b#6519)))))[@inline] in
let length#734 = fun b#6534 -> (({ SIZE })@(b#6534))[@inline] in
let concat#735 =
  fun b1#6536 ->
  (fun b2#6537 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6536 , b2#6537))))[@inline] in
let sub#736 =
  fun s#6539 ->
  (fun l#6540 ->
   (fun b#6541 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6539 ,
                                                                   l#6540) ,
                                                              b#6541)))))[@inline] in
let blake2b#737 = fun b#6543 -> (({ BLAKE2B })@(b#6543))[@inline] in
let sha256#738 = fun b#6545 -> (({ SHA256 })@(b#6545))[@inline] in
let sha512#739 = fun b#6547 -> (({ SHA512 })@(b#6547))[@inline] in
let sha3#740 = fun b#6549 -> (({ SHA3 })@(b#6549))[@inline] in
let keccak#741 = fun b#6551 -> (({ KECCAK })@(b#6551))[@inline] in
let hash_key#742 = fun k#6553 -> (({ HASH_KEY })@(k#6553))[@inline] in
let check#743 =
  fun k#6555 ->
  (fun s#6556 ->
   (fun b#6557 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6555 , s#6556) ,
                                                   b#6557)))))[@inline] in
let assert#744 =
  fun b#6559 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6559))[@inline] in
let abs#747 = fun i#6565 -> (({ ABS })@(i#6565))[@inline] in
let is_nat#748 = fun i#6567 -> (({ ISNAT })@(i#6567))[@inline] in
let true#749 = TRUE()[@inline] in
let false#750 = FALSE()[@inline] in
let unit#751 = UNIT()[@inline] in
let assert_with_error#755 =
  fun b#6577 ->
  (fun s#6578 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6577 , s#6578))))[@inline] in
let poly_stub_156 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_155 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_154 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_153 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_152 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_151 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_150 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_149 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_148 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_147 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_146 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_145 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_144 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_143 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_142 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_141 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_140 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_139 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_138 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_137 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_136 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_135 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_134 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_133 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_132 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_131 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_130 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_129 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_128 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_127 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_126 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_125 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_124 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_123 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_122 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_121 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_120 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_119 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let poly_stub_118 = fun x#6589 -> (({ FAILWITH })@(x#6589))[@inline] in
let get_total_voting_power#763 =
  fun _u#6598 -> ((poly_stub_156)@(L(unit)))[@inline] in
let set_source#766 = fun _a#6604 -> ((poly_stub_155)@(L(unit)))[@inline] in
let get_storage_of_address#767 =
  fun _a#6606 -> ((poly_stub_154)@(L(unit)))[@inline] in
let get_balance#768 = fun _a#6608 -> ((poly_stub_153)@(L(unit)))[@inline] in
let print#769 = fun _v#6610 -> ((poly_stub_152)@(L(unit)))[@inline] in
let eprint#770 = fun _v#6612 -> ((poly_stub_151)@(L(unit)))[@inline] in
let get_voting_power#771 =
  fun _kh#6614 -> ((poly_stub_150)@(L(unit)))[@inline] in
let nth_bootstrap_contract#772 =
  fun _i#6616 -> ((poly_stub_149)@(L(unit)))[@inline] in
let nth_bootstrap_account#773 =
  fun _i#6618 -> ((poly_stub_148)@(L(unit)))[@inline] in
let get_bootstrap_account#774 =
  fun _n#6620 -> ((poly_stub_147)@(L(unit)))[@inline] in
let last_originations#776 =
  fun _u#6624 -> ((poly_stub_146)@(L(unit)))[@inline] in
let new_account#778 = fun _u#6628 -> ((poly_stub_145)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#780 =
  fun _n#6632 -> ((poly_stub_144)@(L(unit)))[@inline] in
let register_delegate#782 =
  fun _kh#6636 -> ((poly_stub_143)@(L(unit)))[@inline] in
let register_constant#783 =
  fun _m#6638 -> ((poly_stub_142)@(L(unit)))[@inline] in
let constant_to_michelson_program#785 =
  fun _s#6642 -> ((poly_stub_141)@(L(unit)))[@inline] in
let restore_context#786 =
  fun _u#6644 -> ((poly_stub_140)@(L(unit)))[@inline] in
let save_context#787 = fun _u#6646 -> ((poly_stub_139)@(L(unit)))[@inline] in
let drop_context#788 = fun _u#6648 -> ((poly_stub_138)@(L(unit)))[@inline] in
let set_baker_policy#791 =
  fun _bp#6654 -> ((poly_stub_137)@(L(unit)))[@inline] in
let set_baker#792 = fun _a#6656 -> ((poly_stub_136)@(L(unit)))[@inline] in
let size#793 = fun _c#6658 -> ((poly_stub_135)@(L(unit)))[@inline] in
let read_contract_from_file#795 =
  fun _fn#6662 -> ((poly_stub_134)@(L(unit)))[@inline] in
let chr#796 = fun _n#6664 -> ((poly_stub_133)@(L(unit)))[@inline] in
let nl#797 = L("NEWLINE")[@inline] in
let println#798 = fun _v#6667 -> ((poly_stub_132)@(L(unit)))[@inline] in
let transfer#799 =
  fun _a#6669 ->
  (fun _s#6670 -> (fun _t#6671 -> ((poly_stub_131)@(L(unit)))))[@inline] in
let transfer_exn#800 =
  fun _a#6673 ->
  (fun _s#6674 -> (fun _t#6675 -> ((poly_stub_130)@(L(unit)))))[@inline] in
let reset_state#802 =
  fun _n#6679 -> (fun _l#6680 -> ((poly_stub_129)@(L(unit))))[@inline] in
let reset_state_at#803 =
  fun _t#6682 ->
  (fun _n#6683 -> (fun _l#6684 -> ((poly_stub_128)@(L(unit)))))[@inline] in
let save_mutation#806 =
  fun _s#6693 -> (fun _m#6694 -> ((poly_stub_127)@(L(unit))))[@inline] in
let sign#809 =
  fun _sk#6702 -> (fun _d#6703 -> ((poly_stub_126)@(L(unit))))[@inline] in
let add_account#810 =
  fun _s#6705 -> (fun _k#6706 -> ((poly_stub_125)@(L(unit))))[@inline] in
let baker_account#811 =
  fun _p#6708 -> (fun _o#6709 -> ((poly_stub_124)@(L(unit))))[@inline] in
let create_chest#813 =
  fun _b#6714 -> (fun _n#6715 -> ((poly_stub_123)@(L(unit))))[@inline] in
let create_chest_key#814 =
  fun _c#6717 -> (fun _n#6718 -> ((poly_stub_122)@(L(unit))))[@inline] in
let michelson_equal#817 =
  fun _m1#6728 -> (fun _m2#6729 -> ((poly_stub_121)@(L(unit))))[@inline] in
let originate_contract#819 =
  fun _c#6734 ->
  (fun _s#6735 -> (fun _t#6736 -> ((poly_stub_120)@(L(unit)))))[@inline] in
let compile_contract_from_file#821 =
  fun _fn#6742 ->
  (fun _e#6743 -> (fun _v#6744 -> ((poly_stub_119)@(L(unit)))))[@inline] in
let originate_from_file#822 =
  fun _fn#6746 ->
  (fun _e#6747 ->
   (fun _v#6748 ->
    (fun _s#6749 -> (fun _t#6750 -> ((poly_stub_118)@(L(unit)))))))[@inline] in
let toto#823 = L(43) in
let get_balance#824 =
  fun _u#6753 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#825 =
  fun _u#6755 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#826 = fun _u#6757 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#827 =
  fun _u#6759 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#828 =
  fun _u#6761 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#829 = fun _u#6763 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#830 = fun _u#6765 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#831 =
  fun _u#6767 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#832 =
  fun _u#6769 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#833 =
  fun _u#6771 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#834 =
  fun kh#6773 -> (({ VOTING_POWER })@(kh#6773))[@inline] in
let implicit_account#836 =
  fun kh#6777 -> (IMPLICIT_ACCOUNT(kh#6777))[@inline] in
let pairing_check#840 =
  fun l#6785 -> (({ PAIRING_CHECK })@(l#6785))[@inline] in
let set_delegate#842 = fun o#6789 -> (SET_DELEGATE(o#6789))[@inline] in
let open_chest#850 =
  fun ck#6810 ->
  (fun c#6811 -> (fun n#6812 -> (OPEN_CHEST(ck#6810 , c#6811 , n#6812))))[@inline] in
let xor#859 =
  fun l#6846 -> (fun r#6847 -> (XOR(l#6846 , r#6847)))[@inline] in
let or#860 = fun l#6849 -> (fun r#6850 -> (OR(l#6849 , r#6850)))[@inline] in
let shift_left#861 =
  fun l#6852 -> (fun r#6853 -> (LSL(l#6852 , r#6853)))[@inline] in
let shift_right#862 =
  fun l#6855 -> (fun r#6856 -> (LSR(l#6855 , r#6856)))[@inline] in
let length#907 = fun b#7001 -> (({ SIZE })@(b#7001))[@inline] in
let concat#908 =
  fun b1#7003 ->
  (fun b2#7004 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7003 , b2#7004))))[@inline] in
let sub#909 =
  fun s#7006 ->
  (fun l#7007 ->
   (fun b#7008 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7006 ,
                                                                   l#7007) ,
                                                              b#7008)))))[@inline] in
let length#915 = fun b#7023 -> (({ SIZE })@(b#7023))[@inline] in
let concat#916 =
  fun b1#7025 ->
  (fun b2#7026 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7025 , b2#7026))))[@inline] in
let sub#917 =
  fun s#7028 ->
  (fun l#7029 ->
   (fun b#7030 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7028 ,
                                                                   l#7029) ,
                                                              b#7030)))))[@inline] in
let blake2b#918 = fun b#7032 -> (({ BLAKE2B })@(b#7032))[@inline] in
let sha256#919 = fun b#7034 -> (({ SHA256 })@(b#7034))[@inline] in
let sha512#920 = fun b#7036 -> (({ SHA512 })@(b#7036))[@inline] in
let sha3#921 = fun b#7038 -> (({ SHA3 })@(b#7038))[@inline] in
let keccak#922 = fun b#7040 -> (({ KECCAK })@(b#7040))[@inline] in
let hash_key#923 = fun k#7042 -> (({ HASH_KEY })@(k#7042))[@inline] in
let check#924 =
  fun k#7044 ->
  (fun s#7045 ->
   (fun b#7046 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7044 , s#7045) ,
                                                   b#7046)))))[@inline] in
let assert#925 =
  fun b#7048 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7048))[@inline] in
let abs#928 = fun i#7054 -> (({ ABS })@(i#7054))[@inline] in
let is_nat#929 = fun i#7056 -> (({ ISNAT })@(i#7056))[@inline] in
let true#930 = TRUE()[@inline] in
let false#931 = FALSE()[@inline] in
let unit#932 = UNIT()[@inline] in
let assert_with_error#936 =
  fun b#7066 ->
  (fun s#7067 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7066 , s#7067))))[@inline] in
let poly_stub_117 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_116 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_115 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_114 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_113 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_112 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_111 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_110 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_109 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_108 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_107 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_106 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_105 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_104 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_103 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_102 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_101 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_100 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_99 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_98 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_97 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_96 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_95 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_94 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_93 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_92 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_91 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_90 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_89 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_88 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_87 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_86 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_85 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_84 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_83 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_82 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_81 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_80 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let poly_stub_79 = fun x#7078 -> (({ FAILWITH })@(x#7078))[@inline] in
let get_total_voting_power#944 =
  fun _u#7087 -> ((poly_stub_117)@(L(unit)))[@inline] in
let set_source#947 = fun _a#7093 -> ((poly_stub_116)@(L(unit)))[@inline] in
let get_storage_of_address#948 =
  fun _a#7095 -> ((poly_stub_115)@(L(unit)))[@inline] in
let get_balance#949 = fun _a#7097 -> ((poly_stub_114)@(L(unit)))[@inline] in
let print#950 = fun _v#7099 -> ((poly_stub_113)@(L(unit)))[@inline] in
let eprint#951 = fun _v#7101 -> ((poly_stub_112)@(L(unit)))[@inline] in
let get_voting_power#952 =
  fun _kh#7103 -> ((poly_stub_111)@(L(unit)))[@inline] in
let nth_bootstrap_contract#953 =
  fun _i#7105 -> ((poly_stub_110)@(L(unit)))[@inline] in
let nth_bootstrap_account#954 =
  fun _i#7107 -> ((poly_stub_109)@(L(unit)))[@inline] in
let get_bootstrap_account#955 =
  fun _n#7109 -> ((poly_stub_108)@(L(unit)))[@inline] in
let last_originations#957 =
  fun _u#7113 -> ((poly_stub_107)@(L(unit)))[@inline] in
let new_account#959 = fun _u#7117 -> ((poly_stub_106)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#961 =
  fun _n#7121 -> ((poly_stub_105)@(L(unit)))[@inline] in
let register_delegate#963 =
  fun _kh#7125 -> ((poly_stub_104)@(L(unit)))[@inline] in
let register_constant#964 =
  fun _m#7127 -> ((poly_stub_103)@(L(unit)))[@inline] in
let constant_to_michelson_program#966 =
  fun _s#7131 -> ((poly_stub_102)@(L(unit)))[@inline] in
let restore_context#967 =
  fun _u#7133 -> ((poly_stub_101)@(L(unit)))[@inline] in
let save_context#968 = fun _u#7135 -> ((poly_stub_100)@(L(unit)))[@inline] in
let drop_context#969 = fun _u#7137 -> ((poly_stub_99)@(L(unit)))[@inline] in
let set_baker_policy#972 =
  fun _bp#7143 -> ((poly_stub_98)@(L(unit)))[@inline] in
let set_baker#973 = fun _a#7145 -> ((poly_stub_97)@(L(unit)))[@inline] in
let size#974 = fun _c#7147 -> ((poly_stub_96)@(L(unit)))[@inline] in
let read_contract_from_file#976 =
  fun _fn#7151 -> ((poly_stub_95)@(L(unit)))[@inline] in
let chr#977 = fun _n#7153 -> ((poly_stub_94)@(L(unit)))[@inline] in
let nl#978 = L("NEWLINE")[@inline] in
let println#979 = fun _v#7156 -> ((poly_stub_93)@(L(unit)))[@inline] in
let transfer#980 =
  fun _a#7158 -> (fun _s#7159 -> (fun _t#7160 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#981 =
  fun _a#7162 -> (fun _s#7163 -> (fun _t#7164 -> ((poly_stub_91)@(L(unit)))))[@inline] in
let reset_state#983 =
  fun _n#7168 -> (fun _l#7169 -> ((poly_stub_90)@(L(unit))))[@inline] in
let reset_state_at#984 =
  fun _t#7171 -> (fun _n#7172 -> (fun _l#7173 -> ((poly_stub_89)@(L(unit)))))[@inline] in
let save_mutation#987 =
  fun _s#7182 -> (fun _m#7183 -> ((poly_stub_88)@(L(unit))))[@inline] in
let sign#990 =
  fun _sk#7191 -> (fun _d#7192 -> ((poly_stub_87)@(L(unit))))[@inline] in
let add_account#991 =
  fun _s#7194 -> (fun _k#7195 -> ((poly_stub_86)@(L(unit))))[@inline] in
let baker_account#992 =
  fun _p#7197 -> (fun _o#7198 -> ((poly_stub_85)@(L(unit))))[@inline] in
let create_chest#994 =
  fun _b#7203 -> (fun _n#7204 -> ((poly_stub_84)@(L(unit))))[@inline] in
let create_chest_key#995 =
  fun _c#7206 -> (fun _n#7207 -> ((poly_stub_83)@(L(unit))))[@inline] in
let michelson_equal#998 =
  fun _m1#7217 -> (fun _m2#7218 -> ((poly_stub_82)@(L(unit))))[@inline] in
let originate_contract#1000 =
  fun _c#7223 -> (fun _s#7224 -> (fun _t#7225 -> ((poly_stub_81)@(L(unit)))))[@inline] in
let compile_contract_from_file#1002 =
  fun _fn#7231 ->
  (fun _e#7232 -> (fun _v#7233 -> ((poly_stub_80)@(L(unit)))))[@inline] in
let originate_from_file#1003 =
  fun _fn#7235 ->
  (fun _e#7236 ->
   (fun _v#7237 ->
    (fun _s#7238 -> (fun _t#7239 -> ((poly_stub_79)@(L(unit)))))))[@inline] in
let tata#1004 = ADD(toto#278 , titi#460) in
let foo#1005 = (f#461)@(PAIR(L(unit) , L(3))) in
let get_balance#1006 =
  fun _u#7243 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1007 =
  fun _u#7245 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1008 = fun _u#7247 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1009 =
  fun _u#7249 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1010 =
  fun _u#7251 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1011 =
  fun _u#7253 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1012 = fun _u#7255 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1013 =
  fun _u#7257 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1014 =
  fun _u#7259 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1015 =
  fun _u#7261 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1016 =
  fun kh#7263 -> (({ VOTING_POWER })@(kh#7263))[@inline] in
let implicit_account#1018 =
  fun kh#7267 -> (IMPLICIT_ACCOUNT(kh#7267))[@inline] in
let pairing_check#1022 =
  fun l#7275 -> (({ PAIRING_CHECK })@(l#7275))[@inline] in
let set_delegate#1024 = fun o#7279 -> (SET_DELEGATE(o#7279))[@inline] in
let open_chest#1032 =
  fun ck#7300 ->
  (fun c#7301 -> (fun n#7302 -> (OPEN_CHEST(ck#7300 , c#7301 , n#7302))))[@inline] in
let xor#1041 =
  fun l#7336 -> (fun r#7337 -> (XOR(l#7336 , r#7337)))[@inline] in
let or#1042 = fun l#7339 -> (fun r#7340 -> (OR(l#7339 , r#7340)))[@inline] in
let shift_left#1043 =
  fun l#7342 -> (fun r#7343 -> (LSL(l#7342 , r#7343)))[@inline] in
let shift_right#1044 =
  fun l#7345 -> (fun r#7346 -> (LSR(l#7345 , r#7346)))[@inline] in
let length#1089 = fun b#7491 -> (({ SIZE })@(b#7491))[@inline] in
let concat#1090 =
  fun b1#7493 ->
  (fun b2#7494 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7493 , b2#7494))))[@inline] in
let sub#1091 =
  fun s#7496 ->
  (fun l#7497 ->
   (fun b#7498 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7496 ,
                                                                   l#7497) ,
                                                              b#7498)))))[@inline] in
let length#1097 = fun b#7513 -> (({ SIZE })@(b#7513))[@inline] in
let concat#1098 =
  fun b1#7515 ->
  (fun b2#7516 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7515 , b2#7516))))[@inline] in
let sub#1099 =
  fun s#7518 ->
  (fun l#7519 ->
   (fun b#7520 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7518 ,
                                                                   l#7519) ,
                                                              b#7520)))))[@inline] in
let blake2b#1100 = fun b#7522 -> (({ BLAKE2B })@(b#7522))[@inline] in
let sha256#1101 = fun b#7524 -> (({ SHA256 })@(b#7524))[@inline] in
let sha512#1102 = fun b#7526 -> (({ SHA512 })@(b#7526))[@inline] in
let sha3#1103 = fun b#7528 -> (({ SHA3 })@(b#7528))[@inline] in
let keccak#1104 = fun b#7530 -> (({ KECCAK })@(b#7530))[@inline] in
let hash_key#1105 = fun k#7532 -> (({ HASH_KEY })@(k#7532))[@inline] in
let check#1106 =
  fun k#7534 ->
  (fun s#7535 ->
   (fun b#7536 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7534 , s#7535) ,
                                                   b#7536)))))[@inline] in
let assert#1107 =
  fun b#7538 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7538))[@inline] in
let abs#1110 = fun i#7544 -> (({ ABS })@(i#7544))[@inline] in
let is_nat#1111 = fun i#7546 -> (({ ISNAT })@(i#7546))[@inline] in
let true#1112 = TRUE()[@inline] in
let false#1113 = FALSE()[@inline] in
let unit#1114 = UNIT()[@inline] in
let assert_with_error#1118 =
  fun b#7556 ->
  (fun s#7557 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7556 , s#7557))))[@inline] in
let poly_stub_78 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_77 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_76 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_75 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_74 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_73 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_72 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_71 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_70 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_69 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_68 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_67 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_66 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_65 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_64 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_63 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_62 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_61 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_60 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_59 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_58 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_57 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_56 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_55 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_54 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_53 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_52 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_51 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_50 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_49 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_48 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_47 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_46 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_45 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_44 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_43 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_42 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_41 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let poly_stub_40 = fun x#7568 -> (({ FAILWITH })@(x#7568))[@inline] in
let get_total_voting_power#1126 =
  fun _u#7577 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#1129 = fun _a#7583 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#1130 =
  fun _a#7585 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#1131 = fun _a#7587 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#1132 = fun _v#7589 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#1133 = fun _v#7591 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#1134 =
  fun _kh#7593 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1135 =
  fun _i#7595 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#1136 =
  fun _i#7597 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#1137 =
  fun _n#7599 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#1139 =
  fun _u#7603 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#1141 = fun _u#7607 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1143 =
  fun _n#7611 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#1145 =
  fun _kh#7615 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#1146 =
  fun _m#7617 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#1148 =
  fun _s#7621 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#1149 =
  fun _u#7623 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#1150 = fun _u#7625 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#1151 = fun _u#7627 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#1154 =
  fun _bp#7633 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#1155 = fun _a#7635 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#1156 = fun _c#7637 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#1158 =
  fun _fn#7641 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#1159 = fun _n#7643 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#1160 = L("NEWLINE")[@inline] in
let println#1161 = fun _v#7646 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#1162 =
  fun _a#7648 -> (fun _s#7649 -> (fun _t#7650 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#1163 =
  fun _a#7652 -> (fun _s#7653 -> (fun _t#7654 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#1165 =
  fun _n#7658 -> (fun _l#7659 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#1166 =
  fun _t#7661 -> (fun _n#7662 -> (fun _l#7663 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#1169 =
  fun _s#7672 -> (fun _m#7673 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#1172 =
  fun _sk#7681 -> (fun _d#7682 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#1173 =
  fun _s#7684 -> (fun _k#7685 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#1174 =
  fun _p#7687 -> (fun _o#7688 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#1176 =
  fun _b#7693 -> (fun _n#7694 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#1177 =
  fun _c#7696 -> (fun _n#7697 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#1180 =
  fun _m1#7707 -> (fun _m2#7708 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#1182 =
  fun _c#7713 -> (fun _s#7714 -> (fun _t#7715 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#1184 =
  fun _fn#7721 ->
  (fun _e#7722 -> (fun _v#7723 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#1185 =
  fun _fn#7725 ->
  (fun _e#7726 ->
   (fun _v#7727 ->
    (fun _s#7728 -> (fun _t#7729 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let toto#1186 = L(10) in
let foo#1187 = L("bar") in
let get_balance#1188 =
  fun _u#7733 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1189 =
  fun _u#7735 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1190 = fun _u#7737 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1191 =
  fun _u#7739 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1192 =
  fun _u#7741 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1193 =
  fun _u#7743 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1194 = fun _u#7745 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1195 =
  fun _u#7747 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1196 =
  fun _u#7749 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1197 =
  fun _u#7751 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1198 =
  fun kh#7753 -> (({ VOTING_POWER })@(kh#7753))[@inline] in
let implicit_account#1200 =
  fun kh#7757 -> (IMPLICIT_ACCOUNT(kh#7757))[@inline] in
let pairing_check#1204 =
  fun l#7765 -> (({ PAIRING_CHECK })@(l#7765))[@inline] in
let set_delegate#1206 = fun o#7769 -> (SET_DELEGATE(o#7769))[@inline] in
let open_chest#1214 =
  fun ck#7790 ->
  (fun c#7791 -> (fun n#7792 -> (OPEN_CHEST(ck#7790 , c#7791 , n#7792))))[@inline] in
let xor#1223 =
  fun l#7826 -> (fun r#7827 -> (XOR(l#7826 , r#7827)))[@inline] in
let or#1224 = fun l#7829 -> (fun r#7830 -> (OR(l#7829 , r#7830)))[@inline] in
let shift_left#1225 =
  fun l#7832 -> (fun r#7833 -> (LSL(l#7832 , r#7833)))[@inline] in
let shift_right#1226 =
  fun l#7835 -> (fun r#7836 -> (LSR(l#7835 , r#7836)))[@inline] in
let length#1271 = fun b#7981 -> (({ SIZE })@(b#7981))[@inline] in
let concat#1272 =
  fun b1#7983 ->
  (fun b2#7984 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7983 , b2#7984))))[@inline] in
let sub#1273 =
  fun s#7986 ->
  (fun l#7987 ->
   (fun b#7988 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7986 ,
                                                                   l#7987) ,
                                                              b#7988)))))[@inline] in
let length#1279 = fun b#8003 -> (({ SIZE })@(b#8003))[@inline] in
let concat#1280 =
  fun b1#8005 ->
  (fun b2#8006 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#8005 , b2#8006))))[@inline] in
let sub#1281 =
  fun s#8008 ->
  (fun l#8009 ->
   (fun b#8010 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#8008 ,
                                                                   l#8009) ,
                                                              b#8010)))))[@inline] in
let blake2b#1282 = fun b#8012 -> (({ BLAKE2B })@(b#8012))[@inline] in
let sha256#1283 = fun b#8014 -> (({ SHA256 })@(b#8014))[@inline] in
let sha512#1284 = fun b#8016 -> (({ SHA512 })@(b#8016))[@inline] in
let sha3#1285 = fun b#8018 -> (({ SHA3 })@(b#8018))[@inline] in
let keccak#1286 = fun b#8020 -> (({ KECCAK })@(b#8020))[@inline] in
let hash_key#1287 = fun k#8022 -> (({ HASH_KEY })@(k#8022))[@inline] in
let check#1288 =
  fun k#8024 ->
  (fun s#8025 ->
   (fun b#8026 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#8024 , s#8025) ,
                                                   b#8026)))))[@inline] in
let assert =
  fun b#8028 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#8028))[@inline] in
let abs = fun i#8034 -> (({ ABS })@(i#8034))[@inline] in
let is_nat = fun i#8036 -> (({ ISNAT })@(i#8036))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#8046 ->
  (fun s#8047 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#8046 , s#8047))))[@inline] in
let poly_stub_39 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_38 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_37 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_36 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_35 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_34 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_33 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_32 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_31 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_30 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_29 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_28 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_27 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_26 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_25 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_24 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_23 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_22 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_21 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_20 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_19 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_18 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_17 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_16 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_15 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_14 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_13 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_12 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_11 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_10 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_9 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_8 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_7 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_6 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_5 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_4 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_3 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_2 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let poly_stub_1 = fun x#8058 -> (({ FAILWITH })@(x#8058))[@inline] in
let get_total_voting_power#1293 =
  fun _u#8067 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#1296 = fun _a#8073 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#1297 =
  fun _a#8075 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#1298 = fun _a#8077 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#1299 = fun _v#8079 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#1300 = fun _v#8081 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#1301 =
  fun _kh#8083 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1302 =
  fun _i#8085 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#1303 =
  fun _i#8087 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#1304 =
  fun _n#8089 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#1306 =
  fun _u#8093 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#1308 = fun _u#8097 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1310 =
  fun _n#8101 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#1312 =
  fun _kh#8105 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#1313 =
  fun _m#8107 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#1315 =
  fun _s#8111 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#1316 =
  fun _u#8113 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#1317 = fun _u#8115 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#1318 = fun _u#8117 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#1321 =
  fun _bp#8123 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#1322 = fun _a#8125 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#1323 = fun _c#8127 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#1325 =
  fun _fn#8131 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1326 = fun _n#8133 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#1327 = L("NEWLINE")[@inline] in
let println#1328 = fun _v#8136 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#1329 =
  fun _a#8138 -> (fun _s#8139 -> (fun _t#8140 -> ((poly_stub_14)@(L(unit)))))[@inline] in
let transfer_exn#1330 =
  fun _a#8142 -> (fun _s#8143 -> (fun _t#8144 -> ((poly_stub_13)@(L(unit)))))[@inline] in
let reset_state#1332 =
  fun _n#8148 -> (fun _l#8149 -> ((poly_stub_12)@(L(unit))))[@inline] in
let reset_state_at#1333 =
  fun _t#8151 -> (fun _n#8152 -> (fun _l#8153 -> ((poly_stub_11)@(L(unit)))))[@inline] in
let save_mutation#1336 =
  fun _s#8162 -> (fun _m#8163 -> ((poly_stub_10)@(L(unit))))[@inline] in
let sign#1339 =
  fun _sk#8171 -> (fun _d#8172 -> ((poly_stub_9)@(L(unit))))[@inline] in
let add_account#1340 =
  fun _s#8174 -> (fun _k#8175 -> ((poly_stub_8)@(L(unit))))[@inline] in
let baker_account#1341 =
  fun _p#8177 -> (fun _o#8178 -> ((poly_stub_7)@(L(unit))))[@inline] in
let create_chest#1343 =
  fun _b#8183 -> (fun _n#8184 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#1344 =
  fun _c#8186 -> (fun _n#8187 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#1347 =
  fun _m1#8197 -> (fun _m2#8198 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#1349 =
  fun _c#8203 -> (fun _s#8204 -> (fun _t#8205 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#1351 =
  fun _fn#8211 -> (fun _e#8212 -> (fun _v#8213 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#1352 =
  fun _fn#8215 ->
  (fun _e#8216 ->
   (fun _v#8217 ->
    (fun _s#8218 -> (fun _t#8219 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1186 , toto#278) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#8223 ->
  (let (gen#8229, gen#8230) = gen#8223 in
   let p#8224 = gen#8229 in
   let s#8225 = gen#8230 in
   let s#8226 = ADD(ADD(p#8224 , s#8225) , toto) in
   PAIR(LIST_EMPTY() , s#8226)) in
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
