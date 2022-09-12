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
        | ( _#4 : unit , _#3 : string ) ->
        ( LIST_EMPTY() , CONCAT(Errors.undefined_token , Storage.s) ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let get_balance#77 =
  fun _u#4694 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#78 =
  fun _u#4696 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#79 = fun _u#4698 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#80 =
  fun _u#4700 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#81 =
  fun _u#4702 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#82 = fun _u#4704 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#83 = fun _u#4706 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#84 =
  fun _u#4708 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#85 =
  fun _u#4710 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#86 =
  fun _u#4712 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#87 =
  fun kh#4714 -> (({ VOTING_POWER })@(kh#4714))[@inline] in
let implicit_account#89 =
  fun kh#4718 -> (IMPLICIT_ACCOUNT(kh#4718))[@inline] in
let pairing_check#93 =
  fun l#4726 -> (({ PAIRING_CHECK })@(l#4726))[@inline] in
let set_delegate#95 = fun o#4730 -> (SET_DELEGATE(o#4730))[@inline] in
let open_chest#103 =
  fun ck#4751 ->
  (fun c#4752 -> (fun n#4753 -> (OPEN_CHEST(ck#4751 , c#4752 , n#4753))))[@inline] in
let xor#112 =
  fun l#4787 -> (fun r#4788 -> (XOR(l#4787 , r#4788)))[@inline] in
let or#113 = fun l#4790 -> (fun r#4791 -> (OR(l#4790 , r#4791)))[@inline] in
let shift_left#114 =
  fun l#4793 -> (fun r#4794 -> (LSL(l#4793 , r#4794)))[@inline] in
let shift_right#115 =
  fun l#4796 -> (fun r#4797 -> (LSR(l#4796 , r#4797)))[@inline] in
let length#159 = fun b#4935 -> (({ SIZE })@(b#4935))[@inline] in
let concat#160 =
  fun b1#4937 ->
  (fun b2#4938 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4937 , b2#4938))))[@inline] in
let sub#161 =
  fun s#4940 ->
  (fun l#4941 ->
   (fun b#4942 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4940 ,
                                                                   l#4941) ,
                                                              b#4942)))))[@inline] in
let length#167 = fun b#4957 -> (({ SIZE })@(b#4957))[@inline] in
let concat#168 =
  fun b1#4959 ->
  (fun b2#4960 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4959 , b2#4960))))[@inline] in
let sub#169 =
  fun s#4962 ->
  (fun l#4963 ->
   (fun b#4964 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4962 ,
                                                                   l#4963) ,
                                                              b#4964)))))[@inline] in
let blake2b#170 = fun b#4966 -> (({ BLAKE2B })@(b#4966))[@inline] in
let sha256#171 = fun b#4968 -> (({ SHA256 })@(b#4968))[@inline] in
let sha512#172 = fun b#4970 -> (({ SHA512 })@(b#4970))[@inline] in
let sha3#173 = fun b#4972 -> (({ SHA3 })@(b#4972))[@inline] in
let keccak#174 = fun b#4974 -> (({ KECCAK })@(b#4974))[@inline] in
let hash_key#175 = fun k#4976 -> (({ HASH_KEY })@(k#4976))[@inline] in
let check#176 =
  fun k#4978 ->
  (fun s#4979 ->
   (fun b#4980 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4978 , s#4979) ,
                                                   b#4980)))))[@inline] in
let assert#177 =
  fun b#4982 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4982))[@inline] in
let abs#180 = fun i#4988 -> (({ ABS })@(i#4988))[@inline] in
let is_nat#181 = fun i#4990 -> (({ ISNAT })@(i#4990))[@inline] in
let true#182 = TRUE()[@inline] in
let false#183 = FALSE()[@inline] in
let unit#184 = UNIT()[@inline] in
let assert_with_error#187 =
  fun b#4998 ->
  (fun s#4999 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4998 , s#4999))))[@inline] in
let poly_stub_273 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_272 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_271 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_270 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_269 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_268 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_267 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_266 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_265 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_264 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_263 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_262 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_261 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_260 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_259 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_258 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_257 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_256 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_255 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_254 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_253 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_252 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_251 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_250 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_249 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_248 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_247 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_246 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_245 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_244 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_243 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_242 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_241 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_240 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_239 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_238 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_237 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_236 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let poly_stub_235 = fun x#5010 -> (({ FAILWITH })@(x#5010))[@inline] in
let get_total_voting_power#195 =
  fun _u#5019 -> ((poly_stub_273)@(L(unit)))[@inline] in
let set_source#198 = fun _a#5025 -> ((poly_stub_272)@(L(unit)))[@inline] in
let get_storage_of_address#199 =
  fun _a#5027 -> ((poly_stub_271)@(L(unit)))[@inline] in
let get_balance#200 = fun _a#5029 -> ((poly_stub_270)@(L(unit)))[@inline] in
let print#201 = fun _v#5031 -> ((poly_stub_269)@(L(unit)))[@inline] in
let eprint#202 = fun _v#5033 -> ((poly_stub_268)@(L(unit)))[@inline] in
let get_voting_power#203 =
  fun _kh#5035 -> ((poly_stub_267)@(L(unit)))[@inline] in
let nth_bootstrap_contract#204 =
  fun _i#5037 -> ((poly_stub_266)@(L(unit)))[@inline] in
let nth_bootstrap_account#205 =
  fun _i#5039 -> ((poly_stub_265)@(L(unit)))[@inline] in
let get_bootstrap_account#206 =
  fun _n#5041 -> ((poly_stub_264)@(L(unit)))[@inline] in
let last_originations#208 =
  fun _u#5045 -> ((poly_stub_263)@(L(unit)))[@inline] in
let new_account#210 = fun _u#5049 -> ((poly_stub_262)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#212 =
  fun _n#5053 -> ((poly_stub_261)@(L(unit)))[@inline] in
let register_delegate#214 =
  fun _kh#5057 -> ((poly_stub_260)@(L(unit)))[@inline] in
let register_constant#215 =
  fun _m#5059 -> ((poly_stub_259)@(L(unit)))[@inline] in
let constant_to_michelson_program#217 =
  fun _s#5063 -> ((poly_stub_258)@(L(unit)))[@inline] in
let restore_context#218 =
  fun _u#5065 -> ((poly_stub_257)@(L(unit)))[@inline] in
let save_context#219 = fun _u#5067 -> ((poly_stub_256)@(L(unit)))[@inline] in
let drop_context#220 = fun _u#5069 -> ((poly_stub_255)@(L(unit)))[@inline] in
let set_baker_policy#223 =
  fun _bp#5075 -> ((poly_stub_254)@(L(unit)))[@inline] in
let set_baker#224 = fun _a#5077 -> ((poly_stub_253)@(L(unit)))[@inline] in
let size#225 = fun _c#5079 -> ((poly_stub_252)@(L(unit)))[@inline] in
let read_contract_from_file#227 =
  fun _fn#5083 -> ((poly_stub_251)@(L(unit)))[@inline] in
let chr#228 = fun _n#5085 -> ((poly_stub_250)@(L(unit)))[@inline] in
let nl#229 = L("NEWLINE")[@inline] in
let println#230 = fun _v#5088 -> ((poly_stub_249)@(L(unit)))[@inline] in
let transfer#231 =
  fun _a#5090 ->
  (fun _s#5091 -> (fun _t#5092 -> ((poly_stub_248)@(L(unit)))))[@inline] in
let transfer_exn#232 =
  fun _a#5094 ->
  (fun _s#5095 -> (fun _t#5096 -> ((poly_stub_247)@(L(unit)))))[@inline] in
let reset_state#234 =
  fun _n#5100 -> (fun _l#5101 -> ((poly_stub_246)@(L(unit))))[@inline] in
let reset_state_at#235 =
  fun _t#5103 ->
  (fun _n#5104 -> (fun _l#5105 -> ((poly_stub_245)@(L(unit)))))[@inline] in
let save_mutation#238 =
  fun _s#5114 -> (fun _m#5115 -> ((poly_stub_244)@(L(unit))))[@inline] in
let sign#241 =
  fun _sk#5123 -> (fun _d#5124 -> ((poly_stub_243)@(L(unit))))[@inline] in
let add_account#242 =
  fun _s#5126 -> (fun _k#5127 -> ((poly_stub_242)@(L(unit))))[@inline] in
let baker_account#243 =
  fun _p#5129 -> (fun _o#5130 -> ((poly_stub_241)@(L(unit))))[@inline] in
let create_chest#245 =
  fun _b#5135 -> (fun _n#5136 -> ((poly_stub_240)@(L(unit))))[@inline] in
let create_chest_key#246 =
  fun _c#5138 -> (fun _n#5139 -> ((poly_stub_239)@(L(unit))))[@inline] in
let michelson_equal#249 =
  fun _m1#5149 -> (fun _m2#5150 -> ((poly_stub_238)@(L(unit))))[@inline] in
let originate_contract#251 =
  fun _c#5155 ->
  (fun _s#5156 -> (fun _t#5157 -> ((poly_stub_237)@(L(unit)))))[@inline] in
let compile_contract_from_file#253 =
  fun _fn#5163 ->
  (fun _e#5164 -> (fun _v#5165 -> ((poly_stub_236)@(L(unit)))))[@inline] in
let originate_from_file#254 =
  fun _fn#5167 ->
  (fun _e#5168 ->
   (fun _v#5169 ->
    (fun _s#5170 -> (fun _t#5171 -> ((poly_stub_235)@(L(unit)))))))[@inline] in
let toto#255 = L(1) in
let get_balance#256 =
  fun _u#5174 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#257 =
  fun _u#5176 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#258 = fun _u#5178 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#259 =
  fun _u#5180 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#260 =
  fun _u#5182 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#261 = fun _u#5184 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#262 = fun _u#5186 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#263 =
  fun _u#5188 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#264 =
  fun _u#5190 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#265 =
  fun _u#5192 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#266 =
  fun kh#5194 -> (({ VOTING_POWER })@(kh#5194))[@inline] in
let implicit_account#268 =
  fun kh#5198 -> (IMPLICIT_ACCOUNT(kh#5198))[@inline] in
let pairing_check#272 =
  fun l#5206 -> (({ PAIRING_CHECK })@(l#5206))[@inline] in
let set_delegate#274 = fun o#5210 -> (SET_DELEGATE(o#5210))[@inline] in
let open_chest#282 =
  fun ck#5231 ->
  (fun c#5232 -> (fun n#5233 -> (OPEN_CHEST(ck#5231 , c#5232 , n#5233))))[@inline] in
let xor#291 =
  fun l#5267 -> (fun r#5268 -> (XOR(l#5267 , r#5268)))[@inline] in
let or#292 = fun l#5270 -> (fun r#5271 -> (OR(l#5270 , r#5271)))[@inline] in
let shift_left#293 =
  fun l#5273 -> (fun r#5274 -> (LSL(l#5273 , r#5274)))[@inline] in
let shift_right#294 =
  fun l#5276 -> (fun r#5277 -> (LSR(l#5276 , r#5277)))[@inline] in
let length#338 = fun b#5415 -> (({ SIZE })@(b#5415))[@inline] in
let concat#339 =
  fun b1#5417 ->
  (fun b2#5418 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5417 , b2#5418))))[@inline] in
let sub#340 =
  fun s#5420 ->
  (fun l#5421 ->
   (fun b#5422 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5420 ,
                                                                   l#5421) ,
                                                              b#5422)))))[@inline] in
let length#346 = fun b#5437 -> (({ SIZE })@(b#5437))[@inline] in
let concat#347 =
  fun b1#5439 ->
  (fun b2#5440 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5439 , b2#5440))))[@inline] in
let sub#348 =
  fun s#5442 ->
  (fun l#5443 ->
   (fun b#5444 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5442 ,
                                                                   l#5443) ,
                                                              b#5444)))))[@inline] in
let blake2b#349 = fun b#5446 -> (({ BLAKE2B })@(b#5446))[@inline] in
let sha256#350 = fun b#5448 -> (({ SHA256 })@(b#5448))[@inline] in
let sha512#351 = fun b#5450 -> (({ SHA512 })@(b#5450))[@inline] in
let sha3#352 = fun b#5452 -> (({ SHA3 })@(b#5452))[@inline] in
let keccak#353 = fun b#5454 -> (({ KECCAK })@(b#5454))[@inline] in
let hash_key#354 = fun k#5456 -> (({ HASH_KEY })@(k#5456))[@inline] in
let check#355 =
  fun k#5458 ->
  (fun s#5459 ->
   (fun b#5460 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5458 , s#5459) ,
                                                   b#5460)))))[@inline] in
let assert#356 =
  fun b#5462 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5462))[@inline] in
let abs#359 = fun i#5468 -> (({ ABS })@(i#5468))[@inline] in
let is_nat#360 = fun i#5470 -> (({ ISNAT })@(i#5470))[@inline] in
let true#361 = TRUE()[@inline] in
let false#362 = FALSE()[@inline] in
let unit#363 = UNIT()[@inline] in
let assert_with_error#366 =
  fun b#5478 ->
  (fun s#5479 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5478 , s#5479))))[@inline] in
let poly_stub_234 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_233 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_232 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_231 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_230 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_229 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_228 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_227 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_226 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_225 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_224 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_223 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_222 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_221 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_220 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_219 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_218 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_217 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_216 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_215 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_214 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_213 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_212 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_211 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_210 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_209 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_208 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_207 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_206 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_205 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_204 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_203 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_202 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_201 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_200 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_199 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_198 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_197 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let poly_stub_196 = fun x#5490 -> (({ FAILWITH })@(x#5490))[@inline] in
let get_total_voting_power#374 =
  fun _u#5499 -> ((poly_stub_234)@(L(unit)))[@inline] in
let set_source#377 = fun _a#5505 -> ((poly_stub_233)@(L(unit)))[@inline] in
let get_storage_of_address#378 =
  fun _a#5507 -> ((poly_stub_232)@(L(unit)))[@inline] in
let get_balance#379 = fun _a#5509 -> ((poly_stub_231)@(L(unit)))[@inline] in
let print#380 = fun _v#5511 -> ((poly_stub_230)@(L(unit)))[@inline] in
let eprint#381 = fun _v#5513 -> ((poly_stub_229)@(L(unit)))[@inline] in
let get_voting_power#382 =
  fun _kh#5515 -> ((poly_stub_228)@(L(unit)))[@inline] in
let nth_bootstrap_contract#383 =
  fun _i#5517 -> ((poly_stub_227)@(L(unit)))[@inline] in
let nth_bootstrap_account#384 =
  fun _i#5519 -> ((poly_stub_226)@(L(unit)))[@inline] in
let get_bootstrap_account#385 =
  fun _n#5521 -> ((poly_stub_225)@(L(unit)))[@inline] in
let last_originations#387 =
  fun _u#5525 -> ((poly_stub_224)@(L(unit)))[@inline] in
let new_account#389 = fun _u#5529 -> ((poly_stub_223)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#391 =
  fun _n#5533 -> ((poly_stub_222)@(L(unit)))[@inline] in
let register_delegate#393 =
  fun _kh#5537 -> ((poly_stub_221)@(L(unit)))[@inline] in
let register_constant#394 =
  fun _m#5539 -> ((poly_stub_220)@(L(unit)))[@inline] in
let constant_to_michelson_program#396 =
  fun _s#5543 -> ((poly_stub_219)@(L(unit)))[@inline] in
let restore_context#397 =
  fun _u#5545 -> ((poly_stub_218)@(L(unit)))[@inline] in
let save_context#398 = fun _u#5547 -> ((poly_stub_217)@(L(unit)))[@inline] in
let drop_context#399 = fun _u#5549 -> ((poly_stub_216)@(L(unit)))[@inline] in
let set_baker_policy#402 =
  fun _bp#5555 -> ((poly_stub_215)@(L(unit)))[@inline] in
let set_baker#403 = fun _a#5557 -> ((poly_stub_214)@(L(unit)))[@inline] in
let size#404 = fun _c#5559 -> ((poly_stub_213)@(L(unit)))[@inline] in
let read_contract_from_file#406 =
  fun _fn#5563 -> ((poly_stub_212)@(L(unit)))[@inline] in
let chr#407 = fun _n#5565 -> ((poly_stub_211)@(L(unit)))[@inline] in
let nl#408 = L("NEWLINE")[@inline] in
let println#409 = fun _v#5568 -> ((poly_stub_210)@(L(unit)))[@inline] in
let transfer#410 =
  fun _a#5570 ->
  (fun _s#5571 -> (fun _t#5572 -> ((poly_stub_209)@(L(unit)))))[@inline] in
let transfer_exn#411 =
  fun _a#5574 ->
  (fun _s#5575 -> (fun _t#5576 -> ((poly_stub_208)@(L(unit)))))[@inline] in
let reset_state#413 =
  fun _n#5580 -> (fun _l#5581 -> ((poly_stub_207)@(L(unit))))[@inline] in
let reset_state_at#414 =
  fun _t#5583 ->
  (fun _n#5584 -> (fun _l#5585 -> ((poly_stub_206)@(L(unit)))))[@inline] in
let save_mutation#417 =
  fun _s#5594 -> (fun _m#5595 -> ((poly_stub_205)@(L(unit))))[@inline] in
let sign#420 =
  fun _sk#5603 -> (fun _d#5604 -> ((poly_stub_204)@(L(unit))))[@inline] in
let add_account#421 =
  fun _s#5606 -> (fun _k#5607 -> ((poly_stub_203)@(L(unit))))[@inline] in
let baker_account#422 =
  fun _p#5609 -> (fun _o#5610 -> ((poly_stub_202)@(L(unit))))[@inline] in
let create_chest#424 =
  fun _b#5615 -> (fun _n#5616 -> ((poly_stub_201)@(L(unit))))[@inline] in
let create_chest_key#425 =
  fun _c#5618 -> (fun _n#5619 -> ((poly_stub_200)@(L(unit))))[@inline] in
let michelson_equal#428 =
  fun _m1#5629 -> (fun _m2#5630 -> ((poly_stub_199)@(L(unit))))[@inline] in
let originate_contract#430 =
  fun _c#5635 ->
  (fun _s#5636 -> (fun _t#5637 -> ((poly_stub_198)@(L(unit)))))[@inline] in
let compile_contract_from_file#432 =
  fun _fn#5643 ->
  (fun _e#5644 -> (fun _v#5645 -> ((poly_stub_197)@(L(unit)))))[@inline] in
let originate_from_file#433 =
  fun _fn#5647 ->
  (fun _e#5648 ->
   (fun _v#5649 ->
    (fun _s#5650 -> (fun _t#5651 -> ((poly_stub_196)@(L(unit)))))))[@inline] in
let toto#434 = L(32) in
let titi#435 = ADD(toto#255 , L(42)) in
let f#436 =
  fun gen#5655 ->
  (let (gen#8067, gen#8068) = gen#5655 in
   let gen#5656 = gen#8067 in
   let x#5657 = gen#8068 in
   let x#5658 = ADD(ADD(x#5657 , toto#255) , titi#435) in
   PAIR(LIST_EMPTY() , x#5658)) in
let get_balance#437 =
  fun _u#5660 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#438 =
  fun _u#5662 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#439 = fun _u#5664 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#440 =
  fun _u#5666 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#441 =
  fun _u#5668 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#442 = fun _u#5670 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#443 = fun _u#5672 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#444 =
  fun _u#5674 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#445 =
  fun _u#5676 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#446 =
  fun _u#5678 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#447 =
  fun kh#5680 -> (({ VOTING_POWER })@(kh#5680))[@inline] in
let implicit_account#449 =
  fun kh#5684 -> (IMPLICIT_ACCOUNT(kh#5684))[@inline] in
let pairing_check#453 =
  fun l#5692 -> (({ PAIRING_CHECK })@(l#5692))[@inline] in
let set_delegate#455 = fun o#5696 -> (SET_DELEGATE(o#5696))[@inline] in
let open_chest#463 =
  fun ck#5717 ->
  (fun c#5718 -> (fun n#5719 -> (OPEN_CHEST(ck#5717 , c#5718 , n#5719))))[@inline] in
let xor#472 =
  fun l#5753 -> (fun r#5754 -> (XOR(l#5753 , r#5754)))[@inline] in
let or#473 = fun l#5756 -> (fun r#5757 -> (OR(l#5756 , r#5757)))[@inline] in
let shift_left#474 =
  fun l#5759 -> (fun r#5760 -> (LSL(l#5759 , r#5760)))[@inline] in
let shift_right#475 =
  fun l#5762 -> (fun r#5763 -> (LSR(l#5762 , r#5763)))[@inline] in
let length#519 = fun b#5901 -> (({ SIZE })@(b#5901))[@inline] in
let concat#520 =
  fun b1#5903 ->
  (fun b2#5904 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5903 , b2#5904))))[@inline] in
let sub#521 =
  fun s#5906 ->
  (fun l#5907 ->
   (fun b#5908 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5906 ,
                                                                   l#5907) ,
                                                              b#5908)))))[@inline] in
let length#527 = fun b#5923 -> (({ SIZE })@(b#5923))[@inline] in
let concat#528 =
  fun b1#5925 ->
  (fun b2#5926 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5925 , b2#5926))))[@inline] in
let sub#529 =
  fun s#5928 ->
  (fun l#5929 ->
   (fun b#5930 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5928 ,
                                                                   l#5929) ,
                                                              b#5930)))))[@inline] in
let blake2b#530 = fun b#5932 -> (({ BLAKE2B })@(b#5932))[@inline] in
let sha256#531 = fun b#5934 -> (({ SHA256 })@(b#5934))[@inline] in
let sha512#532 = fun b#5936 -> (({ SHA512 })@(b#5936))[@inline] in
let sha3#533 = fun b#5938 -> (({ SHA3 })@(b#5938))[@inline] in
let keccak#534 = fun b#5940 -> (({ KECCAK })@(b#5940))[@inline] in
let hash_key#535 = fun k#5942 -> (({ HASH_KEY })@(k#5942))[@inline] in
let check#536 =
  fun k#5944 ->
  (fun s#5945 ->
   (fun b#5946 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5944 , s#5945) ,
                                                   b#5946)))))[@inline] in
let assert#537 =
  fun b#5948 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5948))[@inline] in
let abs#540 = fun i#5954 -> (({ ABS })@(i#5954))[@inline] in
let is_nat#541 = fun i#5956 -> (({ ISNAT })@(i#5956))[@inline] in
let true#542 = TRUE()[@inline] in
let false#543 = FALSE()[@inline] in
let unit#544 = UNIT()[@inline] in
let assert_with_error#547 =
  fun b#5964 ->
  (fun s#5965 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5964 , s#5965))))[@inline] in
let poly_stub_195 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_194 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_193 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_192 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_191 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_190 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_189 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_188 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_187 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_186 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_185 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_184 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_183 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_182 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_181 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_180 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_179 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_178 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_177 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_176 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_175 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_174 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_173 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_172 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_171 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_170 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_169 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_168 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_167 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_166 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_165 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_164 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_163 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_162 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_161 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_160 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_159 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_158 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let poly_stub_157 = fun x#5976 -> (({ FAILWITH })@(x#5976))[@inline] in
let get_total_voting_power#555 =
  fun _u#5985 -> ((poly_stub_195)@(L(unit)))[@inline] in
let set_source#558 = fun _a#5991 -> ((poly_stub_194)@(L(unit)))[@inline] in
let get_storage_of_address#559 =
  fun _a#5993 -> ((poly_stub_193)@(L(unit)))[@inline] in
let get_balance#560 = fun _a#5995 -> ((poly_stub_192)@(L(unit)))[@inline] in
let print#561 = fun _v#5997 -> ((poly_stub_191)@(L(unit)))[@inline] in
let eprint#562 = fun _v#5999 -> ((poly_stub_190)@(L(unit)))[@inline] in
let get_voting_power#563 =
  fun _kh#6001 -> ((poly_stub_189)@(L(unit)))[@inline] in
let nth_bootstrap_contract#564 =
  fun _i#6003 -> ((poly_stub_188)@(L(unit)))[@inline] in
let nth_bootstrap_account#565 =
  fun _i#6005 -> ((poly_stub_187)@(L(unit)))[@inline] in
let get_bootstrap_account#566 =
  fun _n#6007 -> ((poly_stub_186)@(L(unit)))[@inline] in
let last_originations#568 =
  fun _u#6011 -> ((poly_stub_185)@(L(unit)))[@inline] in
let new_account#570 = fun _u#6015 -> ((poly_stub_184)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#572 =
  fun _n#6019 -> ((poly_stub_183)@(L(unit)))[@inline] in
let register_delegate#574 =
  fun _kh#6023 -> ((poly_stub_182)@(L(unit)))[@inline] in
let register_constant#575 =
  fun _m#6025 -> ((poly_stub_181)@(L(unit)))[@inline] in
let constant_to_michelson_program#577 =
  fun _s#6029 -> ((poly_stub_180)@(L(unit)))[@inline] in
let restore_context#578 =
  fun _u#6031 -> ((poly_stub_179)@(L(unit)))[@inline] in
let save_context#579 = fun _u#6033 -> ((poly_stub_178)@(L(unit)))[@inline] in
let drop_context#580 = fun _u#6035 -> ((poly_stub_177)@(L(unit)))[@inline] in
let set_baker_policy#583 =
  fun _bp#6041 -> ((poly_stub_176)@(L(unit)))[@inline] in
let set_baker#584 = fun _a#6043 -> ((poly_stub_175)@(L(unit)))[@inline] in
let size#585 = fun _c#6045 -> ((poly_stub_174)@(L(unit)))[@inline] in
let read_contract_from_file#587 =
  fun _fn#6049 -> ((poly_stub_173)@(L(unit)))[@inline] in
let chr#588 = fun _n#6051 -> ((poly_stub_172)@(L(unit)))[@inline] in
let nl#589 = L("NEWLINE")[@inline] in
let println#590 = fun _v#6054 -> ((poly_stub_171)@(L(unit)))[@inline] in
let transfer#591 =
  fun _a#6056 ->
  (fun _s#6057 -> (fun _t#6058 -> ((poly_stub_170)@(L(unit)))))[@inline] in
let transfer_exn#592 =
  fun _a#6060 ->
  (fun _s#6061 -> (fun _t#6062 -> ((poly_stub_169)@(L(unit)))))[@inline] in
let reset_state#594 =
  fun _n#6066 -> (fun _l#6067 -> ((poly_stub_168)@(L(unit))))[@inline] in
let reset_state_at#595 =
  fun _t#6069 ->
  (fun _n#6070 -> (fun _l#6071 -> ((poly_stub_167)@(L(unit)))))[@inline] in
let save_mutation#598 =
  fun _s#6080 -> (fun _m#6081 -> ((poly_stub_166)@(L(unit))))[@inline] in
let sign#601 =
  fun _sk#6089 -> (fun _d#6090 -> ((poly_stub_165)@(L(unit))))[@inline] in
let add_account#602 =
  fun _s#6092 -> (fun _k#6093 -> ((poly_stub_164)@(L(unit))))[@inline] in
let baker_account#603 =
  fun _p#6095 -> (fun _o#6096 -> ((poly_stub_163)@(L(unit))))[@inline] in
let create_chest#605 =
  fun _b#6101 -> (fun _n#6102 -> ((poly_stub_162)@(L(unit))))[@inline] in
let create_chest_key#606 =
  fun _c#6104 -> (fun _n#6105 -> ((poly_stub_161)@(L(unit))))[@inline] in
let michelson_equal#609 =
  fun _m1#6115 -> (fun _m2#6116 -> ((poly_stub_160)@(L(unit))))[@inline] in
let originate_contract#611 =
  fun _c#6121 ->
  (fun _s#6122 -> (fun _t#6123 -> ((poly_stub_159)@(L(unit)))))[@inline] in
let compile_contract_from_file#613 =
  fun _fn#6129 ->
  (fun _e#6130 -> (fun _v#6131 -> ((poly_stub_158)@(L(unit)))))[@inline] in
let originate_from_file#614 =
  fun _fn#6133 ->
  (fun _e#6134 ->
   (fun _v#6135 ->
    (fun _s#6136 -> (fun _t#6137 -> ((poly_stub_157)@(L(unit)))))))[@inline] in
let toto#615 = L(44) in
let get_balance#616 =
  fun _u#6140 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#617 =
  fun _u#6142 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#618 = fun _u#6144 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#619 =
  fun _u#6146 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#620 =
  fun _u#6148 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#621 = fun _u#6150 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#622 = fun _u#6152 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#623 =
  fun _u#6154 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#624 =
  fun _u#6156 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#625 =
  fun _u#6158 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#626 =
  fun kh#6160 -> (({ VOTING_POWER })@(kh#6160))[@inline] in
let implicit_account#628 =
  fun kh#6164 -> (IMPLICIT_ACCOUNT(kh#6164))[@inline] in
let pairing_check#632 =
  fun l#6172 -> (({ PAIRING_CHECK })@(l#6172))[@inline] in
let set_delegate#634 = fun o#6176 -> (SET_DELEGATE(o#6176))[@inline] in
let open_chest#642 =
  fun ck#6197 ->
  (fun c#6198 -> (fun n#6199 -> (OPEN_CHEST(ck#6197 , c#6198 , n#6199))))[@inline] in
let xor#651 =
  fun l#6233 -> (fun r#6234 -> (XOR(l#6233 , r#6234)))[@inline] in
let or#652 = fun l#6236 -> (fun r#6237 -> (OR(l#6236 , r#6237)))[@inline] in
let shift_left#653 =
  fun l#6239 -> (fun r#6240 -> (LSL(l#6239 , r#6240)))[@inline] in
let shift_right#654 =
  fun l#6242 -> (fun r#6243 -> (LSR(l#6242 , r#6243)))[@inline] in
let length#698 = fun b#6381 -> (({ SIZE })@(b#6381))[@inline] in
let concat#699 =
  fun b1#6383 ->
  (fun b2#6384 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6383 , b2#6384))))[@inline] in
let sub#700 =
  fun s#6386 ->
  (fun l#6387 ->
   (fun b#6388 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6386 ,
                                                                   l#6387) ,
                                                              b#6388)))))[@inline] in
let length#706 = fun b#6403 -> (({ SIZE })@(b#6403))[@inline] in
let concat#707 =
  fun b1#6405 ->
  (fun b2#6406 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6405 , b2#6406))))[@inline] in
let sub#708 =
  fun s#6408 ->
  (fun l#6409 ->
   (fun b#6410 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6408 ,
                                                                   l#6409) ,
                                                              b#6410)))))[@inline] in
let blake2b#709 = fun b#6412 -> (({ BLAKE2B })@(b#6412))[@inline] in
let sha256#710 = fun b#6414 -> (({ SHA256 })@(b#6414))[@inline] in
let sha512#711 = fun b#6416 -> (({ SHA512 })@(b#6416))[@inline] in
let sha3#712 = fun b#6418 -> (({ SHA3 })@(b#6418))[@inline] in
let keccak#713 = fun b#6420 -> (({ KECCAK })@(b#6420))[@inline] in
let hash_key#714 = fun k#6422 -> (({ HASH_KEY })@(k#6422))[@inline] in
let check#715 =
  fun k#6424 ->
  (fun s#6425 ->
   (fun b#6426 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6424 , s#6425) ,
                                                   b#6426)))))[@inline] in
let assert#716 =
  fun b#6428 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6428))[@inline] in
let abs#719 = fun i#6434 -> (({ ABS })@(i#6434))[@inline] in
let is_nat#720 = fun i#6436 -> (({ ISNAT })@(i#6436))[@inline] in
let true#721 = TRUE()[@inline] in
let false#722 = FALSE()[@inline] in
let unit#723 = UNIT()[@inline] in
let assert_with_error#726 =
  fun b#6444 ->
  (fun s#6445 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6444 , s#6445))))[@inline] in
let poly_stub_156 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_155 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_154 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_153 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_152 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_151 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_150 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_149 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_148 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_147 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_146 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_145 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_144 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_143 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_142 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_141 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_140 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_139 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_138 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_137 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_136 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_135 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_134 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_133 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_132 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_131 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_130 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_129 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_128 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_127 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_126 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_125 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_124 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_123 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_122 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_121 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_120 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_119 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let poly_stub_118 = fun x#6456 -> (({ FAILWITH })@(x#6456))[@inline] in
let get_total_voting_power#734 =
  fun _u#6465 -> ((poly_stub_156)@(L(unit)))[@inline] in
let set_source#737 = fun _a#6471 -> ((poly_stub_155)@(L(unit)))[@inline] in
let get_storage_of_address#738 =
  fun _a#6473 -> ((poly_stub_154)@(L(unit)))[@inline] in
let get_balance#739 = fun _a#6475 -> ((poly_stub_153)@(L(unit)))[@inline] in
let print#740 = fun _v#6477 -> ((poly_stub_152)@(L(unit)))[@inline] in
let eprint#741 = fun _v#6479 -> ((poly_stub_151)@(L(unit)))[@inline] in
let get_voting_power#742 =
  fun _kh#6481 -> ((poly_stub_150)@(L(unit)))[@inline] in
let nth_bootstrap_contract#743 =
  fun _i#6483 -> ((poly_stub_149)@(L(unit)))[@inline] in
let nth_bootstrap_account#744 =
  fun _i#6485 -> ((poly_stub_148)@(L(unit)))[@inline] in
let get_bootstrap_account#745 =
  fun _n#6487 -> ((poly_stub_147)@(L(unit)))[@inline] in
let last_originations#747 =
  fun _u#6491 -> ((poly_stub_146)@(L(unit)))[@inline] in
let new_account#749 = fun _u#6495 -> ((poly_stub_145)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#751 =
  fun _n#6499 -> ((poly_stub_144)@(L(unit)))[@inline] in
let register_delegate#753 =
  fun _kh#6503 -> ((poly_stub_143)@(L(unit)))[@inline] in
let register_constant#754 =
  fun _m#6505 -> ((poly_stub_142)@(L(unit)))[@inline] in
let constant_to_michelson_program#756 =
  fun _s#6509 -> ((poly_stub_141)@(L(unit)))[@inline] in
let restore_context#757 =
  fun _u#6511 -> ((poly_stub_140)@(L(unit)))[@inline] in
let save_context#758 = fun _u#6513 -> ((poly_stub_139)@(L(unit)))[@inline] in
let drop_context#759 = fun _u#6515 -> ((poly_stub_138)@(L(unit)))[@inline] in
let set_baker_policy#762 =
  fun _bp#6521 -> ((poly_stub_137)@(L(unit)))[@inline] in
let set_baker#763 = fun _a#6523 -> ((poly_stub_136)@(L(unit)))[@inline] in
let size#764 = fun _c#6525 -> ((poly_stub_135)@(L(unit)))[@inline] in
let read_contract_from_file#766 =
  fun _fn#6529 -> ((poly_stub_134)@(L(unit)))[@inline] in
let chr#767 = fun _n#6531 -> ((poly_stub_133)@(L(unit)))[@inline] in
let nl#768 = L("NEWLINE")[@inline] in
let println#769 = fun _v#6534 -> ((poly_stub_132)@(L(unit)))[@inline] in
let transfer#770 =
  fun _a#6536 ->
  (fun _s#6537 -> (fun _t#6538 -> ((poly_stub_131)@(L(unit)))))[@inline] in
let transfer_exn#771 =
  fun _a#6540 ->
  (fun _s#6541 -> (fun _t#6542 -> ((poly_stub_130)@(L(unit)))))[@inline] in
let reset_state#773 =
  fun _n#6546 -> (fun _l#6547 -> ((poly_stub_129)@(L(unit))))[@inline] in
let reset_state_at#774 =
  fun _t#6549 ->
  (fun _n#6550 -> (fun _l#6551 -> ((poly_stub_128)@(L(unit)))))[@inline] in
let save_mutation#777 =
  fun _s#6560 -> (fun _m#6561 -> ((poly_stub_127)@(L(unit))))[@inline] in
let sign#780 =
  fun _sk#6569 -> (fun _d#6570 -> ((poly_stub_126)@(L(unit))))[@inline] in
let add_account#781 =
  fun _s#6572 -> (fun _k#6573 -> ((poly_stub_125)@(L(unit))))[@inline] in
let baker_account#782 =
  fun _p#6575 -> (fun _o#6576 -> ((poly_stub_124)@(L(unit))))[@inline] in
let create_chest#784 =
  fun _b#6581 -> (fun _n#6582 -> ((poly_stub_123)@(L(unit))))[@inline] in
let create_chest_key#785 =
  fun _c#6584 -> (fun _n#6585 -> ((poly_stub_122)@(L(unit))))[@inline] in
let michelson_equal#788 =
  fun _m1#6595 -> (fun _m2#6596 -> ((poly_stub_121)@(L(unit))))[@inline] in
let originate_contract#790 =
  fun _c#6601 ->
  (fun _s#6602 -> (fun _t#6603 -> ((poly_stub_120)@(L(unit)))))[@inline] in
let compile_contract_from_file#792 =
  fun _fn#6609 ->
  (fun _e#6610 -> (fun _v#6611 -> ((poly_stub_119)@(L(unit)))))[@inline] in
let originate_from_file#793 =
  fun _fn#6613 ->
  (fun _e#6614 ->
   (fun _v#6615 ->
    (fun _s#6616 -> (fun _t#6617 -> ((poly_stub_118)@(L(unit)))))))[@inline] in
let toto#794 = L(43) in
let get_balance#795 =
  fun _u#6620 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#796 =
  fun _u#6622 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#797 = fun _u#6624 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#798 =
  fun _u#6626 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#799 =
  fun _u#6628 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#800 = fun _u#6630 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#801 = fun _u#6632 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#802 =
  fun _u#6634 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#803 =
  fun _u#6636 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#804 =
  fun _u#6638 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#805 =
  fun kh#6640 -> (({ VOTING_POWER })@(kh#6640))[@inline] in
let implicit_account#807 =
  fun kh#6644 -> (IMPLICIT_ACCOUNT(kh#6644))[@inline] in
let pairing_check#811 =
  fun l#6652 -> (({ PAIRING_CHECK })@(l#6652))[@inline] in
let set_delegate#813 = fun o#6656 -> (SET_DELEGATE(o#6656))[@inline] in
let open_chest#821 =
  fun ck#6677 ->
  (fun c#6678 -> (fun n#6679 -> (OPEN_CHEST(ck#6677 , c#6678 , n#6679))))[@inline] in
let xor#830 =
  fun l#6713 -> (fun r#6714 -> (XOR(l#6713 , r#6714)))[@inline] in
let or#831 = fun l#6716 -> (fun r#6717 -> (OR(l#6716 , r#6717)))[@inline] in
let shift_left#832 =
  fun l#6719 -> (fun r#6720 -> (LSL(l#6719 , r#6720)))[@inline] in
let shift_right#833 =
  fun l#6722 -> (fun r#6723 -> (LSR(l#6722 , r#6723)))[@inline] in
let length#877 = fun b#6861 -> (({ SIZE })@(b#6861))[@inline] in
let concat#878 =
  fun b1#6863 ->
  (fun b2#6864 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6863 , b2#6864))))[@inline] in
let sub#879 =
  fun s#6866 ->
  (fun l#6867 ->
   (fun b#6868 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6866 ,
                                                                   l#6867) ,
                                                              b#6868)))))[@inline] in
let length#885 = fun b#6883 -> (({ SIZE })@(b#6883))[@inline] in
let concat#886 =
  fun b1#6885 ->
  (fun b2#6886 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6885 , b2#6886))))[@inline] in
let sub#887 =
  fun s#6888 ->
  (fun l#6889 ->
   (fun b#6890 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6888 ,
                                                                   l#6889) ,
                                                              b#6890)))))[@inline] in
let blake2b#888 = fun b#6892 -> (({ BLAKE2B })@(b#6892))[@inline] in
let sha256#889 = fun b#6894 -> (({ SHA256 })@(b#6894))[@inline] in
let sha512#890 = fun b#6896 -> (({ SHA512 })@(b#6896))[@inline] in
let sha3#891 = fun b#6898 -> (({ SHA3 })@(b#6898))[@inline] in
let keccak#892 = fun b#6900 -> (({ KECCAK })@(b#6900))[@inline] in
let hash_key#893 = fun k#6902 -> (({ HASH_KEY })@(k#6902))[@inline] in
let check#894 =
  fun k#6904 ->
  (fun s#6905 ->
   (fun b#6906 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6904 , s#6905) ,
                                                   b#6906)))))[@inline] in
let assert#895 =
  fun b#6908 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6908))[@inline] in
let abs#898 = fun i#6914 -> (({ ABS })@(i#6914))[@inline] in
let is_nat#899 = fun i#6916 -> (({ ISNAT })@(i#6916))[@inline] in
let true#900 = TRUE()[@inline] in
let false#901 = FALSE()[@inline] in
let unit#902 = UNIT()[@inline] in
let assert_with_error#905 =
  fun b#6924 ->
  (fun s#6925 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6924 , s#6925))))[@inline] in
let poly_stub_117 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_116 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_115 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_114 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_113 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_112 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_111 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_110 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_109 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_108 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_107 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_106 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_105 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_104 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_103 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_102 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_101 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_100 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_99 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_98 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_97 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_96 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_95 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_94 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_93 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_92 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_91 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_90 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_89 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_88 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_87 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_86 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_85 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_84 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_83 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_82 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_81 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_80 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let poly_stub_79 = fun x#6936 -> (({ FAILWITH })@(x#6936))[@inline] in
let get_total_voting_power#913 =
  fun _u#6945 -> ((poly_stub_117)@(L(unit)))[@inline] in
let set_source#916 = fun _a#6951 -> ((poly_stub_116)@(L(unit)))[@inline] in
let get_storage_of_address#917 =
  fun _a#6953 -> ((poly_stub_115)@(L(unit)))[@inline] in
let get_balance#918 = fun _a#6955 -> ((poly_stub_114)@(L(unit)))[@inline] in
let print#919 = fun _v#6957 -> ((poly_stub_113)@(L(unit)))[@inline] in
let eprint#920 = fun _v#6959 -> ((poly_stub_112)@(L(unit)))[@inline] in
let get_voting_power#921 =
  fun _kh#6961 -> ((poly_stub_111)@(L(unit)))[@inline] in
let nth_bootstrap_contract#922 =
  fun _i#6963 -> ((poly_stub_110)@(L(unit)))[@inline] in
let nth_bootstrap_account#923 =
  fun _i#6965 -> ((poly_stub_109)@(L(unit)))[@inline] in
let get_bootstrap_account#924 =
  fun _n#6967 -> ((poly_stub_108)@(L(unit)))[@inline] in
let last_originations#926 =
  fun _u#6971 -> ((poly_stub_107)@(L(unit)))[@inline] in
let new_account#928 = fun _u#6975 -> ((poly_stub_106)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#930 =
  fun _n#6979 -> ((poly_stub_105)@(L(unit)))[@inline] in
let register_delegate#932 =
  fun _kh#6983 -> ((poly_stub_104)@(L(unit)))[@inline] in
let register_constant#933 =
  fun _m#6985 -> ((poly_stub_103)@(L(unit)))[@inline] in
let constant_to_michelson_program#935 =
  fun _s#6989 -> ((poly_stub_102)@(L(unit)))[@inline] in
let restore_context#936 =
  fun _u#6991 -> ((poly_stub_101)@(L(unit)))[@inline] in
let save_context#937 = fun _u#6993 -> ((poly_stub_100)@(L(unit)))[@inline] in
let drop_context#938 = fun _u#6995 -> ((poly_stub_99)@(L(unit)))[@inline] in
let set_baker_policy#941 =
  fun _bp#7001 -> ((poly_stub_98)@(L(unit)))[@inline] in
let set_baker#942 = fun _a#7003 -> ((poly_stub_97)@(L(unit)))[@inline] in
let size#943 = fun _c#7005 -> ((poly_stub_96)@(L(unit)))[@inline] in
let read_contract_from_file#945 =
  fun _fn#7009 -> ((poly_stub_95)@(L(unit)))[@inline] in
let chr#946 = fun _n#7011 -> ((poly_stub_94)@(L(unit)))[@inline] in
let nl#947 = L("NEWLINE")[@inline] in
let println#948 = fun _v#7014 -> ((poly_stub_93)@(L(unit)))[@inline] in
let transfer#949 =
  fun _a#7016 -> (fun _s#7017 -> (fun _t#7018 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#950 =
  fun _a#7020 -> (fun _s#7021 -> (fun _t#7022 -> ((poly_stub_91)@(L(unit)))))[@inline] in
let reset_state#952 =
  fun _n#7026 -> (fun _l#7027 -> ((poly_stub_90)@(L(unit))))[@inline] in
let reset_state_at#953 =
  fun _t#7029 -> (fun _n#7030 -> (fun _l#7031 -> ((poly_stub_89)@(L(unit)))))[@inline] in
let save_mutation#956 =
  fun _s#7040 -> (fun _m#7041 -> ((poly_stub_88)@(L(unit))))[@inline] in
let sign#959 =
  fun _sk#7049 -> (fun _d#7050 -> ((poly_stub_87)@(L(unit))))[@inline] in
let add_account#960 =
  fun _s#7052 -> (fun _k#7053 -> ((poly_stub_86)@(L(unit))))[@inline] in
let baker_account#961 =
  fun _p#7055 -> (fun _o#7056 -> ((poly_stub_85)@(L(unit))))[@inline] in
let create_chest#963 =
  fun _b#7061 -> (fun _n#7062 -> ((poly_stub_84)@(L(unit))))[@inline] in
let create_chest_key#964 =
  fun _c#7064 -> (fun _n#7065 -> ((poly_stub_83)@(L(unit))))[@inline] in
let michelson_equal#967 =
  fun _m1#7075 -> (fun _m2#7076 -> ((poly_stub_82)@(L(unit))))[@inline] in
let originate_contract#969 =
  fun _c#7081 -> (fun _s#7082 -> (fun _t#7083 -> ((poly_stub_81)@(L(unit)))))[@inline] in
let compile_contract_from_file#971 =
  fun _fn#7089 ->
  (fun _e#7090 -> (fun _v#7091 -> ((poly_stub_80)@(L(unit)))))[@inline] in
let originate_from_file#972 =
  fun _fn#7093 ->
  (fun _e#7094 ->
   (fun _v#7095 ->
    (fun _s#7096 -> (fun _t#7097 -> ((poly_stub_79)@(L(unit)))))))[@inline] in
let tata#973 = ADD(toto#255 , titi#435) in
let foo#974 = (f#436)@(PAIR(L(unit) , L(3))) in
let get_balance#975 =
  fun _u#7101 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#976 =
  fun _u#7103 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#977 = fun _u#7105 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#978 =
  fun _u#7107 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#979 =
  fun _u#7109 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#980 = fun _u#7111 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#981 = fun _u#7113 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#982 =
  fun _u#7115 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#983 =
  fun _u#7117 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#984 =
  fun _u#7119 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#985 =
  fun kh#7121 -> (({ VOTING_POWER })@(kh#7121))[@inline] in
let implicit_account#987 =
  fun kh#7125 -> (IMPLICIT_ACCOUNT(kh#7125))[@inline] in
let pairing_check#991 =
  fun l#7133 -> (({ PAIRING_CHECK })@(l#7133))[@inline] in
let set_delegate#993 = fun o#7137 -> (SET_DELEGATE(o#7137))[@inline] in
let open_chest#1001 =
  fun ck#7158 ->
  (fun c#7159 -> (fun n#7160 -> (OPEN_CHEST(ck#7158 , c#7159 , n#7160))))[@inline] in
let xor#1010 =
  fun l#7194 -> (fun r#7195 -> (XOR(l#7194 , r#7195)))[@inline] in
let or#1011 = fun l#7197 -> (fun r#7198 -> (OR(l#7197 , r#7198)))[@inline] in
let shift_left#1012 =
  fun l#7200 -> (fun r#7201 -> (LSL(l#7200 , r#7201)))[@inline] in
let shift_right#1013 =
  fun l#7203 -> (fun r#7204 -> (LSR(l#7203 , r#7204)))[@inline] in
let length#1057 = fun b#7342 -> (({ SIZE })@(b#7342))[@inline] in
let concat#1058 =
  fun b1#7344 ->
  (fun b2#7345 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7344 , b2#7345))))[@inline] in
let sub#1059 =
  fun s#7347 ->
  (fun l#7348 ->
   (fun b#7349 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7347 ,
                                                                   l#7348) ,
                                                              b#7349)))))[@inline] in
let length#1065 = fun b#7364 -> (({ SIZE })@(b#7364))[@inline] in
let concat#1066 =
  fun b1#7366 ->
  (fun b2#7367 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7366 , b2#7367))))[@inline] in
let sub#1067 =
  fun s#7369 ->
  (fun l#7370 ->
   (fun b#7371 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7369 ,
                                                                   l#7370) ,
                                                              b#7371)))))[@inline] in
let blake2b#1068 = fun b#7373 -> (({ BLAKE2B })@(b#7373))[@inline] in
let sha256#1069 = fun b#7375 -> (({ SHA256 })@(b#7375))[@inline] in
let sha512#1070 = fun b#7377 -> (({ SHA512 })@(b#7377))[@inline] in
let sha3#1071 = fun b#7379 -> (({ SHA3 })@(b#7379))[@inline] in
let keccak#1072 = fun b#7381 -> (({ KECCAK })@(b#7381))[@inline] in
let hash_key#1073 = fun k#7383 -> (({ HASH_KEY })@(k#7383))[@inline] in
let check#1074 =
  fun k#7385 ->
  (fun s#7386 ->
   (fun b#7387 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7385 , s#7386) ,
                                                   b#7387)))))[@inline] in
let assert#1075 =
  fun b#7389 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7389))[@inline] in
let abs#1078 = fun i#7395 -> (({ ABS })@(i#7395))[@inline] in
let is_nat#1079 = fun i#7397 -> (({ ISNAT })@(i#7397))[@inline] in
let true#1080 = TRUE()[@inline] in
let false#1081 = FALSE()[@inline] in
let unit#1082 = UNIT()[@inline] in
let assert_with_error#1085 =
  fun b#7405 ->
  (fun s#7406 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7405 , s#7406))))[@inline] in
let poly_stub_78 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_77 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_76 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_75 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_74 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_73 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_72 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_71 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_70 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_69 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_68 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_67 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_66 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_65 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_64 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_63 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_62 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_61 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_60 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_59 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_58 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_57 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_56 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_55 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_54 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_53 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_52 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_51 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_50 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_49 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_48 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_47 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_46 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_45 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_44 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_43 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_42 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_41 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let poly_stub_40 = fun x#7417 -> (({ FAILWITH })@(x#7417))[@inline] in
let get_total_voting_power#1093 =
  fun _u#7426 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#1096 = fun _a#7432 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#1097 =
  fun _a#7434 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#1098 = fun _a#7436 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#1099 = fun _v#7438 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#1100 = fun _v#7440 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#1101 =
  fun _kh#7442 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1102 =
  fun _i#7444 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#1103 =
  fun _i#7446 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#1104 =
  fun _n#7448 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#1106 =
  fun _u#7452 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#1108 = fun _u#7456 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1110 =
  fun _n#7460 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#1112 =
  fun _kh#7464 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#1113 =
  fun _m#7466 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#1115 =
  fun _s#7470 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#1116 =
  fun _u#7472 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#1117 = fun _u#7474 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#1118 = fun _u#7476 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#1121 =
  fun _bp#7482 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#1122 = fun _a#7484 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#1123 = fun _c#7486 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#1125 =
  fun _fn#7490 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#1126 = fun _n#7492 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#1127 = L("NEWLINE")[@inline] in
let println#1128 = fun _v#7495 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#1129 =
  fun _a#7497 -> (fun _s#7498 -> (fun _t#7499 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#1130 =
  fun _a#7501 -> (fun _s#7502 -> (fun _t#7503 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#1132 =
  fun _n#7507 -> (fun _l#7508 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#1133 =
  fun _t#7510 -> (fun _n#7511 -> (fun _l#7512 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#1136 =
  fun _s#7521 -> (fun _m#7522 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#1139 =
  fun _sk#7530 -> (fun _d#7531 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#1140 =
  fun _s#7533 -> (fun _k#7534 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#1141 =
  fun _p#7536 -> (fun _o#7537 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#1143 =
  fun _b#7542 -> (fun _n#7543 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#1144 =
  fun _c#7545 -> (fun _n#7546 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#1147 =
  fun _m1#7556 -> (fun _m2#7557 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#1149 =
  fun _c#7562 -> (fun _s#7563 -> (fun _t#7564 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#1151 =
  fun _fn#7570 ->
  (fun _e#7571 -> (fun _v#7572 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#1152 =
  fun _fn#7574 ->
  (fun _e#7575 ->
   (fun _v#7576 ->
    (fun _s#7577 -> (fun _t#7578 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let toto#1153 = L(10) in
let foo#1154 = L("bar") in
let get_balance#1155 =
  fun _u#7582 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1156 =
  fun _u#7584 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1157 = fun _u#7586 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1158 =
  fun _u#7588 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1159 =
  fun _u#7590 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1160 =
  fun _u#7592 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1161 = fun _u#7594 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1162 =
  fun _u#7596 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1163 =
  fun _u#7598 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#1164 =
  fun _u#7600 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#1165 =
  fun kh#7602 -> (({ VOTING_POWER })@(kh#7602))[@inline] in
let implicit_account#1167 =
  fun kh#7606 -> (IMPLICIT_ACCOUNT(kh#7606))[@inline] in
let pairing_check#1171 =
  fun l#7614 -> (({ PAIRING_CHECK })@(l#7614))[@inline] in
let set_delegate#1173 = fun o#7618 -> (SET_DELEGATE(o#7618))[@inline] in
let open_chest#1181 =
  fun ck#7639 ->
  (fun c#7640 -> (fun n#7641 -> (OPEN_CHEST(ck#7639 , c#7640 , n#7641))))[@inline] in
let xor#1190 =
  fun l#7675 -> (fun r#7676 -> (XOR(l#7675 , r#7676)))[@inline] in
let or#1191 = fun l#7678 -> (fun r#7679 -> (OR(l#7678 , r#7679)))[@inline] in
let shift_left#1192 =
  fun l#7681 -> (fun r#7682 -> (LSL(l#7681 , r#7682)))[@inline] in
let shift_right#1193 =
  fun l#7684 -> (fun r#7685 -> (LSR(l#7684 , r#7685)))[@inline] in
let length#1237 = fun b#7823 -> (({ SIZE })@(b#7823))[@inline] in
let concat#1238 =
  fun b1#7825 ->
  (fun b2#7826 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7825 , b2#7826))))[@inline] in
let sub#1239 =
  fun s#7828 ->
  (fun l#7829 ->
   (fun b#7830 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7828 ,
                                                                   l#7829) ,
                                                              b#7830)))))[@inline] in
let length#1245 = fun b#7845 -> (({ SIZE })@(b#7845))[@inline] in
let concat#1246 =
  fun b1#7847 ->
  (fun b2#7848 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7847 , b2#7848))))[@inline] in
let sub#1247 =
  fun s#7850 ->
  (fun l#7851 ->
   (fun b#7852 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7850 ,
                                                                   l#7851) ,
                                                              b#7852)))))[@inline] in
let blake2b#1248 = fun b#7854 -> (({ BLAKE2B })@(b#7854))[@inline] in
let sha256#1249 = fun b#7856 -> (({ SHA256 })@(b#7856))[@inline] in
let sha512#1250 = fun b#7858 -> (({ SHA512 })@(b#7858))[@inline] in
let sha3#1251 = fun b#7860 -> (({ SHA3 })@(b#7860))[@inline] in
let keccak#1252 = fun b#7862 -> (({ KECCAK })@(b#7862))[@inline] in
let hash_key#1253 = fun k#7864 -> (({ HASH_KEY })@(k#7864))[@inline] in
let check#1254 =
  fun k#7866 ->
  (fun s#7867 ->
   (fun b#7868 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7866 , s#7867) ,
                                                   b#7868)))))[@inline] in
let assert =
  fun b#7870 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7870))[@inline] in
let abs = fun i#7876 -> (({ ABS })@(i#7876))[@inline] in
let is_nat = fun i#7878 -> (({ ISNAT })@(i#7878))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#7886 ->
  (fun s#7887 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7886 , s#7887))))[@inline] in
let poly_stub_39 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_38 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_37 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_36 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_35 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_34 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_33 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_32 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_31 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_30 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_29 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_28 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_27 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_26 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_25 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_24 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_23 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_22 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_21 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_20 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_19 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_18 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_17 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_16 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_15 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_14 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_13 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_12 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_11 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_10 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_9 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_8 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_7 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_6 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_5 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_4 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_3 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_2 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let poly_stub_1 = fun x#7898 -> (({ FAILWITH })@(x#7898))[@inline] in
let get_total_voting_power#1259 =
  fun _u#7907 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#1262 = fun _a#7913 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#1263 =
  fun _a#7915 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#1264 = fun _a#7917 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#1265 = fun _v#7919 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#1266 = fun _v#7921 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#1267 =
  fun _kh#7923 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1268 =
  fun _i#7925 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#1269 =
  fun _i#7927 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#1270 =
  fun _n#7929 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#1272 =
  fun _u#7933 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#1274 = fun _u#7937 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1276 =
  fun _n#7941 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#1278 =
  fun _kh#7945 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#1279 =
  fun _m#7947 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#1281 =
  fun _s#7951 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#1282 =
  fun _u#7953 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#1283 = fun _u#7955 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#1284 = fun _u#7957 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#1287 =
  fun _bp#7963 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#1288 = fun _a#7965 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#1289 = fun _c#7967 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#1291 =
  fun _fn#7971 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1292 = fun _n#7973 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#1293 = L("NEWLINE")[@inline] in
let println#1294 = fun _v#7976 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#1295 =
  fun _a#7978 -> (fun _s#7979 -> (fun _t#7980 -> ((poly_stub_14)@(L(unit)))))[@inline] in
let transfer_exn#1296 =
  fun _a#7982 -> (fun _s#7983 -> (fun _t#7984 -> ((poly_stub_13)@(L(unit)))))[@inline] in
let reset_state#1298 =
  fun _n#7988 -> (fun _l#7989 -> ((poly_stub_12)@(L(unit))))[@inline] in
let reset_state_at#1299 =
  fun _t#7991 -> (fun _n#7992 -> (fun _l#7993 -> ((poly_stub_11)@(L(unit)))))[@inline] in
let save_mutation#1302 =
  fun _s#8002 -> (fun _m#8003 -> ((poly_stub_10)@(L(unit))))[@inline] in
let sign#1305 =
  fun _sk#8011 -> (fun _d#8012 -> ((poly_stub_9)@(L(unit))))[@inline] in
let add_account#1306 =
  fun _s#8014 -> (fun _k#8015 -> ((poly_stub_8)@(L(unit))))[@inline] in
let baker_account#1307 =
  fun _p#8017 -> (fun _o#8018 -> ((poly_stub_7)@(L(unit))))[@inline] in
let create_chest#1309 =
  fun _b#8023 -> (fun _n#8024 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#1310 =
  fun _c#8026 -> (fun _n#8027 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#1313 =
  fun _m1#8037 -> (fun _m2#8038 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#1315 =
  fun _c#8043 -> (fun _s#8044 -> (fun _t#8045 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#1317 =
  fun _fn#8051 -> (fun _e#8052 -> (fun _v#8053 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#1318 =
  fun _fn#8055 ->
  (fun _e#8056 ->
   (fun _v#8057 ->
    (fun _s#8058 -> (fun _t#8059 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1153 , toto#255) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#8063 ->
  (let (gen#8069, gen#8070) = gen#8063 in
   let p#8064 = gen#8069 in
   let s#8065 = gen#8070 in
   let s#8066 = ADD(ADD(p#8064 , s#8065) , toto) in
   PAIR(LIST_EMPTY() , s#8066)) in
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
