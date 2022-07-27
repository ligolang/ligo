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
    module C = Mangled_module_C____mligo.
    module E = Mangled_module_E____mligo.
    const toto = ADD(E.toto , C.B.A.toto)
    const fb = record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main =
      lambda (gen#5 : ( int * int )) return  match gen#5 with
                                              | ( p , s ) ->
                                              let s = ADD(ADD(p , s) ,
                                              toto) in ( LIST_EMPTY() , s ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let balance#49 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#50 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#51 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#52 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#53 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#54 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#55 = SELF_ADDRESS()[@inline] in
let chain_id#56 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#57 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#58 =
  fun _u#4395 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#59 =
  fun _u#4397 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#60 = fun _u#4399 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#61 =
  fun _u#4401 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#62 =
  fun _u#4403 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#63 = fun _u#4405 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#64 = fun _u#4407 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#65 =
  fun _u#4409 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#66 =
  fun _u#4411 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#67 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#68 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#69 =
  fun kh#4415 -> (({ VOTING_POWER })@(kh#4415))[@inline] in
let implicit_account#71 =
  fun kh#4419 -> (IMPLICIT_ACCOUNT(kh#4419))[@inline] in
let pairing_check#75 =
  fun l#4427 -> (({ PAIRING_CHECK })@(l#4427))[@inline] in
let set_delegate#77 = fun o#4431 -> (SET_DELEGATE(o#4431))[@inline] in
let open_chest#83 =
  fun ck#4447 ->
  (fun c#4448 -> (fun n#4449 -> (OPEN_CHEST(ck#4447 , c#4448 , n#4449))))[@inline] in
let xor#86 = fun l#4458 -> (fun r#4459 -> (XOR(l#4458 , r#4459)))[@inline] in
let shift_left#87 =
  fun l#4461 -> (fun r#4462 -> (LSL(l#4461 , r#4462)))[@inline] in
let shift_right#88 =
  fun l#4464 -> (fun r#4465 -> (LSR(l#4464 , r#4465)))[@inline] in
let length#129 = fun b#4595 -> (({ SIZE })@(b#4595))[@inline] in
let concat#130 =
  fun b1#4597 ->
  (fun b2#4598 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4597 , b2#4598))))[@inline] in
let sub#131 =
  fun s#4600 ->
  (fun l#4601 ->
   (fun b#4602 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4600 ,
                                                                   l#4601) ,
                                                              b#4602)))))[@inline] in
let length#136 = fun b#4613 -> (({ SIZE })@(b#4613))[@inline] in
let concat#137 =
  fun b1#4615 ->
  (fun b2#4616 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4615 , b2#4616))))[@inline] in
let sub#138 =
  fun s#4618 ->
  (fun l#4619 ->
   (fun b#4620 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4618 ,
                                                                   l#4619) ,
                                                              b#4620)))))[@inline] in
let blake2b#139 = fun b#4622 -> (({ BLAKE2B })@(b#4622))[@inline] in
let sha256#140 = fun b#4624 -> (({ SHA256 })@(b#4624))[@inline] in
let sha512#141 = fun b#4626 -> (({ SHA512 })@(b#4626))[@inline] in
let sha3#142 = fun b#4628 -> (({ SHA3 })@(b#4628))[@inline] in
let keccak#143 = fun b#4630 -> (({ KECCAK })@(b#4630))[@inline] in
let hash_key#144 = fun k#4632 -> (({ HASH_KEY })@(k#4632))[@inline] in
let check#145 =
  fun k#4634 ->
  (fun s#4635 ->
   (fun b#4636 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4634 , s#4635) ,
                                                   b#4636)))))[@inline] in
let assert#146 =
  fun b#4638 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4638))[@inline] in
let abs#149 = fun i#4644 -> (({ ABS })@(i#4644))[@inline] in
let is_nat#150 = fun i#4646 -> (({ ISNAT })@(i#4646))[@inline] in
let true#151 = TRUE()[@inline] in
let false#152 = FALSE()[@inline] in
let unit#153 = UNIT()[@inline] in
let assert_with_error#156 =
  fun b#4654 ->
  (fun s#4655 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4654 , s#4655))))[@inline] in
let poly_stub_105 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_104 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_103 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_102 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_101 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_100 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_99 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_98 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_97 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_96 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_95 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_94 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_93 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_92 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let poly_stub_91 = fun x#4666 -> (({ FAILWITH })@(x#4666))[@inline] in
let get_total_voting_power#164 = (poly_stub_99)@(L(unit))[@inline] in
let set_source#167 = fun _a#4680 -> ((poly_stub_92)@(L(unit)))[@inline] in
let get_storage_of_address#168 =
  fun _a#4682 -> ((poly_stub_92)@(L(unit)))[@inline] in
let get_balance#169 = fun _a#4684 -> ((poly_stub_105)@(L(unit)))[@inline] in
let print#170 = fun _v#4686 -> ((poly_stub_92)@(L(unit)))[@inline] in
let eprint#171 = fun _v#4688 -> ((poly_stub_92)@(L(unit)))[@inline] in
let get_voting_power#172 =
  fun _kh#4690 -> ((poly_stub_99)@(L(unit)))[@inline] in
let nth_bootstrap_contract#173 =
  fun _i#4692 -> ((poly_stub_93)@(L(unit)))[@inline] in
let nth_bootstrap_account#174 =
  fun _i#4694 -> ((poly_stub_93)@(L(unit)))[@inline] in
let get_bootstrap_account#175 =
  fun _n#4696 -> ((poly_stub_104)@(L(unit)))[@inline] in
let last_originations#177 =
  fun _u#4700 -> ((poly_stub_103)@(L(unit)))[@inline] in
let new_account#179 = fun _u#4704 -> ((poly_stub_102)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#181 =
  fun _n#4708 -> ((poly_stub_92)@(L(unit)))[@inline] in
let register_delegate#183 =
  fun _kh#4712 -> ((poly_stub_92)@(L(unit)))[@inline] in
let register_constant#184 =
  fun _m#4714 -> ((poly_stub_101)@(L(unit)))[@inline] in
let constant_to_michelson_program#186 =
  fun _s#4718 -> ((poly_stub_92)@(L(unit)))[@inline] in
let restore_context#187 =
  fun _u#4720 -> ((poly_stub_92)@(L(unit)))[@inline] in
let save_context#188 = fun _u#4722 -> ((poly_stub_92)@(L(unit)))[@inline] in
let drop_context#189 = fun _u#4724 -> ((poly_stub_92)@(L(unit)))[@inline] in
let set_baker_policy#192 =
  fun _bp#4730 -> ((poly_stub_92)@(L(unit)))[@inline] in
let set_baker#193 = fun _a#4732 -> ((poly_stub_92)@(L(unit)))[@inline] in
let size#194 = fun _c#4734 -> ((poly_stub_100)@(L(unit)))[@inline] in
let read_contract_from_file#196 =
  fun _fn#4738 -> ((poly_stub_92)@(L(unit)))[@inline] in
let chr#197 = fun _n#4740 -> ((poly_stub_98)@(L(unit)))[@inline] in
let nl#198 = L("NEWLINE")[@inline] in
let println#199 = fun _v#4743 -> ((poly_stub_92)@(L(unit)))[@inline] in
let transfer#200 =
  fun _a#4745 -> (fun _s#4746 -> (fun _t#4747 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#201 =
  fun _a#4749 -> (fun _s#4750 -> (fun _t#4751 -> ((poly_stub_99)@(L(unit)))))[@inline] in
let reset_state#203 =
  fun _n#4755 -> (fun _l#4756 -> ((poly_stub_92)@(L(unit))))[@inline] in
let reset_state_at#204 =
  fun _t#4758 -> (fun _n#4759 -> (fun _l#4760 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let save_mutation#207 =
  fun _s#4769 -> (fun _m#4770 -> ((poly_stub_98)@(L(unit))))[@inline] in
let sign#210 =
  fun _sk#4778 -> (fun _d#4779 -> ((poly_stub_97)@(L(unit))))[@inline] in
let add_account#211 =
  fun _s#4781 -> (fun _k#4782 -> ((poly_stub_92)@(L(unit))))[@inline] in
let baker_account#212 =
  fun _p#4784 -> (fun _o#4785 -> ((poly_stub_92)@(L(unit))))[@inline] in
let create_chest#214 =
  fun _b#4790 -> (fun _n#4791 -> ((poly_stub_96)@(L(unit))))[@inline] in
let create_chest_key#215 =
  fun _c#4793 -> (fun _n#4794 -> ((poly_stub_95)@(L(unit))))[@inline] in
let michelson_equal#218 =
  fun _m1#4804 -> (fun _m2#4805 -> ((poly_stub_94)@(L(unit))))[@inline] in
let originate_contract#220 =
  fun _c#4810 -> (fun _s#4811 -> (fun _t#4812 -> ((poly_stub_93)@(L(unit)))))[@inline] in
let compile_contract_from_file#222 =
  fun _fn#4818 ->
  (fun _e#4819 -> (fun _v#4820 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let originate_from_file#223 =
  fun _fn#4822 ->
  (fun _e#4823 ->
   (fun _v#4824 ->
    (fun _s#4825 -> (fun _t#4826 -> ((poly_stub_91)@(L(unit)))))))[@inline] in
let toto#224 = L(1) in
let balance#225 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#226 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#227 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#228 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#229 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#230 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#231 = SELF_ADDRESS()[@inline] in
let chain_id#232 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#233 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#234 =
  fun _u#4838 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#235 =
  fun _u#4840 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#236 = fun _u#4842 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#237 =
  fun _u#4844 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#238 =
  fun _u#4846 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#239 = fun _u#4848 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#240 = fun _u#4850 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#241 =
  fun _u#4852 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#242 =
  fun _u#4854 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#243 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#244 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#245 =
  fun kh#4858 -> (({ VOTING_POWER })@(kh#4858))[@inline] in
let implicit_account#247 =
  fun kh#4862 -> (IMPLICIT_ACCOUNT(kh#4862))[@inline] in
let pairing_check#251 =
  fun l#4870 -> (({ PAIRING_CHECK })@(l#4870))[@inline] in
let set_delegate#253 = fun o#4874 -> (SET_DELEGATE(o#4874))[@inline] in
let open_chest#259 =
  fun ck#4890 ->
  (fun c#4891 -> (fun n#4892 -> (OPEN_CHEST(ck#4890 , c#4891 , n#4892))))[@inline] in
let xor#262 =
  fun l#4901 -> (fun r#4902 -> (XOR(l#4901 , r#4902)))[@inline] in
let shift_left#263 =
  fun l#4904 -> (fun r#4905 -> (LSL(l#4904 , r#4905)))[@inline] in
let shift_right#264 =
  fun l#4907 -> (fun r#4908 -> (LSR(l#4907 , r#4908)))[@inline] in
let length#305 = fun b#5038 -> (({ SIZE })@(b#5038))[@inline] in
let concat#306 =
  fun b1#5040 ->
  (fun b2#5041 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5040 , b2#5041))))[@inline] in
let sub#307 =
  fun s#5043 ->
  (fun l#5044 ->
   (fun b#5045 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5043 ,
                                                                   l#5044) ,
                                                              b#5045)))))[@inline] in
let length#312 = fun b#5056 -> (({ SIZE })@(b#5056))[@inline] in
let concat#313 =
  fun b1#5058 ->
  (fun b2#5059 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5058 , b2#5059))))[@inline] in
let sub#314 =
  fun s#5061 ->
  (fun l#5062 ->
   (fun b#5063 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5061 ,
                                                                   l#5062) ,
                                                              b#5063)))))[@inline] in
let blake2b#315 = fun b#5065 -> (({ BLAKE2B })@(b#5065))[@inline] in
let sha256#316 = fun b#5067 -> (({ SHA256 })@(b#5067))[@inline] in
let sha512#317 = fun b#5069 -> (({ SHA512 })@(b#5069))[@inline] in
let sha3#318 = fun b#5071 -> (({ SHA3 })@(b#5071))[@inline] in
let keccak#319 = fun b#5073 -> (({ KECCAK })@(b#5073))[@inline] in
let hash_key#320 = fun k#5075 -> (({ HASH_KEY })@(k#5075))[@inline] in
let check#321 =
  fun k#5077 ->
  (fun s#5078 ->
   (fun b#5079 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5077 , s#5078) ,
                                                   b#5079)))))[@inline] in
let assert#322 =
  fun b#5081 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5081))[@inline] in
let abs#325 = fun i#5087 -> (({ ABS })@(i#5087))[@inline] in
let is_nat#326 = fun i#5089 -> (({ ISNAT })@(i#5089))[@inline] in
let true#327 = TRUE()[@inline] in
let false#328 = FALSE()[@inline] in
let unit#329 = UNIT()[@inline] in
let assert_with_error#332 =
  fun b#5097 ->
  (fun s#5098 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5097 , s#5098))))[@inline] in
let poly_stub_90 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_89 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_88 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_87 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_86 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_85 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_84 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_83 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_82 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_81 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_80 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_79 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_78 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_77 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let poly_stub_76 = fun x#5109 -> (({ FAILWITH })@(x#5109))[@inline] in
let get_total_voting_power#340 = (poly_stub_84)@(L(unit))[@inline] in
let set_source#343 = fun _a#5123 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#344 =
  fun _a#5125 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_balance#345 = fun _a#5127 -> ((poly_stub_90)@(L(unit)))[@inline] in
let print#346 = fun _v#5129 -> ((poly_stub_77)@(L(unit)))[@inline] in
let eprint#347 = fun _v#5131 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_voting_power#348 =
  fun _kh#5133 -> ((poly_stub_84)@(L(unit)))[@inline] in
let nth_bootstrap_contract#349 =
  fun _i#5135 -> ((poly_stub_78)@(L(unit)))[@inline] in
let nth_bootstrap_account#350 =
  fun _i#5137 -> ((poly_stub_78)@(L(unit)))[@inline] in
let get_bootstrap_account#351 =
  fun _n#5139 -> ((poly_stub_89)@(L(unit)))[@inline] in
let last_originations#353 =
  fun _u#5143 -> ((poly_stub_88)@(L(unit)))[@inline] in
let new_account#355 = fun _u#5147 -> ((poly_stub_87)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#357 =
  fun _n#5151 -> ((poly_stub_77)@(L(unit)))[@inline] in
let register_delegate#359 =
  fun _kh#5155 -> ((poly_stub_77)@(L(unit)))[@inline] in
let register_constant#360 =
  fun _m#5157 -> ((poly_stub_86)@(L(unit)))[@inline] in
let constant_to_michelson_program#362 =
  fun _s#5161 -> ((poly_stub_77)@(L(unit)))[@inline] in
let restore_context#363 =
  fun _u#5163 -> ((poly_stub_77)@(L(unit)))[@inline] in
let save_context#364 = fun _u#5165 -> ((poly_stub_77)@(L(unit)))[@inline] in
let drop_context#365 = fun _u#5167 -> ((poly_stub_77)@(L(unit)))[@inline] in
let set_baker_policy#368 =
  fun _bp#5173 -> ((poly_stub_77)@(L(unit)))[@inline] in
let set_baker#369 = fun _a#5175 -> ((poly_stub_77)@(L(unit)))[@inline] in
let size#370 = fun _c#5177 -> ((poly_stub_85)@(L(unit)))[@inline] in
let read_contract_from_file#372 =
  fun _fn#5181 -> ((poly_stub_77)@(L(unit)))[@inline] in
let chr#373 = fun _n#5183 -> ((poly_stub_83)@(L(unit)))[@inline] in
let nl#374 = L("NEWLINE")[@inline] in
let println#375 = fun _v#5186 -> ((poly_stub_77)@(L(unit)))[@inline] in
let transfer#376 =
  fun _a#5188 -> (fun _s#5189 -> (fun _t#5190 -> ((poly_stub_77)@(L(unit)))))[@inline] in
let transfer_exn#377 =
  fun _a#5192 -> (fun _s#5193 -> (fun _t#5194 -> ((poly_stub_84)@(L(unit)))))[@inline] in
let reset_state#379 =
  fun _n#5198 -> (fun _l#5199 -> ((poly_stub_77)@(L(unit))))[@inline] in
let reset_state_at#380 =
  fun _t#5201 -> (fun _n#5202 -> (fun _l#5203 -> ((poly_stub_77)@(L(unit)))))[@inline] in
let save_mutation#383 =
  fun _s#5212 -> (fun _m#5213 -> ((poly_stub_83)@(L(unit))))[@inline] in
let sign#386 =
  fun _sk#5221 -> (fun _d#5222 -> ((poly_stub_82)@(L(unit))))[@inline] in
let add_account#387 =
  fun _s#5224 -> (fun _k#5225 -> ((poly_stub_77)@(L(unit))))[@inline] in
let baker_account#388 =
  fun _p#5227 -> (fun _o#5228 -> ((poly_stub_77)@(L(unit))))[@inline] in
let create_chest#390 =
  fun _b#5233 -> (fun _n#5234 -> ((poly_stub_81)@(L(unit))))[@inline] in
let create_chest_key#391 =
  fun _c#5236 -> (fun _n#5237 -> ((poly_stub_80)@(L(unit))))[@inline] in
let michelson_equal#394 =
  fun _m1#5247 -> (fun _m2#5248 -> ((poly_stub_79)@(L(unit))))[@inline] in
let originate_contract#396 =
  fun _c#5253 -> (fun _s#5254 -> (fun _t#5255 -> ((poly_stub_78)@(L(unit)))))[@inline] in
let compile_contract_from_file#398 =
  fun _fn#5261 ->
  (fun _e#5262 -> (fun _v#5263 -> ((poly_stub_77)@(L(unit)))))[@inline] in
let originate_from_file#399 =
  fun _fn#5265 ->
  (fun _e#5266 ->
   (fun _v#5267 ->
    (fun _s#5268 -> (fun _t#5269 -> ((poly_stub_76)@(L(unit)))))))[@inline] in
let toto#400 = L(32) in
let titi#401 = ADD(toto#224 , L(42)) in
let f#402 =
  fun gen#5273 ->
  (let (gen#7500, gen#7501) = gen#5273 in
   let gen#5274 = gen#7500 in
   let x#5275 = gen#7501 in
   let x#5276 = ADD(ADD(x#5275 , toto#224) , titi#401) in
   PAIR(LIST_EMPTY() , x#5276)) in
let balance#403 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#404 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#405 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#406 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#407 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#408 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#409 = SELF_ADDRESS()[@inline] in
let chain_id#410 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#411 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#412 =
  fun _u#5287 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#413 =
  fun _u#5289 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#414 = fun _u#5291 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#415 =
  fun _u#5293 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#416 =
  fun _u#5295 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#417 = fun _u#5297 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#418 = fun _u#5299 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#419 =
  fun _u#5301 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#420 =
  fun _u#5303 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#421 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#422 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#423 =
  fun kh#5307 -> (({ VOTING_POWER })@(kh#5307))[@inline] in
let implicit_account#425 =
  fun kh#5311 -> (IMPLICIT_ACCOUNT(kh#5311))[@inline] in
let pairing_check#429 =
  fun l#5319 -> (({ PAIRING_CHECK })@(l#5319))[@inline] in
let set_delegate#431 = fun o#5323 -> (SET_DELEGATE(o#5323))[@inline] in
let open_chest#437 =
  fun ck#5339 ->
  (fun c#5340 -> (fun n#5341 -> (OPEN_CHEST(ck#5339 , c#5340 , n#5341))))[@inline] in
let xor#440 =
  fun l#5350 -> (fun r#5351 -> (XOR(l#5350 , r#5351)))[@inline] in
let shift_left#441 =
  fun l#5353 -> (fun r#5354 -> (LSL(l#5353 , r#5354)))[@inline] in
let shift_right#442 =
  fun l#5356 -> (fun r#5357 -> (LSR(l#5356 , r#5357)))[@inline] in
let length#483 = fun b#5487 -> (({ SIZE })@(b#5487))[@inline] in
let concat#484 =
  fun b1#5489 ->
  (fun b2#5490 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5489 , b2#5490))))[@inline] in
let sub#485 =
  fun s#5492 ->
  (fun l#5493 ->
   (fun b#5494 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5492 ,
                                                                   l#5493) ,
                                                              b#5494)))))[@inline] in
let length#490 = fun b#5505 -> (({ SIZE })@(b#5505))[@inline] in
let concat#491 =
  fun b1#5507 ->
  (fun b2#5508 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5507 , b2#5508))))[@inline] in
let sub#492 =
  fun s#5510 ->
  (fun l#5511 ->
   (fun b#5512 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5510 ,
                                                                   l#5511) ,
                                                              b#5512)))))[@inline] in
let blake2b#493 = fun b#5514 -> (({ BLAKE2B })@(b#5514))[@inline] in
let sha256#494 = fun b#5516 -> (({ SHA256 })@(b#5516))[@inline] in
let sha512#495 = fun b#5518 -> (({ SHA512 })@(b#5518))[@inline] in
let sha3#496 = fun b#5520 -> (({ SHA3 })@(b#5520))[@inline] in
let keccak#497 = fun b#5522 -> (({ KECCAK })@(b#5522))[@inline] in
let hash_key#498 = fun k#5524 -> (({ HASH_KEY })@(k#5524))[@inline] in
let check#499 =
  fun k#5526 ->
  (fun s#5527 ->
   (fun b#5528 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5526 , s#5527) ,
                                                   b#5528)))))[@inline] in
let assert#500 =
  fun b#5530 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5530))[@inline] in
let abs#503 = fun i#5536 -> (({ ABS })@(i#5536))[@inline] in
let is_nat#504 = fun i#5538 -> (({ ISNAT })@(i#5538))[@inline] in
let true#505 = TRUE()[@inline] in
let false#506 = FALSE()[@inline] in
let unit#507 = UNIT()[@inline] in
let assert_with_error#510 =
  fun b#5546 ->
  (fun s#5547 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5546 , s#5547))))[@inline] in
let poly_stub_75 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_74 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_73 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_72 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_71 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_70 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_69 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_68 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_67 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_66 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_65 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_64 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_63 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_62 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let poly_stub_61 = fun x#5558 -> (({ FAILWITH })@(x#5558))[@inline] in
let get_total_voting_power#518 = (poly_stub_69)@(L(unit))[@inline] in
let set_source#521 = fun _a#5572 -> ((poly_stub_62)@(L(unit)))[@inline] in
let get_storage_of_address#522 =
  fun _a#5574 -> ((poly_stub_62)@(L(unit)))[@inline] in
let get_balance#523 = fun _a#5576 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#524 = fun _v#5578 -> ((poly_stub_62)@(L(unit)))[@inline] in
let eprint#525 = fun _v#5580 -> ((poly_stub_62)@(L(unit)))[@inline] in
let get_voting_power#526 =
  fun _kh#5582 -> ((poly_stub_69)@(L(unit)))[@inline] in
let nth_bootstrap_contract#527 =
  fun _i#5584 -> ((poly_stub_63)@(L(unit)))[@inline] in
let nth_bootstrap_account#528 =
  fun _i#5586 -> ((poly_stub_63)@(L(unit)))[@inline] in
let get_bootstrap_account#529 =
  fun _n#5588 -> ((poly_stub_74)@(L(unit)))[@inline] in
let last_originations#531 =
  fun _u#5592 -> ((poly_stub_73)@(L(unit)))[@inline] in
let new_account#533 = fun _u#5596 -> ((poly_stub_72)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#535 =
  fun _n#5600 -> ((poly_stub_62)@(L(unit)))[@inline] in
let register_delegate#537 =
  fun _kh#5604 -> ((poly_stub_62)@(L(unit)))[@inline] in
let register_constant#538 =
  fun _m#5606 -> ((poly_stub_71)@(L(unit)))[@inline] in
let constant_to_michelson_program#540 =
  fun _s#5610 -> ((poly_stub_62)@(L(unit)))[@inline] in
let restore_context#541 =
  fun _u#5612 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#542 = fun _u#5614 -> ((poly_stub_62)@(L(unit)))[@inline] in
let drop_context#543 = fun _u#5616 -> ((poly_stub_62)@(L(unit)))[@inline] in
let set_baker_policy#546 =
  fun _bp#5622 -> ((poly_stub_62)@(L(unit)))[@inline] in
let set_baker#547 = fun _a#5624 -> ((poly_stub_62)@(L(unit)))[@inline] in
let size#548 = fun _c#5626 -> ((poly_stub_70)@(L(unit)))[@inline] in
let read_contract_from_file#550 =
  fun _fn#5630 -> ((poly_stub_62)@(L(unit)))[@inline] in
let chr#551 = fun _n#5632 -> ((poly_stub_68)@(L(unit)))[@inline] in
let nl#552 = L("NEWLINE")[@inline] in
let println#553 = fun _v#5635 -> ((poly_stub_62)@(L(unit)))[@inline] in
let transfer#554 =
  fun _a#5637 -> (fun _s#5638 -> (fun _t#5639 -> ((poly_stub_62)@(L(unit)))))[@inline] in
let transfer_exn#555 =
  fun _a#5641 -> (fun _s#5642 -> (fun _t#5643 -> ((poly_stub_69)@(L(unit)))))[@inline] in
let reset_state#557 =
  fun _n#5647 -> (fun _l#5648 -> ((poly_stub_62)@(L(unit))))[@inline] in
let reset_state_at#558 =
  fun _t#5650 -> (fun _n#5651 -> (fun _l#5652 -> ((poly_stub_62)@(L(unit)))))[@inline] in
let save_mutation#561 =
  fun _s#5661 -> (fun _m#5662 -> ((poly_stub_68)@(L(unit))))[@inline] in
let sign#564 =
  fun _sk#5670 -> (fun _d#5671 -> ((poly_stub_67)@(L(unit))))[@inline] in
let add_account#565 =
  fun _s#5673 -> (fun _k#5674 -> ((poly_stub_62)@(L(unit))))[@inline] in
let baker_account#566 =
  fun _p#5676 -> (fun _o#5677 -> ((poly_stub_62)@(L(unit))))[@inline] in
let create_chest#568 =
  fun _b#5682 -> (fun _n#5683 -> ((poly_stub_66)@(L(unit))))[@inline] in
let create_chest_key#569 =
  fun _c#5685 -> (fun _n#5686 -> ((poly_stub_65)@(L(unit))))[@inline] in
let michelson_equal#572 =
  fun _m1#5696 -> (fun _m2#5697 -> ((poly_stub_64)@(L(unit))))[@inline] in
let originate_contract#574 =
  fun _c#5702 -> (fun _s#5703 -> (fun _t#5704 -> ((poly_stub_63)@(L(unit)))))[@inline] in
let compile_contract_from_file#576 =
  fun _fn#5710 ->
  (fun _e#5711 -> (fun _v#5712 -> ((poly_stub_62)@(L(unit)))))[@inline] in
let originate_from_file#577 =
  fun _fn#5714 ->
  (fun _e#5715 ->
   (fun _v#5716 ->
    (fun _s#5717 -> (fun _t#5718 -> ((poly_stub_61)@(L(unit)))))))[@inline] in
let toto#578 = L(44) in
let balance#579 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#580 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#581 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#582 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#583 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#584 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#585 = SELF_ADDRESS()[@inline] in
let chain_id#586 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#587 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#588 =
  fun _u#5730 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#589 =
  fun _u#5732 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#590 = fun _u#5734 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#591 =
  fun _u#5736 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#592 =
  fun _u#5738 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#593 = fun _u#5740 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#594 = fun _u#5742 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#595 =
  fun _u#5744 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#596 =
  fun _u#5746 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#597 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#598 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#599 =
  fun kh#5750 -> (({ VOTING_POWER })@(kh#5750))[@inline] in
let implicit_account#601 =
  fun kh#5754 -> (IMPLICIT_ACCOUNT(kh#5754))[@inline] in
let pairing_check#605 =
  fun l#5762 -> (({ PAIRING_CHECK })@(l#5762))[@inline] in
let set_delegate#607 = fun o#5766 -> (SET_DELEGATE(o#5766))[@inline] in
let open_chest#613 =
  fun ck#5782 ->
  (fun c#5783 -> (fun n#5784 -> (OPEN_CHEST(ck#5782 , c#5783 , n#5784))))[@inline] in
let xor#616 =
  fun l#5793 -> (fun r#5794 -> (XOR(l#5793 , r#5794)))[@inline] in
let shift_left#617 =
  fun l#5796 -> (fun r#5797 -> (LSL(l#5796 , r#5797)))[@inline] in
let shift_right#618 =
  fun l#5799 -> (fun r#5800 -> (LSR(l#5799 , r#5800)))[@inline] in
let length#659 = fun b#5930 -> (({ SIZE })@(b#5930))[@inline] in
let concat#660 =
  fun b1#5932 ->
  (fun b2#5933 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5932 , b2#5933))))[@inline] in
let sub#661 =
  fun s#5935 ->
  (fun l#5936 ->
   (fun b#5937 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5935 ,
                                                                   l#5936) ,
                                                              b#5937)))))[@inline] in
let length#666 = fun b#5948 -> (({ SIZE })@(b#5948))[@inline] in
let concat#667 =
  fun b1#5950 ->
  (fun b2#5951 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#5950 , b2#5951))))[@inline] in
let sub#668 =
  fun s#5953 ->
  (fun l#5954 ->
   (fun b#5955 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#5953 ,
                                                                   l#5954) ,
                                                              b#5955)))))[@inline] in
let blake2b#669 = fun b#5957 -> (({ BLAKE2B })@(b#5957))[@inline] in
let sha256#670 = fun b#5959 -> (({ SHA256 })@(b#5959))[@inline] in
let sha512#671 = fun b#5961 -> (({ SHA512 })@(b#5961))[@inline] in
let sha3#672 = fun b#5963 -> (({ SHA3 })@(b#5963))[@inline] in
let keccak#673 = fun b#5965 -> (({ KECCAK })@(b#5965))[@inline] in
let hash_key#674 = fun k#5967 -> (({ HASH_KEY })@(k#5967))[@inline] in
let check#675 =
  fun k#5969 ->
  (fun s#5970 ->
   (fun b#5971 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#5969 , s#5970) ,
                                                   b#5971)))))[@inline] in
let assert#676 =
  fun b#5973 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#5973))[@inline] in
let abs#679 = fun i#5979 -> (({ ABS })@(i#5979))[@inline] in
let is_nat#680 = fun i#5981 -> (({ ISNAT })@(i#5981))[@inline] in
let true#681 = TRUE()[@inline] in
let false#682 = FALSE()[@inline] in
let unit#683 = UNIT()[@inline] in
let assert_with_error#686 =
  fun b#5989 ->
  (fun s#5990 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#5989 , s#5990))))[@inline] in
let poly_stub_60 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_59 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_58 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_57 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_56 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_55 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_54 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_53 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_52 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_51 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_50 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_49 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_48 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_47 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let poly_stub_46 = fun x#6001 -> (({ FAILWITH })@(x#6001))[@inline] in
let get_total_voting_power#694 = (poly_stub_54)@(L(unit))[@inline] in
let set_source#697 = fun _a#6015 -> ((poly_stub_47)@(L(unit)))[@inline] in
let get_storage_of_address#698 =
  fun _a#6017 -> ((poly_stub_47)@(L(unit)))[@inline] in
let get_balance#699 = fun _a#6019 -> ((poly_stub_60)@(L(unit)))[@inline] in
let print#700 = fun _v#6021 -> ((poly_stub_47)@(L(unit)))[@inline] in
let eprint#701 = fun _v#6023 -> ((poly_stub_47)@(L(unit)))[@inline] in
let get_voting_power#702 =
  fun _kh#6025 -> ((poly_stub_54)@(L(unit)))[@inline] in
let nth_bootstrap_contract#703 =
  fun _i#6027 -> ((poly_stub_48)@(L(unit)))[@inline] in
let nth_bootstrap_account#704 =
  fun _i#6029 -> ((poly_stub_48)@(L(unit)))[@inline] in
let get_bootstrap_account#705 =
  fun _n#6031 -> ((poly_stub_59)@(L(unit)))[@inline] in
let last_originations#707 =
  fun _u#6035 -> ((poly_stub_58)@(L(unit)))[@inline] in
let new_account#709 = fun _u#6039 -> ((poly_stub_57)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#711 =
  fun _n#6043 -> ((poly_stub_47)@(L(unit)))[@inline] in
let register_delegate#713 =
  fun _kh#6047 -> ((poly_stub_47)@(L(unit)))[@inline] in
let register_constant#714 =
  fun _m#6049 -> ((poly_stub_56)@(L(unit)))[@inline] in
let constant_to_michelson_program#716 =
  fun _s#6053 -> ((poly_stub_47)@(L(unit)))[@inline] in
let restore_context#717 =
  fun _u#6055 -> ((poly_stub_47)@(L(unit)))[@inline] in
let save_context#718 = fun _u#6057 -> ((poly_stub_47)@(L(unit)))[@inline] in
let drop_context#719 = fun _u#6059 -> ((poly_stub_47)@(L(unit)))[@inline] in
let set_baker_policy#722 =
  fun _bp#6065 -> ((poly_stub_47)@(L(unit)))[@inline] in
let set_baker#723 = fun _a#6067 -> ((poly_stub_47)@(L(unit)))[@inline] in
let size#724 = fun _c#6069 -> ((poly_stub_55)@(L(unit)))[@inline] in
let read_contract_from_file#726 =
  fun _fn#6073 -> ((poly_stub_47)@(L(unit)))[@inline] in
let chr#727 = fun _n#6075 -> ((poly_stub_53)@(L(unit)))[@inline] in
let nl#728 = L("NEWLINE")[@inline] in
let println#729 = fun _v#6078 -> ((poly_stub_47)@(L(unit)))[@inline] in
let transfer#730 =
  fun _a#6080 -> (fun _s#6081 -> (fun _t#6082 -> ((poly_stub_47)@(L(unit)))))[@inline] in
let transfer_exn#731 =
  fun _a#6084 -> (fun _s#6085 -> (fun _t#6086 -> ((poly_stub_54)@(L(unit)))))[@inline] in
let reset_state#733 =
  fun _n#6090 -> (fun _l#6091 -> ((poly_stub_47)@(L(unit))))[@inline] in
let reset_state_at#734 =
  fun _t#6093 -> (fun _n#6094 -> (fun _l#6095 -> ((poly_stub_47)@(L(unit)))))[@inline] in
let save_mutation#737 =
  fun _s#6104 -> (fun _m#6105 -> ((poly_stub_53)@(L(unit))))[@inline] in
let sign#740 =
  fun _sk#6113 -> (fun _d#6114 -> ((poly_stub_52)@(L(unit))))[@inline] in
let add_account#741 =
  fun _s#6116 -> (fun _k#6117 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#742 =
  fun _p#6119 -> (fun _o#6120 -> ((poly_stub_47)@(L(unit))))[@inline] in
let create_chest#744 =
  fun _b#6125 -> (fun _n#6126 -> ((poly_stub_51)@(L(unit))))[@inline] in
let create_chest_key#745 =
  fun _c#6128 -> (fun _n#6129 -> ((poly_stub_50)@(L(unit))))[@inline] in
let michelson_equal#748 =
  fun _m1#6139 -> (fun _m2#6140 -> ((poly_stub_49)@(L(unit))))[@inline] in
let originate_contract#750 =
  fun _c#6145 -> (fun _s#6146 -> (fun _t#6147 -> ((poly_stub_48)@(L(unit)))))[@inline] in
let compile_contract_from_file#752 =
  fun _fn#6153 ->
  (fun _e#6154 -> (fun _v#6155 -> ((poly_stub_47)@(L(unit)))))[@inline] in
let originate_from_file#753 =
  fun _fn#6157 ->
  (fun _e#6158 ->
   (fun _v#6159 ->
    (fun _s#6160 -> (fun _t#6161 -> ((poly_stub_46)@(L(unit)))))))[@inline] in
let toto#754 = L(43) in
let balance#755 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#756 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#757 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#758 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#759 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#760 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#761 = SELF_ADDRESS()[@inline] in
let chain_id#762 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#763 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#764 =
  fun _u#6173 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#765 =
  fun _u#6175 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#766 = fun _u#6177 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#767 =
  fun _u#6179 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#768 =
  fun _u#6181 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#769 = fun _u#6183 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#770 = fun _u#6185 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#771 =
  fun _u#6187 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#772 =
  fun _u#6189 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#773 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#774 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#775 =
  fun kh#6193 -> (({ VOTING_POWER })@(kh#6193))[@inline] in
let implicit_account#777 =
  fun kh#6197 -> (IMPLICIT_ACCOUNT(kh#6197))[@inline] in
let pairing_check#781 =
  fun l#6205 -> (({ PAIRING_CHECK })@(l#6205))[@inline] in
let set_delegate#783 = fun o#6209 -> (SET_DELEGATE(o#6209))[@inline] in
let open_chest#789 =
  fun ck#6225 ->
  (fun c#6226 -> (fun n#6227 -> (OPEN_CHEST(ck#6225 , c#6226 , n#6227))))[@inline] in
let xor#792 =
  fun l#6236 -> (fun r#6237 -> (XOR(l#6236 , r#6237)))[@inline] in
let shift_left#793 =
  fun l#6239 -> (fun r#6240 -> (LSL(l#6239 , r#6240)))[@inline] in
let shift_right#794 =
  fun l#6242 -> (fun r#6243 -> (LSR(l#6242 , r#6243)))[@inline] in
let length#835 = fun b#6373 -> (({ SIZE })@(b#6373))[@inline] in
let concat#836 =
  fun b1#6375 ->
  (fun b2#6376 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6375 , b2#6376))))[@inline] in
let sub#837 =
  fun s#6378 ->
  (fun l#6379 ->
   (fun b#6380 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6378 ,
                                                                   l#6379) ,
                                                              b#6380)))))[@inline] in
let length#842 = fun b#6391 -> (({ SIZE })@(b#6391))[@inline] in
let concat#843 =
  fun b1#6393 ->
  (fun b2#6394 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6393 , b2#6394))))[@inline] in
let sub#844 =
  fun s#6396 ->
  (fun l#6397 ->
   (fun b#6398 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6396 ,
                                                                   l#6397) ,
                                                              b#6398)))))[@inline] in
let blake2b#845 = fun b#6400 -> (({ BLAKE2B })@(b#6400))[@inline] in
let sha256#846 = fun b#6402 -> (({ SHA256 })@(b#6402))[@inline] in
let sha512#847 = fun b#6404 -> (({ SHA512 })@(b#6404))[@inline] in
let sha3#848 = fun b#6406 -> (({ SHA3 })@(b#6406))[@inline] in
let keccak#849 = fun b#6408 -> (({ KECCAK })@(b#6408))[@inline] in
let hash_key#850 = fun k#6410 -> (({ HASH_KEY })@(k#6410))[@inline] in
let check#851 =
  fun k#6412 ->
  (fun s#6413 ->
   (fun b#6414 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6412 , s#6413) ,
                                                   b#6414)))))[@inline] in
let assert#852 =
  fun b#6416 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6416))[@inline] in
let abs#855 = fun i#6422 -> (({ ABS })@(i#6422))[@inline] in
let is_nat#856 = fun i#6424 -> (({ ISNAT })@(i#6424))[@inline] in
let true#857 = TRUE()[@inline] in
let false#858 = FALSE()[@inline] in
let unit#859 = UNIT()[@inline] in
let assert_with_error#862 =
  fun b#6432 ->
  (fun s#6433 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6432 , s#6433))))[@inline] in
let poly_stub_45 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_44 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_43 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_42 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_41 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_40 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_39 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_38 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_37 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_36 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_35 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_34 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_33 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_32 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let poly_stub_31 = fun x#6444 -> (({ FAILWITH })@(x#6444))[@inline] in
let get_total_voting_power#870 = (poly_stub_39)@(L(unit))[@inline] in
let set_source#873 = fun _a#6458 -> ((poly_stub_32)@(L(unit)))[@inline] in
let get_storage_of_address#874 =
  fun _a#6460 -> ((poly_stub_32)@(L(unit)))[@inline] in
let get_balance#875 = fun _a#6462 -> ((poly_stub_45)@(L(unit)))[@inline] in
let print#876 = fun _v#6464 -> ((poly_stub_32)@(L(unit)))[@inline] in
let eprint#877 = fun _v#6466 -> ((poly_stub_32)@(L(unit)))[@inline] in
let get_voting_power#878 =
  fun _kh#6468 -> ((poly_stub_39)@(L(unit)))[@inline] in
let nth_bootstrap_contract#879 =
  fun _i#6470 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_account#880 =
  fun _i#6472 -> ((poly_stub_33)@(L(unit)))[@inline] in
let get_bootstrap_account#881 =
  fun _n#6474 -> ((poly_stub_44)@(L(unit)))[@inline] in
let last_originations#883 =
  fun _u#6478 -> ((poly_stub_43)@(L(unit)))[@inline] in
let new_account#885 = fun _u#6482 -> ((poly_stub_42)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#887 =
  fun _n#6486 -> ((poly_stub_32)@(L(unit)))[@inline] in
let register_delegate#889 =
  fun _kh#6490 -> ((poly_stub_32)@(L(unit)))[@inline] in
let register_constant#890 =
  fun _m#6492 -> ((poly_stub_41)@(L(unit)))[@inline] in
let constant_to_michelson_program#892 =
  fun _s#6496 -> ((poly_stub_32)@(L(unit)))[@inline] in
let restore_context#893 =
  fun _u#6498 -> ((poly_stub_32)@(L(unit)))[@inline] in
let save_context#894 = fun _u#6500 -> ((poly_stub_32)@(L(unit)))[@inline] in
let drop_context#895 = fun _u#6502 -> ((poly_stub_32)@(L(unit)))[@inline] in
let set_baker_policy#898 =
  fun _bp#6508 -> ((poly_stub_32)@(L(unit)))[@inline] in
let set_baker#899 = fun _a#6510 -> ((poly_stub_32)@(L(unit)))[@inline] in
let size#900 = fun _c#6512 -> ((poly_stub_40)@(L(unit)))[@inline] in
let read_contract_from_file#902 =
  fun _fn#6516 -> ((poly_stub_32)@(L(unit)))[@inline] in
let chr#903 = fun _n#6518 -> ((poly_stub_38)@(L(unit)))[@inline] in
let nl#904 = L("NEWLINE")[@inline] in
let println#905 = fun _v#6521 -> ((poly_stub_32)@(L(unit)))[@inline] in
let transfer#906 =
  fun _a#6523 -> (fun _s#6524 -> (fun _t#6525 -> ((poly_stub_32)@(L(unit)))))[@inline] in
let transfer_exn#907 =
  fun _a#6527 -> (fun _s#6528 -> (fun _t#6529 -> ((poly_stub_39)@(L(unit)))))[@inline] in
let reset_state#909 =
  fun _n#6533 -> (fun _l#6534 -> ((poly_stub_32)@(L(unit))))[@inline] in
let reset_state_at#910 =
  fun _t#6536 -> (fun _n#6537 -> (fun _l#6538 -> ((poly_stub_32)@(L(unit)))))[@inline] in
let save_mutation#913 =
  fun _s#6547 -> (fun _m#6548 -> ((poly_stub_38)@(L(unit))))[@inline] in
let sign#916 =
  fun _sk#6556 -> (fun _d#6557 -> ((poly_stub_37)@(L(unit))))[@inline] in
let add_account#917 =
  fun _s#6559 -> (fun _k#6560 -> ((poly_stub_32)@(L(unit))))[@inline] in
let baker_account#918 =
  fun _p#6562 -> (fun _o#6563 -> ((poly_stub_32)@(L(unit))))[@inline] in
let create_chest#920 =
  fun _b#6568 -> (fun _n#6569 -> ((poly_stub_36)@(L(unit))))[@inline] in
let create_chest_key#921 =
  fun _c#6571 -> (fun _n#6572 -> ((poly_stub_35)@(L(unit))))[@inline] in
let michelson_equal#924 =
  fun _m1#6582 -> (fun _m2#6583 -> ((poly_stub_34)@(L(unit))))[@inline] in
let originate_contract#926 =
  fun _c#6588 -> (fun _s#6589 -> (fun _t#6590 -> ((poly_stub_33)@(L(unit)))))[@inline] in
let compile_contract_from_file#928 =
  fun _fn#6596 ->
  (fun _e#6597 -> (fun _v#6598 -> ((poly_stub_32)@(L(unit)))))[@inline] in
let originate_from_file#929 =
  fun _fn#6600 ->
  (fun _e#6601 ->
   (fun _v#6602 ->
    (fun _s#6603 -> (fun _t#6604 -> ((poly_stub_31)@(L(unit)))))))[@inline] in
let tata#930 = ADD(toto#224 , titi#401) in
let foo#931 = (f#402)@(PAIR(L(unit) , L(3))) in
let balance#932 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#933 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#934 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#935 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#936 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#937 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#938 = SELF_ADDRESS()[@inline] in
let chain_id#939 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#940 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#941 =
  fun _u#6617 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#942 =
  fun _u#6619 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#943 = fun _u#6621 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#944 =
  fun _u#6623 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#945 =
  fun _u#6625 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#946 = fun _u#6627 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#947 = fun _u#6629 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#948 =
  fun _u#6631 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#949 =
  fun _u#6633 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#950 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#951 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#952 =
  fun kh#6637 -> (({ VOTING_POWER })@(kh#6637))[@inline] in
let implicit_account#954 =
  fun kh#6641 -> (IMPLICIT_ACCOUNT(kh#6641))[@inline] in
let pairing_check#958 =
  fun l#6649 -> (({ PAIRING_CHECK })@(l#6649))[@inline] in
let set_delegate#960 = fun o#6653 -> (SET_DELEGATE(o#6653))[@inline] in
let open_chest#966 =
  fun ck#6669 ->
  (fun c#6670 -> (fun n#6671 -> (OPEN_CHEST(ck#6669 , c#6670 , n#6671))))[@inline] in
let xor#969 =
  fun l#6680 -> (fun r#6681 -> (XOR(l#6680 , r#6681)))[@inline] in
let shift_left#970 =
  fun l#6683 -> (fun r#6684 -> (LSL(l#6683 , r#6684)))[@inline] in
let shift_right#971 =
  fun l#6686 -> (fun r#6687 -> (LSR(l#6686 , r#6687)))[@inline] in
let length#1012 = fun b#6817 -> (({ SIZE })@(b#6817))[@inline] in
let concat#1013 =
  fun b1#6819 ->
  (fun b2#6820 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6819 , b2#6820))))[@inline] in
let sub#1014 =
  fun s#6822 ->
  (fun l#6823 ->
   (fun b#6824 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6822 ,
                                                                   l#6823) ,
                                                              b#6824)))))[@inline] in
let length#1019 = fun b#6835 -> (({ SIZE })@(b#6835))[@inline] in
let concat#1020 =
  fun b1#6837 ->
  (fun b2#6838 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#6837 , b2#6838))))[@inline] in
let sub#1021 =
  fun s#6840 ->
  (fun l#6841 ->
   (fun b#6842 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#6840 ,
                                                                   l#6841) ,
                                                              b#6842)))))[@inline] in
let blake2b#1022 = fun b#6844 -> (({ BLAKE2B })@(b#6844))[@inline] in
let sha256#1023 = fun b#6846 -> (({ SHA256 })@(b#6846))[@inline] in
let sha512#1024 = fun b#6848 -> (({ SHA512 })@(b#6848))[@inline] in
let sha3#1025 = fun b#6850 -> (({ SHA3 })@(b#6850))[@inline] in
let keccak#1026 = fun b#6852 -> (({ KECCAK })@(b#6852))[@inline] in
let hash_key#1027 = fun k#6854 -> (({ HASH_KEY })@(k#6854))[@inline] in
let check#1028 =
  fun k#6856 ->
  (fun s#6857 ->
   (fun b#6858 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#6856 , s#6857) ,
                                                   b#6858)))))[@inline] in
let assert#1029 =
  fun b#6860 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#6860))[@inline] in
let abs#1032 = fun i#6866 -> (({ ABS })@(i#6866))[@inline] in
let is_nat#1033 = fun i#6868 -> (({ ISNAT })@(i#6868))[@inline] in
let true#1034 = TRUE()[@inline] in
let false#1035 = FALSE()[@inline] in
let unit#1036 = UNIT()[@inline] in
let assert_with_error#1039 =
  fun b#6876 ->
  (fun s#6877 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#6876 , s#6877))))[@inline] in
let poly_stub_30 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_29 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_28 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_27 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_26 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_25 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_24 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_23 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_22 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_21 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_20 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_19 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_18 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_17 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let poly_stub_16 = fun x#6888 -> (({ FAILWITH })@(x#6888))[@inline] in
let get_total_voting_power#1047 = (poly_stub_24)@(L(unit))[@inline] in
let set_source#1050 = fun _a#6902 -> ((poly_stub_17)@(L(unit)))[@inline] in
let get_storage_of_address#1051 =
  fun _a#6904 -> ((poly_stub_17)@(L(unit)))[@inline] in
let get_balance#1052 = fun _a#6906 -> ((poly_stub_30)@(L(unit)))[@inline] in
let print#1053 = fun _v#6908 -> ((poly_stub_17)@(L(unit)))[@inline] in
let eprint#1054 = fun _v#6910 -> ((poly_stub_17)@(L(unit)))[@inline] in
let get_voting_power#1055 =
  fun _kh#6912 -> ((poly_stub_24)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1056 =
  fun _i#6914 -> ((poly_stub_18)@(L(unit)))[@inline] in
let nth_bootstrap_account#1057 =
  fun _i#6916 -> ((poly_stub_18)@(L(unit)))[@inline] in
let get_bootstrap_account#1058 =
  fun _n#6918 -> ((poly_stub_29)@(L(unit)))[@inline] in
let last_originations#1060 =
  fun _u#6922 -> ((poly_stub_28)@(L(unit)))[@inline] in
let new_account#1062 = fun _u#6926 -> ((poly_stub_27)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1064 =
  fun _n#6930 -> ((poly_stub_17)@(L(unit)))[@inline] in
let register_delegate#1066 =
  fun _kh#6934 -> ((poly_stub_17)@(L(unit)))[@inline] in
let register_constant#1067 =
  fun _m#6936 -> ((poly_stub_26)@(L(unit)))[@inline] in
let constant_to_michelson_program#1069 =
  fun _s#6940 -> ((poly_stub_17)@(L(unit)))[@inline] in
let restore_context#1070 =
  fun _u#6942 -> ((poly_stub_17)@(L(unit)))[@inline] in
let save_context#1071 = fun _u#6944 -> ((poly_stub_17)@(L(unit)))[@inline] in
let drop_context#1072 = fun _u#6946 -> ((poly_stub_17)@(L(unit)))[@inline] in
let set_baker_policy#1075 =
  fun _bp#6952 -> ((poly_stub_17)@(L(unit)))[@inline] in
let set_baker#1076 = fun _a#6954 -> ((poly_stub_17)@(L(unit)))[@inline] in
let size#1077 = fun _c#6956 -> ((poly_stub_25)@(L(unit)))[@inline] in
let read_contract_from_file#1079 =
  fun _fn#6960 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1080 = fun _n#6962 -> ((poly_stub_23)@(L(unit)))[@inline] in
let nl#1081 = L("NEWLINE")[@inline] in
let println#1082 = fun _v#6965 -> ((poly_stub_17)@(L(unit)))[@inline] in
let transfer#1083 =
  fun _a#6967 -> (fun _s#6968 -> (fun _t#6969 -> ((poly_stub_17)@(L(unit)))))[@inline] in
let transfer_exn#1084 =
  fun _a#6971 -> (fun _s#6972 -> (fun _t#6973 -> ((poly_stub_24)@(L(unit)))))[@inline] in
let reset_state#1086 =
  fun _n#6977 -> (fun _l#6978 -> ((poly_stub_17)@(L(unit))))[@inline] in
let reset_state_at#1087 =
  fun _t#6980 -> (fun _n#6981 -> (fun _l#6982 -> ((poly_stub_17)@(L(unit)))))[@inline] in
let save_mutation#1090 =
  fun _s#6991 -> (fun _m#6992 -> ((poly_stub_23)@(L(unit))))[@inline] in
let sign#1093 =
  fun _sk#7000 -> (fun _d#7001 -> ((poly_stub_22)@(L(unit))))[@inline] in
let add_account#1094 =
  fun _s#7003 -> (fun _k#7004 -> ((poly_stub_17)@(L(unit))))[@inline] in
let baker_account#1095 =
  fun _p#7006 -> (fun _o#7007 -> ((poly_stub_17)@(L(unit))))[@inline] in
let create_chest#1097 =
  fun _b#7012 -> (fun _n#7013 -> ((poly_stub_21)@(L(unit))))[@inline] in
let create_chest_key#1098 =
  fun _c#7015 -> (fun _n#7016 -> ((poly_stub_20)@(L(unit))))[@inline] in
let michelson_equal#1101 =
  fun _m1#7026 -> (fun _m2#7027 -> ((poly_stub_19)@(L(unit))))[@inline] in
let originate_contract#1103 =
  fun _c#7032 -> (fun _s#7033 -> (fun _t#7034 -> ((poly_stub_18)@(L(unit)))))[@inline] in
let compile_contract_from_file#1105 =
  fun _fn#7040 ->
  (fun _e#7041 -> (fun _v#7042 -> ((poly_stub_17)@(L(unit)))))[@inline] in
let originate_from_file#1106 =
  fun _fn#7044 ->
  (fun _e#7045 ->
   (fun _v#7046 ->
    (fun _s#7047 -> (fun _t#7048 -> ((poly_stub_16)@(L(unit)))))))[@inline] in
let toto#1107 = L(10) in
let foo#1108 = L("bar") in
let balance#1109 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#1110 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#1111 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#1112 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#1113 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#1114 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#1115 = SELF_ADDRESS()[@inline] in
let chain_id#1116 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#1117 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#1118 =
  fun _u#7061 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1119 =
  fun _u#7063 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1120 = fun _u#7065 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1121 =
  fun _u#7067 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1122 =
  fun _u#7069 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1123 =
  fun _u#7071 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1124 = fun _u#7073 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1125 =
  fun _u#7075 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1126 =
  fun _u#7077 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#1127 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#1128 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#1129 =
  fun kh#7081 -> (({ VOTING_POWER })@(kh#7081))[@inline] in
let implicit_account#1131 =
  fun kh#7085 -> (IMPLICIT_ACCOUNT(kh#7085))[@inline] in
let pairing_check#1135 =
  fun l#7093 -> (({ PAIRING_CHECK })@(l#7093))[@inline] in
let set_delegate#1137 = fun o#7097 -> (SET_DELEGATE(o#7097))[@inline] in
let open_chest#1143 =
  fun ck#7113 ->
  (fun c#7114 -> (fun n#7115 -> (OPEN_CHEST(ck#7113 , c#7114 , n#7115))))[@inline] in
let xor#1146 =
  fun l#7124 -> (fun r#7125 -> (XOR(l#7124 , r#7125)))[@inline] in
let shift_left#1147 =
  fun l#7127 -> (fun r#7128 -> (LSL(l#7127 , r#7128)))[@inline] in
let shift_right#1148 =
  fun l#7130 -> (fun r#7131 -> (LSR(l#7130 , r#7131)))[@inline] in
let length#1189 = fun b#7261 -> (({ SIZE })@(b#7261))[@inline] in
let concat#1190 =
  fun b1#7263 ->
  (fun b2#7264 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7263 , b2#7264))))[@inline] in
let sub#1191 =
  fun s#7266 ->
  (fun l#7267 ->
   (fun b#7268 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7266 ,
                                                                   l#7267) ,
                                                              b#7268)))))[@inline] in
let length#1196 = fun b#7279 -> (({ SIZE })@(b#7279))[@inline] in
let concat#1197 =
  fun b1#7281 ->
  (fun b2#7282 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#7281 , b2#7282))))[@inline] in
let sub#1198 =
  fun s#7284 ->
  (fun l#7285 ->
   (fun b#7286 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#7284 ,
                                                                   l#7285) ,
                                                              b#7286)))))[@inline] in
let blake2b#1199 = fun b#7288 -> (({ BLAKE2B })@(b#7288))[@inline] in
let sha256#1200 = fun b#7290 -> (({ SHA256 })@(b#7290))[@inline] in
let sha512#1201 = fun b#7292 -> (({ SHA512 })@(b#7292))[@inline] in
let sha3#1202 = fun b#7294 -> (({ SHA3 })@(b#7294))[@inline] in
let keccak#1203 = fun b#7296 -> (({ KECCAK })@(b#7296))[@inline] in
let hash_key#1204 = fun k#7298 -> (({ HASH_KEY })@(k#7298))[@inline] in
let check#1205 =
  fun k#7300 ->
  (fun s#7301 ->
   (fun b#7302 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#7300 , s#7301) ,
                                                   b#7302)))))[@inline] in
let assert =
  fun b#7304 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#7304))[@inline] in
let abs = fun i#7310 -> (({ ABS })@(i#7310))[@inline] in
let is_nat = fun i#7312 -> (({ ISNAT })@(i#7312))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#7320 ->
  (fun s#7321 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#7320 , s#7321))))[@inline] in
let poly_stub_15 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_14 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_13 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_12 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_11 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_10 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_9 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_8 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_7 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_6 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_5 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_4 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_3 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_2 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let poly_stub_1 = fun x#7332 -> (({ FAILWITH })@(x#7332))[@inline] in
let get_total_voting_power#1210 = (poly_stub_9)@(L(unit))[@inline] in
let set_source#1213 = fun _a#7346 -> ((poly_stub_2)@(L(unit)))[@inline] in
let get_storage_of_address#1214 =
  fun _a#7348 -> ((poly_stub_2)@(L(unit)))[@inline] in
let get_balance#1215 = fun _a#7350 -> ((poly_stub_15)@(L(unit)))[@inline] in
let print#1216 = fun _v#7352 -> ((poly_stub_2)@(L(unit)))[@inline] in
let eprint#1217 = fun _v#7354 -> ((poly_stub_2)@(L(unit)))[@inline] in
let get_voting_power#1218 =
  fun _kh#7356 -> ((poly_stub_9)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1219 =
  fun _i#7358 -> ((poly_stub_3)@(L(unit)))[@inline] in
let nth_bootstrap_account#1220 =
  fun _i#7360 -> ((poly_stub_3)@(L(unit)))[@inline] in
let get_bootstrap_account#1221 =
  fun _n#7362 -> ((poly_stub_14)@(L(unit)))[@inline] in
let last_originations#1223 =
  fun _u#7366 -> ((poly_stub_13)@(L(unit)))[@inline] in
let new_account#1225 = fun _u#7370 -> ((poly_stub_12)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1227 =
  fun _n#7374 -> ((poly_stub_2)@(L(unit)))[@inline] in
let register_delegate#1229 =
  fun _kh#7378 -> ((poly_stub_2)@(L(unit)))[@inline] in
let register_constant#1230 =
  fun _m#7380 -> ((poly_stub_11)@(L(unit)))[@inline] in
let constant_to_michelson_program#1232 =
  fun _s#7384 -> ((poly_stub_2)@(L(unit)))[@inline] in
let restore_context#1233 =
  fun _u#7386 -> ((poly_stub_2)@(L(unit)))[@inline] in
let save_context#1234 = fun _u#7388 -> ((poly_stub_2)@(L(unit)))[@inline] in
let drop_context#1235 = fun _u#7390 -> ((poly_stub_2)@(L(unit)))[@inline] in
let set_baker_policy#1238 =
  fun _bp#7396 -> ((poly_stub_2)@(L(unit)))[@inline] in
let set_baker#1239 = fun _a#7398 -> ((poly_stub_2)@(L(unit)))[@inline] in
let size#1240 = fun _c#7400 -> ((poly_stub_10)@(L(unit)))[@inline] in
let read_contract_from_file#1242 =
  fun _fn#7404 -> ((poly_stub_2)@(L(unit)))[@inline] in
let chr#1243 = fun _n#7406 -> ((poly_stub_8)@(L(unit)))[@inline] in
let nl#1244 = L("NEWLINE")[@inline] in
let println#1245 = fun _v#7409 -> ((poly_stub_2)@(L(unit)))[@inline] in
let transfer#1246 =
  fun _a#7411 -> (fun _s#7412 -> (fun _t#7413 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let transfer_exn#1247 =
  fun _a#7415 -> (fun _s#7416 -> (fun _t#7417 -> ((poly_stub_9)@(L(unit)))))[@inline] in
let reset_state#1249 =
  fun _n#7421 -> (fun _l#7422 -> ((poly_stub_2)@(L(unit))))[@inline] in
let reset_state_at#1250 =
  fun _t#7424 -> (fun _n#7425 -> (fun _l#7426 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let save_mutation#1253 =
  fun _s#7435 -> (fun _m#7436 -> ((poly_stub_8)@(L(unit))))[@inline] in
let sign#1256 =
  fun _sk#7444 -> (fun _d#7445 -> ((poly_stub_7)@(L(unit))))[@inline] in
let add_account#1257 =
  fun _s#7447 -> (fun _k#7448 -> ((poly_stub_2)@(L(unit))))[@inline] in
let baker_account#1258 =
  fun _p#7450 -> (fun _o#7451 -> ((poly_stub_2)@(L(unit))))[@inline] in
let create_chest#1260 =
  fun _b#7456 -> (fun _n#7457 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#1261 =
  fun _c#7459 -> (fun _n#7460 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#1264 =
  fun _m1#7470 -> (fun _m2#7471 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#1266 =
  fun _c#7476 -> (fun _s#7477 -> (fun _t#7478 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#1268 =
  fun _fn#7484 -> (fun _e#7485 -> (fun _v#7486 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#1269 =
  fun _fn#7488 ->
  (fun _e#7489 ->
   (fun _v#7490 ->
    (fun _s#7491 -> (fun _t#7492 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1107 , toto#224) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#7496 ->
  (let (gen#7502, gen#7503) = gen#7496 in
   let p#7497 = gen#7502 in
   let s#7498 = gen#7503 in
   let s#7499 = ADD(ADD(p#7497 , s#7498) , toto) in
   PAIR(LIST_EMPTY() , s#7499)) in
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
