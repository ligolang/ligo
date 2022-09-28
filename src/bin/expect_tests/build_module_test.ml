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
  [%expect {xxx|
    module C =
      Mangled_module_____________________test__contracts__build__C____mligo.
    module E =
      Mangled_module_____________________test__contracts__build__E____mligo.
    const toto : int = ADD(E.toto , C.B.A.toto)
    const fb : record[tata -> int , tete -> int , titi -> int , toto -> int] =
      record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main : ( int * int ) -> ( list (operation) * int ) = lambda (
      gen#4( int * int ))( list (operation) * int ) return  match gen#4 with
                                                             | ( p : int , s : int ) ->
                                                             let s : int =
                                                               ADD(ADD(p , s) ,
                                                                   toto) in
                                                             ( LIST_EMPTY() , s ) |xxx}]

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
  [%expect {xxx|
    module Errors =
      Mangled_module_____________________test__contracts__build__instance____________common__errors____mligo.
    module Storage =
      Mangled_module_____________________test__contracts__build__instance____________common__storage____mligo.
    const main : ( unit * string ) -> ( list (operation) * string ) = lambda (
      gen#2( unit * string ))( list (operation) * string ) return  match
                                                                    gen#2 with
                                                                    | ( _#4 : unit , _#3 : string ) ->
                                                                    ( LIST_EMPTY
                                                                      () ,
                                                                      CONCAT
                                                                      (Errors.undefined_token ,
                                                                       Storage.s) ) |xxx}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let get_balance#108 =
  fun _u#479 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#109 =
  fun _u#481 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#110 = fun _u#483 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#111 =
  fun _u#485 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#112 =
  fun _u#487 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#113 = fun _u#489 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#114 = fun _u#491 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#115 =
  fun _u#493 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#116 =
  fun _u#495 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#117 =
  fun _u#497 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#118 =
  fun kh#499 -> (({ VOTING_POWER })@(kh#499))[@inline] in
let implicit_account#120 =
  fun kh#503 -> (IMPLICIT_ACCOUNT(kh#503))[@inline] in
let pairing_check#124 =
  fun l#511 -> (({ PAIRING_CHECK })@(l#511))[@inline] in
let set_delegate#126 = fun o#515 -> (SET_DELEGATE(o#515))[@inline] in
let open_chest#134 =
  fun ck#536 ->
  (fun c#537 -> (fun n#538 -> (OPEN_CHEST(ck#536 , c#537 , n#538))))[@inline] in
let xor#143 = fun l#571 -> (fun r#572 -> (XOR(l#571 , r#572)))[@inline] in
let or#144 = fun l#574 -> (fun r#575 -> (OR(l#574 , r#575)))[@inline] in
let shift_left#145 =
  fun l#577 -> (fun r#578 -> (LSL(l#577 , r#578)))[@inline] in
let shift_right#146 =
  fun l#580 -> (fun r#581 -> (LSR(l#580 , r#581)))[@inline] in
let length#191 = fun b#724 -> (({ SIZE })@(b#724))[@inline] in
let concat#192 =
  fun b1#726 ->
  (fun b2#727 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#726 , b2#727))))[@inline] in
let sub#193 =
  fun s#729 ->
  (fun l#730 ->
   (fun b#731 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#729 ,
                                                                   l#730) ,
                                                              b#731)))))[@inline] in
let length#199 = fun b#745 -> (({ SIZE })@(b#745))[@inline] in
let concat#200 =
  fun b1#747 ->
  (fun b2#748 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#747 , b2#748))))[@inline] in
let sub#201 =
  fun s#750 ->
  (fun l#751 ->
   (fun b#752 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#750 ,
                                                                   l#751) ,
                                                              b#752)))))[@inline] in
let blake2b#202 = fun b#754 -> (({ BLAKE2B })@(b#754))[@inline] in
let sha256#203 = fun b#756 -> (({ SHA256 })@(b#756))[@inline] in
let sha512#204 = fun b#758 -> (({ SHA512 })@(b#758))[@inline] in
let sha3#205 = fun b#760 -> (({ SHA3 })@(b#760))[@inline] in
let keccak#206 = fun b#762 -> (({ KECCAK })@(b#762))[@inline] in
let hash_key#207 = fun k#764 -> (({ HASH_KEY })@(k#764))[@inline] in
let check#208 =
  fun k#766 ->
  (fun s#767 ->
   (fun b#768 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#766 , s#767) ,
                                                   b#768)))))[@inline] in
let assert#209 =
  fun b#770 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#770))[@inline] in
let abs#212 = fun i#776 -> (({ ABS })@(i#776))[@inline] in
let is_nat#213 = fun i#778 -> (({ ISNAT })@(i#778))[@inline] in
let true#214 = TRUE()[@inline] in
let false#215 = FALSE()[@inline] in
let unit#216 = UNIT()[@inline] in
let assert_with_error#220 =
  fun b#788 ->
  (fun s#789 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#788 , s#789))))[@inline] in
let poly_stub_78 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_77 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_76 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_75 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_74 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_73 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_72 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_71 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_70 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_69 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_68 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_67 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_66 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_65 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_64 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_63 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_62 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_61 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_60 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_59 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_58 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_57 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_56 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_55 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_54 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_53 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_52 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_51 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_50 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_49 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_48 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_47 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_46 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_45 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_44 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_43 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_42 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_41 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let poly_stub_40 = fun x#800 -> (({ FAILWITH })@(x#800))[@inline] in
let get_total_voting_power#228 =
  fun _u#809 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#231 = fun _a#815 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#232 =
  fun _a#817 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#233 = fun _a#819 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#234 = fun _v#821 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#235 = fun _v#823 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#236 =
  fun _kh#825 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#237 =
  fun _i#827 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#238 =
  fun _i#829 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#239 =
  fun _n#831 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#241 =
  fun _u#835 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#243 = fun _u#839 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#245 =
  fun _n#843 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#247 =
  fun _kh#847 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#248 =
  fun _m#849 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#250 =
  fun _s#853 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#251 =
  fun _u#855 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#252 = fun _u#857 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#253 = fun _u#859 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#256 =
  fun _bp#865 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#257 = fun _a#867 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#258 = fun _c#869 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#260 =
  fun _fn#873 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#261 = fun _n#875 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#262 = L("NEWLINE")[@inline] in
let println#263 = fun _v#878 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#264 =
  fun _a#880 -> (fun _s#881 -> (fun _t#882 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#265 =
  fun _a#884 -> (fun _s#885 -> (fun _t#886 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#267 =
  fun _n#890 -> (fun _l#891 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#268 =
  fun _t#893 -> (fun _n#894 -> (fun _l#895 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#271 =
  fun _s#904 -> (fun _m#905 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#274 =
  fun _sk#913 -> (fun _d#914 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#275 =
  fun _s#916 -> (fun _k#917 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#276 =
  fun _p#919 -> (fun _o#920 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#278 =
  fun _b#925 -> (fun _n#926 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#279 =
  fun _c#928 -> (fun _n#929 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#282 =
  fun _m1#939 -> (fun _m2#940 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#284 =
  fun _c#945 -> (fun _s#946 -> (fun _t#947 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#286 =
  fun _fn#953 -> (fun _e#954 -> (fun _v#955 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#287 =
  fun _fn#957 ->
  (fun _e#958 ->
   (fun _v#959 -> (fun _s#960 -> (fun _t#961 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let get_balance#288 =
  fun _u#963 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#289 =
  fun _u#965 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#290 = fun _u#967 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#291 =
  fun _u#969 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#292 =
  fun _u#971 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#293 = fun _u#973 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#294 = fun _u#975 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#295 =
  fun _u#977 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#296 =
  fun _u#979 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#297 =
  fun _u#981 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#298 =
  fun kh#983 -> (({ VOTING_POWER })@(kh#983))[@inline] in
let implicit_account#300 =
  fun kh#987 -> (IMPLICIT_ACCOUNT(kh#987))[@inline] in
let pairing_check#304 =
  fun l#995 -> (({ PAIRING_CHECK })@(l#995))[@inline] in
let set_delegate#306 = fun o#999 -> (SET_DELEGATE(o#999))[@inline] in
let open_chest#314 =
  fun gen#1023 ->
  (let (gen#1565, gen#1566) = gen#1023 in
   let (gen#1567, gen#1568) = gen#1565 in
   let ck#1024 = gen#1567 in
   let c#1025 = gen#1568 in
   let n#1026 = gen#1566 in OPEN_CHEST(ck#1024 , c#1025 , n#1026))[@inline] in
let xor#323 =
  fun gen#1068 ->
  (let (gen#1569, gen#1570) = gen#1068 in
   let l#1069 = gen#1569 in let r#1070 = gen#1570 in XOR(l#1069 , r#1070))[@inline] in
let or#324 =
  fun gen#1072 ->
  (let (gen#1571, gen#1572) = gen#1072 in
   let l#1073 = gen#1571 in let r#1074 = gen#1572 in OR(l#1073 , r#1074))[@inline] in
let shift_left#325 =
  fun gen#1076 ->
  (let (gen#1573, gen#1574) = gen#1076 in
   let l#1077 = gen#1573 in let r#1078 = gen#1574 in LSL(l#1077 , r#1078))[@inline] in
let shift_right#326 =
  fun gen#1080 ->
  (let (gen#1575, gen#1576) = gen#1080 in
   let l#1081 = gen#1575 in let r#1082 = gen#1576 in LSR(l#1081 , r#1082))[@inline] in
let length#371 = fun b#1256 -> (({ SIZE })@(b#1256))[@inline] in
let concat#372 =
  fun gen#1258 ->
  (let (gen#1577, gen#1578) = gen#1258 in
   let b1#1259 = gen#1577 in
   let b2#1260 = gen#1578 in ({ UNPAIR ; CONCAT })@(PAIR(b1#1259 , b2#1260)))[@inline] in
let sub#373 =
  fun gen#1262 ->
  (let (gen#1579, gen#1580) = gen#1262 in
   let (gen#1581, gen#1582) = gen#1579 in
   let s#1263 = gen#1581 in
   let l#1264 = gen#1582 in
   let b#1265 = gen#1580 in
   ({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1263 ,
                                                                 l#1264) ,
                                                            b#1265)))[@inline] in
let length#379 = fun b#1281 -> (({ SIZE })@(b#1281))[@inline] in
let concat#380 =
  fun gen#1283 ->
  (let (gen#1583, gen#1584) = gen#1283 in
   let b1#1284 = gen#1583 in
   let b2#1285 = gen#1584 in ({ UNPAIR ; CONCAT })@(PAIR(b1#1284 , b2#1285)))[@inline] in
let sub#381 =
  fun gen#1287 ->
  (let (gen#1585, gen#1586) = gen#1287 in
   let (gen#1587, gen#1588) = gen#1585 in
   let s#1288 = gen#1587 in
   let l#1289 = gen#1588 in
   let b#1290 = gen#1586 in
   ({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1288 ,
                                                                 l#1289) ,
                                                            b#1290)))[@inline] in
let blake2b#382 = fun b#1292 -> (({ BLAKE2B })@(b#1292))[@inline] in
let sha256#383 = fun b#1294 -> (({ SHA256 })@(b#1294))[@inline] in
let sha512#384 = fun b#1296 -> (({ SHA512 })@(b#1296))[@inline] in
let sha3#385 = fun b#1298 -> (({ SHA3 })@(b#1298))[@inline] in
let keccak#386 = fun b#1300 -> (({ KECCAK })@(b#1300))[@inline] in
let hash_key#387 = fun k#1302 -> (({ HASH_KEY })@(k#1302))[@inline] in
let check#388 =
  fun gen#1304 ->
  (let (gen#1589, gen#1590) = gen#1304 in
   let (gen#1591, gen#1592) = gen#1589 in
   let k#1305 = gen#1591 in
   let s#1306 = gen#1592 in
   let b#1307 = gen#1590 in
   ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1305 , s#1306) ,
                                                 b#1307)))[@inline] in
let assert#389 =
  fun b#1309 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1309))[@inline] in
let abs#392 = fun i#1315 -> (({ ABS })@(i#1315))[@inline] in
let is_nat#393 = fun i#1317 -> (({ ISNAT })@(i#1317))[@inline] in
let true#394 = TRUE()[@inline] in
let false#395 = FALSE()[@inline] in
let unit#396 = UNIT()[@inline] in
let assert_with_error#400 =
  fun gen#1327 ->
  (let (gen#1593, gen#1594) = gen#1327 in
   let b#1328 = gen#1593 in
   let s#1329 = gen#1594 in
   ({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1328 , s#1329)))[@inline] in
let poly_stub_39 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_38 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_37 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_36 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_35 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_34 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_33 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_32 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_31 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_30 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_29 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_28 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_27 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_26 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_25 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_24 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_23 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_22 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_21 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_20 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_19 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_18 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_17 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_16 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_15 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_14 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_13 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_12 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_11 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_10 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_9 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_8 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_7 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_6 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_5 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_4 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_3 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_2 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let poly_stub_1 = fun x#1343 -> (({ FAILWITH })@(x#1343))[@inline] in
let get_total_voting_power#408 =
  fun _u#1353 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#411 = fun _a#1359 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#412 =
  fun _a#1361 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#413 = fun _a#1363 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#414 = fun _v#1365 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#415 = fun _v#1367 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#416 =
  fun _kh#1369 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#417 =
  fun _i#1371 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#418 =
  fun _i#1373 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#419 =
  fun _n#1375 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#421 =
  fun _u#1379 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#423 = fun _u#1383 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#425 =
  fun _n#1387 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#427 =
  fun _kh#1391 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#428 =
  fun _m#1393 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#430 =
  fun _s#1397 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#431 =
  fun _u#1399 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#432 = fun _u#1401 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#433 = fun _u#1403 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#436 =
  fun _bp#1409 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#437 = fun _a#1411 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#438 = fun _c#1413 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#440 =
  fun _fn#1417 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#441 = fun _n#1419 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#442 = L("NEWLINE")[@inline] in
let println#443 = fun _v#1422 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#444 =
  fun gen#1424 ->
  (let (gen#1595, gen#1596) = gen#1424 in
   let (gen#1597, gen#1598) = gen#1595 in
   let _a#1425 = gen#1597 in
   let _s#1426 = gen#1598 in
   let _t#1427 = gen#1596 in (poly_stub_14)@(L(unit)))[@inline] in
let transfer_exn#445 =
  fun gen#1429 ->
  (let (gen#1599, gen#1600) = gen#1429 in
   let (gen#1601, gen#1602) = gen#1599 in
   let _a#1430 = gen#1601 in
   let _s#1431 = gen#1602 in
   let _t#1432 = gen#1600 in (poly_stub_13)@(L(unit)))[@inline] in
let reset_state#447 =
  fun gen#1436 ->
  (let (gen#1603, gen#1604) = gen#1436 in
   let _n#1437 = gen#1603 in
   let _l#1438 = gen#1604 in (poly_stub_12)@(L(unit)))[@inline] in
let reset_state_at#448 =
  fun gen#1440 ->
  (let (gen#1605, gen#1606) = gen#1440 in
   let (gen#1607, gen#1608) = gen#1605 in
   let _t#1441 = gen#1607 in
   let _n#1442 = gen#1608 in
   let _l#1443 = gen#1606 in (poly_stub_11)@(L(unit)))[@inline] in
let save_mutation#451 =
  fun gen#1454 ->
  (let (gen#1609, gen#1610) = gen#1454 in
   let _s#1455 = gen#1609 in
   let _m#1456 = gen#1610 in (poly_stub_10)@(L(unit)))[@inline] in
let sign#454 =
  fun gen#1466 ->
  (let (gen#1611, gen#1612) = gen#1466 in
   let _sk#1467 = gen#1611 in
   let _d#1468 = gen#1612 in (poly_stub_9)@(L(unit)))[@inline] in
let add_account#455 =
  fun gen#1470 ->
  (let (gen#1613, gen#1614) = gen#1470 in
   let _s#1471 = gen#1613 in
   let _k#1472 = gen#1614 in (poly_stub_8)@(L(unit)))[@inline] in
let baker_account#456 =
  fun gen#1474 ->
  (let (gen#1615, gen#1616) = gen#1474 in
   let _p#1475 = gen#1615 in
   let _o#1476 = gen#1616 in (poly_stub_7)@(L(unit)))[@inline] in
let create_chest#458 =
  fun gen#1482 ->
  (let (gen#1617, gen#1618) = gen#1482 in
   let _b#1483 = gen#1617 in
   let _n#1484 = gen#1618 in (poly_stub_6)@(L(unit)))[@inline] in
let create_chest_key#459 =
  fun gen#1486 ->
  (let (gen#1619, gen#1620) = gen#1486 in
   let _c#1487 = gen#1619 in
   let _n#1488 = gen#1620 in (poly_stub_5)@(L(unit)))[@inline] in
let michelson_equal#462 =
  fun gen#1500 ->
  (let (gen#1621, gen#1622) = gen#1500 in
   let _m1#1501 = gen#1621 in
   let _m2#1502 = gen#1622 in (poly_stub_4)@(L(unit)))[@inline] in
let originate_contract#464 =
  fun gen#1508 ->
  (let (gen#1623, gen#1624) = gen#1508 in
   let (gen#1625, gen#1626) = gen#1623 in
   let _c#1509 = gen#1625 in
   let _s#1510 = gen#1626 in
   let _t#1511 = gen#1624 in (poly_stub_3)@(L(unit)))[@inline] in
let compile_contract_from_file#466 =
  fun gen#1518 ->
  (let (gen#1627, gen#1628) = gen#1518 in
   let (gen#1629, gen#1630) = gen#1627 in
   let _fn#1519 = gen#1629 in
   let _e#1520 = gen#1630 in
   let _v#1521 = gen#1628 in (poly_stub_2)@(L(unit)))[@inline] in
let originate_from_file#467 =
  fun gen#1523 ->
  (let (gen#1631, gen#1632) = gen#1523 in
   let (gen#1633, gen#1634) = gen#1631 in
   let (gen#1637, gen#1638) = gen#1633 in
   let _fn#1524 = gen#1637 in
   let _e#1525 = gen#1638 in
   let (gen#1635, gen#1636) = gen#1634 in
   let _v#1526 = gen#1635 in
   let _s#1527 = gen#1636 in
   let _t#1528 = gen#1632 in (poly_stub_1)@(L(unit)))[@inline] in
let assert = assert#209[@inline] in
let abs = abs#212[@inline] in
let is_nat = is_nat#213[@inline] in
let true = true#214[@inline] in
let false = false#215[@inline] in
let unit = unit#216[@inline] in
let assert_with_error = assert_with_error#220[@inline] in
let toto#468 = L(1) in
let toto#469 = L(32) in
let titi#470 = ADD(toto#468 , L(42)) in
let f#471 =
  fun gen#1548 ->
  (let (gen#1639, gen#1640) = gen#1548 in
   let gen#1549 = gen#1639 in
   let x#1550 = gen#1640 in
   let x#1551 = ADD(ADD(x#1550 , toto#468) , titi#470) in
   PAIR(LIST_EMPTY() , x#1551)) in
let toto#472 = L(44) in
let toto#473 = L(43) in
let tata#474 = ADD(toto#468 , titi#470) in
let foo#475 = (f#471)@(PAIR(L(unit) , L(3))) in
let toto#476 = L(10) in
let foo#477 = L("bar") in
let toto = ADD(toto#476 , toto#468) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#1561 ->
  (let (gen#1641, gen#1642) = gen#1561 in
   let p#1562 = gen#1641 in
   let s#1563 = gen#1642 in
   let s#1564 = ADD(ADD(p#1562 , s#1563) , toto) in
   PAIR(LIST_EMPTY() , s#1564)) in
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
