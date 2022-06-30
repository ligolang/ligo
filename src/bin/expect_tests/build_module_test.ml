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
let balance#59 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#60 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#61 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#62 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#63 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#64 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#65 = SELF_ADDRESS()[@inline] in
let chain_id#66 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#67 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#68 =
  fun _u#1132 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#69 =
  fun _u#1134 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#70 = fun _u#1136 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#71 =
  fun _u#1138 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#72 =
  fun _u#1140 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#73 = fun _u#1142 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#74 = fun _u#1144 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#75 =
  fun _u#1146 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#76 =
  fun _u#1148 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#77 =
  fun kh#1150 -> (({ VOTING_POWER })@(kh#1150))[@inline] in
let implicit_account#79 =
  fun kh#1154 -> (IMPLICIT_ACCOUNT(kh#1154))[@inline] in
let pairing_check#85 =
  fun l#1168 -> (({ PAIRING_CHECK })@(l#1168))[@inline] in
let open_chest#86 =
  fun ck#1170 ->
  (fun c#1171 -> (fun n#1172 -> (OPEN_CHEST(ck#1170 , c#1171 , n#1172))))[@inline] in
let set_delegate#90 = fun o#1184 -> (SET_DELEGATE(o#1184))[@inline] in
let xor#91 = fun l#1186 -> (fun r#1187 -> (XOR(l#1186 , r#1187)))[@inline] in
let shift_left#92 =
  fun l#1189 -> (fun r#1190 -> (LSL(l#1189 , r#1190)))[@inline] in
let shift_right#93 =
  fun l#1192 -> (fun r#1193 -> (LSR(l#1192 , r#1193)))[@inline] in
let concat#134 =
  fun b1#1323 ->
  (fun b2#1324 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1323 , b2#1324))))[@inline] in
let sub#135 =
  fun s#1326 ->
  (fun l#1327 ->
   (fun b#1328 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1326 ,
                                                                   l#1327) ,
                                                              b#1328)))))[@inline] in
let length#136 = fun b#1330 -> (({ SIZE })@(b#1330))[@inline] in
let concat#139 =
  fun b1#1337 ->
  (fun b2#1338 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1337 , b2#1338))))[@inline] in
let sub#140 =
  fun s#1340 ->
  (fun l#1341 ->
   (fun b#1342 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1340 ,
                                                                   l#1341) ,
                                                              b#1342)))))[@inline] in
let length#143 = fun b#1348 -> (({ SIZE })@(b#1348))[@inline] in
let blake2b#144 = fun b#1350 -> (({ BLAKE2B })@(b#1350))[@inline] in
let sha256#145 = fun b#1352 -> (({ SHA256 })@(b#1352))[@inline] in
let sha512#146 = fun b#1354 -> (({ SHA512 })@(b#1354))[@inline] in
let sha3#147 = fun b#1356 -> (({ SHA3 })@(b#1356))[@inline] in
let keccak#148 = fun b#1358 -> (({ KECCAK })@(b#1358))[@inline] in
let hash_key#149 = fun k#1360 -> (({ HASH_KEY })@(k#1360))[@inline] in
let check#150 =
  fun k#1362 ->
  (fun s#1363 ->
   (fun b#1364 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1362 , s#1363) ,
                                                   b#1364)))))[@inline] in
let assert#151 =
  fun b#1366 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1366))[@inline] in
let assert_with_error#152 =
  fun b#1368 ->
  (fun s#1369 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1368 , s#1369))))[@inline] in
let abs#157 = fun i#1381 -> (({ ABS })@(i#1381))[@inline] in
let is_nat#158 = fun i#1383 -> (({ ISNAT })@(i#1383))[@inline] in
let true#159 = TRUE()[@inline] in
let false#160 = FALSE()[@inline] in
let unit#161 = UNIT()[@inline] in
let poly_failwith_105 = { FAILWITH }[@inline] in
let poly_failwith_104 = { FAILWITH }[@inline] in
let poly_failwith_103 = { FAILWITH }[@inline] in
let poly_failwith_102 = { FAILWITH }[@inline] in
let poly_failwith_101 = { FAILWITH }[@inline] in
let poly_failwith_100 = { FAILWITH }[@inline] in
let poly_failwith_99 = { FAILWITH }[@inline] in
let poly_failwith_98 = { FAILWITH }[@inline] in
let poly_failwith_97 = { FAILWITH }[@inline] in
let poly_failwith_96 = { FAILWITH }[@inline] in
let poly_failwith_95 = { FAILWITH }[@inline] in
let poly_failwith_94 = { FAILWITH }[@inline] in
let poly_failwith_93 = { FAILWITH }[@inline] in
let poly_failwith_92 = { FAILWITH }[@inline] in
let poly_failwith_91 = { FAILWITH }[@inline] in
let originate_from_file#166 =
  fun _fn#1397 ->
  (fun _e#1398 ->
   (fun _v#1399 ->
    (fun _s#1400 -> (fun _t#1401 -> ((poly_failwith_105)@(L("TEST MODE")))))))[@inline] in
let set_source#168 =
  fun _a#1407 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let set_baker#169 =
  fun _a#1409 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let set_baker_policy#170 =
  fun _bp#1411 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let transfer#171 =
  fun _a#1413 ->
  (fun _s#1414 -> (fun _t#1415 -> ((poly_failwith_91)@(L("TEST MODE")))))[@inline] in
let transfer_exn#172 =
  fun _a#1417 ->
  (fun _s#1418 -> (fun _t#1419 -> ((poly_failwith_102)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#176 =
  fun _a#1431 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let get_balance#177 =
  fun _a#1433 -> ((poly_failwith_104)@(L("TEST MODE")))[@inline] in
let michelson_equal#178 =
  fun _m1#1435 -> (fun _m2#1436 -> ((poly_failwith_103)@(L("TEST MODE"))))[@inline] in
let reset_state#180 =
  fun _n#1440 -> (fun _l#1441 -> ((poly_failwith_91)@(L("TEST MODE"))))[@inline] in
let reset_state_at#181 =
  fun _t#1443 ->
  (fun _n#1444 -> (fun _l#1445 -> ((poly_failwith_91)@(L("TEST MODE")))))[@inline] in
let get_voting_power#182 =
  fun _kh#1447 -> ((poly_failwith_102)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#183 =
  (poly_failwith_102)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#185 =
  fun _i#1454 -> ((poly_failwith_96)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#186 =
  fun _i#1456 -> ((poly_failwith_96)@(L("TEST MODE")))[@inline] in
let last_originations#188 =
  fun _u#1460 -> ((poly_failwith_101)@(L("TEST MODE")))[@inline] in
let save_mutation#191 =
  fun _s#1467 -> (fun _m#1468 -> ((poly_failwith_92)@(L("TEST MODE"))))[@inline] in
let add_account#198 =
  fun _s#1485 -> (fun _k#1486 -> ((poly_failwith_91)@(L("TEST MODE"))))[@inline] in
let new_account#199 =
  fun _u#1488 -> ((poly_failwith_100)@(L("TEST MODE")))[@inline] in
let baker_account#200 =
  fun _p#1490 -> (fun _o#1491 -> ((poly_failwith_91)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#201 =
  fun _n#1493 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let register_delegate#202 =
  fun _kh#1495 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let register_constant#203 =
  fun _m#1497 -> ((poly_failwith_99)@(L("TEST MODE")))[@inline] in
let create_chest#208 =
  fun _b#1509 -> (fun _n#1510 -> ((poly_failwith_98)@(L("TEST MODE"))))[@inline] in
let create_chest_key#209 =
  fun _c#1512 -> (fun _n#1513 -> ((poly_failwith_97)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#210 =
  fun _s#1515 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let restore_context#211 =
  fun _u#1517 -> ((poly_failwith_91)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#212 =
  fun _u#1519 -> ((poly_failwith_91)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#213 =
  fun _u#1521 -> ((poly_failwith_91)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#214 =
  fun _fn#1523 -> ((poly_failwith_91)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#215 =
  fun _fn#1525 ->
  (fun _e#1526 ->
   (fun _v#1527 ->
    ((poly_failwith_91)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#217 =
  fun _c#1531 ->
  (fun _s#1532 -> (fun _t#1533 -> ((poly_failwith_96)@(L("TEST_ORIGINATE")))))[@inline] in
let size#218 =
  fun _c#1535 -> ((poly_failwith_95)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#219 =
  fun _n#1537 -> ((poly_failwith_94)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#220 =
  fun _sk#1539 -> (fun _d#1540 -> ((poly_failwith_93)@(L("TEST_SIGN"))))[@inline] in
let chr#221 = fun _n#1542 -> ((poly_failwith_92)@(L("TEST_CHR")))[@inline] in
let nl#222 = L("NEWLINE")[@inline] in
let println#223 =
  fun _v#1545 -> ((poly_failwith_91)@(L("TEST_PRINTLN")))[@inline] in
let print#224 =
  fun _v#1547 -> ((poly_failwith_91)@(L("TEST_PRINT")))[@inline] in
let eprint#225 =
  fun _v#1549 -> ((poly_failwith_91)@(L("TEST_EPRINTL")))[@inline] in
let toto#19 = L(1) in
let balance#234 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#235 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#236 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#237 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#238 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#239 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#240 = SELF_ADDRESS()[@inline] in
let chain_id#241 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#242 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#243 =
  fun _u#1579 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#244 =
  fun _u#1581 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#245 = fun _u#1583 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#246 =
  fun _u#1585 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#247 =
  fun _u#1587 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#248 = fun _u#1589 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#249 = fun _u#1591 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#250 =
  fun _u#1593 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#251 =
  fun _u#1595 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#252 =
  fun kh#1597 -> (({ VOTING_POWER })@(kh#1597))[@inline] in
let implicit_account#254 =
  fun kh#1601 -> (IMPLICIT_ACCOUNT(kh#1601))[@inline] in
let pairing_check#260 =
  fun l#1615 -> (({ PAIRING_CHECK })@(l#1615))[@inline] in
let open_chest#261 =
  fun ck#1617 ->
  (fun c#1618 -> (fun n#1619 -> (OPEN_CHEST(ck#1617 , c#1618 , n#1619))))[@inline] in
let set_delegate#265 = fun o#1631 -> (SET_DELEGATE(o#1631))[@inline] in
let xor#266 =
  fun l#1633 -> (fun r#1634 -> (XOR(l#1633 , r#1634)))[@inline] in
let shift_left#267 =
  fun l#1636 -> (fun r#1637 -> (LSL(l#1636 , r#1637)))[@inline] in
let shift_right#268 =
  fun l#1639 -> (fun r#1640 -> (LSR(l#1639 , r#1640)))[@inline] in
let concat#309 =
  fun b1#1770 ->
  (fun b2#1771 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1770 , b2#1771))))[@inline] in
let sub#310 =
  fun s#1773 ->
  (fun l#1774 ->
   (fun b#1775 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1773 ,
                                                                   l#1774) ,
                                                              b#1775)))))[@inline] in
let length#311 = fun b#1777 -> (({ SIZE })@(b#1777))[@inline] in
let concat#314 =
  fun b1#1784 ->
  (fun b2#1785 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1784 , b2#1785))))[@inline] in
let sub#315 =
  fun s#1787 ->
  (fun l#1788 ->
   (fun b#1789 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1787 ,
                                                                   l#1788) ,
                                                              b#1789)))))[@inline] in
let length#318 = fun b#1795 -> (({ SIZE })@(b#1795))[@inline] in
let blake2b#319 = fun b#1797 -> (({ BLAKE2B })@(b#1797))[@inline] in
let sha256#320 = fun b#1799 -> (({ SHA256 })@(b#1799))[@inline] in
let sha512#321 = fun b#1801 -> (({ SHA512 })@(b#1801))[@inline] in
let sha3#322 = fun b#1803 -> (({ SHA3 })@(b#1803))[@inline] in
let keccak#323 = fun b#1805 -> (({ KECCAK })@(b#1805))[@inline] in
let hash_key#324 = fun k#1807 -> (({ HASH_KEY })@(k#1807))[@inline] in
let check#325 =
  fun k#1809 ->
  (fun s#1810 ->
   (fun b#1811 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1809 , s#1810) ,
                                                   b#1811)))))[@inline] in
let assert#326 =
  fun b#1813 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1813))[@inline] in
let assert_with_error#327 =
  fun b#1815 ->
  (fun s#1816 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1815 , s#1816))))[@inline] in
let abs#332 = fun i#1828 -> (({ ABS })@(i#1828))[@inline] in
let is_nat#333 = fun i#1830 -> (({ ISNAT })@(i#1830))[@inline] in
let true#334 = TRUE()[@inline] in
let false#335 = FALSE()[@inline] in
let unit#336 = UNIT()[@inline] in
let poly_failwith_90 = { FAILWITH }[@inline] in
let poly_failwith_89 = { FAILWITH }[@inline] in
let poly_failwith_88 = { FAILWITH }[@inline] in
let poly_failwith_87 = { FAILWITH }[@inline] in
let poly_failwith_86 = { FAILWITH }[@inline] in
let poly_failwith_85 = { FAILWITH }[@inline] in
let poly_failwith_84 = { FAILWITH }[@inline] in
let poly_failwith_83 = { FAILWITH }[@inline] in
let poly_failwith_82 = { FAILWITH }[@inline] in
let poly_failwith_81 = { FAILWITH }[@inline] in
let poly_failwith_80 = { FAILWITH }[@inline] in
let poly_failwith_79 = { FAILWITH }[@inline] in
let poly_failwith_78 = { FAILWITH }[@inline] in
let poly_failwith_77 = { FAILWITH }[@inline] in
let poly_failwith_76 = { FAILWITH }[@inline] in
let originate_from_file#341 =
  fun _fn#1844 ->
  (fun _e#1845 ->
   (fun _v#1846 ->
    (fun _s#1847 -> (fun _t#1848 -> ((poly_failwith_90)@(L("TEST MODE")))))))[@inline] in
let set_source#343 =
  fun _a#1854 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let set_baker#344 =
  fun _a#1856 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let set_baker_policy#345 =
  fun _bp#1858 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let transfer#346 =
  fun _a#1860 ->
  (fun _s#1861 -> (fun _t#1862 -> ((poly_failwith_76)@(L("TEST MODE")))))[@inline] in
let transfer_exn#347 =
  fun _a#1864 ->
  (fun _s#1865 -> (fun _t#1866 -> ((poly_failwith_87)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#351 =
  fun _a#1878 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let get_balance#352 =
  fun _a#1880 -> ((poly_failwith_89)@(L("TEST MODE")))[@inline] in
let michelson_equal#353 =
  fun _m1#1882 -> (fun _m2#1883 -> ((poly_failwith_88)@(L("TEST MODE"))))[@inline] in
let reset_state#355 =
  fun _n#1887 -> (fun _l#1888 -> ((poly_failwith_76)@(L("TEST MODE"))))[@inline] in
let reset_state_at#356 =
  fun _t#1890 ->
  (fun _n#1891 -> (fun _l#1892 -> ((poly_failwith_76)@(L("TEST MODE")))))[@inline] in
let get_voting_power#357 =
  fun _kh#1894 -> ((poly_failwith_87)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#358 =
  (poly_failwith_87)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#360 =
  fun _i#1901 -> ((poly_failwith_81)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#361 =
  fun _i#1903 -> ((poly_failwith_81)@(L("TEST MODE")))[@inline] in
let last_originations#363 =
  fun _u#1907 -> ((poly_failwith_86)@(L("TEST MODE")))[@inline] in
let save_mutation#366 =
  fun _s#1914 -> (fun _m#1915 -> ((poly_failwith_77)@(L("TEST MODE"))))[@inline] in
let add_account#373 =
  fun _s#1932 -> (fun _k#1933 -> ((poly_failwith_76)@(L("TEST MODE"))))[@inline] in
let new_account#374 =
  fun _u#1935 -> ((poly_failwith_85)@(L("TEST MODE")))[@inline] in
let baker_account#375 =
  fun _p#1937 -> (fun _o#1938 -> ((poly_failwith_76)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#376 =
  fun _n#1940 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let register_delegate#377 =
  fun _kh#1942 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let register_constant#378 =
  fun _m#1944 -> ((poly_failwith_84)@(L("TEST MODE")))[@inline] in
let create_chest#383 =
  fun _b#1956 -> (fun _n#1957 -> ((poly_failwith_83)@(L("TEST MODE"))))[@inline] in
let create_chest_key#384 =
  fun _c#1959 -> (fun _n#1960 -> ((poly_failwith_82)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#385 =
  fun _s#1962 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let restore_context#386 =
  fun _u#1964 -> ((poly_failwith_76)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#387 =
  fun _u#1966 -> ((poly_failwith_76)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#388 =
  fun _u#1968 -> ((poly_failwith_76)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#389 =
  fun _fn#1970 -> ((poly_failwith_76)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#390 =
  fun _fn#1972 ->
  (fun _e#1973 ->
   (fun _v#1974 ->
    ((poly_failwith_76)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#392 =
  fun _c#1978 ->
  (fun _s#1979 -> (fun _t#1980 -> ((poly_failwith_81)@(L("TEST_ORIGINATE")))))[@inline] in
let size#393 =
  fun _c#1982 -> ((poly_failwith_80)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#394 =
  fun _n#1984 -> ((poly_failwith_79)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#395 =
  fun _sk#1986 -> (fun _d#1987 -> ((poly_failwith_78)@(L("TEST_SIGN"))))[@inline] in
let chr#396 = fun _n#1989 -> ((poly_failwith_77)@(L("TEST_CHR")))[@inline] in
let nl#397 = L("NEWLINE")[@inline] in
let println#398 =
  fun _v#1992 -> ((poly_failwith_76)@(L("TEST_PRINTLN")))[@inline] in
let print#399 =
  fun _v#1994 -> ((poly_failwith_76)@(L("TEST_PRINT")))[@inline] in
let eprint#400 =
  fun _v#1996 -> ((poly_failwith_76)@(L("TEST_EPRINTL")))[@inline] in
let toto#406 = L(32) in
let titi#407 = ADD(toto#19 , L(42)) in
let f#408 =
  fun gen#2011 ->
  (let (gen#4258, gen#4259) = gen#2011 in
   let gen#2012 = gen#4258 in
   let x#2013 = gen#4259 in
   let x#2014 = ADD(ADD(x#2013 , toto#19) , titi#407) in
   PAIR(LIST_EMPTY() , x#2014)) in
let balance#412 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#413 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#414 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#415 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#416 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#417 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#418 = SELF_ADDRESS()[@inline] in
let chain_id#419 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#420 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#421 =
  fun _u#2032 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#422 =
  fun _u#2034 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#423 = fun _u#2036 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#424 =
  fun _u#2038 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#425 =
  fun _u#2040 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#426 = fun _u#2042 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#427 = fun _u#2044 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#428 =
  fun _u#2046 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#429 =
  fun _u#2048 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#430 =
  fun kh#2050 -> (({ VOTING_POWER })@(kh#2050))[@inline] in
let implicit_account#432 =
  fun kh#2054 -> (IMPLICIT_ACCOUNT(kh#2054))[@inline] in
let pairing_check#438 =
  fun l#2068 -> (({ PAIRING_CHECK })@(l#2068))[@inline] in
let open_chest#439 =
  fun ck#2070 ->
  (fun c#2071 -> (fun n#2072 -> (OPEN_CHEST(ck#2070 , c#2071 , n#2072))))[@inline] in
let set_delegate#443 = fun o#2084 -> (SET_DELEGATE(o#2084))[@inline] in
let xor#444 =
  fun l#2086 -> (fun r#2087 -> (XOR(l#2086 , r#2087)))[@inline] in
let shift_left#445 =
  fun l#2089 -> (fun r#2090 -> (LSL(l#2089 , r#2090)))[@inline] in
let shift_right#446 =
  fun l#2092 -> (fun r#2093 -> (LSR(l#2092 , r#2093)))[@inline] in
let concat#487 =
  fun b1#2223 ->
  (fun b2#2224 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2223 , b2#2224))))[@inline] in
let sub#488 =
  fun s#2226 ->
  (fun l#2227 ->
   (fun b#2228 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2226 ,
                                                                   l#2227) ,
                                                              b#2228)))))[@inline] in
let length#489 = fun b#2230 -> (({ SIZE })@(b#2230))[@inline] in
let concat#492 =
  fun b1#2237 ->
  (fun b2#2238 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2237 , b2#2238))))[@inline] in
let sub#493 =
  fun s#2240 ->
  (fun l#2241 ->
   (fun b#2242 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2240 ,
                                                                   l#2241) ,
                                                              b#2242)))))[@inline] in
let length#496 = fun b#2248 -> (({ SIZE })@(b#2248))[@inline] in
let blake2b#497 = fun b#2250 -> (({ BLAKE2B })@(b#2250))[@inline] in
let sha256#498 = fun b#2252 -> (({ SHA256 })@(b#2252))[@inline] in
let sha512#499 = fun b#2254 -> (({ SHA512 })@(b#2254))[@inline] in
let sha3#500 = fun b#2256 -> (({ SHA3 })@(b#2256))[@inline] in
let keccak#501 = fun b#2258 -> (({ KECCAK })@(b#2258))[@inline] in
let hash_key#502 = fun k#2260 -> (({ HASH_KEY })@(k#2260))[@inline] in
let check#503 =
  fun k#2262 ->
  (fun s#2263 ->
   (fun b#2264 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#2262 , s#2263) ,
                                                   b#2264)))))[@inline] in
let assert#504 =
  fun b#2266 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#2266))[@inline] in
let assert_with_error#505 =
  fun b#2268 ->
  (fun s#2269 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#2268 , s#2269))))[@inline] in
let abs#510 = fun i#2281 -> (({ ABS })@(i#2281))[@inline] in
let is_nat#511 = fun i#2283 -> (({ ISNAT })@(i#2283))[@inline] in
let true#512 = TRUE()[@inline] in
let false#513 = FALSE()[@inline] in
let unit#514 = UNIT()[@inline] in
let poly_failwith_75 = { FAILWITH }[@inline] in
let poly_failwith_74 = { FAILWITH }[@inline] in
let poly_failwith_73 = { FAILWITH }[@inline] in
let poly_failwith_72 = { FAILWITH }[@inline] in
let poly_failwith_71 = { FAILWITH }[@inline] in
let poly_failwith_70 = { FAILWITH }[@inline] in
let poly_failwith_69 = { FAILWITH }[@inline] in
let poly_failwith_68 = { FAILWITH }[@inline] in
let poly_failwith_67 = { FAILWITH }[@inline] in
let poly_failwith_66 = { FAILWITH }[@inline] in
let poly_failwith_65 = { FAILWITH }[@inline] in
let poly_failwith_64 = { FAILWITH }[@inline] in
let poly_failwith_63 = { FAILWITH }[@inline] in
let poly_failwith_62 = { FAILWITH }[@inline] in
let poly_failwith_61 = { FAILWITH }[@inline] in
let originate_from_file#519 =
  fun _fn#2297 ->
  (fun _e#2298 ->
   (fun _v#2299 ->
    (fun _s#2300 -> (fun _t#2301 -> ((poly_failwith_75)@(L("TEST MODE")))))))[@inline] in
let set_source#521 =
  fun _a#2307 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let set_baker#522 =
  fun _a#2309 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let set_baker_policy#523 =
  fun _bp#2311 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let transfer#524 =
  fun _a#2313 ->
  (fun _s#2314 -> (fun _t#2315 -> ((poly_failwith_61)@(L("TEST MODE")))))[@inline] in
let transfer_exn#525 =
  fun _a#2317 ->
  (fun _s#2318 -> (fun _t#2319 -> ((poly_failwith_72)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#529 =
  fun _a#2331 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let get_balance#530 =
  fun _a#2333 -> ((poly_failwith_74)@(L("TEST MODE")))[@inline] in
let michelson_equal#531 =
  fun _m1#2335 -> (fun _m2#2336 -> ((poly_failwith_73)@(L("TEST MODE"))))[@inline] in
let reset_state#533 =
  fun _n#2340 -> (fun _l#2341 -> ((poly_failwith_61)@(L("TEST MODE"))))[@inline] in
let reset_state_at#534 =
  fun _t#2343 ->
  (fun _n#2344 -> (fun _l#2345 -> ((poly_failwith_61)@(L("TEST MODE")))))[@inline] in
let get_voting_power#535 =
  fun _kh#2347 -> ((poly_failwith_72)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#536 =
  (poly_failwith_72)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#538 =
  fun _i#2354 -> ((poly_failwith_66)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#539 =
  fun _i#2356 -> ((poly_failwith_66)@(L("TEST MODE")))[@inline] in
let last_originations#541 =
  fun _u#2360 -> ((poly_failwith_71)@(L("TEST MODE")))[@inline] in
let save_mutation#544 =
  fun _s#2367 -> (fun _m#2368 -> ((poly_failwith_62)@(L("TEST MODE"))))[@inline] in
let add_account#551 =
  fun _s#2385 -> (fun _k#2386 -> ((poly_failwith_61)@(L("TEST MODE"))))[@inline] in
let new_account#552 =
  fun _u#2388 -> ((poly_failwith_70)@(L("TEST MODE")))[@inline] in
let baker_account#553 =
  fun _p#2390 -> (fun _o#2391 -> ((poly_failwith_61)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#554 =
  fun _n#2393 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let register_delegate#555 =
  fun _kh#2395 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let register_constant#556 =
  fun _m#2397 -> ((poly_failwith_69)@(L("TEST MODE")))[@inline] in
let create_chest#561 =
  fun _b#2409 -> (fun _n#2410 -> ((poly_failwith_68)@(L("TEST MODE"))))[@inline] in
let create_chest_key#562 =
  fun _c#2412 -> (fun _n#2413 -> ((poly_failwith_67)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#563 =
  fun _s#2415 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let restore_context#564 =
  fun _u#2417 -> ((poly_failwith_61)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#565 =
  fun _u#2419 -> ((poly_failwith_61)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#566 =
  fun _u#2421 -> ((poly_failwith_61)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#567 =
  fun _fn#2423 -> ((poly_failwith_61)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#568 =
  fun _fn#2425 ->
  (fun _e#2426 ->
   (fun _v#2427 ->
    ((poly_failwith_61)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#570 =
  fun _c#2431 ->
  (fun _s#2432 -> (fun _t#2433 -> ((poly_failwith_66)@(L("TEST_ORIGINATE")))))[@inline] in
let size#571 =
  fun _c#2435 -> ((poly_failwith_65)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#572 =
  fun _n#2437 -> ((poly_failwith_64)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#573 =
  fun _sk#2439 -> (fun _d#2440 -> ((poly_failwith_63)@(L("TEST_SIGN"))))[@inline] in
let chr#574 = fun _n#2442 -> ((poly_failwith_62)@(L("TEST_CHR")))[@inline] in
let nl#575 = L("NEWLINE")[@inline] in
let println#576 =
  fun _v#2445 -> ((poly_failwith_61)@(L("TEST_PRINTLN")))[@inline] in
let print#577 =
  fun _v#2447 -> ((poly_failwith_61)@(L("TEST_PRINT")))[@inline] in
let eprint#578 =
  fun _v#2449 -> ((poly_failwith_61)@(L("TEST_EPRINTL")))[@inline] in
let toto#584 = L(44) in
let balance#588 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#589 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#590 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#591 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#592 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#593 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#594 = SELF_ADDRESS()[@inline] in
let chain_id#595 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#596 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#597 =
  fun _u#2479 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#598 =
  fun _u#2481 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#599 = fun _u#2483 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#600 =
  fun _u#2485 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#601 =
  fun _u#2487 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#602 = fun _u#2489 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#603 = fun _u#2491 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#604 =
  fun _u#2493 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#605 =
  fun _u#2495 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#606 =
  fun kh#2497 -> (({ VOTING_POWER })@(kh#2497))[@inline] in
let implicit_account#608 =
  fun kh#2501 -> (IMPLICIT_ACCOUNT(kh#2501))[@inline] in
let pairing_check#614 =
  fun l#2515 -> (({ PAIRING_CHECK })@(l#2515))[@inline] in
let open_chest#615 =
  fun ck#2517 ->
  (fun c#2518 -> (fun n#2519 -> (OPEN_CHEST(ck#2517 , c#2518 , n#2519))))[@inline] in
let set_delegate#619 = fun o#2531 -> (SET_DELEGATE(o#2531))[@inline] in
let xor#620 =
  fun l#2533 -> (fun r#2534 -> (XOR(l#2533 , r#2534)))[@inline] in
let shift_left#621 =
  fun l#2536 -> (fun r#2537 -> (LSL(l#2536 , r#2537)))[@inline] in
let shift_right#622 =
  fun l#2539 -> (fun r#2540 -> (LSR(l#2539 , r#2540)))[@inline] in
let concat#663 =
  fun b1#2670 ->
  (fun b2#2671 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2670 , b2#2671))))[@inline] in
let sub#664 =
  fun s#2673 ->
  (fun l#2674 ->
   (fun b#2675 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2673 ,
                                                                   l#2674) ,
                                                              b#2675)))))[@inline] in
let length#665 = fun b#2677 -> (({ SIZE })@(b#2677))[@inline] in
let concat#668 =
  fun b1#2684 ->
  (fun b2#2685 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2684 , b2#2685))))[@inline] in
let sub#669 =
  fun s#2687 ->
  (fun l#2688 ->
   (fun b#2689 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2687 ,
                                                                   l#2688) ,
                                                              b#2689)))))[@inline] in
let length#672 = fun b#2695 -> (({ SIZE })@(b#2695))[@inline] in
let blake2b#673 = fun b#2697 -> (({ BLAKE2B })@(b#2697))[@inline] in
let sha256#674 = fun b#2699 -> (({ SHA256 })@(b#2699))[@inline] in
let sha512#675 = fun b#2701 -> (({ SHA512 })@(b#2701))[@inline] in
let sha3#676 = fun b#2703 -> (({ SHA3 })@(b#2703))[@inline] in
let keccak#677 = fun b#2705 -> (({ KECCAK })@(b#2705))[@inline] in
let hash_key#678 = fun k#2707 -> (({ HASH_KEY })@(k#2707))[@inline] in
let check#679 =
  fun k#2709 ->
  (fun s#2710 ->
   (fun b#2711 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#2709 , s#2710) ,
                                                   b#2711)))))[@inline] in
let assert#680 =
  fun b#2713 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#2713))[@inline] in
let assert_with_error#681 =
  fun b#2715 ->
  (fun s#2716 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#2715 , s#2716))))[@inline] in
let abs#686 = fun i#2728 -> (({ ABS })@(i#2728))[@inline] in
let is_nat#687 = fun i#2730 -> (({ ISNAT })@(i#2730))[@inline] in
let true#688 = TRUE()[@inline] in
let false#689 = FALSE()[@inline] in
let unit#690 = UNIT()[@inline] in
let poly_failwith_60 = { FAILWITH }[@inline] in
let poly_failwith_59 = { FAILWITH }[@inline] in
let poly_failwith_58 = { FAILWITH }[@inline] in
let poly_failwith_57 = { FAILWITH }[@inline] in
let poly_failwith_56 = { FAILWITH }[@inline] in
let poly_failwith_55 = { FAILWITH }[@inline] in
let poly_failwith_54 = { FAILWITH }[@inline] in
let poly_failwith_53 = { FAILWITH }[@inline] in
let poly_failwith_52 = { FAILWITH }[@inline] in
let poly_failwith_51 = { FAILWITH }[@inline] in
let poly_failwith_50 = { FAILWITH }[@inline] in
let poly_failwith_49 = { FAILWITH }[@inline] in
let poly_failwith_48 = { FAILWITH }[@inline] in
let poly_failwith_47 = { FAILWITH }[@inline] in
let poly_failwith_46 = { FAILWITH }[@inline] in
let originate_from_file#695 =
  fun _fn#2744 ->
  (fun _e#2745 ->
   (fun _v#2746 ->
    (fun _s#2747 -> (fun _t#2748 -> ((poly_failwith_60)@(L("TEST MODE")))))))[@inline] in
let set_source#697 =
  fun _a#2754 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let set_baker#698 =
  fun _a#2756 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let set_baker_policy#699 =
  fun _bp#2758 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let transfer#700 =
  fun _a#2760 ->
  (fun _s#2761 -> (fun _t#2762 -> ((poly_failwith_46)@(L("TEST MODE")))))[@inline] in
let transfer_exn#701 =
  fun _a#2764 ->
  (fun _s#2765 -> (fun _t#2766 -> ((poly_failwith_57)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#705 =
  fun _a#2778 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let get_balance#706 =
  fun _a#2780 -> ((poly_failwith_59)@(L("TEST MODE")))[@inline] in
let michelson_equal#707 =
  fun _m1#2782 -> (fun _m2#2783 -> ((poly_failwith_58)@(L("TEST MODE"))))[@inline] in
let reset_state#709 =
  fun _n#2787 -> (fun _l#2788 -> ((poly_failwith_46)@(L("TEST MODE"))))[@inline] in
let reset_state_at#710 =
  fun _t#2790 ->
  (fun _n#2791 -> (fun _l#2792 -> ((poly_failwith_46)@(L("TEST MODE")))))[@inline] in
let get_voting_power#711 =
  fun _kh#2794 -> ((poly_failwith_57)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#712 =
  (poly_failwith_57)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#714 =
  fun _i#2801 -> ((poly_failwith_51)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#715 =
  fun _i#2803 -> ((poly_failwith_51)@(L("TEST MODE")))[@inline] in
let last_originations#717 =
  fun _u#2807 -> ((poly_failwith_56)@(L("TEST MODE")))[@inline] in
let save_mutation#720 =
  fun _s#2814 -> (fun _m#2815 -> ((poly_failwith_47)@(L("TEST MODE"))))[@inline] in
let add_account#727 =
  fun _s#2832 -> (fun _k#2833 -> ((poly_failwith_46)@(L("TEST MODE"))))[@inline] in
let new_account#728 =
  fun _u#2835 -> ((poly_failwith_55)@(L("TEST MODE")))[@inline] in
let baker_account#729 =
  fun _p#2837 -> (fun _o#2838 -> ((poly_failwith_46)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#730 =
  fun _n#2840 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let register_delegate#731 =
  fun _kh#2842 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let register_constant#732 =
  fun _m#2844 -> ((poly_failwith_54)@(L("TEST MODE")))[@inline] in
let create_chest#737 =
  fun _b#2856 -> (fun _n#2857 -> ((poly_failwith_53)@(L("TEST MODE"))))[@inline] in
let create_chest_key#738 =
  fun _c#2859 -> (fun _n#2860 -> ((poly_failwith_52)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#739 =
  fun _s#2862 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let restore_context#740 =
  fun _u#2864 -> ((poly_failwith_46)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#741 =
  fun _u#2866 -> ((poly_failwith_46)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#742 =
  fun _u#2868 -> ((poly_failwith_46)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#743 =
  fun _fn#2870 -> ((poly_failwith_46)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#744 =
  fun _fn#2872 ->
  (fun _e#2873 ->
   (fun _v#2874 ->
    ((poly_failwith_46)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#746 =
  fun _c#2878 ->
  (fun _s#2879 -> (fun _t#2880 -> ((poly_failwith_51)@(L("TEST_ORIGINATE")))))[@inline] in
let size#747 =
  fun _c#2882 -> ((poly_failwith_50)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#748 =
  fun _n#2884 -> ((poly_failwith_49)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#749 =
  fun _sk#2886 -> (fun _d#2887 -> ((poly_failwith_48)@(L("TEST_SIGN"))))[@inline] in
let chr#750 = fun _n#2889 -> ((poly_failwith_47)@(L("TEST_CHR")))[@inline] in
let nl#751 = L("NEWLINE")[@inline] in
let println#752 =
  fun _v#2892 -> ((poly_failwith_46)@(L("TEST_PRINTLN")))[@inline] in
let print#753 =
  fun _v#2894 -> ((poly_failwith_46)@(L("TEST_PRINT")))[@inline] in
let eprint#754 =
  fun _v#2896 -> ((poly_failwith_46)@(L("TEST_EPRINTL")))[@inline] in
let toto#760 = L(43) in
let balance#764 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#765 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#766 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#767 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#768 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#769 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#770 = SELF_ADDRESS()[@inline] in
let chain_id#771 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#772 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#773 =
  fun _u#2926 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#774 =
  fun _u#2928 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#775 = fun _u#2930 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#776 =
  fun _u#2932 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#777 =
  fun _u#2934 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#778 = fun _u#2936 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#779 = fun _u#2938 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#780 =
  fun _u#2940 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#781 =
  fun _u#2942 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#782 =
  fun kh#2944 -> (({ VOTING_POWER })@(kh#2944))[@inline] in
let implicit_account#784 =
  fun kh#2948 -> (IMPLICIT_ACCOUNT(kh#2948))[@inline] in
let pairing_check#790 =
  fun l#2962 -> (({ PAIRING_CHECK })@(l#2962))[@inline] in
let open_chest#791 =
  fun ck#2964 ->
  (fun c#2965 -> (fun n#2966 -> (OPEN_CHEST(ck#2964 , c#2965 , n#2966))))[@inline] in
let set_delegate#795 = fun o#2978 -> (SET_DELEGATE(o#2978))[@inline] in
let xor#796 =
  fun l#2980 -> (fun r#2981 -> (XOR(l#2980 , r#2981)))[@inline] in
let shift_left#797 =
  fun l#2983 -> (fun r#2984 -> (LSL(l#2983 , r#2984)))[@inline] in
let shift_right#798 =
  fun l#2986 -> (fun r#2987 -> (LSR(l#2986 , r#2987)))[@inline] in
let concat#839 =
  fun b1#3117 ->
  (fun b2#3118 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3117 , b2#3118))))[@inline] in
let sub#840 =
  fun s#3120 ->
  (fun l#3121 ->
   (fun b#3122 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3120 ,
                                                                   l#3121) ,
                                                              b#3122)))))[@inline] in
let length#841 = fun b#3124 -> (({ SIZE })@(b#3124))[@inline] in
let concat#844 =
  fun b1#3131 ->
  (fun b2#3132 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3131 , b2#3132))))[@inline] in
let sub#845 =
  fun s#3134 ->
  (fun l#3135 ->
   (fun b#3136 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3134 ,
                                                                   l#3135) ,
                                                              b#3136)))))[@inline] in
let length#848 = fun b#3142 -> (({ SIZE })@(b#3142))[@inline] in
let blake2b#849 = fun b#3144 -> (({ BLAKE2B })@(b#3144))[@inline] in
let sha256#850 = fun b#3146 -> (({ SHA256 })@(b#3146))[@inline] in
let sha512#851 = fun b#3148 -> (({ SHA512 })@(b#3148))[@inline] in
let sha3#852 = fun b#3150 -> (({ SHA3 })@(b#3150))[@inline] in
let keccak#853 = fun b#3152 -> (({ KECCAK })@(b#3152))[@inline] in
let hash_key#854 = fun k#3154 -> (({ HASH_KEY })@(k#3154))[@inline] in
let check#855 =
  fun k#3156 ->
  (fun s#3157 ->
   (fun b#3158 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#3156 , s#3157) ,
                                                   b#3158)))))[@inline] in
let assert#856 =
  fun b#3160 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#3160))[@inline] in
let assert_with_error#857 =
  fun b#3162 ->
  (fun s#3163 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#3162 , s#3163))))[@inline] in
let abs#862 = fun i#3175 -> (({ ABS })@(i#3175))[@inline] in
let is_nat#863 = fun i#3177 -> (({ ISNAT })@(i#3177))[@inline] in
let true#864 = TRUE()[@inline] in
let false#865 = FALSE()[@inline] in
let unit#866 = UNIT()[@inline] in
let poly_failwith_45 = { FAILWITH }[@inline] in
let poly_failwith_44 = { FAILWITH }[@inline] in
let poly_failwith_43 = { FAILWITH }[@inline] in
let poly_failwith_42 = { FAILWITH }[@inline] in
let poly_failwith_41 = { FAILWITH }[@inline] in
let poly_failwith_40 = { FAILWITH }[@inline] in
let poly_failwith_39 = { FAILWITH }[@inline] in
let poly_failwith_38 = { FAILWITH }[@inline] in
let poly_failwith_37 = { FAILWITH }[@inline] in
let poly_failwith_36 = { FAILWITH }[@inline] in
let poly_failwith_35 = { FAILWITH }[@inline] in
let poly_failwith_34 = { FAILWITH }[@inline] in
let poly_failwith_33 = { FAILWITH }[@inline] in
let poly_failwith_32 = { FAILWITH }[@inline] in
let poly_failwith_31 = { FAILWITH }[@inline] in
let originate_from_file#871 =
  fun _fn#3191 ->
  (fun _e#3192 ->
   (fun _v#3193 ->
    (fun _s#3194 -> (fun _t#3195 -> ((poly_failwith_45)@(L("TEST MODE")))))))[@inline] in
let set_source#873 =
  fun _a#3201 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let set_baker#874 =
  fun _a#3203 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let set_baker_policy#875 =
  fun _bp#3205 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let transfer#876 =
  fun _a#3207 ->
  (fun _s#3208 -> (fun _t#3209 -> ((poly_failwith_31)@(L("TEST MODE")))))[@inline] in
let transfer_exn#877 =
  fun _a#3211 ->
  (fun _s#3212 -> (fun _t#3213 -> ((poly_failwith_42)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#881 =
  fun _a#3225 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let get_balance#882 =
  fun _a#3227 -> ((poly_failwith_44)@(L("TEST MODE")))[@inline] in
let michelson_equal#883 =
  fun _m1#3229 -> (fun _m2#3230 -> ((poly_failwith_43)@(L("TEST MODE"))))[@inline] in
let reset_state#885 =
  fun _n#3234 -> (fun _l#3235 -> ((poly_failwith_31)@(L("TEST MODE"))))[@inline] in
let reset_state_at#886 =
  fun _t#3237 ->
  (fun _n#3238 -> (fun _l#3239 -> ((poly_failwith_31)@(L("TEST MODE")))))[@inline] in
let get_voting_power#887 =
  fun _kh#3241 -> ((poly_failwith_42)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#888 =
  (poly_failwith_42)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#890 =
  fun _i#3248 -> ((poly_failwith_36)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#891 =
  fun _i#3250 -> ((poly_failwith_36)@(L("TEST MODE")))[@inline] in
let last_originations#893 =
  fun _u#3254 -> ((poly_failwith_41)@(L("TEST MODE")))[@inline] in
let save_mutation#896 =
  fun _s#3261 -> (fun _m#3262 -> ((poly_failwith_32)@(L("TEST MODE"))))[@inline] in
let add_account#903 =
  fun _s#3279 -> (fun _k#3280 -> ((poly_failwith_31)@(L("TEST MODE"))))[@inline] in
let new_account#904 =
  fun _u#3282 -> ((poly_failwith_40)@(L("TEST MODE")))[@inline] in
let baker_account#905 =
  fun _p#3284 -> (fun _o#3285 -> ((poly_failwith_31)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#906 =
  fun _n#3287 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let register_delegate#907 =
  fun _kh#3289 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let register_constant#908 =
  fun _m#3291 -> ((poly_failwith_39)@(L("TEST MODE")))[@inline] in
let create_chest#913 =
  fun _b#3303 -> (fun _n#3304 -> ((poly_failwith_38)@(L("TEST MODE"))))[@inline] in
let create_chest_key#914 =
  fun _c#3306 -> (fun _n#3307 -> ((poly_failwith_37)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#915 =
  fun _s#3309 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let restore_context#916 =
  fun _u#3311 -> ((poly_failwith_31)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#917 =
  fun _u#3313 -> ((poly_failwith_31)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#918 =
  fun _u#3315 -> ((poly_failwith_31)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#919 =
  fun _fn#3317 -> ((poly_failwith_31)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#920 =
  fun _fn#3319 ->
  (fun _e#3320 ->
   (fun _v#3321 ->
    ((poly_failwith_31)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#922 =
  fun _c#3325 ->
  (fun _s#3326 -> (fun _t#3327 -> ((poly_failwith_36)@(L("TEST_ORIGINATE")))))[@inline] in
let size#923 =
  fun _c#3329 -> ((poly_failwith_35)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#924 =
  fun _n#3331 -> ((poly_failwith_34)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#925 =
  fun _sk#3333 -> (fun _d#3334 -> ((poly_failwith_33)@(L("TEST_SIGN"))))[@inline] in
let chr#926 = fun _n#3336 -> ((poly_failwith_32)@(L("TEST_CHR")))[@inline] in
let nl#927 = L("NEWLINE")[@inline] in
let println#928 =
  fun _v#3339 -> ((poly_failwith_31)@(L("TEST_PRINTLN")))[@inline] in
let print#929 =
  fun _v#3341 -> ((poly_failwith_31)@(L("TEST_PRINT")))[@inline] in
let eprint#930 =
  fun _v#3343 -> ((poly_failwith_31)@(L("TEST_EPRINTL")))[@inline] in
let tata#936 = ADD(toto#19 , titi#407) in
let foo#937 = (f#408)@(PAIR(L(unit) , L(3))) in
let balance#941 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#942 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#943 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#944 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#945 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#946 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#947 = SELF_ADDRESS()[@inline] in
let chain_id#948 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#949 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#950 =
  fun _u#3374 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#951 =
  fun _u#3376 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#952 = fun _u#3378 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#953 =
  fun _u#3380 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#954 =
  fun _u#3382 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#955 = fun _u#3384 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#956 = fun _u#3386 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#957 =
  fun _u#3388 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#958 =
  fun _u#3390 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#959 =
  fun kh#3392 -> (({ VOTING_POWER })@(kh#3392))[@inline] in
let implicit_account#961 =
  fun kh#3396 -> (IMPLICIT_ACCOUNT(kh#3396))[@inline] in
let pairing_check#967 =
  fun l#3410 -> (({ PAIRING_CHECK })@(l#3410))[@inline] in
let open_chest#968 =
  fun ck#3412 ->
  (fun c#3413 -> (fun n#3414 -> (OPEN_CHEST(ck#3412 , c#3413 , n#3414))))[@inline] in
let set_delegate#972 = fun o#3426 -> (SET_DELEGATE(o#3426))[@inline] in
let xor#973 =
  fun l#3428 -> (fun r#3429 -> (XOR(l#3428 , r#3429)))[@inline] in
let shift_left#974 =
  fun l#3431 -> (fun r#3432 -> (LSL(l#3431 , r#3432)))[@inline] in
let shift_right#975 =
  fun l#3434 -> (fun r#3435 -> (LSR(l#3434 , r#3435)))[@inline] in
let concat#1016 =
  fun b1#3565 ->
  (fun b2#3566 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3565 , b2#3566))))[@inline] in
let sub#1017 =
  fun s#3568 ->
  (fun l#3569 ->
   (fun b#3570 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3568 ,
                                                                   l#3569) ,
                                                              b#3570)))))[@inline] in
let length#1018 = fun b#3572 -> (({ SIZE })@(b#3572))[@inline] in
let concat#1021 =
  fun b1#3579 ->
  (fun b2#3580 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3579 , b2#3580))))[@inline] in
let sub#1022 =
  fun s#3582 ->
  (fun l#3583 ->
   (fun b#3584 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3582 ,
                                                                   l#3583) ,
                                                              b#3584)))))[@inline] in
let length#1025 = fun b#3590 -> (({ SIZE })@(b#3590))[@inline] in
let blake2b#1026 = fun b#3592 -> (({ BLAKE2B })@(b#3592))[@inline] in
let sha256#1027 = fun b#3594 -> (({ SHA256 })@(b#3594))[@inline] in
let sha512#1028 = fun b#3596 -> (({ SHA512 })@(b#3596))[@inline] in
let sha3#1029 = fun b#3598 -> (({ SHA3 })@(b#3598))[@inline] in
let keccak#1030 = fun b#3600 -> (({ KECCAK })@(b#3600))[@inline] in
let hash_key#1031 = fun k#3602 -> (({ HASH_KEY })@(k#3602))[@inline] in
let check#1032 =
  fun k#3604 ->
  (fun s#3605 ->
   (fun b#3606 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#3604 , s#3605) ,
                                                   b#3606)))))[@inline] in
let assert#1033 =
  fun b#3608 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#3608))[@inline] in
let assert_with_error#1034 =
  fun b#3610 ->
  (fun s#3611 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#3610 , s#3611))))[@inline] in
let abs#1039 = fun i#3623 -> (({ ABS })@(i#3623))[@inline] in
let is_nat#1040 = fun i#3625 -> (({ ISNAT })@(i#3625))[@inline] in
let true#1041 = TRUE()[@inline] in
let false#1042 = FALSE()[@inline] in
let unit#1043 = UNIT()[@inline] in
let poly_failwith_30 = { FAILWITH }[@inline] in
let poly_failwith_29 = { FAILWITH }[@inline] in
let poly_failwith_28 = { FAILWITH }[@inline] in
let poly_failwith_27 = { FAILWITH }[@inline] in
let poly_failwith_26 = { FAILWITH }[@inline] in
let poly_failwith_25 = { FAILWITH }[@inline] in
let poly_failwith_24 = { FAILWITH }[@inline] in
let poly_failwith_23 = { FAILWITH }[@inline] in
let poly_failwith_22 = { FAILWITH }[@inline] in
let poly_failwith_21 = { FAILWITH }[@inline] in
let poly_failwith_20 = { FAILWITH }[@inline] in
let poly_failwith_19 = { FAILWITH }[@inline] in
let poly_failwith_18 = { FAILWITH }[@inline] in
let poly_failwith_17 = { FAILWITH }[@inline] in
let poly_failwith_16 = { FAILWITH }[@inline] in
let originate_from_file#1048 =
  fun _fn#3639 ->
  (fun _e#3640 ->
   (fun _v#3641 ->
    (fun _s#3642 -> (fun _t#3643 -> ((poly_failwith_30)@(L("TEST MODE")))))))[@inline] in
let set_source#1050 =
  fun _a#3649 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let set_baker#1051 =
  fun _a#3651 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let set_baker_policy#1052 =
  fun _bp#3653 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let transfer#1053 =
  fun _a#3655 ->
  (fun _s#3656 -> (fun _t#3657 -> ((poly_failwith_16)@(L("TEST MODE")))))[@inline] in
let transfer_exn#1054 =
  fun _a#3659 ->
  (fun _s#3660 -> (fun _t#3661 -> ((poly_failwith_27)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#1058 =
  fun _a#3673 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let get_balance#1059 =
  fun _a#3675 -> ((poly_failwith_29)@(L("TEST MODE")))[@inline] in
let michelson_equal#1060 =
  fun _m1#3677 -> (fun _m2#3678 -> ((poly_failwith_28)@(L("TEST MODE"))))[@inline] in
let reset_state#1062 =
  fun _n#3682 -> (fun _l#3683 -> ((poly_failwith_16)@(L("TEST MODE"))))[@inline] in
let reset_state_at#1063 =
  fun _t#3685 ->
  (fun _n#3686 -> (fun _l#3687 -> ((poly_failwith_16)@(L("TEST MODE")))))[@inline] in
let get_voting_power#1064 =
  fun _kh#3689 -> ((poly_failwith_27)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#1065 =
  (poly_failwith_27)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#1067 =
  fun _i#3696 -> ((poly_failwith_21)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#1068 =
  fun _i#3698 -> ((poly_failwith_21)@(L("TEST MODE")))[@inline] in
let last_originations#1070 =
  fun _u#3702 -> ((poly_failwith_26)@(L("TEST MODE")))[@inline] in
let save_mutation#1073 =
  fun _s#3709 -> (fun _m#3710 -> ((poly_failwith_17)@(L("TEST MODE"))))[@inline] in
let add_account#1080 =
  fun _s#3727 -> (fun _k#3728 -> ((poly_failwith_16)@(L("TEST MODE"))))[@inline] in
let new_account#1081 =
  fun _u#3730 -> ((poly_failwith_25)@(L("TEST MODE")))[@inline] in
let baker_account#1082 =
  fun _p#3732 -> (fun _o#3733 -> ((poly_failwith_16)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#1083 =
  fun _n#3735 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let register_delegate#1084 =
  fun _kh#3737 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let register_constant#1085 =
  fun _m#3739 -> ((poly_failwith_24)@(L("TEST MODE")))[@inline] in
let create_chest#1090 =
  fun _b#3751 -> (fun _n#3752 -> ((poly_failwith_23)@(L("TEST MODE"))))[@inline] in
let create_chest_key#1091 =
  fun _c#3754 -> (fun _n#3755 -> ((poly_failwith_22)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#1092 =
  fun _s#3757 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let restore_context#1093 =
  fun _u#3759 -> ((poly_failwith_16)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#1094 =
  fun _u#3761 -> ((poly_failwith_16)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#1095 =
  fun _u#3763 -> ((poly_failwith_16)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#1096 =
  fun _fn#3765 -> ((poly_failwith_16)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#1097 =
  fun _fn#3767 ->
  (fun _e#3768 ->
   (fun _v#3769 ->
    ((poly_failwith_16)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#1099 =
  fun _c#3773 ->
  (fun _s#3774 -> (fun _t#3775 -> ((poly_failwith_21)@(L("TEST_ORIGINATE")))))[@inline] in
let size#1100 =
  fun _c#3777 -> ((poly_failwith_20)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#1101 =
  fun _n#3779 -> ((poly_failwith_19)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#1102 =
  fun _sk#3781 -> (fun _d#3782 -> ((poly_failwith_18)@(L("TEST_SIGN"))))[@inline] in
let chr#1103 =
  fun _n#3784 -> ((poly_failwith_17)@(L("TEST_CHR")))[@inline] in
let nl#1104 = L("NEWLINE")[@inline] in
let println#1105 =
  fun _v#3787 -> ((poly_failwith_16)@(L("TEST_PRINTLN")))[@inline] in
let print#1106 =
  fun _v#3789 -> ((poly_failwith_16)@(L("TEST_PRINT")))[@inline] in
let eprint#1107 =
  fun _v#3791 -> ((poly_failwith_16)@(L("TEST_EPRINTL")))[@inline] in
let toto#1113 = L(10) in
let foo#1114 = L("bar") in
let balance#12 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#13 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#14 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#15 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#16 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#17 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#18 = SELF_ADDRESS()[@inline] in
let chain_id#19 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#20 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#21 =
  fun _u#3822 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#22 =
  fun _u#3824 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#23 = fun _u#3826 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#24 =
  fun _u#3828 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#25 =
  fun _u#3830 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#26 = fun _u#3832 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#27 = fun _u#3834 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#28 =
  fun _u#3836 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#29 =
  fun _u#3838 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#30 =
  fun kh#3840 -> (({ VOTING_POWER })@(kh#3840))[@inline] in
let implicit_account#32 =
  fun kh#3844 -> (IMPLICIT_ACCOUNT(kh#3844))[@inline] in
let pairing_check#38 =
  fun l#3858 -> (({ PAIRING_CHECK })@(l#3858))[@inline] in
let open_chest#39 =
  fun ck#3860 ->
  (fun c#3861 -> (fun n#3862 -> (OPEN_CHEST(ck#3860 , c#3861 , n#3862))))[@inline] in
let set_delegate#43 = fun o#3874 -> (SET_DELEGATE(o#3874))[@inline] in
let xor#44 = fun l#3876 -> (fun r#3877 -> (XOR(l#3876 , r#3877)))[@inline] in
let shift_left#45 =
  fun l#3879 -> (fun r#3880 -> (LSL(l#3879 , r#3880)))[@inline] in
let shift_right#46 =
  fun l#3882 -> (fun r#3883 -> (LSR(l#3882 , r#3883)))[@inline] in
let concat#87 =
  fun b1#4013 ->
  (fun b2#4014 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4013 , b2#4014))))[@inline] in
let sub#88 =
  fun s#4016 ->
  (fun l#4017 ->
   (fun b#4018 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4016 ,
                                                                   l#4017) ,
                                                              b#4018)))))[@inline] in
let length#89 = fun b#4020 -> (({ SIZE })@(b#4020))[@inline] in
let concat#92 =
  fun b1#4027 ->
  (fun b2#4028 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4027 , b2#4028))))[@inline] in
let sub#93 =
  fun s#4030 ->
  (fun l#4031 ->
   (fun b#4032 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4030 ,
                                                                   l#4031) ,
                                                              b#4032)))))[@inline] in
let length#96 = fun b#4038 -> (({ SIZE })@(b#4038))[@inline] in
let blake2b#97 = fun b#4040 -> (({ BLAKE2B })@(b#4040))[@inline] in
let sha256#98 = fun b#4042 -> (({ SHA256 })@(b#4042))[@inline] in
let sha512#99 = fun b#4044 -> (({ SHA512 })@(b#4044))[@inline] in
let sha3#100 = fun b#4046 -> (({ SHA3 })@(b#4046))[@inline] in
let keccak#101 = fun b#4048 -> (({ KECCAK })@(b#4048))[@inline] in
let hash_key#102 = fun k#4050 -> (({ HASH_KEY })@(k#4050))[@inline] in
let check#103 =
  fun k#4052 ->
  (fun s#4053 ->
   (fun b#4054 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4052 , s#4053) ,
                                                   b#4054)))))[@inline] in
let assert =
  fun b#4056 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4056))[@inline] in
let assert_with_error =
  fun b#4058 ->
  (fun s#4059 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4058 , s#4059))))[@inline] in
let abs = fun i#4071 -> (({ ABS })@(i#4071))[@inline] in
let is_nat = fun i#4073 -> (({ ISNAT })@(i#4073))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let poly_failwith_15 = { FAILWITH }[@inline] in
let poly_failwith_14 = { FAILWITH }[@inline] in
let poly_failwith_13 = { FAILWITH }[@inline] in
let poly_failwith_12 = { FAILWITH }[@inline] in
let poly_failwith_11 = { FAILWITH }[@inline] in
let poly_failwith_10 = { FAILWITH }[@inline] in
let poly_failwith_9 = { FAILWITH }[@inline] in
let poly_failwith_8 = { FAILWITH }[@inline] in
let poly_failwith_7 = { FAILWITH }[@inline] in
let poly_failwith_6 = { FAILWITH }[@inline] in
let poly_failwith_5 = { FAILWITH }[@inline] in
let poly_failwith_4 = { FAILWITH }[@inline] in
let poly_failwith_3 = { FAILWITH }[@inline] in
let poly_failwith_2 = { FAILWITH }[@inline] in
let poly_failwith_1 = { FAILWITH }[@inline] in
let originate_from_file#105 =
  fun _fn#4087 ->
  (fun _e#4088 ->
   (fun _v#4089 ->
    (fun _s#4090 -> (fun _t#4091 -> ((poly_failwith_15)@(L("TEST MODE")))))))[@inline] in
let set_source#107 =
  fun _a#4097 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let set_baker#108 =
  fun _a#4099 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let set_baker_policy#109 =
  fun _bp#4101 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let transfer#110 =
  fun _a#4103 ->
  (fun _s#4104 -> (fun _t#4105 -> ((poly_failwith_1)@(L("TEST MODE")))))[@inline] in
let transfer_exn#111 =
  fun _a#4107 ->
  (fun _s#4108 -> (fun _t#4109 -> ((poly_failwith_12)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#115 =
  fun _a#4121 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let get_balance#116 =
  fun _a#4123 -> ((poly_failwith_14)@(L("TEST MODE")))[@inline] in
let michelson_equal#117 =
  fun _m1#4125 -> (fun _m2#4126 -> ((poly_failwith_13)@(L("TEST MODE"))))[@inline] in
let reset_state#119 =
  fun _n#4130 -> (fun _l#4131 -> ((poly_failwith_1)@(L("TEST MODE"))))[@inline] in
let reset_state_at#120 =
  fun _t#4133 ->
  (fun _n#4134 -> (fun _l#4135 -> ((poly_failwith_1)@(L("TEST MODE")))))[@inline] in
let get_voting_power#121 =
  fun _kh#4137 -> ((poly_failwith_12)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#122 =
  (poly_failwith_12)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#124 =
  fun _i#4144 -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#125 =
  fun _i#4146 -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
let last_originations#127 =
  fun _u#4150 -> ((poly_failwith_11)@(L("TEST MODE")))[@inline] in
let save_mutation#130 =
  fun _s#4157 -> (fun _m#4158 -> ((poly_failwith_2)@(L("TEST MODE"))))[@inline] in
let add_account#137 =
  fun _s#4175 -> (fun _k#4176 -> ((poly_failwith_1)@(L("TEST MODE"))))[@inline] in
let new_account#138 =
  fun _u#4178 -> ((poly_failwith_10)@(L("TEST MODE")))[@inline] in
let baker_account#139 =
  fun _p#4180 -> (fun _o#4181 -> ((poly_failwith_1)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#140 =
  fun _n#4183 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let register_delegate#141 =
  fun _kh#4185 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let register_constant#142 =
  fun _m#4187 -> ((poly_failwith_9)@(L("TEST MODE")))[@inline] in
let create_chest#147 =
  fun _b#4199 -> (fun _n#4200 -> ((poly_failwith_8)@(L("TEST MODE"))))[@inline] in
let create_chest_key#148 =
  fun _c#4202 -> (fun _n#4203 -> ((poly_failwith_7)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#149 =
  fun _s#4205 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let restore_context#150 =
  fun _u#4207 -> ((poly_failwith_1)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#151 =
  fun _u#4209 -> ((poly_failwith_1)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#152 =
  fun _u#4211 -> ((poly_failwith_1)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#153 =
  fun _fn#4213 -> ((poly_failwith_1)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#154 =
  fun _fn#4215 ->
  (fun _e#4216 ->
   (fun _v#4217 -> ((poly_failwith_1)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#156 =
  fun _c#4221 ->
  (fun _s#4222 -> (fun _t#4223 -> ((poly_failwith_6)@(L("TEST_ORIGINATE")))))[@inline] in
let size#157 =
  fun _c#4225 -> ((poly_failwith_5)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#158 =
  fun _n#4227 -> ((poly_failwith_4)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#159 =
  fun _sk#4229 -> (fun _d#4230 -> ((poly_failwith_3)@(L("TEST_SIGN"))))[@inline] in
let chr#160 = fun _n#4232 -> ((poly_failwith_2)@(L("TEST_CHR")))[@inline] in
let nl#161 = L("NEWLINE")[@inline] in
let println#162 =
  fun _v#4235 -> ((poly_failwith_1)@(L("TEST_PRINTLN")))[@inline] in
let print#163 =
  fun _v#4237 -> ((poly_failwith_1)@(L("TEST_PRINT")))[@inline] in
let eprint#164 =
  fun _v#4239 -> ((poly_failwith_1)@(L("TEST_EPRINTL")))[@inline] in
let toto = ADD(toto#1113 , toto#19) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#4254 ->
  (let (gen#4260, gen#4261) = gen#4254 in
   let p#4255 = gen#4260 in
   let s#4256 = gen#4261 in
   let s#4257 = ADD(ADD(p#4255 , s#4256) , toto) in
   PAIR(LIST_EMPTY() , s#4257)) in
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
