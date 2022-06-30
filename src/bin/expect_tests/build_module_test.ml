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
let balance#52 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#53 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#54 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#55 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#56 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#57 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#58 = SELF_ADDRESS()[@inline] in
let chain_id#59 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#60 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#61 =
  fun _u#1101 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#62 =
  fun _u#1103 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#63 = fun _u#1105 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#64 =
  fun _u#1107 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#65 =
  fun _u#1109 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#66 = fun _u#1111 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#67 = fun _u#1113 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#68 =
  fun _u#1115 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#69 =
  fun _u#1117 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#70 =
  fun kh#1119 -> (({ VOTING_POWER })@(kh#1119))[@inline] in
let implicit_account#72 =
  fun kh#1123 -> (IMPLICIT_ACCOUNT(kh#1123))[@inline] in
let pairing_check#78 =
  fun l#1137 -> (({ PAIRING_CHECK })@(l#1137))[@inline] in
let open_chest#79 =
  fun ck#1139 ->
  (fun c#1140 -> (fun n#1141 -> (OPEN_CHEST(ck#1139 , c#1140 , n#1141))))[@inline] in
let set_delegate#83 = fun o#1153 -> (SET_DELEGATE(o#1153))[@inline] in
let xor#84 = fun l#1155 -> (fun r#1156 -> (XOR(l#1155 , r#1156)))[@inline] in
let shift_left#85 =
  fun l#1158 -> (fun r#1159 -> (LSL(l#1158 , r#1159)))[@inline] in
let shift_right#86 =
  fun l#1161 -> (fun r#1162 -> (LSR(l#1161 , r#1162)))[@inline] in
let concat#127 =
  fun b1#1292 ->
  (fun b2#1293 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1292 , b2#1293))))[@inline] in
let sub#128 =
  fun s#1295 ->
  (fun l#1296 ->
   (fun b#1297 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1295 ,
                                                                   l#1296) ,
                                                              b#1297)))))[@inline] in
let length#129 = fun b#1299 -> (({ SIZE })@(b#1299))[@inline] in
let concat#132 =
  fun b1#1306 ->
  (fun b2#1307 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1306 , b2#1307))))[@inline] in
let sub#133 =
  fun s#1309 ->
  (fun l#1310 ->
   (fun b#1311 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1309 ,
                                                                   l#1310) ,
                                                              b#1311)))))[@inline] in
let length#136 = fun b#1317 -> (({ SIZE })@(b#1317))[@inline] in
let blake2b#137 = fun b#1319 -> (({ BLAKE2B })@(b#1319))[@inline] in
let sha256#138 = fun b#1321 -> (({ SHA256 })@(b#1321))[@inline] in
let sha512#139 = fun b#1323 -> (({ SHA512 })@(b#1323))[@inline] in
let sha3#140 = fun b#1325 -> (({ SHA3 })@(b#1325))[@inline] in
let keccak#141 = fun b#1327 -> (({ KECCAK })@(b#1327))[@inline] in
let hash_key#142 = fun k#1329 -> (({ HASH_KEY })@(k#1329))[@inline] in
let check#143 =
  fun k#1331 ->
  (fun s#1332 ->
   (fun b#1333 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1331 , s#1332) ,
                                                   b#1333)))))[@inline] in
let assert#144 =
  fun b#1335 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1335))[@inline] in
let assert_with_error#145 =
  fun b#1337 ->
  (fun s#1338 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1337 , s#1338))))[@inline] in
let abs#150 = fun i#1350 -> (({ ABS })@(i#1350))[@inline] in
let is_nat#151 = fun i#1352 -> (({ ISNAT })@(i#1352))[@inline] in
let true#152 = TRUE()[@inline] in
let false#153 = FALSE()[@inline] in
let unit#154 = UNIT()[@inline] in
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
let originate_from_file#159 =
  fun _fn#1366 ->
  (fun _e#1367 ->
   (fun _v#1368 ->
    (fun _s#1369 -> (fun _t#1370 -> ((poly_failwith_105)@(L("TEST MODE")))))))[@inline] in
let set_source#161 =
  fun _a#1376 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let set_baker#162 =
  fun _a#1378 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let set_baker_policy#163 =
  fun _bp#1380 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let transfer#164 =
  fun _a#1382 ->
  (fun _s#1383 -> (fun _t#1384 -> ((poly_failwith_91)@(L("TEST MODE")))))[@inline] in
let transfer_exn#165 =
  fun _a#1386 ->
  (fun _s#1387 -> (fun _t#1388 -> ((poly_failwith_102)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#169 =
  fun _a#1400 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let get_balance#170 =
  fun _a#1402 -> ((poly_failwith_104)@(L("TEST MODE")))[@inline] in
let michelson_equal#171 =
  fun _m1#1404 -> (fun _m2#1405 -> ((poly_failwith_103)@(L("TEST MODE"))))[@inline] in
let reset_state#173 =
  fun _n#1409 -> (fun _l#1410 -> ((poly_failwith_91)@(L("TEST MODE"))))[@inline] in
let reset_state_at#174 =
  fun _t#1412 ->
  (fun _n#1413 -> (fun _l#1414 -> ((poly_failwith_91)@(L("TEST MODE")))))[@inline] in
let get_voting_power#175 =
  fun _kh#1416 -> ((poly_failwith_102)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#176 =
  (poly_failwith_102)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#178 =
  fun _i#1423 -> ((poly_failwith_96)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#179 =
  fun _i#1425 -> ((poly_failwith_96)@(L("TEST MODE")))[@inline] in
let last_originations#181 =
  fun _u#1429 -> ((poly_failwith_101)@(L("TEST MODE")))[@inline] in
let save_mutation#184 =
  fun _s#1436 -> (fun _m#1437 -> ((poly_failwith_92)@(L("TEST MODE"))))[@inline] in
let add_account#191 =
  fun _s#1454 -> (fun _k#1455 -> ((poly_failwith_91)@(L("TEST MODE"))))[@inline] in
let new_account#192 =
  fun _u#1457 -> ((poly_failwith_100)@(L("TEST MODE")))[@inline] in
let baker_account#193 =
  fun _p#1459 -> (fun _o#1460 -> ((poly_failwith_91)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#194 =
  fun _n#1462 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let register_delegate#195 =
  fun _kh#1464 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let register_constant#196 =
  fun _m#1466 -> ((poly_failwith_99)@(L("TEST MODE")))[@inline] in
let create_chest#201 =
  fun _b#1478 -> (fun _n#1479 -> ((poly_failwith_98)@(L("TEST MODE"))))[@inline] in
let create_chest_key#202 =
  fun _c#1481 -> (fun _n#1482 -> ((poly_failwith_97)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#203 =
  fun _s#1484 -> ((poly_failwith_91)@(L("TEST MODE")))[@inline] in
let restore_context#204 =
  fun _u#1486 -> ((poly_failwith_91)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#205 =
  fun _u#1488 -> ((poly_failwith_91)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#206 =
  fun _u#1490 -> ((poly_failwith_91)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#207 =
  fun _fn#1492 -> ((poly_failwith_91)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#208 =
  fun _fn#1494 ->
  (fun _e#1495 ->
   (fun _v#1496 ->
    ((poly_failwith_91)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#210 =
  fun _c#1500 ->
  (fun _s#1501 -> (fun _t#1502 -> ((poly_failwith_96)@(L("TEST_ORIGINATE")))))[@inline] in
let size#211 =
  fun _c#1504 -> ((poly_failwith_95)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#212 =
  fun _n#1506 -> ((poly_failwith_94)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#213 =
  fun _sk#1508 -> (fun _d#1509 -> ((poly_failwith_93)@(L("TEST_SIGN"))))[@inline] in
let chr#214 = fun _n#1511 -> ((poly_failwith_92)@(L("TEST_CHR")))[@inline] in
let nl#215 = L("NEWLINE")[@inline] in
let println#216 =
  fun _v#1514 -> ((poly_failwith_91)@(L("TEST_PRINTLN")))[@inline] in
let print#217 =
  fun _v#1516 -> ((poly_failwith_91)@(L("TEST_PRINT")))[@inline] in
let eprint#218 =
  fun _v#1518 -> ((poly_failwith_91)@(L("TEST_EPRINTL")))[@inline] in
let toto#17 = L(1) in
let balance#223 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#224 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#225 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#226 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#227 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#228 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#229 = SELF_ADDRESS()[@inline] in
let chain_id#230 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#231 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#232 =
  fun _u#1539 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#233 =
  fun _u#1541 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#234 = fun _u#1543 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#235 =
  fun _u#1545 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#236 =
  fun _u#1547 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#237 = fun _u#1549 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#238 = fun _u#1551 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#239 =
  fun _u#1553 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#240 =
  fun _u#1555 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#241 =
  fun kh#1557 -> (({ VOTING_POWER })@(kh#1557))[@inline] in
let implicit_account#243 =
  fun kh#1561 -> (IMPLICIT_ACCOUNT(kh#1561))[@inline] in
let pairing_check#249 =
  fun l#1575 -> (({ PAIRING_CHECK })@(l#1575))[@inline] in
let open_chest#250 =
  fun ck#1577 ->
  (fun c#1578 -> (fun n#1579 -> (OPEN_CHEST(ck#1577 , c#1578 , n#1579))))[@inline] in
let set_delegate#254 = fun o#1591 -> (SET_DELEGATE(o#1591))[@inline] in
let xor#255 =
  fun l#1593 -> (fun r#1594 -> (XOR(l#1593 , r#1594)))[@inline] in
let shift_left#256 =
  fun l#1596 -> (fun r#1597 -> (LSL(l#1596 , r#1597)))[@inline] in
let shift_right#257 =
  fun l#1599 -> (fun r#1600 -> (LSR(l#1599 , r#1600)))[@inline] in
let concat#298 =
  fun b1#1730 ->
  (fun b2#1731 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1730 , b2#1731))))[@inline] in
let sub#299 =
  fun s#1733 ->
  (fun l#1734 ->
   (fun b#1735 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1733 ,
                                                                   l#1734) ,
                                                              b#1735)))))[@inline] in
let length#300 = fun b#1737 -> (({ SIZE })@(b#1737))[@inline] in
let concat#303 =
  fun b1#1744 ->
  (fun b2#1745 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1744 , b2#1745))))[@inline] in
let sub#304 =
  fun s#1747 ->
  (fun l#1748 ->
   (fun b#1749 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1747 ,
                                                                   l#1748) ,
                                                              b#1749)))))[@inline] in
let length#307 = fun b#1755 -> (({ SIZE })@(b#1755))[@inline] in
let blake2b#308 = fun b#1757 -> (({ BLAKE2B })@(b#1757))[@inline] in
let sha256#309 = fun b#1759 -> (({ SHA256 })@(b#1759))[@inline] in
let sha512#310 = fun b#1761 -> (({ SHA512 })@(b#1761))[@inline] in
let sha3#311 = fun b#1763 -> (({ SHA3 })@(b#1763))[@inline] in
let keccak#312 = fun b#1765 -> (({ KECCAK })@(b#1765))[@inline] in
let hash_key#313 = fun k#1767 -> (({ HASH_KEY })@(k#1767))[@inline] in
let check#314 =
  fun k#1769 ->
  (fun s#1770 ->
   (fun b#1771 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1769 , s#1770) ,
                                                   b#1771)))))[@inline] in
let assert#315 =
  fun b#1773 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1773))[@inline] in
let assert_with_error#316 =
  fun b#1775 ->
  (fun s#1776 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1775 , s#1776))))[@inline] in
let abs#321 = fun i#1788 -> (({ ABS })@(i#1788))[@inline] in
let is_nat#322 = fun i#1790 -> (({ ISNAT })@(i#1790))[@inline] in
let true#323 = TRUE()[@inline] in
let false#324 = FALSE()[@inline] in
let unit#325 = UNIT()[@inline] in
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
let originate_from_file#330 =
  fun _fn#1804 ->
  (fun _e#1805 ->
   (fun _v#1806 ->
    (fun _s#1807 -> (fun _t#1808 -> ((poly_failwith_90)@(L("TEST MODE")))))))[@inline] in
let set_source#332 =
  fun _a#1814 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let set_baker#333 =
  fun _a#1816 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let set_baker_policy#334 =
  fun _bp#1818 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let transfer#335 =
  fun _a#1820 ->
  (fun _s#1821 -> (fun _t#1822 -> ((poly_failwith_76)@(L("TEST MODE")))))[@inline] in
let transfer_exn#336 =
  fun _a#1824 ->
  (fun _s#1825 -> (fun _t#1826 -> ((poly_failwith_87)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#340 =
  fun _a#1838 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let get_balance#341 =
  fun _a#1840 -> ((poly_failwith_89)@(L("TEST MODE")))[@inline] in
let michelson_equal#342 =
  fun _m1#1842 -> (fun _m2#1843 -> ((poly_failwith_88)@(L("TEST MODE"))))[@inline] in
let reset_state#344 =
  fun _n#1847 -> (fun _l#1848 -> ((poly_failwith_76)@(L("TEST MODE"))))[@inline] in
let reset_state_at#345 =
  fun _t#1850 ->
  (fun _n#1851 -> (fun _l#1852 -> ((poly_failwith_76)@(L("TEST MODE")))))[@inline] in
let get_voting_power#346 =
  fun _kh#1854 -> ((poly_failwith_87)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#347 =
  (poly_failwith_87)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#349 =
  fun _i#1861 -> ((poly_failwith_81)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#350 =
  fun _i#1863 -> ((poly_failwith_81)@(L("TEST MODE")))[@inline] in
let last_originations#352 =
  fun _u#1867 -> ((poly_failwith_86)@(L("TEST MODE")))[@inline] in
let save_mutation#355 =
  fun _s#1874 -> (fun _m#1875 -> ((poly_failwith_77)@(L("TEST MODE"))))[@inline] in
let add_account#362 =
  fun _s#1892 -> (fun _k#1893 -> ((poly_failwith_76)@(L("TEST MODE"))))[@inline] in
let new_account#363 =
  fun _u#1895 -> ((poly_failwith_85)@(L("TEST MODE")))[@inline] in
let baker_account#364 =
  fun _p#1897 -> (fun _o#1898 -> ((poly_failwith_76)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#365 =
  fun _n#1900 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let register_delegate#366 =
  fun _kh#1902 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let register_constant#367 =
  fun _m#1904 -> ((poly_failwith_84)@(L("TEST MODE")))[@inline] in
let create_chest#372 =
  fun _b#1916 -> (fun _n#1917 -> ((poly_failwith_83)@(L("TEST MODE"))))[@inline] in
let create_chest_key#373 =
  fun _c#1919 -> (fun _n#1920 -> ((poly_failwith_82)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#374 =
  fun _s#1922 -> ((poly_failwith_76)@(L("TEST MODE")))[@inline] in
let restore_context#375 =
  fun _u#1924 -> ((poly_failwith_76)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#376 =
  fun _u#1926 -> ((poly_failwith_76)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#377 =
  fun _u#1928 -> ((poly_failwith_76)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#378 =
  fun _fn#1930 -> ((poly_failwith_76)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#379 =
  fun _fn#1932 ->
  (fun _e#1933 ->
   (fun _v#1934 ->
    ((poly_failwith_76)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#381 =
  fun _c#1938 ->
  (fun _s#1939 -> (fun _t#1940 -> ((poly_failwith_81)@(L("TEST_ORIGINATE")))))[@inline] in
let size#382 =
  fun _c#1942 -> ((poly_failwith_80)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#383 =
  fun _n#1944 -> ((poly_failwith_79)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#384 =
  fun _sk#1946 -> (fun _d#1947 -> ((poly_failwith_78)@(L("TEST_SIGN"))))[@inline] in
let chr#385 = fun _n#1949 -> ((poly_failwith_77)@(L("TEST_CHR")))[@inline] in
let nl#386 = L("NEWLINE")[@inline] in
let println#387 =
  fun _v#1952 -> ((poly_failwith_76)@(L("TEST_PRINTLN")))[@inline] in
let print#388 =
  fun _v#1954 -> ((poly_failwith_76)@(L("TEST_PRINT")))[@inline] in
let eprint#389 =
  fun _v#1956 -> ((poly_failwith_76)@(L("TEST_EPRINTL")))[@inline] in
let toto#391 = L(32) in
let titi#392 = ADD(toto#17 , L(42)) in
let f#393 =
  fun gen#1962 ->
  (let (gen#4164, gen#4165) = gen#1962 in
   let gen#1963 = gen#4164 in
   let x#1964 = gen#4165 in
   let x#1965 = ADD(ADD(x#1964 , toto#17) , titi#392) in
   PAIR(LIST_EMPTY() , x#1965)) in
let balance#397 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#398 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#399 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#400 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#401 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#402 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#403 = SELF_ADDRESS()[@inline] in
let chain_id#404 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#405 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#406 =
  fun _u#1983 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#407 =
  fun _u#1985 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#408 = fun _u#1987 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#409 =
  fun _u#1989 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#410 =
  fun _u#1991 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#411 = fun _u#1993 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#412 = fun _u#1995 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#413 =
  fun _u#1997 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#414 =
  fun _u#1999 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#415 =
  fun kh#2001 -> (({ VOTING_POWER })@(kh#2001))[@inline] in
let implicit_account#417 =
  fun kh#2005 -> (IMPLICIT_ACCOUNT(kh#2005))[@inline] in
let pairing_check#423 =
  fun l#2019 -> (({ PAIRING_CHECK })@(l#2019))[@inline] in
let open_chest#424 =
  fun ck#2021 ->
  (fun c#2022 -> (fun n#2023 -> (OPEN_CHEST(ck#2021 , c#2022 , n#2023))))[@inline] in
let set_delegate#428 = fun o#2035 -> (SET_DELEGATE(o#2035))[@inline] in
let xor#429 =
  fun l#2037 -> (fun r#2038 -> (XOR(l#2037 , r#2038)))[@inline] in
let shift_left#430 =
  fun l#2040 -> (fun r#2041 -> (LSL(l#2040 , r#2041)))[@inline] in
let shift_right#431 =
  fun l#2043 -> (fun r#2044 -> (LSR(l#2043 , r#2044)))[@inline] in
let concat#472 =
  fun b1#2174 ->
  (fun b2#2175 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2174 , b2#2175))))[@inline] in
let sub#473 =
  fun s#2177 ->
  (fun l#2178 ->
   (fun b#2179 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2177 ,
                                                                   l#2178) ,
                                                              b#2179)))))[@inline] in
let length#474 = fun b#2181 -> (({ SIZE })@(b#2181))[@inline] in
let concat#477 =
  fun b1#2188 ->
  (fun b2#2189 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2188 , b2#2189))))[@inline] in
let sub#478 =
  fun s#2191 ->
  (fun l#2192 ->
   (fun b#2193 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2191 ,
                                                                   l#2192) ,
                                                              b#2193)))))[@inline] in
let length#481 = fun b#2199 -> (({ SIZE })@(b#2199))[@inline] in
let blake2b#482 = fun b#2201 -> (({ BLAKE2B })@(b#2201))[@inline] in
let sha256#483 = fun b#2203 -> (({ SHA256 })@(b#2203))[@inline] in
let sha512#484 = fun b#2205 -> (({ SHA512 })@(b#2205))[@inline] in
let sha3#485 = fun b#2207 -> (({ SHA3 })@(b#2207))[@inline] in
let keccak#486 = fun b#2209 -> (({ KECCAK })@(b#2209))[@inline] in
let hash_key#487 = fun k#2211 -> (({ HASH_KEY })@(k#2211))[@inline] in
let check#488 =
  fun k#2213 ->
  (fun s#2214 ->
   (fun b#2215 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#2213 , s#2214) ,
                                                   b#2215)))))[@inline] in
let assert#489 =
  fun b#2217 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#2217))[@inline] in
let assert_with_error#490 =
  fun b#2219 ->
  (fun s#2220 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#2219 , s#2220))))[@inline] in
let abs#495 = fun i#2232 -> (({ ABS })@(i#2232))[@inline] in
let is_nat#496 = fun i#2234 -> (({ ISNAT })@(i#2234))[@inline] in
let true#497 = TRUE()[@inline] in
let false#498 = FALSE()[@inline] in
let unit#499 = UNIT()[@inline] in
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
let originate_from_file#504 =
  fun _fn#2248 ->
  (fun _e#2249 ->
   (fun _v#2250 ->
    (fun _s#2251 -> (fun _t#2252 -> ((poly_failwith_75)@(L("TEST MODE")))))))[@inline] in
let set_source#506 =
  fun _a#2258 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let set_baker#507 =
  fun _a#2260 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let set_baker_policy#508 =
  fun _bp#2262 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let transfer#509 =
  fun _a#2264 ->
  (fun _s#2265 -> (fun _t#2266 -> ((poly_failwith_61)@(L("TEST MODE")))))[@inline] in
let transfer_exn#510 =
  fun _a#2268 ->
  (fun _s#2269 -> (fun _t#2270 -> ((poly_failwith_72)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#514 =
  fun _a#2282 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let get_balance#515 =
  fun _a#2284 -> ((poly_failwith_74)@(L("TEST MODE")))[@inline] in
let michelson_equal#516 =
  fun _m1#2286 -> (fun _m2#2287 -> ((poly_failwith_73)@(L("TEST MODE"))))[@inline] in
let reset_state#518 =
  fun _n#2291 -> (fun _l#2292 -> ((poly_failwith_61)@(L("TEST MODE"))))[@inline] in
let reset_state_at#519 =
  fun _t#2294 ->
  (fun _n#2295 -> (fun _l#2296 -> ((poly_failwith_61)@(L("TEST MODE")))))[@inline] in
let get_voting_power#520 =
  fun _kh#2298 -> ((poly_failwith_72)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#521 =
  (poly_failwith_72)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#523 =
  fun _i#2305 -> ((poly_failwith_66)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#524 =
  fun _i#2307 -> ((poly_failwith_66)@(L("TEST MODE")))[@inline] in
let last_originations#526 =
  fun _u#2311 -> ((poly_failwith_71)@(L("TEST MODE")))[@inline] in
let save_mutation#529 =
  fun _s#2318 -> (fun _m#2319 -> ((poly_failwith_62)@(L("TEST MODE"))))[@inline] in
let add_account#536 =
  fun _s#2336 -> (fun _k#2337 -> ((poly_failwith_61)@(L("TEST MODE"))))[@inline] in
let new_account#537 =
  fun _u#2339 -> ((poly_failwith_70)@(L("TEST MODE")))[@inline] in
let baker_account#538 =
  fun _p#2341 -> (fun _o#2342 -> ((poly_failwith_61)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#539 =
  fun _n#2344 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let register_delegate#540 =
  fun _kh#2346 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let register_constant#541 =
  fun _m#2348 -> ((poly_failwith_69)@(L("TEST MODE")))[@inline] in
let create_chest#546 =
  fun _b#2360 -> (fun _n#2361 -> ((poly_failwith_68)@(L("TEST MODE"))))[@inline] in
let create_chest_key#547 =
  fun _c#2363 -> (fun _n#2364 -> ((poly_failwith_67)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#548 =
  fun _s#2366 -> ((poly_failwith_61)@(L("TEST MODE")))[@inline] in
let restore_context#549 =
  fun _u#2368 -> ((poly_failwith_61)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#550 =
  fun _u#2370 -> ((poly_failwith_61)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#551 =
  fun _u#2372 -> ((poly_failwith_61)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#552 =
  fun _fn#2374 -> ((poly_failwith_61)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#553 =
  fun _fn#2376 ->
  (fun _e#2377 ->
   (fun _v#2378 ->
    ((poly_failwith_61)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#555 =
  fun _c#2382 ->
  (fun _s#2383 -> (fun _t#2384 -> ((poly_failwith_66)@(L("TEST_ORIGINATE")))))[@inline] in
let size#556 =
  fun _c#2386 -> ((poly_failwith_65)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#557 =
  fun _n#2388 -> ((poly_failwith_64)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#558 =
  fun _sk#2390 -> (fun _d#2391 -> ((poly_failwith_63)@(L("TEST_SIGN"))))[@inline] in
let chr#559 = fun _n#2393 -> ((poly_failwith_62)@(L("TEST_CHR")))[@inline] in
let nl#560 = L("NEWLINE")[@inline] in
let println#561 =
  fun _v#2396 -> ((poly_failwith_61)@(L("TEST_PRINTLN")))[@inline] in
let print#562 =
  fun _v#2398 -> ((poly_failwith_61)@(L("TEST_PRINT")))[@inline] in
let eprint#563 =
  fun _v#2400 -> ((poly_failwith_61)@(L("TEST_EPRINTL")))[@inline] in
let toto#565 = L(44) in
let balance#569 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#570 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#571 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#572 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#573 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#574 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#575 = SELF_ADDRESS()[@inline] in
let chain_id#576 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#577 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#578 =
  fun _u#2421 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#579 =
  fun _u#2423 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#580 = fun _u#2425 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#581 =
  fun _u#2427 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#582 =
  fun _u#2429 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#583 = fun _u#2431 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#584 = fun _u#2433 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#585 =
  fun _u#2435 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#586 =
  fun _u#2437 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#587 =
  fun kh#2439 -> (({ VOTING_POWER })@(kh#2439))[@inline] in
let implicit_account#589 =
  fun kh#2443 -> (IMPLICIT_ACCOUNT(kh#2443))[@inline] in
let pairing_check#595 =
  fun l#2457 -> (({ PAIRING_CHECK })@(l#2457))[@inline] in
let open_chest#596 =
  fun ck#2459 ->
  (fun c#2460 -> (fun n#2461 -> (OPEN_CHEST(ck#2459 , c#2460 , n#2461))))[@inline] in
let set_delegate#600 = fun o#2473 -> (SET_DELEGATE(o#2473))[@inline] in
let xor#601 =
  fun l#2475 -> (fun r#2476 -> (XOR(l#2475 , r#2476)))[@inline] in
let shift_left#602 =
  fun l#2478 -> (fun r#2479 -> (LSL(l#2478 , r#2479)))[@inline] in
let shift_right#603 =
  fun l#2481 -> (fun r#2482 -> (LSR(l#2481 , r#2482)))[@inline] in
let concat#644 =
  fun b1#2612 ->
  (fun b2#2613 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2612 , b2#2613))))[@inline] in
let sub#645 =
  fun s#2615 ->
  (fun l#2616 ->
   (fun b#2617 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2615 ,
                                                                   l#2616) ,
                                                              b#2617)))))[@inline] in
let length#646 = fun b#2619 -> (({ SIZE })@(b#2619))[@inline] in
let concat#649 =
  fun b1#2626 ->
  (fun b2#2627 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2626 , b2#2627))))[@inline] in
let sub#650 =
  fun s#2629 ->
  (fun l#2630 ->
   (fun b#2631 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2629 ,
                                                                   l#2630) ,
                                                              b#2631)))))[@inline] in
let length#653 = fun b#2637 -> (({ SIZE })@(b#2637))[@inline] in
let blake2b#654 = fun b#2639 -> (({ BLAKE2B })@(b#2639))[@inline] in
let sha256#655 = fun b#2641 -> (({ SHA256 })@(b#2641))[@inline] in
let sha512#656 = fun b#2643 -> (({ SHA512 })@(b#2643))[@inline] in
let sha3#657 = fun b#2645 -> (({ SHA3 })@(b#2645))[@inline] in
let keccak#658 = fun b#2647 -> (({ KECCAK })@(b#2647))[@inline] in
let hash_key#659 = fun k#2649 -> (({ HASH_KEY })@(k#2649))[@inline] in
let check#660 =
  fun k#2651 ->
  (fun s#2652 ->
   (fun b#2653 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#2651 , s#2652) ,
                                                   b#2653)))))[@inline] in
let assert#661 =
  fun b#2655 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#2655))[@inline] in
let assert_with_error#662 =
  fun b#2657 ->
  (fun s#2658 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#2657 , s#2658))))[@inline] in
let abs#667 = fun i#2670 -> (({ ABS })@(i#2670))[@inline] in
let is_nat#668 = fun i#2672 -> (({ ISNAT })@(i#2672))[@inline] in
let true#669 = TRUE()[@inline] in
let false#670 = FALSE()[@inline] in
let unit#671 = UNIT()[@inline] in
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
let originate_from_file#676 =
  fun _fn#2686 ->
  (fun _e#2687 ->
   (fun _v#2688 ->
    (fun _s#2689 -> (fun _t#2690 -> ((poly_failwith_60)@(L("TEST MODE")))))))[@inline] in
let set_source#678 =
  fun _a#2696 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let set_baker#679 =
  fun _a#2698 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let set_baker_policy#680 =
  fun _bp#2700 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let transfer#681 =
  fun _a#2702 ->
  (fun _s#2703 -> (fun _t#2704 -> ((poly_failwith_46)@(L("TEST MODE")))))[@inline] in
let transfer_exn#682 =
  fun _a#2706 ->
  (fun _s#2707 -> (fun _t#2708 -> ((poly_failwith_57)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#686 =
  fun _a#2720 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let get_balance#687 =
  fun _a#2722 -> ((poly_failwith_59)@(L("TEST MODE")))[@inline] in
let michelson_equal#688 =
  fun _m1#2724 -> (fun _m2#2725 -> ((poly_failwith_58)@(L("TEST MODE"))))[@inline] in
let reset_state#690 =
  fun _n#2729 -> (fun _l#2730 -> ((poly_failwith_46)@(L("TEST MODE"))))[@inline] in
let reset_state_at#691 =
  fun _t#2732 ->
  (fun _n#2733 -> (fun _l#2734 -> ((poly_failwith_46)@(L("TEST MODE")))))[@inline] in
let get_voting_power#692 =
  fun _kh#2736 -> ((poly_failwith_57)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#693 =
  (poly_failwith_57)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#695 =
  fun _i#2743 -> ((poly_failwith_51)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#696 =
  fun _i#2745 -> ((poly_failwith_51)@(L("TEST MODE")))[@inline] in
let last_originations#698 =
  fun _u#2749 -> ((poly_failwith_56)@(L("TEST MODE")))[@inline] in
let save_mutation#701 =
  fun _s#2756 -> (fun _m#2757 -> ((poly_failwith_47)@(L("TEST MODE"))))[@inline] in
let add_account#708 =
  fun _s#2774 -> (fun _k#2775 -> ((poly_failwith_46)@(L("TEST MODE"))))[@inline] in
let new_account#709 =
  fun _u#2777 -> ((poly_failwith_55)@(L("TEST MODE")))[@inline] in
let baker_account#710 =
  fun _p#2779 -> (fun _o#2780 -> ((poly_failwith_46)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#711 =
  fun _n#2782 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let register_delegate#712 =
  fun _kh#2784 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let register_constant#713 =
  fun _m#2786 -> ((poly_failwith_54)@(L("TEST MODE")))[@inline] in
let create_chest#718 =
  fun _b#2798 -> (fun _n#2799 -> ((poly_failwith_53)@(L("TEST MODE"))))[@inline] in
let create_chest_key#719 =
  fun _c#2801 -> (fun _n#2802 -> ((poly_failwith_52)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#720 =
  fun _s#2804 -> ((poly_failwith_46)@(L("TEST MODE")))[@inline] in
let restore_context#721 =
  fun _u#2806 -> ((poly_failwith_46)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#722 =
  fun _u#2808 -> ((poly_failwith_46)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#723 =
  fun _u#2810 -> ((poly_failwith_46)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#724 =
  fun _fn#2812 -> ((poly_failwith_46)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#725 =
  fun _fn#2814 ->
  (fun _e#2815 ->
   (fun _v#2816 ->
    ((poly_failwith_46)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#727 =
  fun _c#2820 ->
  (fun _s#2821 -> (fun _t#2822 -> ((poly_failwith_51)@(L("TEST_ORIGINATE")))))[@inline] in
let size#728 =
  fun _c#2824 -> ((poly_failwith_50)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#729 =
  fun _n#2826 -> ((poly_failwith_49)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#730 =
  fun _sk#2828 -> (fun _d#2829 -> ((poly_failwith_48)@(L("TEST_SIGN"))))[@inline] in
let chr#731 = fun _n#2831 -> ((poly_failwith_47)@(L("TEST_CHR")))[@inline] in
let nl#732 = L("NEWLINE")[@inline] in
let println#733 =
  fun _v#2834 -> ((poly_failwith_46)@(L("TEST_PRINTLN")))[@inline] in
let print#734 =
  fun _v#2836 -> ((poly_failwith_46)@(L("TEST_PRINT")))[@inline] in
let eprint#735 =
  fun _v#2838 -> ((poly_failwith_46)@(L("TEST_EPRINTL")))[@inline] in
let toto#737 = L(43) in
let balance#741 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#742 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#743 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#744 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#745 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#746 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#747 = SELF_ADDRESS()[@inline] in
let chain_id#748 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#749 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#750 =
  fun _u#2859 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#751 =
  fun _u#2861 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#752 = fun _u#2863 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#753 =
  fun _u#2865 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#754 =
  fun _u#2867 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#755 = fun _u#2869 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#756 = fun _u#2871 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#757 =
  fun _u#2873 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#758 =
  fun _u#2875 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#759 =
  fun kh#2877 -> (({ VOTING_POWER })@(kh#2877))[@inline] in
let implicit_account#761 =
  fun kh#2881 -> (IMPLICIT_ACCOUNT(kh#2881))[@inline] in
let pairing_check#767 =
  fun l#2895 -> (({ PAIRING_CHECK })@(l#2895))[@inline] in
let open_chest#768 =
  fun ck#2897 ->
  (fun c#2898 -> (fun n#2899 -> (OPEN_CHEST(ck#2897 , c#2898 , n#2899))))[@inline] in
let set_delegate#772 = fun o#2911 -> (SET_DELEGATE(o#2911))[@inline] in
let xor#773 =
  fun l#2913 -> (fun r#2914 -> (XOR(l#2913 , r#2914)))[@inline] in
let shift_left#774 =
  fun l#2916 -> (fun r#2917 -> (LSL(l#2916 , r#2917)))[@inline] in
let shift_right#775 =
  fun l#2919 -> (fun r#2920 -> (LSR(l#2919 , r#2920)))[@inline] in
let concat#816 =
  fun b1#3050 ->
  (fun b2#3051 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3050 , b2#3051))))[@inline] in
let sub#817 =
  fun s#3053 ->
  (fun l#3054 ->
   (fun b#3055 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3053 ,
                                                                   l#3054) ,
                                                              b#3055)))))[@inline] in
let length#818 = fun b#3057 -> (({ SIZE })@(b#3057))[@inline] in
let concat#821 =
  fun b1#3064 ->
  (fun b2#3065 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3064 , b2#3065))))[@inline] in
let sub#822 =
  fun s#3067 ->
  (fun l#3068 ->
   (fun b#3069 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3067 ,
                                                                   l#3068) ,
                                                              b#3069)))))[@inline] in
let length#825 = fun b#3075 -> (({ SIZE })@(b#3075))[@inline] in
let blake2b#826 = fun b#3077 -> (({ BLAKE2B })@(b#3077))[@inline] in
let sha256#827 = fun b#3079 -> (({ SHA256 })@(b#3079))[@inline] in
let sha512#828 = fun b#3081 -> (({ SHA512 })@(b#3081))[@inline] in
let sha3#829 = fun b#3083 -> (({ SHA3 })@(b#3083))[@inline] in
let keccak#830 = fun b#3085 -> (({ KECCAK })@(b#3085))[@inline] in
let hash_key#831 = fun k#3087 -> (({ HASH_KEY })@(k#3087))[@inline] in
let check#832 =
  fun k#3089 ->
  (fun s#3090 ->
   (fun b#3091 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#3089 , s#3090) ,
                                                   b#3091)))))[@inline] in
let assert#833 =
  fun b#3093 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#3093))[@inline] in
let assert_with_error#834 =
  fun b#3095 ->
  (fun s#3096 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#3095 , s#3096))))[@inline] in
let abs#839 = fun i#3108 -> (({ ABS })@(i#3108))[@inline] in
let is_nat#840 = fun i#3110 -> (({ ISNAT })@(i#3110))[@inline] in
let true#841 = TRUE()[@inline] in
let false#842 = FALSE()[@inline] in
let unit#843 = UNIT()[@inline] in
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
let originate_from_file#848 =
  fun _fn#3124 ->
  (fun _e#3125 ->
   (fun _v#3126 ->
    (fun _s#3127 -> (fun _t#3128 -> ((poly_failwith_45)@(L("TEST MODE")))))))[@inline] in
let set_source#850 =
  fun _a#3134 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let set_baker#851 =
  fun _a#3136 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let set_baker_policy#852 =
  fun _bp#3138 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let transfer#853 =
  fun _a#3140 ->
  (fun _s#3141 -> (fun _t#3142 -> ((poly_failwith_31)@(L("TEST MODE")))))[@inline] in
let transfer_exn#854 =
  fun _a#3144 ->
  (fun _s#3145 -> (fun _t#3146 -> ((poly_failwith_42)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#858 =
  fun _a#3158 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let get_balance#859 =
  fun _a#3160 -> ((poly_failwith_44)@(L("TEST MODE")))[@inline] in
let michelson_equal#860 =
  fun _m1#3162 -> (fun _m2#3163 -> ((poly_failwith_43)@(L("TEST MODE"))))[@inline] in
let reset_state#862 =
  fun _n#3167 -> (fun _l#3168 -> ((poly_failwith_31)@(L("TEST MODE"))))[@inline] in
let reset_state_at#863 =
  fun _t#3170 ->
  (fun _n#3171 -> (fun _l#3172 -> ((poly_failwith_31)@(L("TEST MODE")))))[@inline] in
let get_voting_power#864 =
  fun _kh#3174 -> ((poly_failwith_42)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#865 =
  (poly_failwith_42)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#867 =
  fun _i#3181 -> ((poly_failwith_36)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#868 =
  fun _i#3183 -> ((poly_failwith_36)@(L("TEST MODE")))[@inline] in
let last_originations#870 =
  fun _u#3187 -> ((poly_failwith_41)@(L("TEST MODE")))[@inline] in
let save_mutation#873 =
  fun _s#3194 -> (fun _m#3195 -> ((poly_failwith_32)@(L("TEST MODE"))))[@inline] in
let add_account#880 =
  fun _s#3212 -> (fun _k#3213 -> ((poly_failwith_31)@(L("TEST MODE"))))[@inline] in
let new_account#881 =
  fun _u#3215 -> ((poly_failwith_40)@(L("TEST MODE")))[@inline] in
let baker_account#882 =
  fun _p#3217 -> (fun _o#3218 -> ((poly_failwith_31)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#883 =
  fun _n#3220 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let register_delegate#884 =
  fun _kh#3222 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let register_constant#885 =
  fun _m#3224 -> ((poly_failwith_39)@(L("TEST MODE")))[@inline] in
let create_chest#890 =
  fun _b#3236 -> (fun _n#3237 -> ((poly_failwith_38)@(L("TEST MODE"))))[@inline] in
let create_chest_key#891 =
  fun _c#3239 -> (fun _n#3240 -> ((poly_failwith_37)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#892 =
  fun _s#3242 -> ((poly_failwith_31)@(L("TEST MODE")))[@inline] in
let restore_context#893 =
  fun _u#3244 -> ((poly_failwith_31)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#894 =
  fun _u#3246 -> ((poly_failwith_31)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#895 =
  fun _u#3248 -> ((poly_failwith_31)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#896 =
  fun _fn#3250 -> ((poly_failwith_31)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#897 =
  fun _fn#3252 ->
  (fun _e#3253 ->
   (fun _v#3254 ->
    ((poly_failwith_31)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#899 =
  fun _c#3258 ->
  (fun _s#3259 -> (fun _t#3260 -> ((poly_failwith_36)@(L("TEST_ORIGINATE")))))[@inline] in
let size#900 =
  fun _c#3262 -> ((poly_failwith_35)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#901 =
  fun _n#3264 -> ((poly_failwith_34)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#902 =
  fun _sk#3266 -> (fun _d#3267 -> ((poly_failwith_33)@(L("TEST_SIGN"))))[@inline] in
let chr#903 = fun _n#3269 -> ((poly_failwith_32)@(L("TEST_CHR")))[@inline] in
let nl#904 = L("NEWLINE")[@inline] in
let println#905 =
  fun _v#3272 -> ((poly_failwith_31)@(L("TEST_PRINTLN")))[@inline] in
let print#906 =
  fun _v#3274 -> ((poly_failwith_31)@(L("TEST_PRINT")))[@inline] in
let eprint#907 =
  fun _v#3276 -> ((poly_failwith_31)@(L("TEST_EPRINTL")))[@inline] in
let tata#909 = ADD(toto#17 , titi#392) in
let foo#910 = (f#393)@(PAIR(L(unit) , L(3))) in
let balance#914 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#915 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#916 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#917 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#918 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#919 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#920 = SELF_ADDRESS()[@inline] in
let chain_id#921 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#922 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#923 =
  fun _u#3298 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#924 =
  fun _u#3300 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#925 = fun _u#3302 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#926 =
  fun _u#3304 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#927 =
  fun _u#3306 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#928 = fun _u#3308 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#929 = fun _u#3310 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#930 =
  fun _u#3312 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#931 =
  fun _u#3314 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#932 =
  fun kh#3316 -> (({ VOTING_POWER })@(kh#3316))[@inline] in
let implicit_account#934 =
  fun kh#3320 -> (IMPLICIT_ACCOUNT(kh#3320))[@inline] in
let pairing_check#940 =
  fun l#3334 -> (({ PAIRING_CHECK })@(l#3334))[@inline] in
let open_chest#941 =
  fun ck#3336 ->
  (fun c#3337 -> (fun n#3338 -> (OPEN_CHEST(ck#3336 , c#3337 , n#3338))))[@inline] in
let set_delegate#945 = fun o#3350 -> (SET_DELEGATE(o#3350))[@inline] in
let xor#946 =
  fun l#3352 -> (fun r#3353 -> (XOR(l#3352 , r#3353)))[@inline] in
let shift_left#947 =
  fun l#3355 -> (fun r#3356 -> (LSL(l#3355 , r#3356)))[@inline] in
let shift_right#948 =
  fun l#3358 -> (fun r#3359 -> (LSR(l#3358 , r#3359)))[@inline] in
let concat#989 =
  fun b1#3489 ->
  (fun b2#3490 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3489 , b2#3490))))[@inline] in
let sub#990 =
  fun s#3492 ->
  (fun l#3493 ->
   (fun b#3494 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3492 ,
                                                                   l#3493) ,
                                                              b#3494)))))[@inline] in
let length#991 = fun b#3496 -> (({ SIZE })@(b#3496))[@inline] in
let concat#994 =
  fun b1#3503 ->
  (fun b2#3504 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3503 , b2#3504))))[@inline] in
let sub#995 =
  fun s#3506 ->
  (fun l#3507 ->
   (fun b#3508 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3506 ,
                                                                   l#3507) ,
                                                              b#3508)))))[@inline] in
let length#998 = fun b#3514 -> (({ SIZE })@(b#3514))[@inline] in
let blake2b#999 = fun b#3516 -> (({ BLAKE2B })@(b#3516))[@inline] in
let sha256#1000 = fun b#3518 -> (({ SHA256 })@(b#3518))[@inline] in
let sha512#1001 = fun b#3520 -> (({ SHA512 })@(b#3520))[@inline] in
let sha3#1002 = fun b#3522 -> (({ SHA3 })@(b#3522))[@inline] in
let keccak#1003 = fun b#3524 -> (({ KECCAK })@(b#3524))[@inline] in
let hash_key#1004 = fun k#3526 -> (({ HASH_KEY })@(k#3526))[@inline] in
let check#1005 =
  fun k#3528 ->
  (fun s#3529 ->
   (fun b#3530 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#3528 , s#3529) ,
                                                   b#3530)))))[@inline] in
let assert#1006 =
  fun b#3532 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#3532))[@inline] in
let assert_with_error#1007 =
  fun b#3534 ->
  (fun s#3535 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#3534 , s#3535))))[@inline] in
let abs#1012 = fun i#3547 -> (({ ABS })@(i#3547))[@inline] in
let is_nat#1013 = fun i#3549 -> (({ ISNAT })@(i#3549))[@inline] in
let true#1014 = TRUE()[@inline] in
let false#1015 = FALSE()[@inline] in
let unit#1016 = UNIT()[@inline] in
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
let originate_from_file#1021 =
  fun _fn#3563 ->
  (fun _e#3564 ->
   (fun _v#3565 ->
    (fun _s#3566 -> (fun _t#3567 -> ((poly_failwith_30)@(L("TEST MODE")))))))[@inline] in
let set_source#1023 =
  fun _a#3573 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let set_baker#1024 =
  fun _a#3575 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let set_baker_policy#1025 =
  fun _bp#3577 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let transfer#1026 =
  fun _a#3579 ->
  (fun _s#3580 -> (fun _t#3581 -> ((poly_failwith_16)@(L("TEST MODE")))))[@inline] in
let transfer_exn#1027 =
  fun _a#3583 ->
  (fun _s#3584 -> (fun _t#3585 -> ((poly_failwith_27)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#1031 =
  fun _a#3597 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let get_balance#1032 =
  fun _a#3599 -> ((poly_failwith_29)@(L("TEST MODE")))[@inline] in
let michelson_equal#1033 =
  fun _m1#3601 -> (fun _m2#3602 -> ((poly_failwith_28)@(L("TEST MODE"))))[@inline] in
let reset_state#1035 =
  fun _n#3606 -> (fun _l#3607 -> ((poly_failwith_16)@(L("TEST MODE"))))[@inline] in
let reset_state_at#1036 =
  fun _t#3609 ->
  (fun _n#3610 -> (fun _l#3611 -> ((poly_failwith_16)@(L("TEST MODE")))))[@inline] in
let get_voting_power#1037 =
  fun _kh#3613 -> ((poly_failwith_27)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#1038 =
  (poly_failwith_27)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#1040 =
  fun _i#3620 -> ((poly_failwith_21)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#1041 =
  fun _i#3622 -> ((poly_failwith_21)@(L("TEST MODE")))[@inline] in
let last_originations#1043 =
  fun _u#3626 -> ((poly_failwith_26)@(L("TEST MODE")))[@inline] in
let save_mutation#1046 =
  fun _s#3633 -> (fun _m#3634 -> ((poly_failwith_17)@(L("TEST MODE"))))[@inline] in
let add_account#1053 =
  fun _s#3651 -> (fun _k#3652 -> ((poly_failwith_16)@(L("TEST MODE"))))[@inline] in
let new_account#1054 =
  fun _u#3654 -> ((poly_failwith_25)@(L("TEST MODE")))[@inline] in
let baker_account#1055 =
  fun _p#3656 -> (fun _o#3657 -> ((poly_failwith_16)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#1056 =
  fun _n#3659 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let register_delegate#1057 =
  fun _kh#3661 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let register_constant#1058 =
  fun _m#3663 -> ((poly_failwith_24)@(L("TEST MODE")))[@inline] in
let create_chest#1063 =
  fun _b#3675 -> (fun _n#3676 -> ((poly_failwith_23)@(L("TEST MODE"))))[@inline] in
let create_chest_key#1064 =
  fun _c#3678 -> (fun _n#3679 -> ((poly_failwith_22)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#1065 =
  fun _s#3681 -> ((poly_failwith_16)@(L("TEST MODE")))[@inline] in
let restore_context#1066 =
  fun _u#3683 -> ((poly_failwith_16)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#1067 =
  fun _u#3685 -> ((poly_failwith_16)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#1068 =
  fun _u#3687 -> ((poly_failwith_16)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#1069 =
  fun _fn#3689 -> ((poly_failwith_16)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#1070 =
  fun _fn#3691 ->
  (fun _e#3692 ->
   (fun _v#3693 ->
    ((poly_failwith_16)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#1072 =
  fun _c#3697 ->
  (fun _s#3698 -> (fun _t#3699 -> ((poly_failwith_21)@(L("TEST_ORIGINATE")))))[@inline] in
let size#1073 =
  fun _c#3701 -> ((poly_failwith_20)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#1074 =
  fun _n#3703 -> ((poly_failwith_19)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#1075 =
  fun _sk#3705 -> (fun _d#3706 -> ((poly_failwith_18)@(L("TEST_SIGN"))))[@inline] in
let chr#1076 =
  fun _n#3708 -> ((poly_failwith_17)@(L("TEST_CHR")))[@inline] in
let nl#1077 = L("NEWLINE")[@inline] in
let println#1078 =
  fun _v#3711 -> ((poly_failwith_16)@(L("TEST_PRINTLN")))[@inline] in
let print#1079 =
  fun _v#3713 -> ((poly_failwith_16)@(L("TEST_PRINT")))[@inline] in
let eprint#1080 =
  fun _v#3715 -> ((poly_failwith_16)@(L("TEST_EPRINTL")))[@inline] in
let toto#1082 = L(10) in
let foo#1083 = L("bar") in
let balance#11 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#12 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#13 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#14 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#15 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#16 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#17 = SELF_ADDRESS()[@inline] in
let chain_id#18 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#19 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#20 =
  fun _u#3737 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#21 =
  fun _u#3739 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#22 = fun _u#3741 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#23 =
  fun _u#3743 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#24 =
  fun _u#3745 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#25 = fun _u#3747 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#26 = fun _u#3749 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#27 =
  fun _u#3751 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#28 =
  fun _u#3753 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let voting_power#29 =
  fun kh#3755 -> (({ VOTING_POWER })@(kh#3755))[@inline] in
let implicit_account#31 =
  fun kh#3759 -> (IMPLICIT_ACCOUNT(kh#3759))[@inline] in
let pairing_check#37 =
  fun l#3773 -> (({ PAIRING_CHECK })@(l#3773))[@inline] in
let open_chest#38 =
  fun ck#3775 ->
  (fun c#3776 -> (fun n#3777 -> (OPEN_CHEST(ck#3775 , c#3776 , n#3777))))[@inline] in
let set_delegate#42 = fun o#3789 -> (SET_DELEGATE(o#3789))[@inline] in
let xor#43 = fun l#3791 -> (fun r#3792 -> (XOR(l#3791 , r#3792)))[@inline] in
let shift_left#44 =
  fun l#3794 -> (fun r#3795 -> (LSL(l#3794 , r#3795)))[@inline] in
let shift_right#45 =
  fun l#3797 -> (fun r#3798 -> (LSR(l#3797 , r#3798)))[@inline] in
let concat#86 =
  fun b1#3928 ->
  (fun b2#3929 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3928 , b2#3929))))[@inline] in
let sub#87 =
  fun s#3931 ->
  (fun l#3932 ->
   (fun b#3933 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3931 ,
                                                                   l#3932) ,
                                                              b#3933)))))[@inline] in
let length#88 = fun b#3935 -> (({ SIZE })@(b#3935))[@inline] in
let concat#91 =
  fun b1#3942 ->
  (fun b2#3943 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3942 , b2#3943))))[@inline] in
let sub#92 =
  fun s#3945 ->
  (fun l#3946 ->
   (fun b#3947 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3945 ,
                                                                   l#3946) ,
                                                              b#3947)))))[@inline] in
let length#95 = fun b#3953 -> (({ SIZE })@(b#3953))[@inline] in
let blake2b#96 = fun b#3955 -> (({ BLAKE2B })@(b#3955))[@inline] in
let sha256#97 = fun b#3957 -> (({ SHA256 })@(b#3957))[@inline] in
let sha512#98 = fun b#3959 -> (({ SHA512 })@(b#3959))[@inline] in
let sha3#99 = fun b#3961 -> (({ SHA3 })@(b#3961))[@inline] in
let keccak#100 = fun b#3963 -> (({ KECCAK })@(b#3963))[@inline] in
let hash_key#101 = fun k#3965 -> (({ HASH_KEY })@(k#3965))[@inline] in
let check#102 =
  fun k#3967 ->
  (fun s#3968 ->
   (fun b#3969 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#3967 , s#3968) ,
                                                   b#3969)))))[@inline] in
let assert =
  fun b#3971 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#3971))[@inline] in
let assert_with_error =
  fun b#3973 ->
  (fun s#3974 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#3973 , s#3974))))[@inline] in
let abs = fun i#3986 -> (({ ABS })@(i#3986))[@inline] in
let is_nat = fun i#3988 -> (({ ISNAT })@(i#3988))[@inline] in
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
let originate_from_file#104 =
  fun _fn#4002 ->
  (fun _e#4003 ->
   (fun _v#4004 ->
    (fun _s#4005 -> (fun _t#4006 -> ((poly_failwith_15)@(L("TEST MODE")))))))[@inline] in
let set_source#106 =
  fun _a#4012 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let set_baker#107 =
  fun _a#4014 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let set_baker_policy#108 =
  fun _bp#4016 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let transfer#109 =
  fun _a#4018 ->
  (fun _s#4019 -> (fun _t#4020 -> ((poly_failwith_1)@(L("TEST MODE")))))[@inline] in
let transfer_exn#110 =
  fun _a#4022 ->
  (fun _s#4023 -> (fun _t#4024 -> ((poly_failwith_12)@(L("TEST MODE")))))[@inline] in
let get_storage_of_address#114 =
  fun _a#4036 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let get_balance#115 =
  fun _a#4038 -> ((poly_failwith_14)@(L("TEST MODE")))[@inline] in
let michelson_equal#116 =
  fun _m1#4040 -> (fun _m2#4041 -> ((poly_failwith_13)@(L("TEST MODE"))))[@inline] in
let reset_state#118 =
  fun _n#4045 -> (fun _l#4046 -> ((poly_failwith_1)@(L("TEST MODE"))))[@inline] in
let reset_state_at#119 =
  fun _t#4048 ->
  (fun _n#4049 -> (fun _l#4050 -> ((poly_failwith_1)@(L("TEST MODE")))))[@inline] in
let get_voting_power#120 =
  fun _kh#4052 -> ((poly_failwith_12)@(L("TEST MODE")))[@inline] in
let get_total_voting_power#121 =
  (poly_failwith_12)@(L("TEST MODE"))[@inline] in
let nth_bootstrap_contract#123 =
  fun _i#4059 -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
let nth_bootstrap_account#124 =
  fun _i#4061 -> ((poly_failwith_6)@(L("TEST MODE")))[@inline] in
let last_originations#126 =
  fun _u#4065 -> ((poly_failwith_11)@(L("TEST MODE")))[@inline] in
let save_mutation#129 =
  fun _s#4072 -> (fun _m#4073 -> ((poly_failwith_2)@(L("TEST MODE"))))[@inline] in
let add_account#136 =
  fun _s#4090 -> (fun _k#4091 -> ((poly_failwith_1)@(L("TEST MODE"))))[@inline] in
let new_account#137 =
  fun _u#4093 -> ((poly_failwith_10)@(L("TEST MODE")))[@inline] in
let baker_account#138 =
  fun _p#4095 -> (fun _o#4096 -> ((poly_failwith_1)@(L("TEST MODE"))))[@inline] in
let bake_until_n_cycle_end#139 =
  fun _n#4098 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let register_delegate#140 =
  fun _kh#4100 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let register_constant#141 =
  fun _m#4102 -> ((poly_failwith_9)@(L("TEST MODE")))[@inline] in
let create_chest#146 =
  fun _b#4114 -> (fun _n#4115 -> ((poly_failwith_8)@(L("TEST MODE"))))[@inline] in
let create_chest_key#147 =
  fun _c#4117 -> (fun _n#4118 -> ((poly_failwith_7)@(L("TEST MODE"))))[@inline] in
let constant_to_michelson_program#148 =
  fun _s#4120 -> ((poly_failwith_1)@(L("TEST MODE")))[@inline] in
let restore_context#149 =
  fun _u#4122 -> ((poly_failwith_1)@(L("TEST_POP_CONTEXT")))[@inline] in
let save_context#150 =
  fun _u#4124 -> ((poly_failwith_1)@(L("TEST_PUSH_CONTEXT")))[@inline] in
let drop_context#151 =
  fun _u#4126 -> ((poly_failwith_1)@(L("TEST_DROP_CONTEXT")))[@inline] in
let read_contract_from_file#152 =
  fun _fn#4128 -> ((poly_failwith_1)@(L("TEST_READ_CONTRACT_FROM_FILE")))[@inline] in
let compile_contract_from_file#153 =
  fun _fn#4130 ->
  (fun _e#4131 ->
   (fun _v#4132 -> ((poly_failwith_1)@(L("TEST_COMPILE_CONTRACT_FROM_FILE")))))[@inline] in
let originate_contract#155 =
  fun _c#4136 ->
  (fun _s#4137 -> (fun _t#4138 -> ((poly_failwith_6)@(L("TEST_ORIGINATE")))))[@inline] in
let size#156 =
  fun _c#4140 -> ((poly_failwith_5)@(L("TEST_SIZE")))[@inline] in
let get_bootstrap_account#157 =
  fun _n#4142 -> ((poly_failwith_4)@(L("TEST_GET_BOOTSTRAP_ACCOUNT")))[@inline] in
let sign#158 =
  fun _sk#4144 -> (fun _d#4145 -> ((poly_failwith_3)@(L("TEST_SIGN"))))[@inline] in
let chr#159 = fun _n#4147 -> ((poly_failwith_2)@(L("TEST_CHR")))[@inline] in
let nl#160 = L("NEWLINE")[@inline] in
let println#161 =
  fun _v#4150 -> ((poly_failwith_1)@(L("TEST_PRINTLN")))[@inline] in
let print#162 =
  fun _v#4152 -> ((poly_failwith_1)@(L("TEST_PRINT")))[@inline] in
let eprint#163 =
  fun _v#4154 -> ((poly_failwith_1)@(L("TEST_EPRINTL")))[@inline] in
let toto = ADD(toto#1082 , toto#17) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#4160 ->
  (let (gen#4166, gen#4167) = gen#4160 in
   let p#4161 = gen#4166 in
   let s#4162 = gen#4167 in
   let s#4163 = ADD(ADD(p#4161 , s#4162) , toto) in
   PAIR(LIST_EMPTY() , s#4163)) in
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
