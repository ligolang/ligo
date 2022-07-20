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
  fun _u#1280 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#59 =
  fun _u#1282 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#60 = fun _u#1284 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#61 =
  fun _u#1286 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#62 =
  fun _u#1288 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#63 = fun _u#1290 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#64 = fun _u#1292 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#65 =
  fun _u#1294 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#66 =
  fun _u#1296 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#67 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#68 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#69 =
  fun kh#1300 -> (({ VOTING_POWER })@(kh#1300))[@inline] in
let implicit_account#71 =
  fun kh#1304 -> (IMPLICIT_ACCOUNT(kh#1304))[@inline] in
let pairing_check#75 =
  fun l#1312 -> (({ PAIRING_CHECK })@(l#1312))[@inline] in
let set_delegate#77 = fun o#1316 -> (SET_DELEGATE(o#1316))[@inline] in
let open_chest#83 =
  fun ck#1332 ->
  (fun c#1333 -> (fun n#1334 -> (OPEN_CHEST(ck#1332 , c#1333 , n#1334))))[@inline] in
let xor#86 = fun l#1343 -> (fun r#1344 -> (XOR(l#1343 , r#1344)))[@inline] in
let shift_left#87 =
  fun l#1346 -> (fun r#1347 -> (LSL(l#1346 , r#1347)))[@inline] in
let shift_right#88 =
  fun l#1349 -> (fun r#1350 -> (LSR(l#1349 , r#1350)))[@inline] in
let length#129 = fun b#1480 -> (({ SIZE })@(b#1480))[@inline] in
let concat#130 =
  fun b1#1482 ->
  (fun b2#1483 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1482 , b2#1483))))[@inline] in
let sub#131 =
  fun s#1485 ->
  (fun l#1486 ->
   (fun b#1487 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1485 ,
                                                                   l#1486) ,
                                                              b#1487)))))[@inline] in
let length#136 = fun b#1498 -> (({ SIZE })@(b#1498))[@inline] in
let concat#137 =
  fun b1#1500 ->
  (fun b2#1501 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1500 , b2#1501))))[@inline] in
let sub#138 =
  fun s#1503 ->
  (fun l#1504 ->
   (fun b#1505 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1503 ,
                                                                   l#1504) ,
                                                              b#1505)))))[@inline] in
let blake2b#139 = fun b#1507 -> (({ BLAKE2B })@(b#1507))[@inline] in
let sha256#140 = fun b#1509 -> (({ SHA256 })@(b#1509))[@inline] in
let sha512#141 = fun b#1511 -> (({ SHA512 })@(b#1511))[@inline] in
let sha3#142 = fun b#1513 -> (({ SHA3 })@(b#1513))[@inline] in
let keccak#143 = fun b#1515 -> (({ KECCAK })@(b#1515))[@inline] in
let hash_key#144 = fun k#1517 -> (({ HASH_KEY })@(k#1517))[@inline] in
let check#145 =
  fun k#1519 ->
  (fun s#1520 ->
   (fun b#1521 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1519 , s#1520) ,
                                                   b#1521)))))[@inline] in
let assert#146 =
  fun b#1523 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1523))[@inline] in
let abs#149 = fun i#1529 -> (({ ABS })@(i#1529))[@inline] in
let is_nat#150 = fun i#1531 -> (({ ISNAT })@(i#1531))[@inline] in
let true#151 = TRUE()[@inline] in
let false#152 = FALSE()[@inline] in
let unit#153 = UNIT()[@inline] in
let assert_with_error#156 =
  fun b#1539 ->
  (fun s#1540 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1539 , s#1540))))[@inline] in
let poly_stub_105 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_104 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_103 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_102 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_101 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_100 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_99 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_98 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_97 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_96 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_95 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_94 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_93 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_92 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let poly_stub_91 = fun x#1551 -> (({ FAILWITH })@(x#1551))[@inline] in
let get_total_voting_power#164 = (poly_stub_99)@(L(unit))[@inline] in
let set_source#167 = fun _a#1565 -> ((poly_stub_92)@(L(unit)))[@inline] in
let get_storage_of_address#168 =
  fun _a#1567 -> ((poly_stub_92)@(L(unit)))[@inline] in
let get_balance#169 = fun _a#1569 -> ((poly_stub_105)@(L(unit)))[@inline] in
let print#170 = fun _v#1571 -> ((poly_stub_92)@(L(unit)))[@inline] in
let eprint#171 = fun _v#1573 -> ((poly_stub_92)@(L(unit)))[@inline] in
let get_voting_power#172 =
  fun _kh#1575 -> ((poly_stub_99)@(L(unit)))[@inline] in
let nth_bootstrap_contract#173 =
  fun _i#1577 -> ((poly_stub_93)@(L(unit)))[@inline] in
let nth_bootstrap_account#174 =
  fun _i#1579 -> ((poly_stub_93)@(L(unit)))[@inline] in
let get_bootstrap_account#175 =
  fun _n#1581 -> ((poly_stub_104)@(L(unit)))[@inline] in
let last_originations#177 =
  fun _u#1585 -> ((poly_stub_103)@(L(unit)))[@inline] in
let new_account#179 = fun _u#1589 -> ((poly_stub_102)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#181 =
  fun _n#1593 -> ((poly_stub_92)@(L(unit)))[@inline] in
let register_delegate#183 =
  fun _kh#1597 -> ((poly_stub_92)@(L(unit)))[@inline] in
let register_constant#184 =
  fun _m#1599 -> ((poly_stub_101)@(L(unit)))[@inline] in
let constant_to_michelson_program#186 =
  fun _s#1603 -> ((poly_stub_92)@(L(unit)))[@inline] in
let restore_context#187 =
  fun _u#1605 -> ((poly_stub_92)@(L(unit)))[@inline] in
let save_context#188 = fun _u#1607 -> ((poly_stub_92)@(L(unit)))[@inline] in
let drop_context#189 = fun _u#1609 -> ((poly_stub_92)@(L(unit)))[@inline] in
let set_baker_policy#192 =
  fun _bp#1615 -> ((poly_stub_92)@(L(unit)))[@inline] in
let set_baker#193 = fun _a#1617 -> ((poly_stub_92)@(L(unit)))[@inline] in
let size#194 = fun _c#1619 -> ((poly_stub_100)@(L(unit)))[@inline] in
let read_contract_from_file#196 =
  fun _fn#1623 -> ((poly_stub_92)@(L(unit)))[@inline] in
let chr#197 = fun _n#1625 -> ((poly_stub_98)@(L(unit)))[@inline] in
let nl#198 = L("NEWLINE")[@inline] in
let println#199 = fun _v#1628 -> ((poly_stub_92)@(L(unit)))[@inline] in
let transfer#200 =
  fun _a#1630 -> (fun _s#1631 -> (fun _t#1632 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#201 =
  fun _a#1634 -> (fun _s#1635 -> (fun _t#1636 -> ((poly_stub_99)@(L(unit)))))[@inline] in
let reset_state#203 =
  fun _n#1640 -> (fun _l#1641 -> ((poly_stub_92)@(L(unit))))[@inline] in
let reset_state_at#204 =
  fun _t#1643 -> (fun _n#1644 -> (fun _l#1645 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let save_mutation#207 =
  fun _s#1654 -> (fun _m#1655 -> ((poly_stub_98)@(L(unit))))[@inline] in
let sign#210 =
  fun _sk#1663 -> (fun _d#1664 -> ((poly_stub_97)@(L(unit))))[@inline] in
let add_account#211 =
  fun _s#1666 -> (fun _k#1667 -> ((poly_stub_92)@(L(unit))))[@inline] in
let baker_account#212 =
  fun _p#1669 -> (fun _o#1670 -> ((poly_stub_92)@(L(unit))))[@inline] in
let create_chest#214 =
  fun _b#1675 -> (fun _n#1676 -> ((poly_stub_96)@(L(unit))))[@inline] in
let create_chest_key#215 =
  fun _c#1678 -> (fun _n#1679 -> ((poly_stub_95)@(L(unit))))[@inline] in
let michelson_equal#218 =
  fun _m1#1689 -> (fun _m2#1690 -> ((poly_stub_94)@(L(unit))))[@inline] in
let originate_contract#220 =
  fun _c#1695 -> (fun _s#1696 -> (fun _t#1697 -> ((poly_stub_93)@(L(unit)))))[@inline] in
let compile_contract_from_file#222 =
  fun _fn#1703 ->
  (fun _e#1704 -> (fun _v#1705 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let originate_from_file#223 =
  fun _fn#1707 ->
  (fun _e#1708 ->
   (fun _v#1709 ->
    (fun _s#1710 -> (fun _t#1711 -> ((poly_stub_91)@(L(unit)))))))[@inline] in
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
  fun _u#1723 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#235 =
  fun _u#1725 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#236 = fun _u#1727 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#237 =
  fun _u#1729 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#238 =
  fun _u#1731 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#239 = fun _u#1733 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#240 = fun _u#1735 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#241 =
  fun _u#1737 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#242 =
  fun _u#1739 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#243 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#244 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#245 =
  fun kh#1743 -> (({ VOTING_POWER })@(kh#1743))[@inline] in
let implicit_account#247 =
  fun kh#1747 -> (IMPLICIT_ACCOUNT(kh#1747))[@inline] in
let pairing_check#251 =
  fun l#1755 -> (({ PAIRING_CHECK })@(l#1755))[@inline] in
let set_delegate#253 = fun o#1759 -> (SET_DELEGATE(o#1759))[@inline] in
let open_chest#259 =
  fun ck#1775 ->
  (fun c#1776 -> (fun n#1777 -> (OPEN_CHEST(ck#1775 , c#1776 , n#1777))))[@inline] in
let xor#262 =
  fun l#1786 -> (fun r#1787 -> (XOR(l#1786 , r#1787)))[@inline] in
let shift_left#263 =
  fun l#1789 -> (fun r#1790 -> (LSL(l#1789 , r#1790)))[@inline] in
let shift_right#264 =
  fun l#1792 -> (fun r#1793 -> (LSR(l#1792 , r#1793)))[@inline] in
let length#305 = fun b#1923 -> (({ SIZE })@(b#1923))[@inline] in
let concat#306 =
  fun b1#1925 ->
  (fun b2#1926 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1925 , b2#1926))))[@inline] in
let sub#307 =
  fun s#1928 ->
  (fun l#1929 ->
   (fun b#1930 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1928 ,
                                                                   l#1929) ,
                                                              b#1930)))))[@inline] in
let length#312 = fun b#1941 -> (({ SIZE })@(b#1941))[@inline] in
let concat#313 =
  fun b1#1943 ->
  (fun b2#1944 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1943 , b2#1944))))[@inline] in
let sub#314 =
  fun s#1946 ->
  (fun l#1947 ->
   (fun b#1948 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1946 ,
                                                                   l#1947) ,
                                                              b#1948)))))[@inline] in
let blake2b#315 = fun b#1950 -> (({ BLAKE2B })@(b#1950))[@inline] in
let sha256#316 = fun b#1952 -> (({ SHA256 })@(b#1952))[@inline] in
let sha512#317 = fun b#1954 -> (({ SHA512 })@(b#1954))[@inline] in
let sha3#318 = fun b#1956 -> (({ SHA3 })@(b#1956))[@inline] in
let keccak#319 = fun b#1958 -> (({ KECCAK })@(b#1958))[@inline] in
let hash_key#320 = fun k#1960 -> (({ HASH_KEY })@(k#1960))[@inline] in
let check#321 =
  fun k#1962 ->
  (fun s#1963 ->
   (fun b#1964 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1962 , s#1963) ,
                                                   b#1964)))))[@inline] in
let assert#322 =
  fun b#1966 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1966))[@inline] in
let abs#325 = fun i#1972 -> (({ ABS })@(i#1972))[@inline] in
let is_nat#326 = fun i#1974 -> (({ ISNAT })@(i#1974))[@inline] in
let true#327 = TRUE()[@inline] in
let false#328 = FALSE()[@inline] in
let unit#329 = UNIT()[@inline] in
let assert_with_error#332 =
  fun b#1982 ->
  (fun s#1983 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1982 , s#1983))))[@inline] in
let poly_stub_90 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_89 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_88 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_87 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_86 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_85 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_84 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_83 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_82 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_81 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_80 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_79 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_78 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_77 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let poly_stub_76 = fun x#1994 -> (({ FAILWITH })@(x#1994))[@inline] in
let get_total_voting_power#340 = (poly_stub_84)@(L(unit))[@inline] in
let set_source#343 = fun _a#2008 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#344 =
  fun _a#2010 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_balance#345 = fun _a#2012 -> ((poly_stub_90)@(L(unit)))[@inline] in
let print#346 = fun _v#2014 -> ((poly_stub_77)@(L(unit)))[@inline] in
let eprint#347 = fun _v#2016 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_voting_power#348 =
  fun _kh#2018 -> ((poly_stub_84)@(L(unit)))[@inline] in
let nth_bootstrap_contract#349 =
  fun _i#2020 -> ((poly_stub_78)@(L(unit)))[@inline] in
let nth_bootstrap_account#350 =
  fun _i#2022 -> ((poly_stub_78)@(L(unit)))[@inline] in
let get_bootstrap_account#351 =
  fun _n#2024 -> ((poly_stub_89)@(L(unit)))[@inline] in
let last_originations#353 =
  fun _u#2028 -> ((poly_stub_88)@(L(unit)))[@inline] in
let new_account#355 = fun _u#2032 -> ((poly_stub_87)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#357 =
  fun _n#2036 -> ((poly_stub_77)@(L(unit)))[@inline] in
let register_delegate#359 =
  fun _kh#2040 -> ((poly_stub_77)@(L(unit)))[@inline] in
let register_constant#360 =
  fun _m#2042 -> ((poly_stub_86)@(L(unit)))[@inline] in
let constant_to_michelson_program#362 =
  fun _s#2046 -> ((poly_stub_77)@(L(unit)))[@inline] in
let restore_context#363 =
  fun _u#2048 -> ((poly_stub_77)@(L(unit)))[@inline] in
let save_context#364 = fun _u#2050 -> ((poly_stub_77)@(L(unit)))[@inline] in
let drop_context#365 = fun _u#2052 -> ((poly_stub_77)@(L(unit)))[@inline] in
let set_baker_policy#368 =
  fun _bp#2058 -> ((poly_stub_77)@(L(unit)))[@inline] in
let set_baker#369 = fun _a#2060 -> ((poly_stub_77)@(L(unit)))[@inline] in
let size#370 = fun _c#2062 -> ((poly_stub_85)@(L(unit)))[@inline] in
let read_contract_from_file#372 =
  fun _fn#2066 -> ((poly_stub_77)@(L(unit)))[@inline] in
let chr#373 = fun _n#2068 -> ((poly_stub_83)@(L(unit)))[@inline] in
let nl#374 = L("NEWLINE")[@inline] in
let println#375 = fun _v#2071 -> ((poly_stub_77)@(L(unit)))[@inline] in
let transfer#376 =
  fun _a#2073 -> (fun _s#2074 -> (fun _t#2075 -> ((poly_stub_77)@(L(unit)))))[@inline] in
let transfer_exn#377 =
  fun _a#2077 -> (fun _s#2078 -> (fun _t#2079 -> ((poly_stub_84)@(L(unit)))))[@inline] in
let reset_state#379 =
  fun _n#2083 -> (fun _l#2084 -> ((poly_stub_77)@(L(unit))))[@inline] in
let reset_state_at#380 =
  fun _t#2086 -> (fun _n#2087 -> (fun _l#2088 -> ((poly_stub_77)@(L(unit)))))[@inline] in
let save_mutation#383 =
  fun _s#2097 -> (fun _m#2098 -> ((poly_stub_83)@(L(unit))))[@inline] in
let sign#386 =
  fun _sk#2106 -> (fun _d#2107 -> ((poly_stub_82)@(L(unit))))[@inline] in
let add_account#387 =
  fun _s#2109 -> (fun _k#2110 -> ((poly_stub_77)@(L(unit))))[@inline] in
let baker_account#388 =
  fun _p#2112 -> (fun _o#2113 -> ((poly_stub_77)@(L(unit))))[@inline] in
let create_chest#390 =
  fun _b#2118 -> (fun _n#2119 -> ((poly_stub_81)@(L(unit))))[@inline] in
let create_chest_key#391 =
  fun _c#2121 -> (fun _n#2122 -> ((poly_stub_80)@(L(unit))))[@inline] in
let michelson_equal#394 =
  fun _m1#2132 -> (fun _m2#2133 -> ((poly_stub_79)@(L(unit))))[@inline] in
let originate_contract#396 =
  fun _c#2138 -> (fun _s#2139 -> (fun _t#2140 -> ((poly_stub_78)@(L(unit)))))[@inline] in
let compile_contract_from_file#398 =
  fun _fn#2146 ->
  (fun _e#2147 -> (fun _v#2148 -> ((poly_stub_77)@(L(unit)))))[@inline] in
let originate_from_file#399 =
  fun _fn#2150 ->
  (fun _e#2151 ->
   (fun _v#2152 ->
    (fun _s#2153 -> (fun _t#2154 -> ((poly_stub_76)@(L(unit)))))))[@inline] in
let toto#400 = L(32) in
let titi#401 = ADD(toto#224 , L(42)) in
let f#402 =
  fun gen#2158 ->
  (let (gen#4385, gen#4386) = gen#2158 in
   let gen#2159 = gen#4385 in
   let x#2160 = gen#4386 in
   let x#2161 = ADD(ADD(x#2160 , toto#224) , titi#401) in
   PAIR(LIST_EMPTY() , x#2161)) in
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
  fun _u#2172 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#413 =
  fun _u#2174 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#414 = fun _u#2176 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#415 =
  fun _u#2178 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#416 =
  fun _u#2180 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#417 = fun _u#2182 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#418 = fun _u#2184 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#419 =
  fun _u#2186 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#420 =
  fun _u#2188 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#421 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#422 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#423 =
  fun kh#2192 -> (({ VOTING_POWER })@(kh#2192))[@inline] in
let implicit_account#425 =
  fun kh#2196 -> (IMPLICIT_ACCOUNT(kh#2196))[@inline] in
let pairing_check#429 =
  fun l#2204 -> (({ PAIRING_CHECK })@(l#2204))[@inline] in
let set_delegate#431 = fun o#2208 -> (SET_DELEGATE(o#2208))[@inline] in
let open_chest#437 =
  fun ck#2224 ->
  (fun c#2225 -> (fun n#2226 -> (OPEN_CHEST(ck#2224 , c#2225 , n#2226))))[@inline] in
let xor#440 =
  fun l#2235 -> (fun r#2236 -> (XOR(l#2235 , r#2236)))[@inline] in
let shift_left#441 =
  fun l#2238 -> (fun r#2239 -> (LSL(l#2238 , r#2239)))[@inline] in
let shift_right#442 =
  fun l#2241 -> (fun r#2242 -> (LSR(l#2241 , r#2242)))[@inline] in
let length#483 = fun b#2372 -> (({ SIZE })@(b#2372))[@inline] in
let concat#484 =
  fun b1#2374 ->
  (fun b2#2375 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2374 , b2#2375))))[@inline] in
let sub#485 =
  fun s#2377 ->
  (fun l#2378 ->
   (fun b#2379 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2377 ,
                                                                   l#2378) ,
                                                              b#2379)))))[@inline] in
let length#490 = fun b#2390 -> (({ SIZE })@(b#2390))[@inline] in
let concat#491 =
  fun b1#2392 ->
  (fun b2#2393 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2392 , b2#2393))))[@inline] in
let sub#492 =
  fun s#2395 ->
  (fun l#2396 ->
   (fun b#2397 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2395 ,
                                                                   l#2396) ,
                                                              b#2397)))))[@inline] in
let blake2b#493 = fun b#2399 -> (({ BLAKE2B })@(b#2399))[@inline] in
let sha256#494 = fun b#2401 -> (({ SHA256 })@(b#2401))[@inline] in
let sha512#495 = fun b#2403 -> (({ SHA512 })@(b#2403))[@inline] in
let sha3#496 = fun b#2405 -> (({ SHA3 })@(b#2405))[@inline] in
let keccak#497 = fun b#2407 -> (({ KECCAK })@(b#2407))[@inline] in
let hash_key#498 = fun k#2409 -> (({ HASH_KEY })@(k#2409))[@inline] in
let check#499 =
  fun k#2411 ->
  (fun s#2412 ->
   (fun b#2413 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#2411 , s#2412) ,
                                                   b#2413)))))[@inline] in
let assert#500 =
  fun b#2415 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#2415))[@inline] in
let abs#503 = fun i#2421 -> (({ ABS })@(i#2421))[@inline] in
let is_nat#504 = fun i#2423 -> (({ ISNAT })@(i#2423))[@inline] in
let true#505 = TRUE()[@inline] in
let false#506 = FALSE()[@inline] in
let unit#507 = UNIT()[@inline] in
let assert_with_error#510 =
  fun b#2431 ->
  (fun s#2432 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#2431 , s#2432))))[@inline] in
let poly_stub_75 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_74 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_73 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_72 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_71 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_70 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_69 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_68 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_67 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_66 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_65 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_64 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_63 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_62 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let poly_stub_61 = fun x#2443 -> (({ FAILWITH })@(x#2443))[@inline] in
let get_total_voting_power#518 = (poly_stub_69)@(L(unit))[@inline] in
let set_source#521 = fun _a#2457 -> ((poly_stub_62)@(L(unit)))[@inline] in
let get_storage_of_address#522 =
  fun _a#2459 -> ((poly_stub_62)@(L(unit)))[@inline] in
let get_balance#523 = fun _a#2461 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#524 = fun _v#2463 -> ((poly_stub_62)@(L(unit)))[@inline] in
let eprint#525 = fun _v#2465 -> ((poly_stub_62)@(L(unit)))[@inline] in
let get_voting_power#526 =
  fun _kh#2467 -> ((poly_stub_69)@(L(unit)))[@inline] in
let nth_bootstrap_contract#527 =
  fun _i#2469 -> ((poly_stub_63)@(L(unit)))[@inline] in
let nth_bootstrap_account#528 =
  fun _i#2471 -> ((poly_stub_63)@(L(unit)))[@inline] in
let get_bootstrap_account#529 =
  fun _n#2473 -> ((poly_stub_74)@(L(unit)))[@inline] in
let last_originations#531 =
  fun _u#2477 -> ((poly_stub_73)@(L(unit)))[@inline] in
let new_account#533 = fun _u#2481 -> ((poly_stub_72)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#535 =
  fun _n#2485 -> ((poly_stub_62)@(L(unit)))[@inline] in
let register_delegate#537 =
  fun _kh#2489 -> ((poly_stub_62)@(L(unit)))[@inline] in
let register_constant#538 =
  fun _m#2491 -> ((poly_stub_71)@(L(unit)))[@inline] in
let constant_to_michelson_program#540 =
  fun _s#2495 -> ((poly_stub_62)@(L(unit)))[@inline] in
let restore_context#541 =
  fun _u#2497 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#542 = fun _u#2499 -> ((poly_stub_62)@(L(unit)))[@inline] in
let drop_context#543 = fun _u#2501 -> ((poly_stub_62)@(L(unit)))[@inline] in
let set_baker_policy#546 =
  fun _bp#2507 -> ((poly_stub_62)@(L(unit)))[@inline] in
let set_baker#547 = fun _a#2509 -> ((poly_stub_62)@(L(unit)))[@inline] in
let size#548 = fun _c#2511 -> ((poly_stub_70)@(L(unit)))[@inline] in
let read_contract_from_file#550 =
  fun _fn#2515 -> ((poly_stub_62)@(L(unit)))[@inline] in
let chr#551 = fun _n#2517 -> ((poly_stub_68)@(L(unit)))[@inline] in
let nl#552 = L("NEWLINE")[@inline] in
let println#553 = fun _v#2520 -> ((poly_stub_62)@(L(unit)))[@inline] in
let transfer#554 =
  fun _a#2522 -> (fun _s#2523 -> (fun _t#2524 -> ((poly_stub_62)@(L(unit)))))[@inline] in
let transfer_exn#555 =
  fun _a#2526 -> (fun _s#2527 -> (fun _t#2528 -> ((poly_stub_69)@(L(unit)))))[@inline] in
let reset_state#557 =
  fun _n#2532 -> (fun _l#2533 -> ((poly_stub_62)@(L(unit))))[@inline] in
let reset_state_at#558 =
  fun _t#2535 -> (fun _n#2536 -> (fun _l#2537 -> ((poly_stub_62)@(L(unit)))))[@inline] in
let save_mutation#561 =
  fun _s#2546 -> (fun _m#2547 -> ((poly_stub_68)@(L(unit))))[@inline] in
let sign#564 =
  fun _sk#2555 -> (fun _d#2556 -> ((poly_stub_67)@(L(unit))))[@inline] in
let add_account#565 =
  fun _s#2558 -> (fun _k#2559 -> ((poly_stub_62)@(L(unit))))[@inline] in
let baker_account#566 =
  fun _p#2561 -> (fun _o#2562 -> ((poly_stub_62)@(L(unit))))[@inline] in
let create_chest#568 =
  fun _b#2567 -> (fun _n#2568 -> ((poly_stub_66)@(L(unit))))[@inline] in
let create_chest_key#569 =
  fun _c#2570 -> (fun _n#2571 -> ((poly_stub_65)@(L(unit))))[@inline] in
let michelson_equal#572 =
  fun _m1#2581 -> (fun _m2#2582 -> ((poly_stub_64)@(L(unit))))[@inline] in
let originate_contract#574 =
  fun _c#2587 -> (fun _s#2588 -> (fun _t#2589 -> ((poly_stub_63)@(L(unit)))))[@inline] in
let compile_contract_from_file#576 =
  fun _fn#2595 ->
  (fun _e#2596 -> (fun _v#2597 -> ((poly_stub_62)@(L(unit)))))[@inline] in
let originate_from_file#577 =
  fun _fn#2599 ->
  (fun _e#2600 ->
   (fun _v#2601 ->
    (fun _s#2602 -> (fun _t#2603 -> ((poly_stub_61)@(L(unit)))))))[@inline] in
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
  fun _u#2615 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#589 =
  fun _u#2617 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#590 = fun _u#2619 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#591 =
  fun _u#2621 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#592 =
  fun _u#2623 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#593 = fun _u#2625 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#594 = fun _u#2627 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#595 =
  fun _u#2629 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#596 =
  fun _u#2631 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#597 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#598 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#599 =
  fun kh#2635 -> (({ VOTING_POWER })@(kh#2635))[@inline] in
let implicit_account#601 =
  fun kh#2639 -> (IMPLICIT_ACCOUNT(kh#2639))[@inline] in
let pairing_check#605 =
  fun l#2647 -> (({ PAIRING_CHECK })@(l#2647))[@inline] in
let set_delegate#607 = fun o#2651 -> (SET_DELEGATE(o#2651))[@inline] in
let open_chest#613 =
  fun ck#2667 ->
  (fun c#2668 -> (fun n#2669 -> (OPEN_CHEST(ck#2667 , c#2668 , n#2669))))[@inline] in
let xor#616 =
  fun l#2678 -> (fun r#2679 -> (XOR(l#2678 , r#2679)))[@inline] in
let shift_left#617 =
  fun l#2681 -> (fun r#2682 -> (LSL(l#2681 , r#2682)))[@inline] in
let shift_right#618 =
  fun l#2684 -> (fun r#2685 -> (LSR(l#2684 , r#2685)))[@inline] in
let length#659 = fun b#2815 -> (({ SIZE })@(b#2815))[@inline] in
let concat#660 =
  fun b1#2817 ->
  (fun b2#2818 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2817 , b2#2818))))[@inline] in
let sub#661 =
  fun s#2820 ->
  (fun l#2821 ->
   (fun b#2822 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2820 ,
                                                                   l#2821) ,
                                                              b#2822)))))[@inline] in
let length#666 = fun b#2833 -> (({ SIZE })@(b#2833))[@inline] in
let concat#667 =
  fun b1#2835 ->
  (fun b2#2836 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2835 , b2#2836))))[@inline] in
let sub#668 =
  fun s#2838 ->
  (fun l#2839 ->
   (fun b#2840 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2838 ,
                                                                   l#2839) ,
                                                              b#2840)))))[@inline] in
let blake2b#669 = fun b#2842 -> (({ BLAKE2B })@(b#2842))[@inline] in
let sha256#670 = fun b#2844 -> (({ SHA256 })@(b#2844))[@inline] in
let sha512#671 = fun b#2846 -> (({ SHA512 })@(b#2846))[@inline] in
let sha3#672 = fun b#2848 -> (({ SHA3 })@(b#2848))[@inline] in
let keccak#673 = fun b#2850 -> (({ KECCAK })@(b#2850))[@inline] in
let hash_key#674 = fun k#2852 -> (({ HASH_KEY })@(k#2852))[@inline] in
let check#675 =
  fun k#2854 ->
  (fun s#2855 ->
   (fun b#2856 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#2854 , s#2855) ,
                                                   b#2856)))))[@inline] in
let assert#676 =
  fun b#2858 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#2858))[@inline] in
let abs#679 = fun i#2864 -> (({ ABS })@(i#2864))[@inline] in
let is_nat#680 = fun i#2866 -> (({ ISNAT })@(i#2866))[@inline] in
let true#681 = TRUE()[@inline] in
let false#682 = FALSE()[@inline] in
let unit#683 = UNIT()[@inline] in
let assert_with_error#686 =
  fun b#2874 ->
  (fun s#2875 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#2874 , s#2875))))[@inline] in
let poly_stub_60 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_59 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_58 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_57 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_56 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_55 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_54 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_53 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_52 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_51 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_50 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_49 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_48 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_47 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let poly_stub_46 = fun x#2886 -> (({ FAILWITH })@(x#2886))[@inline] in
let get_total_voting_power#694 = (poly_stub_54)@(L(unit))[@inline] in
let set_source#697 = fun _a#2900 -> ((poly_stub_47)@(L(unit)))[@inline] in
let get_storage_of_address#698 =
  fun _a#2902 -> ((poly_stub_47)@(L(unit)))[@inline] in
let get_balance#699 = fun _a#2904 -> ((poly_stub_60)@(L(unit)))[@inline] in
let print#700 = fun _v#2906 -> ((poly_stub_47)@(L(unit)))[@inline] in
let eprint#701 = fun _v#2908 -> ((poly_stub_47)@(L(unit)))[@inline] in
let get_voting_power#702 =
  fun _kh#2910 -> ((poly_stub_54)@(L(unit)))[@inline] in
let nth_bootstrap_contract#703 =
  fun _i#2912 -> ((poly_stub_48)@(L(unit)))[@inline] in
let nth_bootstrap_account#704 =
  fun _i#2914 -> ((poly_stub_48)@(L(unit)))[@inline] in
let get_bootstrap_account#705 =
  fun _n#2916 -> ((poly_stub_59)@(L(unit)))[@inline] in
let last_originations#707 =
  fun _u#2920 -> ((poly_stub_58)@(L(unit)))[@inline] in
let new_account#709 = fun _u#2924 -> ((poly_stub_57)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#711 =
  fun _n#2928 -> ((poly_stub_47)@(L(unit)))[@inline] in
let register_delegate#713 =
  fun _kh#2932 -> ((poly_stub_47)@(L(unit)))[@inline] in
let register_constant#714 =
  fun _m#2934 -> ((poly_stub_56)@(L(unit)))[@inline] in
let constant_to_michelson_program#716 =
  fun _s#2938 -> ((poly_stub_47)@(L(unit)))[@inline] in
let restore_context#717 =
  fun _u#2940 -> ((poly_stub_47)@(L(unit)))[@inline] in
let save_context#718 = fun _u#2942 -> ((poly_stub_47)@(L(unit)))[@inline] in
let drop_context#719 = fun _u#2944 -> ((poly_stub_47)@(L(unit)))[@inline] in
let set_baker_policy#722 =
  fun _bp#2950 -> ((poly_stub_47)@(L(unit)))[@inline] in
let set_baker#723 = fun _a#2952 -> ((poly_stub_47)@(L(unit)))[@inline] in
let size#724 = fun _c#2954 -> ((poly_stub_55)@(L(unit)))[@inline] in
let read_contract_from_file#726 =
  fun _fn#2958 -> ((poly_stub_47)@(L(unit)))[@inline] in
let chr#727 = fun _n#2960 -> ((poly_stub_53)@(L(unit)))[@inline] in
let nl#728 = L("NEWLINE")[@inline] in
let println#729 = fun _v#2963 -> ((poly_stub_47)@(L(unit)))[@inline] in
let transfer#730 =
  fun _a#2965 -> (fun _s#2966 -> (fun _t#2967 -> ((poly_stub_47)@(L(unit)))))[@inline] in
let transfer_exn#731 =
  fun _a#2969 -> (fun _s#2970 -> (fun _t#2971 -> ((poly_stub_54)@(L(unit)))))[@inline] in
let reset_state#733 =
  fun _n#2975 -> (fun _l#2976 -> ((poly_stub_47)@(L(unit))))[@inline] in
let reset_state_at#734 =
  fun _t#2978 -> (fun _n#2979 -> (fun _l#2980 -> ((poly_stub_47)@(L(unit)))))[@inline] in
let save_mutation#737 =
  fun _s#2989 -> (fun _m#2990 -> ((poly_stub_53)@(L(unit))))[@inline] in
let sign#740 =
  fun _sk#2998 -> (fun _d#2999 -> ((poly_stub_52)@(L(unit))))[@inline] in
let add_account#741 =
  fun _s#3001 -> (fun _k#3002 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#742 =
  fun _p#3004 -> (fun _o#3005 -> ((poly_stub_47)@(L(unit))))[@inline] in
let create_chest#744 =
  fun _b#3010 -> (fun _n#3011 -> ((poly_stub_51)@(L(unit))))[@inline] in
let create_chest_key#745 =
  fun _c#3013 -> (fun _n#3014 -> ((poly_stub_50)@(L(unit))))[@inline] in
let michelson_equal#748 =
  fun _m1#3024 -> (fun _m2#3025 -> ((poly_stub_49)@(L(unit))))[@inline] in
let originate_contract#750 =
  fun _c#3030 -> (fun _s#3031 -> (fun _t#3032 -> ((poly_stub_48)@(L(unit)))))[@inline] in
let compile_contract_from_file#752 =
  fun _fn#3038 ->
  (fun _e#3039 -> (fun _v#3040 -> ((poly_stub_47)@(L(unit)))))[@inline] in
let originate_from_file#753 =
  fun _fn#3042 ->
  (fun _e#3043 ->
   (fun _v#3044 ->
    (fun _s#3045 -> (fun _t#3046 -> ((poly_stub_46)@(L(unit)))))))[@inline] in
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
  fun _u#3058 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#765 =
  fun _u#3060 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#766 = fun _u#3062 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#767 =
  fun _u#3064 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#768 =
  fun _u#3066 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#769 = fun _u#3068 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#770 = fun _u#3070 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#771 =
  fun _u#3072 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#772 =
  fun _u#3074 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#773 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#774 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#775 =
  fun kh#3078 -> (({ VOTING_POWER })@(kh#3078))[@inline] in
let implicit_account#777 =
  fun kh#3082 -> (IMPLICIT_ACCOUNT(kh#3082))[@inline] in
let pairing_check#781 =
  fun l#3090 -> (({ PAIRING_CHECK })@(l#3090))[@inline] in
let set_delegate#783 = fun o#3094 -> (SET_DELEGATE(o#3094))[@inline] in
let open_chest#789 =
  fun ck#3110 ->
  (fun c#3111 -> (fun n#3112 -> (OPEN_CHEST(ck#3110 , c#3111 , n#3112))))[@inline] in
let xor#792 =
  fun l#3121 -> (fun r#3122 -> (XOR(l#3121 , r#3122)))[@inline] in
let shift_left#793 =
  fun l#3124 -> (fun r#3125 -> (LSL(l#3124 , r#3125)))[@inline] in
let shift_right#794 =
  fun l#3127 -> (fun r#3128 -> (LSR(l#3127 , r#3128)))[@inline] in
let length#835 = fun b#3258 -> (({ SIZE })@(b#3258))[@inline] in
let concat#836 =
  fun b1#3260 ->
  (fun b2#3261 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3260 , b2#3261))))[@inline] in
let sub#837 =
  fun s#3263 ->
  (fun l#3264 ->
   (fun b#3265 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3263 ,
                                                                   l#3264) ,
                                                              b#3265)))))[@inline] in
let length#842 = fun b#3276 -> (({ SIZE })@(b#3276))[@inline] in
let concat#843 =
  fun b1#3278 ->
  (fun b2#3279 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3278 , b2#3279))))[@inline] in
let sub#844 =
  fun s#3281 ->
  (fun l#3282 ->
   (fun b#3283 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3281 ,
                                                                   l#3282) ,
                                                              b#3283)))))[@inline] in
let blake2b#845 = fun b#3285 -> (({ BLAKE2B })@(b#3285))[@inline] in
let sha256#846 = fun b#3287 -> (({ SHA256 })@(b#3287))[@inline] in
let sha512#847 = fun b#3289 -> (({ SHA512 })@(b#3289))[@inline] in
let sha3#848 = fun b#3291 -> (({ SHA3 })@(b#3291))[@inline] in
let keccak#849 = fun b#3293 -> (({ KECCAK })@(b#3293))[@inline] in
let hash_key#850 = fun k#3295 -> (({ HASH_KEY })@(k#3295))[@inline] in
let check#851 =
  fun k#3297 ->
  (fun s#3298 ->
   (fun b#3299 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#3297 , s#3298) ,
                                                   b#3299)))))[@inline] in
let assert#852 =
  fun b#3301 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#3301))[@inline] in
let abs#855 = fun i#3307 -> (({ ABS })@(i#3307))[@inline] in
let is_nat#856 = fun i#3309 -> (({ ISNAT })@(i#3309))[@inline] in
let true#857 = TRUE()[@inline] in
let false#858 = FALSE()[@inline] in
let unit#859 = UNIT()[@inline] in
let assert_with_error#862 =
  fun b#3317 ->
  (fun s#3318 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#3317 , s#3318))))[@inline] in
let poly_stub_45 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_44 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_43 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_42 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_41 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_40 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_39 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_38 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_37 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_36 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_35 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_34 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_33 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_32 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let poly_stub_31 = fun x#3329 -> (({ FAILWITH })@(x#3329))[@inline] in
let get_total_voting_power#870 = (poly_stub_39)@(L(unit))[@inline] in
let set_source#873 = fun _a#3343 -> ((poly_stub_32)@(L(unit)))[@inline] in
let get_storage_of_address#874 =
  fun _a#3345 -> ((poly_stub_32)@(L(unit)))[@inline] in
let get_balance#875 = fun _a#3347 -> ((poly_stub_45)@(L(unit)))[@inline] in
let print#876 = fun _v#3349 -> ((poly_stub_32)@(L(unit)))[@inline] in
let eprint#877 = fun _v#3351 -> ((poly_stub_32)@(L(unit)))[@inline] in
let get_voting_power#878 =
  fun _kh#3353 -> ((poly_stub_39)@(L(unit)))[@inline] in
let nth_bootstrap_contract#879 =
  fun _i#3355 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_account#880 =
  fun _i#3357 -> ((poly_stub_33)@(L(unit)))[@inline] in
let get_bootstrap_account#881 =
  fun _n#3359 -> ((poly_stub_44)@(L(unit)))[@inline] in
let last_originations#883 =
  fun _u#3363 -> ((poly_stub_43)@(L(unit)))[@inline] in
let new_account#885 = fun _u#3367 -> ((poly_stub_42)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#887 =
  fun _n#3371 -> ((poly_stub_32)@(L(unit)))[@inline] in
let register_delegate#889 =
  fun _kh#3375 -> ((poly_stub_32)@(L(unit)))[@inline] in
let register_constant#890 =
  fun _m#3377 -> ((poly_stub_41)@(L(unit)))[@inline] in
let constant_to_michelson_program#892 =
  fun _s#3381 -> ((poly_stub_32)@(L(unit)))[@inline] in
let restore_context#893 =
  fun _u#3383 -> ((poly_stub_32)@(L(unit)))[@inline] in
let save_context#894 = fun _u#3385 -> ((poly_stub_32)@(L(unit)))[@inline] in
let drop_context#895 = fun _u#3387 -> ((poly_stub_32)@(L(unit)))[@inline] in
let set_baker_policy#898 =
  fun _bp#3393 -> ((poly_stub_32)@(L(unit)))[@inline] in
let set_baker#899 = fun _a#3395 -> ((poly_stub_32)@(L(unit)))[@inline] in
let size#900 = fun _c#3397 -> ((poly_stub_40)@(L(unit)))[@inline] in
let read_contract_from_file#902 =
  fun _fn#3401 -> ((poly_stub_32)@(L(unit)))[@inline] in
let chr#903 = fun _n#3403 -> ((poly_stub_38)@(L(unit)))[@inline] in
let nl#904 = L("NEWLINE")[@inline] in
let println#905 = fun _v#3406 -> ((poly_stub_32)@(L(unit)))[@inline] in
let transfer#906 =
  fun _a#3408 -> (fun _s#3409 -> (fun _t#3410 -> ((poly_stub_32)@(L(unit)))))[@inline] in
let transfer_exn#907 =
  fun _a#3412 -> (fun _s#3413 -> (fun _t#3414 -> ((poly_stub_39)@(L(unit)))))[@inline] in
let reset_state#909 =
  fun _n#3418 -> (fun _l#3419 -> ((poly_stub_32)@(L(unit))))[@inline] in
let reset_state_at#910 =
  fun _t#3421 -> (fun _n#3422 -> (fun _l#3423 -> ((poly_stub_32)@(L(unit)))))[@inline] in
let save_mutation#913 =
  fun _s#3432 -> (fun _m#3433 -> ((poly_stub_38)@(L(unit))))[@inline] in
let sign#916 =
  fun _sk#3441 -> (fun _d#3442 -> ((poly_stub_37)@(L(unit))))[@inline] in
let add_account#917 =
  fun _s#3444 -> (fun _k#3445 -> ((poly_stub_32)@(L(unit))))[@inline] in
let baker_account#918 =
  fun _p#3447 -> (fun _o#3448 -> ((poly_stub_32)@(L(unit))))[@inline] in
let create_chest#920 =
  fun _b#3453 -> (fun _n#3454 -> ((poly_stub_36)@(L(unit))))[@inline] in
let create_chest_key#921 =
  fun _c#3456 -> (fun _n#3457 -> ((poly_stub_35)@(L(unit))))[@inline] in
let michelson_equal#924 =
  fun _m1#3467 -> (fun _m2#3468 -> ((poly_stub_34)@(L(unit))))[@inline] in
let originate_contract#926 =
  fun _c#3473 -> (fun _s#3474 -> (fun _t#3475 -> ((poly_stub_33)@(L(unit)))))[@inline] in
let compile_contract_from_file#928 =
  fun _fn#3481 ->
  (fun _e#3482 -> (fun _v#3483 -> ((poly_stub_32)@(L(unit)))))[@inline] in
let originate_from_file#929 =
  fun _fn#3485 ->
  (fun _e#3486 ->
   (fun _v#3487 ->
    (fun _s#3488 -> (fun _t#3489 -> ((poly_stub_31)@(L(unit)))))))[@inline] in
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
  fun _u#3502 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#942 =
  fun _u#3504 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#943 = fun _u#3506 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#944 =
  fun _u#3508 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#945 =
  fun _u#3510 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#946 = fun _u#3512 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#947 = fun _u#3514 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#948 =
  fun _u#3516 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#949 =
  fun _u#3518 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#950 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#951 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#952 =
  fun kh#3522 -> (({ VOTING_POWER })@(kh#3522))[@inline] in
let implicit_account#954 =
  fun kh#3526 -> (IMPLICIT_ACCOUNT(kh#3526))[@inline] in
let pairing_check#958 =
  fun l#3534 -> (({ PAIRING_CHECK })@(l#3534))[@inline] in
let set_delegate#960 = fun o#3538 -> (SET_DELEGATE(o#3538))[@inline] in
let open_chest#966 =
  fun ck#3554 ->
  (fun c#3555 -> (fun n#3556 -> (OPEN_CHEST(ck#3554 , c#3555 , n#3556))))[@inline] in
let xor#969 =
  fun l#3565 -> (fun r#3566 -> (XOR(l#3565 , r#3566)))[@inline] in
let shift_left#970 =
  fun l#3568 -> (fun r#3569 -> (LSL(l#3568 , r#3569)))[@inline] in
let shift_right#971 =
  fun l#3571 -> (fun r#3572 -> (LSR(l#3571 , r#3572)))[@inline] in
let length#1012 = fun b#3702 -> (({ SIZE })@(b#3702))[@inline] in
let concat#1013 =
  fun b1#3704 ->
  (fun b2#3705 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3704 , b2#3705))))[@inline] in
let sub#1014 =
  fun s#3707 ->
  (fun l#3708 ->
   (fun b#3709 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3707 ,
                                                                   l#3708) ,
                                                              b#3709)))))[@inline] in
let length#1019 = fun b#3720 -> (({ SIZE })@(b#3720))[@inline] in
let concat#1020 =
  fun b1#3722 ->
  (fun b2#3723 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3722 , b2#3723))))[@inline] in
let sub#1021 =
  fun s#3725 ->
  (fun l#3726 ->
   (fun b#3727 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3725 ,
                                                                   l#3726) ,
                                                              b#3727)))))[@inline] in
let blake2b#1022 = fun b#3729 -> (({ BLAKE2B })@(b#3729))[@inline] in
let sha256#1023 = fun b#3731 -> (({ SHA256 })@(b#3731))[@inline] in
let sha512#1024 = fun b#3733 -> (({ SHA512 })@(b#3733))[@inline] in
let sha3#1025 = fun b#3735 -> (({ SHA3 })@(b#3735))[@inline] in
let keccak#1026 = fun b#3737 -> (({ KECCAK })@(b#3737))[@inline] in
let hash_key#1027 = fun k#3739 -> (({ HASH_KEY })@(k#3739))[@inline] in
let check#1028 =
  fun k#3741 ->
  (fun s#3742 ->
   (fun b#3743 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#3741 , s#3742) ,
                                                   b#3743)))))[@inline] in
let assert#1029 =
  fun b#3745 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#3745))[@inline] in
let abs#1032 = fun i#3751 -> (({ ABS })@(i#3751))[@inline] in
let is_nat#1033 = fun i#3753 -> (({ ISNAT })@(i#3753))[@inline] in
let true#1034 = TRUE()[@inline] in
let false#1035 = FALSE()[@inline] in
let unit#1036 = UNIT()[@inline] in
let assert_with_error#1039 =
  fun b#3761 ->
  (fun s#3762 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#3761 , s#3762))))[@inline] in
let poly_stub_30 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_29 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_28 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_27 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_26 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_25 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_24 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_23 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_22 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_21 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_20 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_19 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_18 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_17 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let poly_stub_16 = fun x#3773 -> (({ FAILWITH })@(x#3773))[@inline] in
let get_total_voting_power#1047 = (poly_stub_24)@(L(unit))[@inline] in
let set_source#1050 = fun _a#3787 -> ((poly_stub_17)@(L(unit)))[@inline] in
let get_storage_of_address#1051 =
  fun _a#3789 -> ((poly_stub_17)@(L(unit)))[@inline] in
let get_balance#1052 = fun _a#3791 -> ((poly_stub_30)@(L(unit)))[@inline] in
let print#1053 = fun _v#3793 -> ((poly_stub_17)@(L(unit)))[@inline] in
let eprint#1054 = fun _v#3795 -> ((poly_stub_17)@(L(unit)))[@inline] in
let get_voting_power#1055 =
  fun _kh#3797 -> ((poly_stub_24)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1056 =
  fun _i#3799 -> ((poly_stub_18)@(L(unit)))[@inline] in
let nth_bootstrap_account#1057 =
  fun _i#3801 -> ((poly_stub_18)@(L(unit)))[@inline] in
let get_bootstrap_account#1058 =
  fun _n#3803 -> ((poly_stub_29)@(L(unit)))[@inline] in
let last_originations#1060 =
  fun _u#3807 -> ((poly_stub_28)@(L(unit)))[@inline] in
let new_account#1062 = fun _u#3811 -> ((poly_stub_27)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1064 =
  fun _n#3815 -> ((poly_stub_17)@(L(unit)))[@inline] in
let register_delegate#1066 =
  fun _kh#3819 -> ((poly_stub_17)@(L(unit)))[@inline] in
let register_constant#1067 =
  fun _m#3821 -> ((poly_stub_26)@(L(unit)))[@inline] in
let constant_to_michelson_program#1069 =
  fun _s#3825 -> ((poly_stub_17)@(L(unit)))[@inline] in
let restore_context#1070 =
  fun _u#3827 -> ((poly_stub_17)@(L(unit)))[@inline] in
let save_context#1071 = fun _u#3829 -> ((poly_stub_17)@(L(unit)))[@inline] in
let drop_context#1072 = fun _u#3831 -> ((poly_stub_17)@(L(unit)))[@inline] in
let set_baker_policy#1075 =
  fun _bp#3837 -> ((poly_stub_17)@(L(unit)))[@inline] in
let set_baker#1076 = fun _a#3839 -> ((poly_stub_17)@(L(unit)))[@inline] in
let size#1077 = fun _c#3841 -> ((poly_stub_25)@(L(unit)))[@inline] in
let read_contract_from_file#1079 =
  fun _fn#3845 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1080 = fun _n#3847 -> ((poly_stub_23)@(L(unit)))[@inline] in
let nl#1081 = L("NEWLINE")[@inline] in
let println#1082 = fun _v#3850 -> ((poly_stub_17)@(L(unit)))[@inline] in
let transfer#1083 =
  fun _a#3852 -> (fun _s#3853 -> (fun _t#3854 -> ((poly_stub_17)@(L(unit)))))[@inline] in
let transfer_exn#1084 =
  fun _a#3856 -> (fun _s#3857 -> (fun _t#3858 -> ((poly_stub_24)@(L(unit)))))[@inline] in
let reset_state#1086 =
  fun _n#3862 -> (fun _l#3863 -> ((poly_stub_17)@(L(unit))))[@inline] in
let reset_state_at#1087 =
  fun _t#3865 -> (fun _n#3866 -> (fun _l#3867 -> ((poly_stub_17)@(L(unit)))))[@inline] in
let save_mutation#1090 =
  fun _s#3876 -> (fun _m#3877 -> ((poly_stub_23)@(L(unit))))[@inline] in
let sign#1093 =
  fun _sk#3885 -> (fun _d#3886 -> ((poly_stub_22)@(L(unit))))[@inline] in
let add_account#1094 =
  fun _s#3888 -> (fun _k#3889 -> ((poly_stub_17)@(L(unit))))[@inline] in
let baker_account#1095 =
  fun _p#3891 -> (fun _o#3892 -> ((poly_stub_17)@(L(unit))))[@inline] in
let create_chest#1097 =
  fun _b#3897 -> (fun _n#3898 -> ((poly_stub_21)@(L(unit))))[@inline] in
let create_chest_key#1098 =
  fun _c#3900 -> (fun _n#3901 -> ((poly_stub_20)@(L(unit))))[@inline] in
let michelson_equal#1101 =
  fun _m1#3911 -> (fun _m2#3912 -> ((poly_stub_19)@(L(unit))))[@inline] in
let originate_contract#1103 =
  fun _c#3917 -> (fun _s#3918 -> (fun _t#3919 -> ((poly_stub_18)@(L(unit)))))[@inline] in
let compile_contract_from_file#1105 =
  fun _fn#3925 ->
  (fun _e#3926 -> (fun _v#3927 -> ((poly_stub_17)@(L(unit)))))[@inline] in
let originate_from_file#1106 =
  fun _fn#3929 ->
  (fun _e#3930 ->
   (fun _v#3931 ->
    (fun _s#3932 -> (fun _t#3933 -> ((poly_stub_16)@(L(unit)))))))[@inline] in
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
  fun _u#3946 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#1119 =
  fun _u#3948 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#1120 = fun _u#3950 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#1121 =
  fun _u#3952 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#1122 =
  fun _u#3954 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#1123 =
  fun _u#3956 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#1124 = fun _u#3958 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#1125 =
  fun _u#3960 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#1126 =
  fun _u#3962 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#1127 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#1128 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#1129 =
  fun kh#3966 -> (({ VOTING_POWER })@(kh#3966))[@inline] in
let implicit_account#1131 =
  fun kh#3970 -> (IMPLICIT_ACCOUNT(kh#3970))[@inline] in
let pairing_check#1135 =
  fun l#3978 -> (({ PAIRING_CHECK })@(l#3978))[@inline] in
let set_delegate#1137 = fun o#3982 -> (SET_DELEGATE(o#3982))[@inline] in
let open_chest#1143 =
  fun ck#3998 ->
  (fun c#3999 -> (fun n#4000 -> (OPEN_CHEST(ck#3998 , c#3999 , n#4000))))[@inline] in
let xor#1146 =
  fun l#4009 -> (fun r#4010 -> (XOR(l#4009 , r#4010)))[@inline] in
let shift_left#1147 =
  fun l#4012 -> (fun r#4013 -> (LSL(l#4012 , r#4013)))[@inline] in
let shift_right#1148 =
  fun l#4015 -> (fun r#4016 -> (LSR(l#4015 , r#4016)))[@inline] in
let length#1189 = fun b#4146 -> (({ SIZE })@(b#4146))[@inline] in
let concat#1190 =
  fun b1#4148 ->
  (fun b2#4149 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4148 , b2#4149))))[@inline] in
let sub#1191 =
  fun s#4151 ->
  (fun l#4152 ->
   (fun b#4153 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4151 ,
                                                                   l#4152) ,
                                                              b#4153)))))[@inline] in
let length#1196 = fun b#4164 -> (({ SIZE })@(b#4164))[@inline] in
let concat#1197 =
  fun b1#4166 ->
  (fun b2#4167 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4166 , b2#4167))))[@inline] in
let sub#1198 =
  fun s#4169 ->
  (fun l#4170 ->
   (fun b#4171 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4169 ,
                                                                   l#4170) ,
                                                              b#4171)))))[@inline] in
let blake2b#1199 = fun b#4173 -> (({ BLAKE2B })@(b#4173))[@inline] in
let sha256#1200 = fun b#4175 -> (({ SHA256 })@(b#4175))[@inline] in
let sha512#1201 = fun b#4177 -> (({ SHA512 })@(b#4177))[@inline] in
let sha3#1202 = fun b#4179 -> (({ SHA3 })@(b#4179))[@inline] in
let keccak#1203 = fun b#4181 -> (({ KECCAK })@(b#4181))[@inline] in
let hash_key#1204 = fun k#4183 -> (({ HASH_KEY })@(k#4183))[@inline] in
let check#1205 =
  fun k#4185 ->
  (fun s#4186 ->
   (fun b#4187 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4185 , s#4186) ,
                                                   b#4187)))))[@inline] in
let assert =
  fun b#4189 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4189))[@inline] in
let abs = fun i#4195 -> (({ ABS })@(i#4195))[@inline] in
let is_nat = fun i#4197 -> (({ ISNAT })@(i#4197))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#4205 ->
  (fun s#4206 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4205 , s#4206))))[@inline] in
let poly_stub_15 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_14 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_13 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_12 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_11 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_10 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_9 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_8 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_7 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_6 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_5 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_4 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_3 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_2 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let poly_stub_1 = fun x#4217 -> (({ FAILWITH })@(x#4217))[@inline] in
let get_total_voting_power#1210 = (poly_stub_9)@(L(unit))[@inline] in
let set_source#1213 = fun _a#4231 -> ((poly_stub_2)@(L(unit)))[@inline] in
let get_storage_of_address#1214 =
  fun _a#4233 -> ((poly_stub_2)@(L(unit)))[@inline] in
let get_balance#1215 = fun _a#4235 -> ((poly_stub_15)@(L(unit)))[@inline] in
let print#1216 = fun _v#4237 -> ((poly_stub_2)@(L(unit)))[@inline] in
let eprint#1217 = fun _v#4239 -> ((poly_stub_2)@(L(unit)))[@inline] in
let get_voting_power#1218 =
  fun _kh#4241 -> ((poly_stub_9)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1219 =
  fun _i#4243 -> ((poly_stub_3)@(L(unit)))[@inline] in
let nth_bootstrap_account#1220 =
  fun _i#4245 -> ((poly_stub_3)@(L(unit)))[@inline] in
let get_bootstrap_account#1221 =
  fun _n#4247 -> ((poly_stub_14)@(L(unit)))[@inline] in
let last_originations#1223 =
  fun _u#4251 -> ((poly_stub_13)@(L(unit)))[@inline] in
let new_account#1225 = fun _u#4255 -> ((poly_stub_12)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1227 =
  fun _n#4259 -> ((poly_stub_2)@(L(unit)))[@inline] in
let register_delegate#1229 =
  fun _kh#4263 -> ((poly_stub_2)@(L(unit)))[@inline] in
let register_constant#1230 =
  fun _m#4265 -> ((poly_stub_11)@(L(unit)))[@inline] in
let constant_to_michelson_program#1232 =
  fun _s#4269 -> ((poly_stub_2)@(L(unit)))[@inline] in
let restore_context#1233 =
  fun _u#4271 -> ((poly_stub_2)@(L(unit)))[@inline] in
let save_context#1234 = fun _u#4273 -> ((poly_stub_2)@(L(unit)))[@inline] in
let drop_context#1235 = fun _u#4275 -> ((poly_stub_2)@(L(unit)))[@inline] in
let set_baker_policy#1238 =
  fun _bp#4281 -> ((poly_stub_2)@(L(unit)))[@inline] in
let set_baker#1239 = fun _a#4283 -> ((poly_stub_2)@(L(unit)))[@inline] in
let size#1240 = fun _c#4285 -> ((poly_stub_10)@(L(unit)))[@inline] in
let read_contract_from_file#1242 =
  fun _fn#4289 -> ((poly_stub_2)@(L(unit)))[@inline] in
let chr#1243 = fun _n#4291 -> ((poly_stub_8)@(L(unit)))[@inline] in
let nl#1244 = L("NEWLINE")[@inline] in
let println#1245 = fun _v#4294 -> ((poly_stub_2)@(L(unit)))[@inline] in
let transfer#1246 =
  fun _a#4296 -> (fun _s#4297 -> (fun _t#4298 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let transfer_exn#1247 =
  fun _a#4300 -> (fun _s#4301 -> (fun _t#4302 -> ((poly_stub_9)@(L(unit)))))[@inline] in
let reset_state#1249 =
  fun _n#4306 -> (fun _l#4307 -> ((poly_stub_2)@(L(unit))))[@inline] in
let reset_state_at#1250 =
  fun _t#4309 -> (fun _n#4310 -> (fun _l#4311 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let save_mutation#1253 =
  fun _s#4320 -> (fun _m#4321 -> ((poly_stub_8)@(L(unit))))[@inline] in
let sign#1256 =
  fun _sk#4329 -> (fun _d#4330 -> ((poly_stub_7)@(L(unit))))[@inline] in
let add_account#1257 =
  fun _s#4332 -> (fun _k#4333 -> ((poly_stub_2)@(L(unit))))[@inline] in
let baker_account#1258 =
  fun _p#4335 -> (fun _o#4336 -> ((poly_stub_2)@(L(unit))))[@inline] in
let create_chest#1260 =
  fun _b#4341 -> (fun _n#4342 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#1261 =
  fun _c#4344 -> (fun _n#4345 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#1264 =
  fun _m1#4355 -> (fun _m2#4356 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#1266 =
  fun _c#4361 -> (fun _s#4362 -> (fun _t#4363 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#1268 =
  fun _fn#4369 -> (fun _e#4370 -> (fun _v#4371 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#1269 =
  fun _fn#4373 ->
  (fun _e#4374 ->
   (fun _v#4375 ->
    (fun _s#4376 -> (fun _t#4377 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1107 , toto#224) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#4381 ->
  (let (gen#4387, gen#4388) = gen#4381 in
   let p#4382 = gen#4387 in
   let s#4383 = gen#4388 in
   let s#4384 = ADD(ADD(p#4382 , s#4383) , toto) in
   PAIR(LIST_EMPTY() , s#4384)) in
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
