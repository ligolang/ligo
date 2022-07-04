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
  fun _u#1118 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#59 =
  fun _u#1120 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#60 = fun _u#1122 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#61 =
  fun _u#1124 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#62 =
  fun _u#1126 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#63 = fun _u#1128 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#64 = fun _u#1130 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#65 =
  fun _u#1132 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#66 =
  fun _u#1134 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#67 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#68 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#69 =
  fun kh#1138 -> (({ VOTING_POWER })@(kh#1138))[@inline] in
let implicit_account#71 =
  fun kh#1142 -> (IMPLICIT_ACCOUNT(kh#1142))[@inline] in
let pairing_check#75 =
  fun l#1150 -> (({ PAIRING_CHECK })@(l#1150))[@inline] in
let set_delegate#77 = fun o#1154 -> (SET_DELEGATE(o#1154))[@inline] in
let open_chest#83 =
  fun ck#1170 ->
  (fun c#1171 -> (fun n#1172 -> (OPEN_CHEST(ck#1170 , c#1171 , n#1172))))[@inline] in
let xor#86 = fun l#1181 -> (fun r#1182 -> (XOR(l#1181 , r#1182)))[@inline] in
let shift_left#87 =
  fun l#1184 -> (fun r#1185 -> (LSL(l#1184 , r#1185)))[@inline] in
let shift_right#88 =
  fun l#1187 -> (fun r#1188 -> (LSR(l#1187 , r#1188)))[@inline] in
let length#129 = fun b#1318 -> (({ SIZE })@(b#1318))[@inline] in
let concat#130 =
  fun b1#1320 ->
  (fun b2#1321 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1320 , b2#1321))))[@inline] in
let sub#131 =
  fun s#1323 ->
  (fun l#1324 ->
   (fun b#1325 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1323 ,
                                                                   l#1324) ,
                                                              b#1325)))))[@inline] in
let length#136 = fun b#1336 -> (({ SIZE })@(b#1336))[@inline] in
let concat#137 =
  fun b1#1338 ->
  (fun b2#1339 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1338 , b2#1339))))[@inline] in
let sub#138 =
  fun s#1341 ->
  (fun l#1342 ->
   (fun b#1343 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1341 ,
                                                                   l#1342) ,
                                                              b#1343)))))[@inline] in
let blake2b#139 = fun b#1345 -> (({ BLAKE2B })@(b#1345))[@inline] in
let sha256#140 = fun b#1347 -> (({ SHA256 })@(b#1347))[@inline] in
let sha512#141 = fun b#1349 -> (({ SHA512 })@(b#1349))[@inline] in
let sha3#142 = fun b#1351 -> (({ SHA3 })@(b#1351))[@inline] in
let keccak#143 = fun b#1353 -> (({ KECCAK })@(b#1353))[@inline] in
let hash_key#144 = fun k#1355 -> (({ HASH_KEY })@(k#1355))[@inline] in
let check#145 =
  fun k#1357 ->
  (fun s#1358 ->
   (fun b#1359 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1357 , s#1358) ,
                                                   b#1359)))))[@inline] in
let assert#146 =
  fun b#1361 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1361))[@inline] in
let abs#149 = fun i#1367 -> (({ ABS })@(i#1367))[@inline] in
let is_nat#150 = fun i#1369 -> (({ ISNAT })@(i#1369))[@inline] in
let true#151 = TRUE()[@inline] in
let false#152 = FALSE()[@inline] in
let unit#153 = UNIT()[@inline] in
let assert_with_error#156 =
  fun b#1377 ->
  (fun s#1378 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1377 , s#1378))))[@inline] in
let poly_stub_105 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_104 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_103 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_102 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_101 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_100 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_99 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_98 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_97 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_96 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_95 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_94 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_93 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_92 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let poly_stub_91 = fun x#1389 -> (({ FAILWITH })@(x#1389))[@inline] in
let get_total_voting_power#164 = (poly_stub_99)@(L(unit))[@inline] in
let set_source#167 = fun _a#1403 -> ((poly_stub_92)@(L(unit)))[@inline] in
let get_storage_of_address#168 =
  fun _a#1405 -> ((poly_stub_92)@(L(unit)))[@inline] in
let get_balance#169 = fun _a#1407 -> ((poly_stub_105)@(L(unit)))[@inline] in
let print#170 = fun _v#1409 -> ((poly_stub_92)@(L(unit)))[@inline] in
let eprint#171 = fun _v#1411 -> ((poly_stub_92)@(L(unit)))[@inline] in
let get_voting_power#172 =
  fun _kh#1413 -> ((poly_stub_99)@(L(unit)))[@inline] in
let nth_bootstrap_contract#173 =
  fun _i#1415 -> ((poly_stub_93)@(L(unit)))[@inline] in
let nth_bootstrap_account#174 =
  fun _i#1417 -> ((poly_stub_93)@(L(unit)))[@inline] in
let get_bootstrap_account#175 =
  fun _n#1419 -> ((poly_stub_104)@(L(unit)))[@inline] in
let last_originations#177 =
  fun _u#1423 -> ((poly_stub_103)@(L(unit)))[@inline] in
let new_account#179 = fun _u#1427 -> ((poly_stub_102)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#181 =
  fun _n#1431 -> ((poly_stub_92)@(L(unit)))[@inline] in
let register_delegate#183 =
  fun _kh#1435 -> ((poly_stub_92)@(L(unit)))[@inline] in
let register_constant#184 =
  fun _m#1437 -> ((poly_stub_101)@(L(unit)))[@inline] in
let constant_to_michelson_program#186 =
  fun _s#1441 -> ((poly_stub_92)@(L(unit)))[@inline] in
let restore_context#187 =
  fun _u#1443 -> ((poly_stub_92)@(L(unit)))[@inline] in
let save_context#188 = fun _u#1445 -> ((poly_stub_92)@(L(unit)))[@inline] in
let drop_context#189 = fun _u#1447 -> ((poly_stub_92)@(L(unit)))[@inline] in
let set_baker_policy#192 =
  fun _bp#1453 -> ((poly_stub_92)@(L(unit)))[@inline] in
let set_baker#193 = fun _a#1455 -> ((poly_stub_92)@(L(unit)))[@inline] in
let size#194 = fun _c#1457 -> ((poly_stub_100)@(L(unit)))[@inline] in
let read_contract_from_file#196 =
  fun _fn#1461 -> ((poly_stub_92)@(L(unit)))[@inline] in
let chr#197 = fun _n#1463 -> ((poly_stub_98)@(L(unit)))[@inline] in
let nl#198 = L("NEWLINE")[@inline] in
let println#199 = fun _v#1466 -> ((poly_stub_92)@(L(unit)))[@inline] in
let transfer#200 =
  fun _a#1468 -> (fun _s#1469 -> (fun _t#1470 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let transfer_exn#201 =
  fun _a#1472 -> (fun _s#1473 -> (fun _t#1474 -> ((poly_stub_99)@(L(unit)))))[@inline] in
let reset_state#203 =
  fun _n#1478 -> (fun _l#1479 -> ((poly_stub_92)@(L(unit))))[@inline] in
let reset_state_at#204 =
  fun _t#1481 -> (fun _n#1482 -> (fun _l#1483 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let save_mutation#207 =
  fun _s#1492 -> (fun _m#1493 -> ((poly_stub_98)@(L(unit))))[@inline] in
let sign#210 =
  fun _sk#1501 -> (fun _d#1502 -> ((poly_stub_97)@(L(unit))))[@inline] in
let add_account#211 =
  fun _s#1504 -> (fun _k#1505 -> ((poly_stub_92)@(L(unit))))[@inline] in
let baker_account#212 =
  fun _p#1507 -> (fun _o#1508 -> ((poly_stub_92)@(L(unit))))[@inline] in
let create_chest#214 =
  fun _b#1513 -> (fun _n#1514 -> ((poly_stub_96)@(L(unit))))[@inline] in
let create_chest_key#215 =
  fun _c#1516 -> (fun _n#1517 -> ((poly_stub_95)@(L(unit))))[@inline] in
let michelson_equal#218 =
  fun _m1#1527 -> (fun _m2#1528 -> ((poly_stub_94)@(L(unit))))[@inline] in
let originate_contract#220 =
  fun _c#1533 -> (fun _s#1534 -> (fun _t#1535 -> ((poly_stub_93)@(L(unit)))))[@inline] in
let compile_contract_from_file#222 =
  fun _fn#1541 ->
  (fun _e#1542 -> (fun _v#1543 -> ((poly_stub_92)@(L(unit)))))[@inline] in
let originate_from_file#223 =
  fun _fn#1545 ->
  (fun _e#1546 ->
   (fun _v#1547 ->
    (fun _s#1548 -> (fun _t#1549 -> ((poly_stub_91)@(L(unit)))))))[@inline] in
let toto#17 = L(1) in
let balance#224 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#225 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#226 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#227 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#228 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#229 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#230 = SELF_ADDRESS()[@inline] in
let chain_id#231 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#232 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#233 =
  fun _u#1561 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#234 =
  fun _u#1563 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#235 = fun _u#1565 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#236 =
  fun _u#1567 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#237 =
  fun _u#1569 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#238 = fun _u#1571 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#239 = fun _u#1573 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#240 =
  fun _u#1575 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#241 =
  fun _u#1577 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#242 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#243 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#244 =
  fun kh#1581 -> (({ VOTING_POWER })@(kh#1581))[@inline] in
let implicit_account#246 =
  fun kh#1585 -> (IMPLICIT_ACCOUNT(kh#1585))[@inline] in
let pairing_check#250 =
  fun l#1593 -> (({ PAIRING_CHECK })@(l#1593))[@inline] in
let set_delegate#252 = fun o#1597 -> (SET_DELEGATE(o#1597))[@inline] in
let open_chest#258 =
  fun ck#1613 ->
  (fun c#1614 -> (fun n#1615 -> (OPEN_CHEST(ck#1613 , c#1614 , n#1615))))[@inline] in
let xor#261 =
  fun l#1624 -> (fun r#1625 -> (XOR(l#1624 , r#1625)))[@inline] in
let shift_left#262 =
  fun l#1627 -> (fun r#1628 -> (LSL(l#1627 , r#1628)))[@inline] in
let shift_right#263 =
  fun l#1630 -> (fun r#1631 -> (LSR(l#1630 , r#1631)))[@inline] in
let length#304 = fun b#1761 -> (({ SIZE })@(b#1761))[@inline] in
let concat#305 =
  fun b1#1763 ->
  (fun b2#1764 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1763 , b2#1764))))[@inline] in
let sub#306 =
  fun s#1766 ->
  (fun l#1767 ->
   (fun b#1768 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1766 ,
                                                                   l#1767) ,
                                                              b#1768)))))[@inline] in
let length#311 = fun b#1779 -> (({ SIZE })@(b#1779))[@inline] in
let concat#312 =
  fun b1#1781 ->
  (fun b2#1782 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1781 , b2#1782))))[@inline] in
let sub#313 =
  fun s#1784 ->
  (fun l#1785 ->
   (fun b#1786 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1784 ,
                                                                   l#1785) ,
                                                              b#1786)))))[@inline] in
let blake2b#314 = fun b#1788 -> (({ BLAKE2B })@(b#1788))[@inline] in
let sha256#315 = fun b#1790 -> (({ SHA256 })@(b#1790))[@inline] in
let sha512#316 = fun b#1792 -> (({ SHA512 })@(b#1792))[@inline] in
let sha3#317 = fun b#1794 -> (({ SHA3 })@(b#1794))[@inline] in
let keccak#318 = fun b#1796 -> (({ KECCAK })@(b#1796))[@inline] in
let hash_key#319 = fun k#1798 -> (({ HASH_KEY })@(k#1798))[@inline] in
let check#320 =
  fun k#1800 ->
  (fun s#1801 ->
   (fun b#1802 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1800 , s#1801) ,
                                                   b#1802)))))[@inline] in
let assert#321 =
  fun b#1804 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1804))[@inline] in
let abs#324 = fun i#1810 -> (({ ABS })@(i#1810))[@inline] in
let is_nat#325 = fun i#1812 -> (({ ISNAT })@(i#1812))[@inline] in
let true#326 = TRUE()[@inline] in
let false#327 = FALSE()[@inline] in
let unit#328 = UNIT()[@inline] in
let assert_with_error#331 =
  fun b#1820 ->
  (fun s#1821 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1820 , s#1821))))[@inline] in
let poly_stub_90 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_89 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_88 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_87 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_86 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_85 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_84 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_83 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_82 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_81 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_80 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_79 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_78 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_77 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let poly_stub_76 = fun x#1832 -> (({ FAILWITH })@(x#1832))[@inline] in
let get_total_voting_power#339 = (poly_stub_84)@(L(unit))[@inline] in
let set_source#342 = fun _a#1846 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#343 =
  fun _a#1848 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_balance#344 = fun _a#1850 -> ((poly_stub_90)@(L(unit)))[@inline] in
let print#345 = fun _v#1852 -> ((poly_stub_77)@(L(unit)))[@inline] in
let eprint#346 = fun _v#1854 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_voting_power#347 =
  fun _kh#1856 -> ((poly_stub_84)@(L(unit)))[@inline] in
let nth_bootstrap_contract#348 =
  fun _i#1858 -> ((poly_stub_78)@(L(unit)))[@inline] in
let nth_bootstrap_account#349 =
  fun _i#1860 -> ((poly_stub_78)@(L(unit)))[@inline] in
let get_bootstrap_account#350 =
  fun _n#1862 -> ((poly_stub_89)@(L(unit)))[@inline] in
let last_originations#352 =
  fun _u#1866 -> ((poly_stub_88)@(L(unit)))[@inline] in
let new_account#354 = fun _u#1870 -> ((poly_stub_87)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#356 =
  fun _n#1874 -> ((poly_stub_77)@(L(unit)))[@inline] in
let register_delegate#358 =
  fun _kh#1878 -> ((poly_stub_77)@(L(unit)))[@inline] in
let register_constant#359 =
  fun _m#1880 -> ((poly_stub_86)@(L(unit)))[@inline] in
let constant_to_michelson_program#361 =
  fun _s#1884 -> ((poly_stub_77)@(L(unit)))[@inline] in
let restore_context#362 =
  fun _u#1886 -> ((poly_stub_77)@(L(unit)))[@inline] in
let save_context#363 = fun _u#1888 -> ((poly_stub_77)@(L(unit)))[@inline] in
let drop_context#364 = fun _u#1890 -> ((poly_stub_77)@(L(unit)))[@inline] in
let set_baker_policy#367 =
  fun _bp#1896 -> ((poly_stub_77)@(L(unit)))[@inline] in
let set_baker#368 = fun _a#1898 -> ((poly_stub_77)@(L(unit)))[@inline] in
let size#369 = fun _c#1900 -> ((poly_stub_85)@(L(unit)))[@inline] in
let read_contract_from_file#371 =
  fun _fn#1904 -> ((poly_stub_77)@(L(unit)))[@inline] in
let chr#372 = fun _n#1906 -> ((poly_stub_83)@(L(unit)))[@inline] in
let nl#373 = L("NEWLINE")[@inline] in
let println#374 = fun _v#1909 -> ((poly_stub_77)@(L(unit)))[@inline] in
let transfer#375 =
  fun _a#1911 -> (fun _s#1912 -> (fun _t#1913 -> ((poly_stub_77)@(L(unit)))))[@inline] in
let transfer_exn#376 =
  fun _a#1915 -> (fun _s#1916 -> (fun _t#1917 -> ((poly_stub_84)@(L(unit)))))[@inline] in
let reset_state#378 =
  fun _n#1921 -> (fun _l#1922 -> ((poly_stub_77)@(L(unit))))[@inline] in
let reset_state_at#379 =
  fun _t#1924 -> (fun _n#1925 -> (fun _l#1926 -> ((poly_stub_77)@(L(unit)))))[@inline] in
let save_mutation#382 =
  fun _s#1935 -> (fun _m#1936 -> ((poly_stub_83)@(L(unit))))[@inline] in
let sign#385 =
  fun _sk#1944 -> (fun _d#1945 -> ((poly_stub_82)@(L(unit))))[@inline] in
let add_account#386 =
  fun _s#1947 -> (fun _k#1948 -> ((poly_stub_77)@(L(unit))))[@inline] in
let baker_account#387 =
  fun _p#1950 -> (fun _o#1951 -> ((poly_stub_77)@(L(unit))))[@inline] in
let create_chest#389 =
  fun _b#1956 -> (fun _n#1957 -> ((poly_stub_81)@(L(unit))))[@inline] in
let create_chest_key#390 =
  fun _c#1959 -> (fun _n#1960 -> ((poly_stub_80)@(L(unit))))[@inline] in
let michelson_equal#393 =
  fun _m1#1970 -> (fun _m2#1971 -> ((poly_stub_79)@(L(unit))))[@inline] in
let originate_contract#395 =
  fun _c#1976 -> (fun _s#1977 -> (fun _t#1978 -> ((poly_stub_78)@(L(unit)))))[@inline] in
let compile_contract_from_file#397 =
  fun _fn#1984 ->
  (fun _e#1985 -> (fun _v#1986 -> ((poly_stub_77)@(L(unit)))))[@inline] in
let originate_from_file#398 =
  fun _fn#1988 ->
  (fun _e#1989 ->
   (fun _v#1990 ->
    (fun _s#1991 -> (fun _t#1992 -> ((poly_stub_76)@(L(unit)))))))[@inline] in
let toto#399 = L(32) in
let titi#400 = ADD(toto#17 , L(42)) in
let f#401 =
  fun gen#1996 ->
  (let (gen#4223, gen#4224) = gen#1996 in
   let gen#1997 = gen#4223 in
   let x#1998 = gen#4224 in
   let x#1999 = ADD(ADD(x#1998 , toto#17) , titi#400) in
   PAIR(LIST_EMPTY() , x#1999)) in
let balance#402 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#403 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#404 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#405 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#406 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#407 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#408 = SELF_ADDRESS()[@inline] in
let chain_id#409 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#410 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#411 =
  fun _u#2010 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#412 =
  fun _u#2012 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#413 = fun _u#2014 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#414 =
  fun _u#2016 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#415 =
  fun _u#2018 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#416 = fun _u#2020 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#417 = fun _u#2022 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#418 =
  fun _u#2024 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#419 =
  fun _u#2026 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#420 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#421 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#422 =
  fun kh#2030 -> (({ VOTING_POWER })@(kh#2030))[@inline] in
let implicit_account#424 =
  fun kh#2034 -> (IMPLICIT_ACCOUNT(kh#2034))[@inline] in
let pairing_check#428 =
  fun l#2042 -> (({ PAIRING_CHECK })@(l#2042))[@inline] in
let set_delegate#430 = fun o#2046 -> (SET_DELEGATE(o#2046))[@inline] in
let open_chest#436 =
  fun ck#2062 ->
  (fun c#2063 -> (fun n#2064 -> (OPEN_CHEST(ck#2062 , c#2063 , n#2064))))[@inline] in
let xor#439 =
  fun l#2073 -> (fun r#2074 -> (XOR(l#2073 , r#2074)))[@inline] in
let shift_left#440 =
  fun l#2076 -> (fun r#2077 -> (LSL(l#2076 , r#2077)))[@inline] in
let shift_right#441 =
  fun l#2079 -> (fun r#2080 -> (LSR(l#2079 , r#2080)))[@inline] in
let length#482 = fun b#2210 -> (({ SIZE })@(b#2210))[@inline] in
let concat#483 =
  fun b1#2212 ->
  (fun b2#2213 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2212 , b2#2213))))[@inline] in
let sub#484 =
  fun s#2215 ->
  (fun l#2216 ->
   (fun b#2217 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2215 ,
                                                                   l#2216) ,
                                                              b#2217)))))[@inline] in
let length#489 = fun b#2228 -> (({ SIZE })@(b#2228))[@inline] in
let concat#490 =
  fun b1#2230 ->
  (fun b2#2231 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2230 , b2#2231))))[@inline] in
let sub#491 =
  fun s#2233 ->
  (fun l#2234 ->
   (fun b#2235 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2233 ,
                                                                   l#2234) ,
                                                              b#2235)))))[@inline] in
let blake2b#492 = fun b#2237 -> (({ BLAKE2B })@(b#2237))[@inline] in
let sha256#493 = fun b#2239 -> (({ SHA256 })@(b#2239))[@inline] in
let sha512#494 = fun b#2241 -> (({ SHA512 })@(b#2241))[@inline] in
let sha3#495 = fun b#2243 -> (({ SHA3 })@(b#2243))[@inline] in
let keccak#496 = fun b#2245 -> (({ KECCAK })@(b#2245))[@inline] in
let hash_key#497 = fun k#2247 -> (({ HASH_KEY })@(k#2247))[@inline] in
let check#498 =
  fun k#2249 ->
  (fun s#2250 ->
   (fun b#2251 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#2249 , s#2250) ,
                                                   b#2251)))))[@inline] in
let assert#499 =
  fun b#2253 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#2253))[@inline] in
let abs#502 = fun i#2259 -> (({ ABS })@(i#2259))[@inline] in
let is_nat#503 = fun i#2261 -> (({ ISNAT })@(i#2261))[@inline] in
let true#504 = TRUE()[@inline] in
let false#505 = FALSE()[@inline] in
let unit#506 = UNIT()[@inline] in
let assert_with_error#509 =
  fun b#2269 ->
  (fun s#2270 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#2269 , s#2270))))[@inline] in
let poly_stub_75 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_74 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_73 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_72 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_71 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_70 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_69 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_68 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_67 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_66 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_65 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_64 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_63 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_62 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let poly_stub_61 = fun x#2281 -> (({ FAILWITH })@(x#2281))[@inline] in
let get_total_voting_power#517 = (poly_stub_69)@(L(unit))[@inline] in
let set_source#520 = fun _a#2295 -> ((poly_stub_62)@(L(unit)))[@inline] in
let get_storage_of_address#521 =
  fun _a#2297 -> ((poly_stub_62)@(L(unit)))[@inline] in
let get_balance#522 = fun _a#2299 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#523 = fun _v#2301 -> ((poly_stub_62)@(L(unit)))[@inline] in
let eprint#524 = fun _v#2303 -> ((poly_stub_62)@(L(unit)))[@inline] in
let get_voting_power#525 =
  fun _kh#2305 -> ((poly_stub_69)@(L(unit)))[@inline] in
let nth_bootstrap_contract#526 =
  fun _i#2307 -> ((poly_stub_63)@(L(unit)))[@inline] in
let nth_bootstrap_account#527 =
  fun _i#2309 -> ((poly_stub_63)@(L(unit)))[@inline] in
let get_bootstrap_account#528 =
  fun _n#2311 -> ((poly_stub_74)@(L(unit)))[@inline] in
let last_originations#530 =
  fun _u#2315 -> ((poly_stub_73)@(L(unit)))[@inline] in
let new_account#532 = fun _u#2319 -> ((poly_stub_72)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#534 =
  fun _n#2323 -> ((poly_stub_62)@(L(unit)))[@inline] in
let register_delegate#536 =
  fun _kh#2327 -> ((poly_stub_62)@(L(unit)))[@inline] in
let register_constant#537 =
  fun _m#2329 -> ((poly_stub_71)@(L(unit)))[@inline] in
let constant_to_michelson_program#539 =
  fun _s#2333 -> ((poly_stub_62)@(L(unit)))[@inline] in
let restore_context#540 =
  fun _u#2335 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#541 = fun _u#2337 -> ((poly_stub_62)@(L(unit)))[@inline] in
let drop_context#542 = fun _u#2339 -> ((poly_stub_62)@(L(unit)))[@inline] in
let set_baker_policy#545 =
  fun _bp#2345 -> ((poly_stub_62)@(L(unit)))[@inline] in
let set_baker#546 = fun _a#2347 -> ((poly_stub_62)@(L(unit)))[@inline] in
let size#547 = fun _c#2349 -> ((poly_stub_70)@(L(unit)))[@inline] in
let read_contract_from_file#549 =
  fun _fn#2353 -> ((poly_stub_62)@(L(unit)))[@inline] in
let chr#550 = fun _n#2355 -> ((poly_stub_68)@(L(unit)))[@inline] in
let nl#551 = L("NEWLINE")[@inline] in
let println#552 = fun _v#2358 -> ((poly_stub_62)@(L(unit)))[@inline] in
let transfer#553 =
  fun _a#2360 -> (fun _s#2361 -> (fun _t#2362 -> ((poly_stub_62)@(L(unit)))))[@inline] in
let transfer_exn#554 =
  fun _a#2364 -> (fun _s#2365 -> (fun _t#2366 -> ((poly_stub_69)@(L(unit)))))[@inline] in
let reset_state#556 =
  fun _n#2370 -> (fun _l#2371 -> ((poly_stub_62)@(L(unit))))[@inline] in
let reset_state_at#557 =
  fun _t#2373 -> (fun _n#2374 -> (fun _l#2375 -> ((poly_stub_62)@(L(unit)))))[@inline] in
let save_mutation#560 =
  fun _s#2384 -> (fun _m#2385 -> ((poly_stub_68)@(L(unit))))[@inline] in
let sign#563 =
  fun _sk#2393 -> (fun _d#2394 -> ((poly_stub_67)@(L(unit))))[@inline] in
let add_account#564 =
  fun _s#2396 -> (fun _k#2397 -> ((poly_stub_62)@(L(unit))))[@inline] in
let baker_account#565 =
  fun _p#2399 -> (fun _o#2400 -> ((poly_stub_62)@(L(unit))))[@inline] in
let create_chest#567 =
  fun _b#2405 -> (fun _n#2406 -> ((poly_stub_66)@(L(unit))))[@inline] in
let create_chest_key#568 =
  fun _c#2408 -> (fun _n#2409 -> ((poly_stub_65)@(L(unit))))[@inline] in
let michelson_equal#571 =
  fun _m1#2419 -> (fun _m2#2420 -> ((poly_stub_64)@(L(unit))))[@inline] in
let originate_contract#573 =
  fun _c#2425 -> (fun _s#2426 -> (fun _t#2427 -> ((poly_stub_63)@(L(unit)))))[@inline] in
let compile_contract_from_file#575 =
  fun _fn#2433 ->
  (fun _e#2434 -> (fun _v#2435 -> ((poly_stub_62)@(L(unit)))))[@inline] in
let originate_from_file#576 =
  fun _fn#2437 ->
  (fun _e#2438 ->
   (fun _v#2439 ->
    (fun _s#2440 -> (fun _t#2441 -> ((poly_stub_61)@(L(unit)))))))[@inline] in
let toto#577 = L(44) in
let balance#578 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#579 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#580 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#581 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#582 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#583 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#584 = SELF_ADDRESS()[@inline] in
let chain_id#585 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#586 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#587 =
  fun _u#2453 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#588 =
  fun _u#2455 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#589 = fun _u#2457 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#590 =
  fun _u#2459 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#591 =
  fun _u#2461 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#592 = fun _u#2463 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#593 = fun _u#2465 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#594 =
  fun _u#2467 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#595 =
  fun _u#2469 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#596 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#597 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#598 =
  fun kh#2473 -> (({ VOTING_POWER })@(kh#2473))[@inline] in
let implicit_account#600 =
  fun kh#2477 -> (IMPLICIT_ACCOUNT(kh#2477))[@inline] in
let pairing_check#604 =
  fun l#2485 -> (({ PAIRING_CHECK })@(l#2485))[@inline] in
let set_delegate#606 = fun o#2489 -> (SET_DELEGATE(o#2489))[@inline] in
let open_chest#612 =
  fun ck#2505 ->
  (fun c#2506 -> (fun n#2507 -> (OPEN_CHEST(ck#2505 , c#2506 , n#2507))))[@inline] in
let xor#615 =
  fun l#2516 -> (fun r#2517 -> (XOR(l#2516 , r#2517)))[@inline] in
let shift_left#616 =
  fun l#2519 -> (fun r#2520 -> (LSL(l#2519 , r#2520)))[@inline] in
let shift_right#617 =
  fun l#2522 -> (fun r#2523 -> (LSR(l#2522 , r#2523)))[@inline] in
let length#658 = fun b#2653 -> (({ SIZE })@(b#2653))[@inline] in
let concat#659 =
  fun b1#2655 ->
  (fun b2#2656 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2655 , b2#2656))))[@inline] in
let sub#660 =
  fun s#2658 ->
  (fun l#2659 ->
   (fun b#2660 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2658 ,
                                                                   l#2659) ,
                                                              b#2660)))))[@inline] in
let length#665 = fun b#2671 -> (({ SIZE })@(b#2671))[@inline] in
let concat#666 =
  fun b1#2673 ->
  (fun b2#2674 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#2673 , b2#2674))))[@inline] in
let sub#667 =
  fun s#2676 ->
  (fun l#2677 ->
   (fun b#2678 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2676 ,
                                                                   l#2677) ,
                                                              b#2678)))))[@inline] in
let blake2b#668 = fun b#2680 -> (({ BLAKE2B })@(b#2680))[@inline] in
let sha256#669 = fun b#2682 -> (({ SHA256 })@(b#2682))[@inline] in
let sha512#670 = fun b#2684 -> (({ SHA512 })@(b#2684))[@inline] in
let sha3#671 = fun b#2686 -> (({ SHA3 })@(b#2686))[@inline] in
let keccak#672 = fun b#2688 -> (({ KECCAK })@(b#2688))[@inline] in
let hash_key#673 = fun k#2690 -> (({ HASH_KEY })@(k#2690))[@inline] in
let check#674 =
  fun k#2692 ->
  (fun s#2693 ->
   (fun b#2694 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#2692 , s#2693) ,
                                                   b#2694)))))[@inline] in
let assert#675 =
  fun b#2696 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#2696))[@inline] in
let abs#678 = fun i#2702 -> (({ ABS })@(i#2702))[@inline] in
let is_nat#679 = fun i#2704 -> (({ ISNAT })@(i#2704))[@inline] in
let true#680 = TRUE()[@inline] in
let false#681 = FALSE()[@inline] in
let unit#682 = UNIT()[@inline] in
let assert_with_error#685 =
  fun b#2712 ->
  (fun s#2713 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#2712 , s#2713))))[@inline] in
let poly_stub_60 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_59 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_58 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_57 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_56 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_55 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_54 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_53 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_52 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_51 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_50 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_49 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_48 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_47 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let poly_stub_46 = fun x#2724 -> (({ FAILWITH })@(x#2724))[@inline] in
let get_total_voting_power#693 = (poly_stub_54)@(L(unit))[@inline] in
let set_source#696 = fun _a#2738 -> ((poly_stub_47)@(L(unit)))[@inline] in
let get_storage_of_address#697 =
  fun _a#2740 -> ((poly_stub_47)@(L(unit)))[@inline] in
let get_balance#698 = fun _a#2742 -> ((poly_stub_60)@(L(unit)))[@inline] in
let print#699 = fun _v#2744 -> ((poly_stub_47)@(L(unit)))[@inline] in
let eprint#700 = fun _v#2746 -> ((poly_stub_47)@(L(unit)))[@inline] in
let get_voting_power#701 =
  fun _kh#2748 -> ((poly_stub_54)@(L(unit)))[@inline] in
let nth_bootstrap_contract#702 =
  fun _i#2750 -> ((poly_stub_48)@(L(unit)))[@inline] in
let nth_bootstrap_account#703 =
  fun _i#2752 -> ((poly_stub_48)@(L(unit)))[@inline] in
let get_bootstrap_account#704 =
  fun _n#2754 -> ((poly_stub_59)@(L(unit)))[@inline] in
let last_originations#706 =
  fun _u#2758 -> ((poly_stub_58)@(L(unit)))[@inline] in
let new_account#708 = fun _u#2762 -> ((poly_stub_57)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#710 =
  fun _n#2766 -> ((poly_stub_47)@(L(unit)))[@inline] in
let register_delegate#712 =
  fun _kh#2770 -> ((poly_stub_47)@(L(unit)))[@inline] in
let register_constant#713 =
  fun _m#2772 -> ((poly_stub_56)@(L(unit)))[@inline] in
let constant_to_michelson_program#715 =
  fun _s#2776 -> ((poly_stub_47)@(L(unit)))[@inline] in
let restore_context#716 =
  fun _u#2778 -> ((poly_stub_47)@(L(unit)))[@inline] in
let save_context#717 = fun _u#2780 -> ((poly_stub_47)@(L(unit)))[@inline] in
let drop_context#718 = fun _u#2782 -> ((poly_stub_47)@(L(unit)))[@inline] in
let set_baker_policy#721 =
  fun _bp#2788 -> ((poly_stub_47)@(L(unit)))[@inline] in
let set_baker#722 = fun _a#2790 -> ((poly_stub_47)@(L(unit)))[@inline] in
let size#723 = fun _c#2792 -> ((poly_stub_55)@(L(unit)))[@inline] in
let read_contract_from_file#725 =
  fun _fn#2796 -> ((poly_stub_47)@(L(unit)))[@inline] in
let chr#726 = fun _n#2798 -> ((poly_stub_53)@(L(unit)))[@inline] in
let nl#727 = L("NEWLINE")[@inline] in
let println#728 = fun _v#2801 -> ((poly_stub_47)@(L(unit)))[@inline] in
let transfer#729 =
  fun _a#2803 -> (fun _s#2804 -> (fun _t#2805 -> ((poly_stub_47)@(L(unit)))))[@inline] in
let transfer_exn#730 =
  fun _a#2807 -> (fun _s#2808 -> (fun _t#2809 -> ((poly_stub_54)@(L(unit)))))[@inline] in
let reset_state#732 =
  fun _n#2813 -> (fun _l#2814 -> ((poly_stub_47)@(L(unit))))[@inline] in
let reset_state_at#733 =
  fun _t#2816 -> (fun _n#2817 -> (fun _l#2818 -> ((poly_stub_47)@(L(unit)))))[@inline] in
let save_mutation#736 =
  fun _s#2827 -> (fun _m#2828 -> ((poly_stub_53)@(L(unit))))[@inline] in
let sign#739 =
  fun _sk#2836 -> (fun _d#2837 -> ((poly_stub_52)@(L(unit))))[@inline] in
let add_account#740 =
  fun _s#2839 -> (fun _k#2840 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#741 =
  fun _p#2842 -> (fun _o#2843 -> ((poly_stub_47)@(L(unit))))[@inline] in
let create_chest#743 =
  fun _b#2848 -> (fun _n#2849 -> ((poly_stub_51)@(L(unit))))[@inline] in
let create_chest_key#744 =
  fun _c#2851 -> (fun _n#2852 -> ((poly_stub_50)@(L(unit))))[@inline] in
let michelson_equal#747 =
  fun _m1#2862 -> (fun _m2#2863 -> ((poly_stub_49)@(L(unit))))[@inline] in
let originate_contract#749 =
  fun _c#2868 -> (fun _s#2869 -> (fun _t#2870 -> ((poly_stub_48)@(L(unit)))))[@inline] in
let compile_contract_from_file#751 =
  fun _fn#2876 ->
  (fun _e#2877 -> (fun _v#2878 -> ((poly_stub_47)@(L(unit)))))[@inline] in
let originate_from_file#752 =
  fun _fn#2880 ->
  (fun _e#2881 ->
   (fun _v#2882 ->
    (fun _s#2883 -> (fun _t#2884 -> ((poly_stub_46)@(L(unit)))))))[@inline] in
let toto#753 = L(43) in
let balance#754 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#755 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#756 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#757 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#758 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#759 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#760 = SELF_ADDRESS()[@inline] in
let chain_id#761 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#762 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#763 =
  fun _u#2896 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#764 =
  fun _u#2898 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#765 = fun _u#2900 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#766 =
  fun _u#2902 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#767 =
  fun _u#2904 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#768 = fun _u#2906 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#769 = fun _u#2908 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#770 =
  fun _u#2910 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#771 =
  fun _u#2912 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#772 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#773 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#774 =
  fun kh#2916 -> (({ VOTING_POWER })@(kh#2916))[@inline] in
let implicit_account#776 =
  fun kh#2920 -> (IMPLICIT_ACCOUNT(kh#2920))[@inline] in
let pairing_check#780 =
  fun l#2928 -> (({ PAIRING_CHECK })@(l#2928))[@inline] in
let set_delegate#782 = fun o#2932 -> (SET_DELEGATE(o#2932))[@inline] in
let open_chest#788 =
  fun ck#2948 ->
  (fun c#2949 -> (fun n#2950 -> (OPEN_CHEST(ck#2948 , c#2949 , n#2950))))[@inline] in
let xor#791 =
  fun l#2959 -> (fun r#2960 -> (XOR(l#2959 , r#2960)))[@inline] in
let shift_left#792 =
  fun l#2962 -> (fun r#2963 -> (LSL(l#2962 , r#2963)))[@inline] in
let shift_right#793 =
  fun l#2965 -> (fun r#2966 -> (LSR(l#2965 , r#2966)))[@inline] in
let length#834 = fun b#3096 -> (({ SIZE })@(b#3096))[@inline] in
let concat#835 =
  fun b1#3098 ->
  (fun b2#3099 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3098 , b2#3099))))[@inline] in
let sub#836 =
  fun s#3101 ->
  (fun l#3102 ->
   (fun b#3103 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3101 ,
                                                                   l#3102) ,
                                                              b#3103)))))[@inline] in
let length#841 = fun b#3114 -> (({ SIZE })@(b#3114))[@inline] in
let concat#842 =
  fun b1#3116 ->
  (fun b2#3117 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3116 , b2#3117))))[@inline] in
let sub#843 =
  fun s#3119 ->
  (fun l#3120 ->
   (fun b#3121 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3119 ,
                                                                   l#3120) ,
                                                              b#3121)))))[@inline] in
let blake2b#844 = fun b#3123 -> (({ BLAKE2B })@(b#3123))[@inline] in
let sha256#845 = fun b#3125 -> (({ SHA256 })@(b#3125))[@inline] in
let sha512#846 = fun b#3127 -> (({ SHA512 })@(b#3127))[@inline] in
let sha3#847 = fun b#3129 -> (({ SHA3 })@(b#3129))[@inline] in
let keccak#848 = fun b#3131 -> (({ KECCAK })@(b#3131))[@inline] in
let hash_key#849 = fun k#3133 -> (({ HASH_KEY })@(k#3133))[@inline] in
let check#850 =
  fun k#3135 ->
  (fun s#3136 ->
   (fun b#3137 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#3135 , s#3136) ,
                                                   b#3137)))))[@inline] in
let assert#851 =
  fun b#3139 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#3139))[@inline] in
let abs#854 = fun i#3145 -> (({ ABS })@(i#3145))[@inline] in
let is_nat#855 = fun i#3147 -> (({ ISNAT })@(i#3147))[@inline] in
let true#856 = TRUE()[@inline] in
let false#857 = FALSE()[@inline] in
let unit#858 = UNIT()[@inline] in
let assert_with_error#861 =
  fun b#3155 ->
  (fun s#3156 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#3155 , s#3156))))[@inline] in
let poly_stub_45 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_44 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_43 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_42 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_41 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_40 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_39 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_38 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_37 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_36 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_35 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_34 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_33 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_32 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let poly_stub_31 = fun x#3167 -> (({ FAILWITH })@(x#3167))[@inline] in
let get_total_voting_power#869 = (poly_stub_39)@(L(unit))[@inline] in
let set_source#872 = fun _a#3181 -> ((poly_stub_32)@(L(unit)))[@inline] in
let get_storage_of_address#873 =
  fun _a#3183 -> ((poly_stub_32)@(L(unit)))[@inline] in
let get_balance#874 = fun _a#3185 -> ((poly_stub_45)@(L(unit)))[@inline] in
let print#875 = fun _v#3187 -> ((poly_stub_32)@(L(unit)))[@inline] in
let eprint#876 = fun _v#3189 -> ((poly_stub_32)@(L(unit)))[@inline] in
let get_voting_power#877 =
  fun _kh#3191 -> ((poly_stub_39)@(L(unit)))[@inline] in
let nth_bootstrap_contract#878 =
  fun _i#3193 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_account#879 =
  fun _i#3195 -> ((poly_stub_33)@(L(unit)))[@inline] in
let get_bootstrap_account#880 =
  fun _n#3197 -> ((poly_stub_44)@(L(unit)))[@inline] in
let last_originations#882 =
  fun _u#3201 -> ((poly_stub_43)@(L(unit)))[@inline] in
let new_account#884 = fun _u#3205 -> ((poly_stub_42)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#886 =
  fun _n#3209 -> ((poly_stub_32)@(L(unit)))[@inline] in
let register_delegate#888 =
  fun _kh#3213 -> ((poly_stub_32)@(L(unit)))[@inline] in
let register_constant#889 =
  fun _m#3215 -> ((poly_stub_41)@(L(unit)))[@inline] in
let constant_to_michelson_program#891 =
  fun _s#3219 -> ((poly_stub_32)@(L(unit)))[@inline] in
let restore_context#892 =
  fun _u#3221 -> ((poly_stub_32)@(L(unit)))[@inline] in
let save_context#893 = fun _u#3223 -> ((poly_stub_32)@(L(unit)))[@inline] in
let drop_context#894 = fun _u#3225 -> ((poly_stub_32)@(L(unit)))[@inline] in
let set_baker_policy#897 =
  fun _bp#3231 -> ((poly_stub_32)@(L(unit)))[@inline] in
let set_baker#898 = fun _a#3233 -> ((poly_stub_32)@(L(unit)))[@inline] in
let size#899 = fun _c#3235 -> ((poly_stub_40)@(L(unit)))[@inline] in
let read_contract_from_file#901 =
  fun _fn#3239 -> ((poly_stub_32)@(L(unit)))[@inline] in
let chr#902 = fun _n#3241 -> ((poly_stub_38)@(L(unit)))[@inline] in
let nl#903 = L("NEWLINE")[@inline] in
let println#904 = fun _v#3244 -> ((poly_stub_32)@(L(unit)))[@inline] in
let transfer#905 =
  fun _a#3246 -> (fun _s#3247 -> (fun _t#3248 -> ((poly_stub_32)@(L(unit)))))[@inline] in
let transfer_exn#906 =
  fun _a#3250 -> (fun _s#3251 -> (fun _t#3252 -> ((poly_stub_39)@(L(unit)))))[@inline] in
let reset_state#908 =
  fun _n#3256 -> (fun _l#3257 -> ((poly_stub_32)@(L(unit))))[@inline] in
let reset_state_at#909 =
  fun _t#3259 -> (fun _n#3260 -> (fun _l#3261 -> ((poly_stub_32)@(L(unit)))))[@inline] in
let save_mutation#912 =
  fun _s#3270 -> (fun _m#3271 -> ((poly_stub_38)@(L(unit))))[@inline] in
let sign#915 =
  fun _sk#3279 -> (fun _d#3280 -> ((poly_stub_37)@(L(unit))))[@inline] in
let add_account#916 =
  fun _s#3282 -> (fun _k#3283 -> ((poly_stub_32)@(L(unit))))[@inline] in
let baker_account#917 =
  fun _p#3285 -> (fun _o#3286 -> ((poly_stub_32)@(L(unit))))[@inline] in
let create_chest#919 =
  fun _b#3291 -> (fun _n#3292 -> ((poly_stub_36)@(L(unit))))[@inline] in
let create_chest_key#920 =
  fun _c#3294 -> (fun _n#3295 -> ((poly_stub_35)@(L(unit))))[@inline] in
let michelson_equal#923 =
  fun _m1#3305 -> (fun _m2#3306 -> ((poly_stub_34)@(L(unit))))[@inline] in
let originate_contract#925 =
  fun _c#3311 -> (fun _s#3312 -> (fun _t#3313 -> ((poly_stub_33)@(L(unit)))))[@inline] in
let compile_contract_from_file#927 =
  fun _fn#3319 ->
  (fun _e#3320 -> (fun _v#3321 -> ((poly_stub_32)@(L(unit)))))[@inline] in
let originate_from_file#928 =
  fun _fn#3323 ->
  (fun _e#3324 ->
   (fun _v#3325 ->
    (fun _s#3326 -> (fun _t#3327 -> ((poly_stub_31)@(L(unit)))))))[@inline] in
let tata#929 = ADD(toto#17 , titi#400) in
let foo#930 = (f#401)@(PAIR(L(unit) , L(3))) in
let balance#931 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#932 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#933 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#934 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#935 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#936 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#937 = SELF_ADDRESS()[@inline] in
let chain_id#938 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#939 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#940 =
  fun _u#3340 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#941 =
  fun _u#3342 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#942 = fun _u#3344 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#943 =
  fun _u#3346 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#944 =
  fun _u#3348 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#945 = fun _u#3350 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#946 = fun _u#3352 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#947 =
  fun _u#3354 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#948 =
  fun _u#3356 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#949 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#950 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#951 =
  fun kh#3360 -> (({ VOTING_POWER })@(kh#3360))[@inline] in
let implicit_account#953 =
  fun kh#3364 -> (IMPLICIT_ACCOUNT(kh#3364))[@inline] in
let pairing_check#957 =
  fun l#3372 -> (({ PAIRING_CHECK })@(l#3372))[@inline] in
let set_delegate#959 = fun o#3376 -> (SET_DELEGATE(o#3376))[@inline] in
let open_chest#965 =
  fun ck#3392 ->
  (fun c#3393 -> (fun n#3394 -> (OPEN_CHEST(ck#3392 , c#3393 , n#3394))))[@inline] in
let xor#968 =
  fun l#3403 -> (fun r#3404 -> (XOR(l#3403 , r#3404)))[@inline] in
let shift_left#969 =
  fun l#3406 -> (fun r#3407 -> (LSL(l#3406 , r#3407)))[@inline] in
let shift_right#970 =
  fun l#3409 -> (fun r#3410 -> (LSR(l#3409 , r#3410)))[@inline] in
let length#1011 = fun b#3540 -> (({ SIZE })@(b#3540))[@inline] in
let concat#1012 =
  fun b1#3542 ->
  (fun b2#3543 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3542 , b2#3543))))[@inline] in
let sub#1013 =
  fun s#3545 ->
  (fun l#3546 ->
   (fun b#3547 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3545 ,
                                                                   l#3546) ,
                                                              b#3547)))))[@inline] in
let length#1018 = fun b#3558 -> (({ SIZE })@(b#3558))[@inline] in
let concat#1019 =
  fun b1#3560 ->
  (fun b2#3561 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3560 , b2#3561))))[@inline] in
let sub#1020 =
  fun s#3563 ->
  (fun l#3564 ->
   (fun b#3565 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3563 ,
                                                                   l#3564) ,
                                                              b#3565)))))[@inline] in
let blake2b#1021 = fun b#3567 -> (({ BLAKE2B })@(b#3567))[@inline] in
let sha256#1022 = fun b#3569 -> (({ SHA256 })@(b#3569))[@inline] in
let sha512#1023 = fun b#3571 -> (({ SHA512 })@(b#3571))[@inline] in
let sha3#1024 = fun b#3573 -> (({ SHA3 })@(b#3573))[@inline] in
let keccak#1025 = fun b#3575 -> (({ KECCAK })@(b#3575))[@inline] in
let hash_key#1026 = fun k#3577 -> (({ HASH_KEY })@(k#3577))[@inline] in
let check#1027 =
  fun k#3579 ->
  (fun s#3580 ->
   (fun b#3581 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#3579 , s#3580) ,
                                                   b#3581)))))[@inline] in
let assert#1028 =
  fun b#3583 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#3583))[@inline] in
let abs#1031 = fun i#3589 -> (({ ABS })@(i#3589))[@inline] in
let is_nat#1032 = fun i#3591 -> (({ ISNAT })@(i#3591))[@inline] in
let true#1033 = TRUE()[@inline] in
let false#1034 = FALSE()[@inline] in
let unit#1035 = UNIT()[@inline] in
let assert_with_error#1038 =
  fun b#3599 ->
  (fun s#3600 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#3599 , s#3600))))[@inline] in
let poly_stub_30 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_29 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_28 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_27 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_26 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_25 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_24 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_23 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_22 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_21 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_20 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_19 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_18 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_17 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let poly_stub_16 = fun x#3611 -> (({ FAILWITH })@(x#3611))[@inline] in
let get_total_voting_power#1046 = (poly_stub_24)@(L(unit))[@inline] in
let set_source#1049 = fun _a#3625 -> ((poly_stub_17)@(L(unit)))[@inline] in
let get_storage_of_address#1050 =
  fun _a#3627 -> ((poly_stub_17)@(L(unit)))[@inline] in
let get_balance#1051 = fun _a#3629 -> ((poly_stub_30)@(L(unit)))[@inline] in
let print#1052 = fun _v#3631 -> ((poly_stub_17)@(L(unit)))[@inline] in
let eprint#1053 = fun _v#3633 -> ((poly_stub_17)@(L(unit)))[@inline] in
let get_voting_power#1054 =
  fun _kh#3635 -> ((poly_stub_24)@(L(unit)))[@inline] in
let nth_bootstrap_contract#1055 =
  fun _i#3637 -> ((poly_stub_18)@(L(unit)))[@inline] in
let nth_bootstrap_account#1056 =
  fun _i#3639 -> ((poly_stub_18)@(L(unit)))[@inline] in
let get_bootstrap_account#1057 =
  fun _n#3641 -> ((poly_stub_29)@(L(unit)))[@inline] in
let last_originations#1059 =
  fun _u#3645 -> ((poly_stub_28)@(L(unit)))[@inline] in
let new_account#1061 = fun _u#3649 -> ((poly_stub_27)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#1063 =
  fun _n#3653 -> ((poly_stub_17)@(L(unit)))[@inline] in
let register_delegate#1065 =
  fun _kh#3657 -> ((poly_stub_17)@(L(unit)))[@inline] in
let register_constant#1066 =
  fun _m#3659 -> ((poly_stub_26)@(L(unit)))[@inline] in
let constant_to_michelson_program#1068 =
  fun _s#3663 -> ((poly_stub_17)@(L(unit)))[@inline] in
let restore_context#1069 =
  fun _u#3665 -> ((poly_stub_17)@(L(unit)))[@inline] in
let save_context#1070 = fun _u#3667 -> ((poly_stub_17)@(L(unit)))[@inline] in
let drop_context#1071 = fun _u#3669 -> ((poly_stub_17)@(L(unit)))[@inline] in
let set_baker_policy#1074 =
  fun _bp#3675 -> ((poly_stub_17)@(L(unit)))[@inline] in
let set_baker#1075 = fun _a#3677 -> ((poly_stub_17)@(L(unit)))[@inline] in
let size#1076 = fun _c#3679 -> ((poly_stub_25)@(L(unit)))[@inline] in
let read_contract_from_file#1078 =
  fun _fn#3683 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#1079 = fun _n#3685 -> ((poly_stub_23)@(L(unit)))[@inline] in
let nl#1080 = L("NEWLINE")[@inline] in
let println#1081 = fun _v#3688 -> ((poly_stub_17)@(L(unit)))[@inline] in
let transfer#1082 =
  fun _a#3690 -> (fun _s#3691 -> (fun _t#3692 -> ((poly_stub_17)@(L(unit)))))[@inline] in
let transfer_exn#1083 =
  fun _a#3694 -> (fun _s#3695 -> (fun _t#3696 -> ((poly_stub_24)@(L(unit)))))[@inline] in
let reset_state#1085 =
  fun _n#3700 -> (fun _l#3701 -> ((poly_stub_17)@(L(unit))))[@inline] in
let reset_state_at#1086 =
  fun _t#3703 -> (fun _n#3704 -> (fun _l#3705 -> ((poly_stub_17)@(L(unit)))))[@inline] in
let save_mutation#1089 =
  fun _s#3714 -> (fun _m#3715 -> ((poly_stub_23)@(L(unit))))[@inline] in
let sign#1092 =
  fun _sk#3723 -> (fun _d#3724 -> ((poly_stub_22)@(L(unit))))[@inline] in
let add_account#1093 =
  fun _s#3726 -> (fun _k#3727 -> ((poly_stub_17)@(L(unit))))[@inline] in
let baker_account#1094 =
  fun _p#3729 -> (fun _o#3730 -> ((poly_stub_17)@(L(unit))))[@inline] in
let create_chest#1096 =
  fun _b#3735 -> (fun _n#3736 -> ((poly_stub_21)@(L(unit))))[@inline] in
let create_chest_key#1097 =
  fun _c#3738 -> (fun _n#3739 -> ((poly_stub_20)@(L(unit))))[@inline] in
let michelson_equal#1100 =
  fun _m1#3749 -> (fun _m2#3750 -> ((poly_stub_19)@(L(unit))))[@inline] in
let originate_contract#1102 =
  fun _c#3755 -> (fun _s#3756 -> (fun _t#3757 -> ((poly_stub_18)@(L(unit)))))[@inline] in
let compile_contract_from_file#1104 =
  fun _fn#3763 ->
  (fun _e#3764 -> (fun _v#3765 -> ((poly_stub_17)@(L(unit)))))[@inline] in
let originate_from_file#1105 =
  fun _fn#3767 ->
  (fun _e#3768 ->
   (fun _v#3769 ->
    (fun _s#3770 -> (fun _t#3771 -> ((poly_stub_16)@(L(unit)))))))[@inline] in
let toto#1106 = L(10) in
let foo#1107 = L("bar") in
let balance#8 = ({ DROP ; BALANCE })@(L(unit))[@inline] in
let amount#9 = ({ DROP ; AMOUNT })@(L(unit))[@inline] in
let now#10 = ({ DROP ; NOW })@(L(unit))[@inline] in
let sender#11 = ({ DROP ; SENDER })@(L(unit))[@inline] in
let source#12 = ({ DROP ; SOURCE })@(L(unit))[@inline] in
let level#13 = ({ DROP ; LEVEL })@(L(unit))[@inline] in
let self_address#14 = SELF_ADDRESS()[@inline] in
let chain_id#15 = ({ DROP ; CHAIN_ID })@(L(unit))[@inline] in
let total_voting_power#16 =
  ({ DROP ; TOTAL_VOTING_POWER })@(L(unit))[@inline] in
let get_balance#17 =
  fun _u#3784 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#18 =
  fun _u#3786 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#19 = fun _u#3788 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#20 =
  fun _u#3790 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#21 =
  fun _u#3792 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#22 = fun _u#3794 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#23 = fun _u#3796 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#24 =
  fun _u#3798 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#25 =
  fun _u#3800 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let min_block_time#26 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let get_min_block_time#27 = { DROP ; MIN_BLOCK_TIME }[@inline] in
let voting_power#28 =
  fun kh#3804 -> (({ VOTING_POWER })@(kh#3804))[@inline] in
let implicit_account#30 =
  fun kh#3808 -> (IMPLICIT_ACCOUNT(kh#3808))[@inline] in
let pairing_check#34 =
  fun l#3816 -> (({ PAIRING_CHECK })@(l#3816))[@inline] in
let set_delegate#36 = fun o#3820 -> (SET_DELEGATE(o#3820))[@inline] in
let open_chest#42 =
  fun ck#3836 ->
  (fun c#3837 -> (fun n#3838 -> (OPEN_CHEST(ck#3836 , c#3837 , n#3838))))[@inline] in
let xor#45 = fun l#3847 -> (fun r#3848 -> (XOR(l#3847 , r#3848)))[@inline] in
let shift_left#46 =
  fun l#3850 -> (fun r#3851 -> (LSL(l#3850 , r#3851)))[@inline] in
let shift_right#47 =
  fun l#3853 -> (fun r#3854 -> (LSR(l#3853 , r#3854)))[@inline] in
let length#88 = fun b#3984 -> (({ SIZE })@(b#3984))[@inline] in
let concat#89 =
  fun b1#3986 ->
  (fun b2#3987 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#3986 , b2#3987))))[@inline] in
let sub#90 =
  fun s#3989 ->
  (fun l#3990 ->
   (fun b#3991 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#3989 ,
                                                                   l#3990) ,
                                                              b#3991)))))[@inline] in
let length#95 = fun b#4002 -> (({ SIZE })@(b#4002))[@inline] in
let concat#96 =
  fun b1#4004 ->
  (fun b2#4005 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#4004 , b2#4005))))[@inline] in
let sub#97 =
  fun s#4007 ->
  (fun l#4008 ->
   (fun b#4009 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#4007 ,
                                                                   l#4008) ,
                                                              b#4009)))))[@inline] in
let blake2b#98 = fun b#4011 -> (({ BLAKE2B })@(b#4011))[@inline] in
let sha256#99 = fun b#4013 -> (({ SHA256 })@(b#4013))[@inline] in
let sha512#100 = fun b#4015 -> (({ SHA512 })@(b#4015))[@inline] in
let sha3#101 = fun b#4017 -> (({ SHA3 })@(b#4017))[@inline] in
let keccak#102 = fun b#4019 -> (({ KECCAK })@(b#4019))[@inline] in
let hash_key#103 = fun k#4021 -> (({ HASH_KEY })@(k#4021))[@inline] in
let check#104 =
  fun k#4023 ->
  (fun s#4024 ->
   (fun b#4025 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#4023 , s#4024) ,
                                                   b#4025)))))[@inline] in
let assert =
  fun b#4027 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#4027))[@inline] in
let abs = fun i#4033 -> (({ ABS })@(i#4033))[@inline] in
let is_nat = fun i#4035 -> (({ ISNAT })@(i#4035))[@inline] in
let true = TRUE()[@inline] in
let false = FALSE()[@inline] in
let unit = UNIT()[@inline] in
let assert_with_error =
  fun b#4043 ->
  (fun s#4044 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#4043 , s#4044))))[@inline] in
let poly_stub_15 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_14 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_13 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_12 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_11 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_10 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_9 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_8 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_7 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_6 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_5 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_4 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_3 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_2 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let poly_stub_1 = fun x#4055 -> (({ FAILWITH })@(x#4055))[@inline] in
let get_total_voting_power#109 = (poly_stub_9)@(L(unit))[@inline] in
let set_source#112 = fun _a#4069 -> ((poly_stub_2)@(L(unit)))[@inline] in
let get_storage_of_address#113 =
  fun _a#4071 -> ((poly_stub_2)@(L(unit)))[@inline] in
let get_balance#114 = fun _a#4073 -> ((poly_stub_15)@(L(unit)))[@inline] in
let print#115 = fun _v#4075 -> ((poly_stub_2)@(L(unit)))[@inline] in
let eprint#116 = fun _v#4077 -> ((poly_stub_2)@(L(unit)))[@inline] in
let get_voting_power#117 =
  fun _kh#4079 -> ((poly_stub_9)@(L(unit)))[@inline] in
let nth_bootstrap_contract#118 =
  fun _i#4081 -> ((poly_stub_3)@(L(unit)))[@inline] in
let nth_bootstrap_account#119 =
  fun _i#4083 -> ((poly_stub_3)@(L(unit)))[@inline] in
let get_bootstrap_account#120 =
  fun _n#4085 -> ((poly_stub_14)@(L(unit)))[@inline] in
let last_originations#122 =
  fun _u#4089 -> ((poly_stub_13)@(L(unit)))[@inline] in
let new_account#124 = fun _u#4093 -> ((poly_stub_12)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#126 =
  fun _n#4097 -> ((poly_stub_2)@(L(unit)))[@inline] in
let register_delegate#128 =
  fun _kh#4101 -> ((poly_stub_2)@(L(unit)))[@inline] in
let register_constant#129 =
  fun _m#4103 -> ((poly_stub_11)@(L(unit)))[@inline] in
let constant_to_michelson_program#131 =
  fun _s#4107 -> ((poly_stub_2)@(L(unit)))[@inline] in
let restore_context#132 =
  fun _u#4109 -> ((poly_stub_2)@(L(unit)))[@inline] in
let save_context#133 = fun _u#4111 -> ((poly_stub_2)@(L(unit)))[@inline] in
let drop_context#134 = fun _u#4113 -> ((poly_stub_2)@(L(unit)))[@inline] in
let set_baker_policy#137 =
  fun _bp#4119 -> ((poly_stub_2)@(L(unit)))[@inline] in
let set_baker#138 = fun _a#4121 -> ((poly_stub_2)@(L(unit)))[@inline] in
let size#139 = fun _c#4123 -> ((poly_stub_10)@(L(unit)))[@inline] in
let read_contract_from_file#141 =
  fun _fn#4127 -> ((poly_stub_2)@(L(unit)))[@inline] in
let chr#142 = fun _n#4129 -> ((poly_stub_8)@(L(unit)))[@inline] in
let nl#143 = L("NEWLINE")[@inline] in
let println#144 = fun _v#4132 -> ((poly_stub_2)@(L(unit)))[@inline] in
let transfer#145 =
  fun _a#4134 -> (fun _s#4135 -> (fun _t#4136 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let transfer_exn#146 =
  fun _a#4138 -> (fun _s#4139 -> (fun _t#4140 -> ((poly_stub_9)@(L(unit)))))[@inline] in
let reset_state#148 =
  fun _n#4144 -> (fun _l#4145 -> ((poly_stub_2)@(L(unit))))[@inline] in
let reset_state_at#149 =
  fun _t#4147 -> (fun _n#4148 -> (fun _l#4149 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let save_mutation#152 =
  fun _s#4158 -> (fun _m#4159 -> ((poly_stub_8)@(L(unit))))[@inline] in
let sign#155 =
  fun _sk#4167 -> (fun _d#4168 -> ((poly_stub_7)@(L(unit))))[@inline] in
let add_account#156 =
  fun _s#4170 -> (fun _k#4171 -> ((poly_stub_2)@(L(unit))))[@inline] in
let baker_account#157 =
  fun _p#4173 -> (fun _o#4174 -> ((poly_stub_2)@(L(unit))))[@inline] in
let create_chest#159 =
  fun _b#4179 -> (fun _n#4180 -> ((poly_stub_6)@(L(unit))))[@inline] in
let create_chest_key#160 =
  fun _c#4182 -> (fun _n#4183 -> ((poly_stub_5)@(L(unit))))[@inline] in
let michelson_equal#163 =
  fun _m1#4193 -> (fun _m2#4194 -> ((poly_stub_4)@(L(unit))))[@inline] in
let originate_contract#165 =
  fun _c#4199 -> (fun _s#4200 -> (fun _t#4201 -> ((poly_stub_3)@(L(unit)))))[@inline] in
let compile_contract_from_file#167 =
  fun _fn#4207 -> (fun _e#4208 -> (fun _v#4209 -> ((poly_stub_2)@(L(unit)))))[@inline] in
let originate_from_file#168 =
  fun _fn#4211 ->
  (fun _e#4212 ->
   (fun _v#4213 ->
    (fun _s#4214 -> (fun _t#4215 -> ((poly_stub_1)@(L(unit)))))))[@inline] in
let toto = ADD(toto#1106 , toto#17) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#4219 ->
  (let (gen#4225, gen#4226) = gen#4219 in
   let p#4220 = gen#4225 in
   let s#4221 = gen#4226 in
   let s#4222 = ADD(ADD(p#4220 , s#4221) , toto) in
   PAIR(LIST_EMPTY() , s#4222)) in
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
