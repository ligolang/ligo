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
    const main : ( int * int ) -> ( list (operation) * int ) =
      lambda (gen#4( int * int ))( list (operation) * int ) return  match
                                                                     gen#4 with
                                                                     | ( p : int , s : int ) ->
                                                                     let s : int =
                                                                       ADD
                                                                       (ADD
                                                                        (p ,
                                                                        s) ,
                                                                        toto) in
                                                                     ( LIST_EMPTY
                                                                       () ,
                                                                       s ) |xxx}]

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
    const main : ( unit * string ) -> ( list (operation) * string ) =
      lambda (gen#2( unit * string ))( list (operation) * string ) return
       match gen#2 with
        | ( _#4 : unit , _#3 : string ) ->
        ( LIST_EMPTY() , CONCAT(Errors.undefined_token , Storage.s) ) |xxx}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let get_balance#265 =
  fun _u#1400 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#266 =
  fun _u#1402 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#267 = fun _u#1404 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#268 =
  fun _u#1406 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#269 =
  fun _u#1408 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#270 = fun _u#1410 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#271 = fun _u#1412 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#272 =
  fun _u#1414 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#273 =
  fun _u#1416 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#274 =
  fun _u#1418 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#275 =
  fun kh#1420 -> (({ VOTING_POWER })@(kh#1420))[@inline] in
let implicit_account#277 =
  fun kh#1424 -> (IMPLICIT_ACCOUNT(kh#1424))[@inline] in
let pairing_check#281 =
  fun l#1432 -> (({ PAIRING_CHECK })@(l#1432))[@inline] in
let set_delegate#283 = fun o#1436 -> (SET_DELEGATE(o#1436))[@inline] in
let open_chest#291 =
  fun ck#1457 ->
  (fun c#1458 -> (fun n#1459 -> (OPEN_CHEST(ck#1457 , c#1458 , n#1459))))[@inline] in
let xor#300 =
  fun l#1493 -> (fun r#1494 -> (XOR(l#1493 , r#1494)))[@inline] in
let or#301 = fun l#1496 -> (fun r#1497 -> (OR(l#1496 , r#1497)))[@inline] in
let shift_left#302 =
  fun l#1499 -> (fun r#1500 -> (LSL(l#1499 , r#1500)))[@inline] in
let shift_right#303 =
  fun l#1502 -> (fun r#1503 -> (LSR(l#1502 , r#1503)))[@inline] in
let length#348 = fun b#1648 -> (({ SIZE })@(b#1648))[@inline] in
let concat#349 =
  fun b1#1650 ->
  (fun b2#1651 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1650 , b2#1651))))[@inline] in
let sub#350 =
  fun s#1653 ->
  (fun l#1654 ->
   (fun b#1655 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1653 ,
                                                                   l#1654) ,
                                                              b#1655)))))[@inline] in
let length#356 = fun b#1670 -> (({ SIZE })@(b#1670))[@inline] in
let concat#357 =
  fun b1#1672 ->
  (fun b2#1673 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1672 , b2#1673))))[@inline] in
let sub#358 =
  fun s#1675 ->
  (fun l#1676 ->
   (fun b#1677 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1675 ,
                                                                   l#1676) ,
                                                              b#1677)))))[@inline] in
let blake2b#359 = fun b#1679 -> (({ BLAKE2B })@(b#1679))[@inline] in
let sha256#360 = fun b#1681 -> (({ SHA256 })@(b#1681))[@inline] in
let sha512#361 = fun b#1683 -> (({ SHA512 })@(b#1683))[@inline] in
let sha3#362 = fun b#1685 -> (({ SHA3 })@(b#1685))[@inline] in
let keccak#363 = fun b#1687 -> (({ KECCAK })@(b#1687))[@inline] in
let hash_key#364 = fun k#1689 -> (({ HASH_KEY })@(k#1689))[@inline] in
let check#365 =
  fun k#1691 ->
  (fun s#1692 ->
   (fun b#1693 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1691 , s#1692) ,
                                                   b#1693)))))[@inline] in
let assert#366 =
  fun b#1695 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1695))[@inline] in
let abs#369 = fun i#1701 -> (({ ABS })@(i#1701))[@inline] in
let is_nat#370 = fun i#1703 -> (({ ISNAT })@(i#1703))[@inline] in
let true#371 = TRUE()[@inline] in
let false#372 = FALSE()[@inline] in
let unit#373 = UNIT()[@inline] in
let assert_with_error#377 =
  fun b#1713 ->
  (fun s#1714 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1713 , s#1714))))[@inline] in
let get_balance#451 =
  fun _u#1728 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#452 =
  fun _u#1730 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#453 = fun _u#1732 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#454 =
  fun _u#1734 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#455 =
  fun _u#1736 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#456 = fun _u#1738 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#457 = fun _u#1740 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#458 =
  fun _u#1742 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#459 =
  fun _u#1744 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#460 =
  fun _u#1746 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#461 =
  fun kh#1748 -> (({ VOTING_POWER })@(kh#1748))[@inline] in
let implicit_account#463 =
  fun kh#1752 -> (IMPLICIT_ACCOUNT(kh#1752))[@inline] in
let pairing_check#467 =
  fun l#1760 -> (({ PAIRING_CHECK })@(l#1760))[@inline] in
let set_delegate#469 = fun o#1764 -> (SET_DELEGATE(o#1764))[@inline] in
let open_chest#477 =
  fun gen#1788 ->
  (let (gen#2151, gen#2152) = gen#1788 in
   let (gen#2153, gen#2154) = gen#2151 in
   let ck#1789 = gen#2153 in
   let c#1790 = gen#2154 in
   let n#1791 = gen#2152 in OPEN_CHEST(ck#1789 , c#1790 , n#1791))[@inline] in
let xor#486 =
  fun gen#1834 ->
  (let (gen#2155, gen#2156) = gen#1834 in
   let l#1835 = gen#2155 in let r#1836 = gen#2156 in XOR(l#1835 , r#1836))[@inline] in
let or#487 =
  fun gen#1838 ->
  (let (gen#2157, gen#2158) = gen#1838 in
   let l#1839 = gen#2157 in let r#1840 = gen#2158 in OR(l#1839 , r#1840))[@inline] in
let shift_left#488 =
  fun gen#1842 ->
  (let (gen#2159, gen#2160) = gen#1842 in
   let l#1843 = gen#2159 in let r#1844 = gen#2160 in LSL(l#1843 , r#1844))[@inline] in
let shift_right#489 =
  fun gen#1846 ->
  (let (gen#2161, gen#2162) = gen#1846 in
   let l#1847 = gen#2161 in let r#1848 = gen#2162 in LSR(l#1847 , r#1848))[@inline] in
let length#534 = fun b#2024 -> (({ SIZE })@(b#2024))[@inline] in
let concat#535 =
  fun gen#2026 ->
  (let (gen#2163, gen#2164) = gen#2026 in
   let b1#2027 = gen#2163 in
   let b2#2028 = gen#2164 in ({ UNPAIR ; CONCAT })@(PAIR(b1#2027 , b2#2028)))[@inline] in
let sub#536 =
  fun gen#2030 ->
  (let (gen#2165, gen#2166) = gen#2030 in
   let (gen#2167, gen#2168) = gen#2165 in
   let s#2031 = gen#2167 in
   let l#2032 = gen#2168 in
   let b#2033 = gen#2166 in
   ({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2031 ,
                                                                 l#2032) ,
                                                            b#2033)))[@inline] in
let length#542 = fun b#2050 -> (({ SIZE })@(b#2050))[@inline] in
let concat#543 =
  fun gen#2052 ->
  (let (gen#2169, gen#2170) = gen#2052 in
   let b1#2053 = gen#2169 in
   let b2#2054 = gen#2170 in ({ UNPAIR ; CONCAT })@(PAIR(b1#2053 , b2#2054)))[@inline] in
let sub#544 =
  fun gen#2056 ->
  (let (gen#2171, gen#2172) = gen#2056 in
   let (gen#2173, gen#2174) = gen#2171 in
   let s#2057 = gen#2173 in
   let l#2058 = gen#2174 in
   let b#2059 = gen#2172 in
   ({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2057 ,
                                                                 l#2058) ,
                                                            b#2059)))[@inline] in
let blake2b#545 = fun b#2061 -> (({ BLAKE2B })@(b#2061))[@inline] in
let sha256#546 = fun b#2063 -> (({ SHA256 })@(b#2063))[@inline] in
let sha512#547 = fun b#2065 -> (({ SHA512 })@(b#2065))[@inline] in
let sha3#548 = fun b#2067 -> (({ SHA3 })@(b#2067))[@inline] in
let keccak#549 = fun b#2069 -> (({ KECCAK })@(b#2069))[@inline] in
let hash_key#550 = fun k#2071 -> (({ HASH_KEY })@(k#2071))[@inline] in
let check#551 =
  fun gen#2073 ->
  (let (gen#2175, gen#2176) = gen#2073 in
   let (gen#2177, gen#2178) = gen#2175 in
   let k#2074 = gen#2177 in
   let s#2075 = gen#2178 in
   let b#2076 = gen#2176 in
   ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#2074 , s#2075) ,
                                                 b#2076)))[@inline] in
let assert#552 =
  fun b#2078 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#2078))[@inline] in
let abs#555 = fun i#2084 -> (({ ABS })@(i#2084))[@inline] in
let is_nat#556 = fun i#2086 -> (({ ISNAT })@(i#2086))[@inline] in
let true#557 = TRUE()[@inline] in
let false#558 = FALSE()[@inline] in
let unit#559 = UNIT()[@inline] in
let assert_with_error#563 =
  fun gen#2096 ->
  (let (gen#2179, gen#2180) = gen#2096 in
   let b#2097 = gen#2179 in
   let s#2098 = gen#2180 in
   ({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#2097 , s#2098)))[@inline] in
let assert = assert#366[@inline] in
let abs = abs#369[@inline] in
let is_nat = is_nat#370[@inline] in
let true = true#371[@inline] in
let false = false#372[@inline] in
let unit = unit#373[@inline] in
let assert_with_error = assert_with_error#377[@inline] in
let toto#637 = L(1) in
let toto#638 = L(32) in
let titi#639 = ADD(toto#637 , L(42)) in
let f#640 =
  fun gen#2134 ->
  (let (gen#2181, gen#2182) = gen#2134 in
   let gen#2135 = gen#2181 in
   let x#2136 = gen#2182 in
   let x#2137 = ADD(ADD(x#2136 , toto#637) , titi#639) in
   PAIR(LIST_EMPTY() , x#2137)) in
let toto#641 = L(44) in
let toto#642 = L(43) in
let tata#643 = ADD(toto#637 , titi#639) in
let foo#644 = (f#640)@(PAIR(L(unit) , L(3))) in
let toto#645 = L(10) in
let foo#646 = L("bar") in
let toto = ADD(toto#645 , toto#637) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#2147 ->
  (let (gen#2183, gen#2184) = gen#2147 in
   let p#2148 = gen#2183 in
   let s#2149 = gen#2184 in
   let s#2150 = ADD(ADD(p#2148 , s#2149) , toto) in
   PAIR(LIST_EMPTY() , s#2150)) in
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
