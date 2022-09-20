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
let get_balance#116 =
  fun _u#1582 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#117 =
  fun _u#1584 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#118 = fun _u#1586 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#119 =
  fun _u#1588 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#120 =
  fun _u#1590 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#121 = fun _u#1592 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#122 = fun _u#1594 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#123 =
  fun _u#1596 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#124 =
  fun _u#1598 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#125 =
  fun _u#1600 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#126 =
  fun kh#1602 -> (({ VOTING_POWER })@(kh#1602))[@inline] in
let implicit_account#128 =
  fun kh#1606 -> (IMPLICIT_ACCOUNT(kh#1606))[@inline] in
let pairing_check#132 =
  fun l#1614 -> (({ PAIRING_CHECK })@(l#1614))[@inline] in
let set_delegate#134 = fun o#1618 -> (SET_DELEGATE(o#1618))[@inline] in
let open_chest#142 =
  fun ck#1639 ->
  (fun c#1640 -> (fun n#1641 -> (OPEN_CHEST(ck#1639 , c#1640 , n#1641))))[@inline] in
let xor#151 =
  fun l#1675 -> (fun r#1676 -> (XOR(l#1675 , r#1676)))[@inline] in
let or#152 = fun l#1678 -> (fun r#1679 -> (OR(l#1678 , r#1679)))[@inline] in
let shift_left#153 =
  fun l#1681 -> (fun r#1682 -> (LSL(l#1681 , r#1682)))[@inline] in
let shift_right#154 =
  fun l#1684 -> (fun r#1685 -> (LSR(l#1684 , r#1685)))[@inline] in
let length#199 = fun b#1830 -> (({ SIZE })@(b#1830))[@inline] in
let concat#200 =
  fun b1#1832 ->
  (fun b2#1833 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1832 , b2#1833))))[@inline] in
let sub#201 =
  fun s#1835 ->
  (fun l#1836 ->
   (fun b#1837 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1835 ,
                                                                   l#1836) ,
                                                              b#1837)))))[@inline] in
let length#207 = fun b#1852 -> (({ SIZE })@(b#1852))[@inline] in
let concat#208 =
  fun b1#1854 ->
  (fun b2#1855 -> (({ UNPAIR ; CONCAT })@(PAIR(b1#1854 , b2#1855))))[@inline] in
let sub#209 =
  fun s#1857 ->
  (fun l#1858 ->
   (fun b#1859 ->
    (({ UNPAIR ;
       UNPAIR ;
       SLICE ;
       IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#1857 ,
                                                                   l#1858) ,
                                                              b#1859)))))[@inline] in
let blake2b#210 = fun b#1861 -> (({ BLAKE2B })@(b#1861))[@inline] in
let sha256#211 = fun b#1863 -> (({ SHA256 })@(b#1863))[@inline] in
let sha512#212 = fun b#1865 -> (({ SHA512 })@(b#1865))[@inline] in
let sha3#213 = fun b#1867 -> (({ SHA3 })@(b#1867))[@inline] in
let keccak#214 = fun b#1869 -> (({ KECCAK })@(b#1869))[@inline] in
let hash_key#215 = fun k#1871 -> (({ HASH_KEY })@(k#1871))[@inline] in
let check#216 =
  fun k#1873 ->
  (fun s#1874 ->
   (fun b#1875 ->
    (({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#1873 , s#1874) ,
                                                   b#1875)))))[@inline] in
let assert#217 =
  fun b#1877 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#1877))[@inline] in
let abs#220 = fun i#1883 -> (({ ABS })@(i#1883))[@inline] in
let is_nat#221 = fun i#1885 -> (({ ISNAT })@(i#1885))[@inline] in
let true#222 = TRUE()[@inline] in
let false#223 = FALSE()[@inline] in
let unit#224 = UNIT()[@inline] in
let assert_with_error#228 =
  fun b#1895 ->
  (fun s#1896 ->
   (({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#1895 , s#1896))))[@inline] in
let poly_stub_78 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_77 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_76 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_75 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_74 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_73 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_72 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_71 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_70 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_69 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_68 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_67 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_66 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_65 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_64 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_63 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_62 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_61 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_60 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_59 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_58 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_57 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_56 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_55 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_54 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_53 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_52 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_51 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_50 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_49 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_48 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_47 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_46 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_45 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_44 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_43 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_42 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_41 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let poly_stub_40 = fun x#1907 -> (({ FAILWITH })@(x#1907))[@inline] in
let get_total_voting_power#236 =
  fun _u#1916 -> ((poly_stub_78)@(L(unit)))[@inline] in
let set_source#239 = fun _a#1922 -> ((poly_stub_77)@(L(unit)))[@inline] in
let get_storage_of_address#240 =
  fun _a#1924 -> ((poly_stub_76)@(L(unit)))[@inline] in
let get_balance#241 = fun _a#1926 -> ((poly_stub_75)@(L(unit)))[@inline] in
let print#242 = fun _v#1928 -> ((poly_stub_74)@(L(unit)))[@inline] in
let eprint#243 = fun _v#1930 -> ((poly_stub_73)@(L(unit)))[@inline] in
let get_voting_power#244 =
  fun _kh#1932 -> ((poly_stub_72)@(L(unit)))[@inline] in
let nth_bootstrap_contract#245 =
  fun _i#1934 -> ((poly_stub_71)@(L(unit)))[@inline] in
let nth_bootstrap_account#246 =
  fun _i#1936 -> ((poly_stub_70)@(L(unit)))[@inline] in
let get_bootstrap_account#247 =
  fun _n#1938 -> ((poly_stub_69)@(L(unit)))[@inline] in
let last_originations#249 =
  fun _u#1942 -> ((poly_stub_68)@(L(unit)))[@inline] in
let new_account#251 = fun _u#1946 -> ((poly_stub_67)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#253 =
  fun _n#1950 -> ((poly_stub_66)@(L(unit)))[@inline] in
let register_delegate#255 =
  fun _kh#1954 -> ((poly_stub_65)@(L(unit)))[@inline] in
let register_constant#256 =
  fun _m#1956 -> ((poly_stub_64)@(L(unit)))[@inline] in
let constant_to_michelson_program#258 =
  fun _s#1960 -> ((poly_stub_63)@(L(unit)))[@inline] in
let restore_context#259 =
  fun _u#1962 -> ((poly_stub_62)@(L(unit)))[@inline] in
let save_context#260 = fun _u#1964 -> ((poly_stub_61)@(L(unit)))[@inline] in
let drop_context#261 = fun _u#1966 -> ((poly_stub_60)@(L(unit)))[@inline] in
let set_baker_policy#264 =
  fun _bp#1972 -> ((poly_stub_59)@(L(unit)))[@inline] in
let set_baker#265 = fun _a#1974 -> ((poly_stub_58)@(L(unit)))[@inline] in
let size#266 = fun _c#1976 -> ((poly_stub_57)@(L(unit)))[@inline] in
let read_contract_from_file#268 =
  fun _fn#1980 -> ((poly_stub_56)@(L(unit)))[@inline] in
let chr#269 = fun _n#1982 -> ((poly_stub_55)@(L(unit)))[@inline] in
let nl#270 = L("NEWLINE")[@inline] in
let println#271 = fun _v#1985 -> ((poly_stub_54)@(L(unit)))[@inline] in
let transfer#272 =
  fun _a#1987 -> (fun _s#1988 -> (fun _t#1989 -> ((poly_stub_53)@(L(unit)))))[@inline] in
let transfer_exn#273 =
  fun _a#1991 -> (fun _s#1992 -> (fun _t#1993 -> ((poly_stub_52)@(L(unit)))))[@inline] in
let reset_state#275 =
  fun _n#1997 -> (fun _l#1998 -> ((poly_stub_51)@(L(unit))))[@inline] in
let reset_state_at#276 =
  fun _t#2000 -> (fun _n#2001 -> (fun _l#2002 -> ((poly_stub_50)@(L(unit)))))[@inline] in
let save_mutation#279 =
  fun _s#2011 -> (fun _m#2012 -> ((poly_stub_49)@(L(unit))))[@inline] in
let sign#282 =
  fun _sk#2020 -> (fun _d#2021 -> ((poly_stub_48)@(L(unit))))[@inline] in
let add_account#283 =
  fun _s#2023 -> (fun _k#2024 -> ((poly_stub_47)@(L(unit))))[@inline] in
let baker_account#284 =
  fun _p#2026 -> (fun _o#2027 -> ((poly_stub_46)@(L(unit))))[@inline] in
let create_chest#286 =
  fun _b#2032 -> (fun _n#2033 -> ((poly_stub_45)@(L(unit))))[@inline] in
let create_chest_key#287 =
  fun _c#2035 -> (fun _n#2036 -> ((poly_stub_44)@(L(unit))))[@inline] in
let michelson_equal#290 =
  fun _m1#2046 -> (fun _m2#2047 -> ((poly_stub_43)@(L(unit))))[@inline] in
let originate_contract#292 =
  fun _c#2052 -> (fun _s#2053 -> (fun _t#2054 -> ((poly_stub_42)@(L(unit)))))[@inline] in
let compile_contract_from_file#294 =
  fun _fn#2060 ->
  (fun _e#2061 -> (fun _v#2062 -> ((poly_stub_41)@(L(unit)))))[@inline] in
let originate_from_file#295 =
  fun _fn#2064 ->
  (fun _e#2065 ->
   (fun _v#2066 ->
    (fun _s#2067 -> (fun _t#2068 -> ((poly_stub_40)@(L(unit)))))))[@inline] in
let get_balance#296 =
  fun _u#2070 -> (({ DROP ; BALANCE })@(L(unit)))[@inline] in
let get_amount#297 =
  fun _u#2072 -> (({ DROP ; AMOUNT })@(L(unit)))[@inline] in
let get_now#298 = fun _u#2074 -> (({ DROP ; NOW })@(L(unit)))[@inline] in
let get_sender#299 =
  fun _u#2076 -> (({ DROP ; SENDER })@(L(unit)))[@inline] in
let get_source#300 =
  fun _u#2078 -> (({ DROP ; SOURCE })@(L(unit)))[@inline] in
let get_level#301 = fun _u#2080 -> (({ DROP ; LEVEL })@(L(unit)))[@inline] in
let get_self_address#302 = fun _u#2082 -> (SELF_ADDRESS())[@inline] in
let get_chain_id#303 =
  fun _u#2084 -> (({ DROP ; CHAIN_ID })@(L(unit)))[@inline] in
let get_total_voting_power#304 =
  fun _u#2086 -> (({ DROP ; TOTAL_VOTING_POWER })@(L(unit)))[@inline] in
let get_min_block_time#305 =
  fun _u#2088 -> (({ DROP ; MIN_BLOCK_TIME })@(L(unit)))[@inline] in
let voting_power#306 =
  fun kh#2090 -> (({ VOTING_POWER })@(kh#2090))[@inline] in
let implicit_account#308 =
  fun kh#2094 -> (IMPLICIT_ACCOUNT(kh#2094))[@inline] in
let pairing_check#312 =
  fun l#2102 -> (({ PAIRING_CHECK })@(l#2102))[@inline] in
let set_delegate#314 = fun o#2106 -> (SET_DELEGATE(o#2106))[@inline] in
let open_chest#322 =
  fun gen#2130 ->
  (let (gen#2676, gen#2677) = gen#2130 in
   let (gen#2678, gen#2679) = gen#2676 in
   let ck#2131 = gen#2678 in
   let c#2132 = gen#2679 in
   let n#2133 = gen#2677 in OPEN_CHEST(ck#2131 , c#2132 , n#2133))[@inline] in
let xor#331 =
  fun gen#2176 ->
  (let (gen#2680, gen#2681) = gen#2176 in
   let l#2177 = gen#2680 in let r#2178 = gen#2681 in XOR(l#2177 , r#2178))[@inline] in
let or#332 =
  fun gen#2180 ->
  (let (gen#2682, gen#2683) = gen#2180 in
   let l#2181 = gen#2682 in let r#2182 = gen#2683 in OR(l#2181 , r#2182))[@inline] in
let shift_left#333 =
  fun gen#2184 ->
  (let (gen#2684, gen#2685) = gen#2184 in
   let l#2185 = gen#2684 in let r#2186 = gen#2685 in LSL(l#2185 , r#2186))[@inline] in
let shift_right#334 =
  fun gen#2188 ->
  (let (gen#2686, gen#2687) = gen#2188 in
   let l#2189 = gen#2686 in let r#2190 = gen#2687 in LSR(l#2189 , r#2190))[@inline] in
let length#379 = fun b#2366 -> (({ SIZE })@(b#2366))[@inline] in
let concat#380 =
  fun gen#2368 ->
  (let (gen#2688, gen#2689) = gen#2368 in
   let b1#2369 = gen#2688 in
   let b2#2370 = gen#2689 in ({ UNPAIR ; CONCAT })@(PAIR(b1#2369 , b2#2370)))[@inline] in
let sub#381 =
  fun gen#2372 ->
  (let (gen#2690, gen#2691) = gen#2372 in
   let (gen#2692, gen#2693) = gen#2690 in
   let s#2373 = gen#2692 in
   let l#2374 = gen#2693 in
   let b#2375 = gen#2691 in
   ({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2373 ,
                                                                 l#2374) ,
                                                            b#2375)))[@inline] in
let length#387 = fun b#2392 -> (({ SIZE })@(b#2392))[@inline] in
let concat#388 =
  fun gen#2394 ->
  (let (gen#2694, gen#2695) = gen#2394 in
   let b1#2395 = gen#2694 in
   let b2#2396 = gen#2695 in ({ UNPAIR ; CONCAT })@(PAIR(b1#2395 , b2#2396)))[@inline] in
let sub#389 =
  fun gen#2398 ->
  (let (gen#2696, gen#2697) = gen#2398 in
   let (gen#2698, gen#2699) = gen#2696 in
   let s#2399 = gen#2698 in
   let l#2400 = gen#2699 in
   let b#2401 = gen#2697 in
   ({ UNPAIR ;
     UNPAIR ;
     SLICE ;
     IF_NONE { PUSH string "SLICE" ; FAILWITH } {} })@(PAIR(PAIR(s#2399 ,
                                                                 l#2400) ,
                                                            b#2401)))[@inline] in
let blake2b#390 = fun b#2403 -> (({ BLAKE2B })@(b#2403))[@inline] in
let sha256#391 = fun b#2405 -> (({ SHA256 })@(b#2405))[@inline] in
let sha512#392 = fun b#2407 -> (({ SHA512 })@(b#2407))[@inline] in
let sha3#393 = fun b#2409 -> (({ SHA3 })@(b#2409))[@inline] in
let keccak#394 = fun b#2411 -> (({ KECCAK })@(b#2411))[@inline] in
let hash_key#395 = fun k#2413 -> (({ HASH_KEY })@(k#2413))[@inline] in
let check#396 =
  fun gen#2415 ->
  (let (gen#2700, gen#2701) = gen#2415 in
   let (gen#2702, gen#2703) = gen#2700 in
   let k#2416 = gen#2702 in
   let s#2417 = gen#2703 in
   let b#2418 = gen#2701 in
   ({ UNPAIR ; UNPAIR ; CHECK_SIGNATURE })@(PAIR(PAIR(k#2416 , s#2417) ,
                                                 b#2418)))[@inline] in
let assert#397 =
  fun b#2420 ->
  (({ IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } })@(b#2420))[@inline] in
let abs#400 = fun i#2426 -> (({ ABS })@(i#2426))[@inline] in
let is_nat#401 = fun i#2428 -> (({ ISNAT })@(i#2428))[@inline] in
let true#402 = TRUE()[@inline] in
let false#403 = FALSE()[@inline] in
let unit#404 = UNIT()[@inline] in
let assert_with_error#408 =
  fun gen#2438 ->
  (let (gen#2704, gen#2705) = gen#2438 in
   let b#2439 = gen#2704 in
   let s#2440 = gen#2705 in
   ({ UNPAIR ; IF { DROP ; UNIT } { FAILWITH } })@(PAIR(b#2439 , s#2440)))[@inline] in
let poly_stub_39 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_38 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_37 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_36 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_35 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_34 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_33 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_32 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_31 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_30 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_29 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_28 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_27 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_26 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_25 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_24 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_23 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_22 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_21 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_20 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_19 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_18 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_17 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_16 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_15 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_14 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_13 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_12 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_11 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_10 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_9 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_8 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_7 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_6 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_5 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_4 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_3 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_2 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let poly_stub_1 = fun x#2454 -> (({ FAILWITH })@(x#2454))[@inline] in
let get_total_voting_power#416 =
  fun _u#2464 -> ((poly_stub_39)@(L(unit)))[@inline] in
let set_source#419 = fun _a#2470 -> ((poly_stub_38)@(L(unit)))[@inline] in
let get_storage_of_address#420 =
  fun _a#2472 -> ((poly_stub_37)@(L(unit)))[@inline] in
let get_balance#421 = fun _a#2474 -> ((poly_stub_36)@(L(unit)))[@inline] in
let print#422 = fun _v#2476 -> ((poly_stub_35)@(L(unit)))[@inline] in
let eprint#423 = fun _v#2478 -> ((poly_stub_34)@(L(unit)))[@inline] in
let get_voting_power#424 =
  fun _kh#2480 -> ((poly_stub_33)@(L(unit)))[@inline] in
let nth_bootstrap_contract#425 =
  fun _i#2482 -> ((poly_stub_32)@(L(unit)))[@inline] in
let nth_bootstrap_account#426 =
  fun _i#2484 -> ((poly_stub_31)@(L(unit)))[@inline] in
let get_bootstrap_account#427 =
  fun _n#2486 -> ((poly_stub_30)@(L(unit)))[@inline] in
let last_originations#429 =
  fun _u#2490 -> ((poly_stub_29)@(L(unit)))[@inline] in
let new_account#431 = fun _u#2494 -> ((poly_stub_28)@(L(unit)))[@inline] in
let bake_until_n_cycle_end#433 =
  fun _n#2498 -> ((poly_stub_27)@(L(unit)))[@inline] in
let register_delegate#435 =
  fun _kh#2502 -> ((poly_stub_26)@(L(unit)))[@inline] in
let register_constant#436 =
  fun _m#2504 -> ((poly_stub_25)@(L(unit)))[@inline] in
let constant_to_michelson_program#438 =
  fun _s#2508 -> ((poly_stub_24)@(L(unit)))[@inline] in
let restore_context#439 =
  fun _u#2510 -> ((poly_stub_23)@(L(unit)))[@inline] in
let save_context#440 = fun _u#2512 -> ((poly_stub_22)@(L(unit)))[@inline] in
let drop_context#441 = fun _u#2514 -> ((poly_stub_21)@(L(unit)))[@inline] in
let set_baker_policy#444 =
  fun _bp#2520 -> ((poly_stub_20)@(L(unit)))[@inline] in
let set_baker#445 = fun _a#2522 -> ((poly_stub_19)@(L(unit)))[@inline] in
let size#446 = fun _c#2524 -> ((poly_stub_18)@(L(unit)))[@inline] in
let read_contract_from_file#448 =
  fun _fn#2528 -> ((poly_stub_17)@(L(unit)))[@inline] in
let chr#449 = fun _n#2530 -> ((poly_stub_16)@(L(unit)))[@inline] in
let nl#450 = L("NEWLINE")[@inline] in
let println#451 = fun _v#2533 -> ((poly_stub_15)@(L(unit)))[@inline] in
let transfer#452 =
  fun gen#2535 ->
  (let (gen#2706, gen#2707) = gen#2535 in
   let (gen#2708, gen#2709) = gen#2706 in
   let _a#2536 = gen#2708 in
   let _s#2537 = gen#2709 in
   let _t#2538 = gen#2707 in (poly_stub_14)@(L(unit)))[@inline] in
let transfer_exn#453 =
  fun gen#2540 ->
  (let (gen#2710, gen#2711) = gen#2540 in
   let (gen#2712, gen#2713) = gen#2710 in
   let _a#2541 = gen#2712 in
   let _s#2542 = gen#2713 in
   let _t#2543 = gen#2711 in (poly_stub_13)@(L(unit)))[@inline] in
let reset_state#455 =
  fun gen#2547 ->
  (let (gen#2714, gen#2715) = gen#2547 in
   let _n#2548 = gen#2714 in
   let _l#2549 = gen#2715 in (poly_stub_12)@(L(unit)))[@inline] in
let reset_state_at#456 =
  fun gen#2551 ->
  (let (gen#2716, gen#2717) = gen#2551 in
   let (gen#2718, gen#2719) = gen#2716 in
   let _t#2552 = gen#2718 in
   let _n#2553 = gen#2719 in
   let _l#2554 = gen#2717 in (poly_stub_11)@(L(unit)))[@inline] in
let save_mutation#459 =
  fun gen#2565 ->
  (let (gen#2720, gen#2721) = gen#2565 in
   let _s#2566 = gen#2720 in
   let _m#2567 = gen#2721 in (poly_stub_10)@(L(unit)))[@inline] in
let sign#462 =
  fun gen#2577 ->
  (let (gen#2722, gen#2723) = gen#2577 in
   let _sk#2578 = gen#2722 in
   let _d#2579 = gen#2723 in (poly_stub_9)@(L(unit)))[@inline] in
let add_account#463 =
  fun gen#2581 ->
  (let (gen#2724, gen#2725) = gen#2581 in
   let _s#2582 = gen#2724 in
   let _k#2583 = gen#2725 in (poly_stub_8)@(L(unit)))[@inline] in
let baker_account#464 =
  fun gen#2585 ->
  (let (gen#2726, gen#2727) = gen#2585 in
   let _p#2586 = gen#2726 in
   let _o#2587 = gen#2727 in (poly_stub_7)@(L(unit)))[@inline] in
let create_chest#466 =
  fun gen#2593 ->
  (let (gen#2728, gen#2729) = gen#2593 in
   let _b#2594 = gen#2728 in
   let _n#2595 = gen#2729 in (poly_stub_6)@(L(unit)))[@inline] in
let create_chest_key#467 =
  fun gen#2597 ->
  (let (gen#2730, gen#2731) = gen#2597 in
   let _c#2598 = gen#2730 in
   let _n#2599 = gen#2731 in (poly_stub_5)@(L(unit)))[@inline] in
let michelson_equal#470 =
  fun gen#2611 ->
  (let (gen#2732, gen#2733) = gen#2611 in
   let _m1#2612 = gen#2732 in
   let _m2#2613 = gen#2733 in (poly_stub_4)@(L(unit)))[@inline] in
let originate_contract#472 =
  fun gen#2619 ->
  (let (gen#2734, gen#2735) = gen#2619 in
   let (gen#2736, gen#2737) = gen#2734 in
   let _c#2620 = gen#2736 in
   let _s#2621 = gen#2737 in
   let _t#2622 = gen#2735 in (poly_stub_3)@(L(unit)))[@inline] in
let compile_contract_from_file#474 =
  fun gen#2629 ->
  (let (gen#2738, gen#2739) = gen#2629 in
   let (gen#2740, gen#2741) = gen#2738 in
   let _fn#2630 = gen#2740 in
   let _e#2631 = gen#2741 in
   let _v#2632 = gen#2739 in (poly_stub_2)@(L(unit)))[@inline] in
let originate_from_file#475 =
  fun gen#2634 ->
  (let (gen#2742, gen#2743) = gen#2634 in
   let (gen#2744, gen#2745) = gen#2742 in
   let (gen#2748, gen#2749) = gen#2744 in
   let _fn#2635 = gen#2748 in
   let _e#2636 = gen#2749 in
   let (gen#2746, gen#2747) = gen#2745 in
   let _v#2637 = gen#2746 in
   let _s#2638 = gen#2747 in
   let _t#2639 = gen#2743 in (poly_stub_1)@(L(unit)))[@inline] in
let assert = assert#217[@inline] in
let abs = abs#220[@inline] in
let is_nat = is_nat#221[@inline] in
let true = true#222[@inline] in
let false = false#223[@inline] in
let unit = unit#224[@inline] in
let assert_with_error = assert_with_error#228[@inline] in
let toto#476 = L(1) in
let toto#477 = L(32) in
let titi#478 = ADD(toto#476 , L(42)) in
let f#479 =
  fun gen#2659 ->
  (let (gen#2750, gen#2751) = gen#2659 in
   let gen#2660 = gen#2750 in
   let x#2661 = gen#2751 in
   let x#2662 = ADD(ADD(x#2661 , toto#476) , titi#478) in
   PAIR(LIST_EMPTY() , x#2662)) in
let toto#480 = L(44) in
let toto#481 = L(43) in
let tata#482 = ADD(toto#476 , titi#478) in
let foo#483 = (f#479)@(PAIR(L(unit) , L(3))) in
let toto#484 = L(10) in
let foo#485 = L("bar") in
let toto = ADD(toto#484 , toto#476) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun gen#2672 ->
  (let (gen#2752, gen#2753) = gen#2672 in
   let p#2673 = gen#2752 in
   let s#2674 = gen#2753 in
   let s#2675 = ADD(ADD(p#2673 , s#2674) , toto) in
   PAIR(LIST_EMPTY() , s#2675)) in
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
