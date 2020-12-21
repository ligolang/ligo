open Cli_expect

let contract basename =
  "../../test/contracts/build/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print-graph" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    `-- ../../test/contracts/build/cycle_A.mligo
        `-- ../../test/contracts/build/cycle_B.mligo
            `-- ../../test/contracts/build/cycle_C.mligo
                `-- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print-graph" ; contract "cycle_A.mligo"; "--display-format=json" ] ;
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
  run_ligo_good [ "print-graph" ; contract "D.mligo" ] ;
  [%expect {|
    `-- ../../test/contracts/build/D.mligo
        |-- ../../test/contracts/build/C.mligo
        |   |-- ../../test/contracts/build/A.mligo
        |   `-- ../../test/contracts/build/B.mligo
        |       `-- ../../test/contracts/build/A.mligo
        `-- ../../test/contracts/build/E.mligo
            |-- ../../test/contracts/build/F.mligo
            `-- ../../test/contracts/build/G.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print-graph" ; contract "D.mligo"; "--display-format=json" ] ;
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
  run_ligo_good [ "compile-contract" ; contract "B.mligo" ; "f" ] ;
  [%expect{|
    { parameter unit ;
      storage int ;
      code { PUSH int 42 ;
             PUSH int 1 ;
             ADD ;
             PUSH int 1 ;
             DIG 2 ;
             CDR ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "print-ast-typed" ; contract "D.mligo" ] ;
  [%expect {|
    const toto = ADD(E.toto ,
    C.B.titi)
    const fb = record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main = lambda (#3) return let s = #3.1 in let p = #3.0 in let s = ADD(ADD(p ,
    s) ,
    toto) in ( LIST_EMPTY() , s ) |}]

let%expect_test _ =
  run_ligo_good [ "print-mini-c" ; contract "D.mligo" ] ;
  [%expect{|
    let ../../test/contracts/build/A.mligo = let toto = L(1) in toto
    let ../../test/contracts/build/B.mligo =
      let A = ../../test/contracts/build/A.mligo[@inline] in
      let toto = L(32) in
      let titi = ADD(A , L(42)) in
      let f =
        fun #2 ->
        (let x = CDR(#2) in
         let #1 = CAR(#2) in
         let x = ADD(ADD(x , A) , titi) in PAIR(LIST_EMPTY() , x)) in
      PAIR(PAIR(A , f) , PAIR(titi , toto))
    let ../../test/contracts/build/C.mligo =
      let A = ../../test/contracts/build/A.mligo[@inline] in
      let B = ../../test/contracts/build/B.mligo[@inline] in
      let tata = ADD(A , CAR(CDR(B))) in
      let foo = (CDR(CAR(B)))@(PAIR(L(unit) , L(3))) in
      PAIR(PAIR(A , B) , PAIR(foo , tata))
    let ../../test/contracts/build/F.mligo = let toto = L(44) in toto
    let ../../test/contracts/build/G.mligo = let toto = L(43) in toto
    let ../../test/contracts/build/E.mligo =
      let F = ../../test/contracts/build/F.mligo[@inline] in
      let G = ../../test/contracts/build/G.mligo[@inline] in
      let toto = L(10) in
      let foo = L("bar") in PAIR(PAIR(F , G) , PAIR(foo , toto))
    let C = ../../test/contracts/build/C.mligo[@inline]
    let E = ../../test/contracts/build/E.mligo[@inline]
    let toto = ADD(CDR(CDR(E)) , CAR(CDR(CDR(CAR(C)))))
    let fb = PAIR(L(1) , PAIR(toto , PAIR(L(2) , L(3))))
    let main =
      fun #3 ->
      (let s = CDR(#3) in
       let p = CAR(#3) in
       let s = ADD(ADD(p , s) , toto) in PAIR(LIST_EMPTY() , s)) |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "D.mligo"; "main" ] ;
  [%expect{|
    { parameter int ;
      storage int ;
      code { PUSH int 1 ;
             PUSH int 42 ;
             SWAP ;
             DUP ;
             DUG 2 ;
             ADD ;
             PUSH int 32 ;
             SWAP ;
             DUP ;
             DUG 2 ;
             PAIR ;
             DIG 2 ;
             DUP ;
             DUG 3 ;
             DIG 2 ;
             PAIR ;
             LAMBDA
               (pair (pair int int) (pair unit int))
               (pair (list operation) int)
               { DUP ;
                 CDR ;
                 SWAP ;
                 CAR ;
                 DUP ;
                 CDR ;
                 SWAP ;
                 CAR ;
                 DIG 2 ;
                 SWAP ;
                 DUG 2 ;
                 CDR ;
                 ADD ;
                 ADD ;
                 NIL operation ;
                 PAIR } ;
             SWAP ;
             APPLY ;
             DIG 2 ;
             DUP ;
             DUG 3 ;
             PAIR ;
             PAIR ;
             DUP ;
             CDR ;
             CAR ;
             DIG 2 ;
             DUP ;
             DUG 3 ;
             ADD ;
             PUSH int 3 ;
             PUSH unit Unit ;
             PAIR ;
             DIG 2 ;
             DUP ;
             DUG 3 ;
             CAR ;
             CDR ;
             SWAP ;
             EXEC ;
             PAIR ;
             SWAP ;
             DIG 2 ;
             PAIR ;
             PAIR ;
             CAR ;
             CDR ;
             CDR ;
             CAR ;
             PUSH int 10 ;
             ADD ;
             SWAP ;
             DUP ;
             DUG 2 ;
             CDR ;
             DIG 2 ;
             CAR ;
             ADD ;
             ADD ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    Dependency cycle detected :
     `-- ../../test/contracts/build/cycle_A.mligo
        `-- ../../test/contracts/build/cycle_B.mligo
            `-- ../../test/contracts/build/cycle_C.mligo
                `-- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "compile-contract" ; contract "type_B.mligo"; "main" ] ;
  [%expect {|
    { parameter string ;
      storage int ;
      code { PUSH int 1 ; SWAP ; CDR ; ADD ; NIL operation ; PAIR } } |}]
