open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    let #A#a#1 = 42 in
    let #B#b#2 = 1 in
    let x = #A#a#1 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {|
    let #A#a#1 = 40 in
    let #B#b#4 = let #LOCAL#inA#ba#2 = 1 in
    let #LOCAL#inA#baa#3 = #LOCAL#inA#ba#2 in
    ADD(#LOCAL#inA#ba#2 ,
    #LOCAL#inA#baa#3) in
    let x = ADD(#A#a#1 , #B#b#4) in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {|
    let #A#a#1 = 1 in
    let #A_s#as#2 = 42 in
    let #B#x#3 = #A#a#1 in
    let #B#b#4 = #A_s#as#2 in
    let x = #A_s#as#2 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
  let #A_s#as#1 = 20 in
  let #A#s_as#2 = 22 in
  let x = ADD(#A_s#as#1 , #A#s_as#2) in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {|
  let #A#a#1 = 1 in
  let #A#A_s#as#2 = 42 in
  let #A#A_s#as#3 = 3 in
  let x = #A#A_s#as#2 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {|
  let #Foo#x#1 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#2 = x in
  let #LOCAL#inFoo#y#3 = #Foo#x#1 in
  let #LOCAL#inFoo#z#4 = #LOCAL#inFoo#y#3 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#2 , #LOCAL#inFoo#y#3) , x) ,
  #LOCAL#inFoo#z#4) in
  let x = foo in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
  let #A#v#1 = 40 in
  let #A#B#v#2 = ADD(#A#v#1 , 1) in
  let #A#B#C#v#3 = ADD(#A#B#v#2 , 1) in
  let x = #A#B#C#v#3 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {|
  let #Foo#x#1 = 41 in
  let x = 1 in
  let #TFoo#x#2 = x in
  let #TFoo#y#3 = #Foo#x#1 in
  let u = ADD(#TFoo#x#2 , #TFoo#y#3) in
  let x = u in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {|
  let #A#B#x#1 = 41 in
  let #A#B#x#2 = ADD(#A#B#x#1 , 1) in
  let x = #A#B#x#2 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {|
  let #A#B#x#1 = 42 in
  let #A#B#x#2 = 2 in
  let #A#y#3 = #A#B#x#1 in
  let x = #A#y#3 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {|
  let #Foo#x#1 = 19 in
  let #Foo#y#2 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#1 in
  let v = #Foo#y#2 in
  ADD(ADD(u , v) ,
  x) in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {|
  let #F#F#a#1 = 42 in
  let #F#F#x#2 = #F#F#a#1 in
  let x = #F#F#x#2 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {|
  let #A#current_turn#4 = lambda (i) return ADD(i , +1) in
  let #A#other#5 = lambda (n) return let current_turn = (#A#current_turn#4)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (#1) return  match #1 with
                                  | ( _p , _s ) ->
                                  ( LIST_EMPTY() , (#A#other#5)@(+2) ) in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "bug_alias13.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage unit ;
      code { DROP ;
             PUSH nat 1 ;
             PUSH nat 1 ;
             ADD ;
             PUSH nat 2 ;
             COMPARE ;
             EQ ;
             IF { UNIT } { PUSH string "failed assertion" ; FAILWITH } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "effects.mligo" ] ;
  [%expect {|
    { parameter int ;
      storage int ;
      code { CDR ; PUSH string "foo" ; FAILWITH } } |}]
