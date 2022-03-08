open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    let #A#a#2 = 42 in
    let #B#b#3 = 1 in
    let x = #A#a#2 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect{|
    let #A#a#2 = 40 in
    let #B#b#5 = let #LOCAL#inA#ba#3 = 1 in
    let #LOCAL#inA#baa#4 = #LOCAL#inA#ba#3 in
    ADD(#LOCAL#inA#ba#3 ,
    #LOCAL#inA#baa#4) in
    let x = ADD(#A#a#2 , #B#b#5) in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect{|
    let #A#a#2 = 1 in
    let #A_s#as#3 = 42 in
    let #B#x#4 = #A#a#2 in
    let #B#b#5 = #A_s#as#3 in
    let x = #A_s#as#3 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
  let #A_s#as#2 = 20 in
  let #A#s_as#3 = 22 in
  let x = ADD(#A_s#as#2 , #A#s_as#3) in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect{|
    let #A#a#2 = 1 in
    let #A#A_s#as#3 = 42 in
    let #A#A_s#as#4 = 3 in
    let x = #A#A_s#as#3 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect{|
    let #Foo#x#2 = 1 in
    let foo = let x = 20 in
    let #LOCAL#inFoo#x#3 = x in
    let #LOCAL#inFoo#y#4 = #Foo#x#2 in
    let #LOCAL#inFoo#z#5 = #LOCAL#inFoo#y#4 in
    ADD(ADD(ADD(#LOCAL#inFoo#x#3 , #LOCAL#inFoo#y#4) , x) ,
    #LOCAL#inFoo#z#5) in
    let x = foo in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
  let #A#v#2 = 40 in
  let #A#B#v#3 = ADD(#A#v#2 , 1) in
  let #A#B#C#v#4 = ADD(#A#B#v#3 , 1) in
  let x = #A#B#C#v#4 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect{|
    let #Foo#x#2 = 41 in
    let x = 1 in
    let #TFoo#x#3 = x in
    let #TFoo#y#4 = #Foo#x#2 in
    let u = ADD(#TFoo#x#3 , #TFoo#y#4) in
    let x = u in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect{|
    let #A#B#x#2 = 41 in
    let #A#B#x#3 = ADD(#A#B#x#2 , 1) in
    let x = #A#B#x#3 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect{|
    let #A#B#x#2 = 42 in
    let #A#B#x#3 = 2 in
    let #A#y#4 = #A#B#x#2 in
    let x = #A#y#4 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect{|
    let #Foo#x#2 = 19 in
    let #Foo#y#3 = 22 in
    let x = let x = 1 in
    let u = #Foo#x#2 in
    let v = #Foo#y#3 in
    ADD(ADD(u , v) ,
    x) in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect{|
    let #F#F#a#2 = 42 in
    let #F#F#x#3 = #F#F#a#2 in
    let x = #F#F#x#3 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {|
  let #A#current_turn#5 = lambda (i) return ADD(i , +1) in
  let #A#other#6 = lambda (n) return let current_turn = (#A#current_turn#5)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (gen#2) return  match gen#2 with
                                     | ( _p , _s ) ->
                                     ( LIST_EMPTY() , (#A#other#6)@(+2) ) in
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
