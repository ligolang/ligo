open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    let #A#a#40 = 42 in
    let #B#b#41 = 1 in
    let x = #A#a#40 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {|
    let #A#a#40 = 40 in
    let #B#b#43 = let #LOCAL#inA#ba#41 = 1 in
    let #LOCAL#inA#baa#42 = #LOCAL#inA#ba#41 in
    ADD(#LOCAL#inA#ba#41 ,
    #LOCAL#inA#baa#42) in
    let x = ADD(#A#a#40 , #B#b#43) in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {|
    let #A#a#40 = 1 in
    let #A_s#as#41 = 42 in
    let #B#x#42 = #A#a#40 in
    let #B#b#43 = #A_s#as#41 in
    let x = #A_s#as#41 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
  let #A_s#as#40 = 20 in
  let #A#s_as#41 = 22 in
  let x = ADD(#A_s#as#40 , #A#s_as#41) in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {|
  let #A#a#40 = 1 in
  let #A#A_s#as#41 = 42 in
  let #A#A_s#as#42 = 3 in
  let x = #A#A_s#as#41 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {|
  let #Foo#x#40 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#41 = x in
  let #LOCAL#inFoo#y#42 = #Foo#x#40 in
  let #LOCAL#inFoo#z#43 = #LOCAL#inFoo#y#42 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#41 , #LOCAL#inFoo#y#42) , x) ,
  #LOCAL#inFoo#z#43) in
  let x = foo in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
  let #A#v#40 = 40 in
  let #A#B#v#41 = ADD(#A#v#40 , 1) in
  let #A#B#C#v#42 = ADD(#A#B#v#41 , 1) in
  let x = #A#B#C#v#42 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {|
  let #Foo#x#40 = 41 in
  let x = 1 in
  let #TFoo#x#41 = x in
  let #TFoo#y#42 = #Foo#x#40 in
  let u = ADD(#TFoo#x#41 , #TFoo#y#42) in
  let x = u in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {|
  let #A#B#x#40 = 41 in
  let #A#B#x#41 = ADD(#A#B#x#40 , 1) in
  let x = #A#B#x#41 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {|
  let #A#B#x#40 = 42 in
  let #A#B#x#41 = 2 in
  let #A#y#42 = #A#B#x#40 in
  let x = #A#y#42 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {|
  let #Foo#x#40 = 19 in
  let #Foo#y#41 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#40 in
  let v = #Foo#y#41 in
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
  let #F#F#a#40 = 42 in
  let #F#F#x#41 = #F#F#a#40 in
  let x = #F#F#x#41 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {|
  let #A#current_turn#45 = lambda (i) return ADD(i , +1) in
  let #A#other#46 = lambda (n) return let current_turn = (#A#current_turn#45)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (gen#40) return  match gen#40 with
                                      | ( _p , _s ) ->
                                      ( LIST_EMPTY() , (#A#other#46)@(+2) ) in
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
