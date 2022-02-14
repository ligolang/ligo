open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    let #A#a#37 = 42 in
    let #B#b#38 = 1 in
    let x = #A#a#37 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {|
    let #A#a#37 = 40 in
    let #B#b#40 = let #LOCAL#inA#ba#38 = 1 in
    let #LOCAL#inA#baa#39 = #LOCAL#inA#ba#38 in
    ADD(#LOCAL#inA#ba#38 ,
    #LOCAL#inA#baa#39) in
    let x = ADD(#A#a#37 , #B#b#40) in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {|
    let #A#a#37 = 1 in
    let #A_s#as#38 = 42 in
    let #B#x#39 = #A#a#37 in
    let #B#b#40 = #A_s#as#38 in
    let x = #A_s#as#38 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
  let #A_s#as#37 = 20 in
  let #A#s_as#38 = 22 in
  let x = ADD(#A_s#as#37 , #A#s_as#38) in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {|
  let #A#a#37 = 1 in
  let #A#A_s#as#38 = 42 in
  let #A#A_s#as#39 = 3 in
  let x = #A#A_s#as#38 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {|
  let #Foo#x#37 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#38 = x in
  let #LOCAL#inFoo#y#39 = #Foo#x#37 in
  let #LOCAL#inFoo#z#40 = #LOCAL#inFoo#y#39 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#38 , #LOCAL#inFoo#y#39) , x) ,
  #LOCAL#inFoo#z#40) in
  let x = foo in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
  let #A#v#37 = 40 in
  let #A#B#v#38 = ADD(#A#v#37 , 1) in
  let #A#B#C#v#39 = ADD(#A#B#v#38 , 1) in
  let x = #A#B#C#v#39 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {|
  let #Foo#x#37 = 41 in
  let x = 1 in
  let #TFoo#x#38 = x in
  let #TFoo#y#39 = #Foo#x#37 in
  let u = ADD(#TFoo#x#38 , #TFoo#y#39) in
  let x = u in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {|
  let #A#B#x#37 = 41 in
  let #A#B#x#38 = ADD(#A#B#x#37 , 1) in
  let x = #A#B#x#38 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {|
  let #A#B#x#37 = 42 in
  let #A#B#x#38 = 2 in
  let #A#y#39 = #A#B#x#37 in
  let x = #A#y#39 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {|
  let #Foo#x#37 = 19 in
  let #Foo#y#38 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#37 in
  let v = #Foo#y#38 in
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
  let #F#F#a#37 = 42 in
  let #F#F#x#38 = #F#F#a#37 in
  let x = #F#F#x#38 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {|
  let #A#current_turn#42 = lambda (i) return ADD(i , +1) in
  let #A#other#43 = lambda (n) return let current_turn = (#A#current_turn#42)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (gen#37) return  match gen#37 with
                                      | ( _p , _s ) ->
                                      ( LIST_EMPTY() , (#A#other#43)@(+2) ) in
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
