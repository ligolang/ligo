open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    let #A#a#35 = 42 in
    let #B#b#36 = 1 in
    let x = #A#a#35 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {|
    let #A#a#35 = 40 in
    let #B#b#38 = let #LOCAL#inA#ba#36 = 1 in
    let #LOCAL#inA#baa#37 = #LOCAL#inA#ba#36 in
    ADD(#LOCAL#inA#ba#36 ,
    #LOCAL#inA#baa#37) in
    let x = ADD(#A#a#35 , #B#b#38) in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {|
    let #A#a#35 = 1 in
    let #A_s#as#36 = 42 in
    let #B#x#37 = #A#a#35 in
    let #B#b#38 = #A_s#as#36 in
    let x = #A_s#as#36 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
  let #A_s#as#35 = 20 in
  let #A#s_as#36 = 22 in
  let x = ADD(#A_s#as#35 , #A#s_as#36) in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {|
  let #A#a#35 = 1 in
  let #A#A_s#as#36 = 42 in
  let #A#A_s#as#37 = 3 in
  let x = #A#A_s#as#36 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {|
  let #Foo#x#35 = 1 in
  let foo = let x = 20 in
  let #LOCAL#inFoo#x#36 = x in
  let #LOCAL#inFoo#y#37 = #Foo#x#35 in
  let #LOCAL#inFoo#z#38 = #LOCAL#inFoo#y#37 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#36 , #LOCAL#inFoo#y#37) , x) ,
  #LOCAL#inFoo#z#38) in
  let x = foo in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
  let #A#v#35 = 40 in
  let #A#B#v#36 = ADD(#A#v#35 , 1) in
  let #A#B#C#v#37 = ADD(#A#B#v#36 , 1) in
  let x = #A#B#C#v#37 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {|
  let #Foo#x#35 = 41 in
  let x = 1 in
  let #TFoo#x#36 = x in
  let #TFoo#y#37 = #Foo#x#35 in
  let u = ADD(#TFoo#x#36 , #TFoo#y#37) in
  let x = u in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {|
  let #A#B#x#35 = 41 in
  let #A#B#x#36 = ADD(#A#B#x#35 , 1) in
  let x = #A#B#x#36 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {|
  let #A#B#x#35 = 42 in
  let #A#B#x#36 = 2 in
  let #A#y#37 = #A#B#x#35 in
  let x = #A#y#37 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {|
  let #Foo#x#35 = 19 in
  let #Foo#y#36 = 22 in
  let x = let x = 1 in
  let u = #Foo#x#35 in
  let v = #Foo#y#36 in
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
  let #F#F#a#35 = 42 in
  let #F#F#x#36 = #F#F#a#35 in
  let x = #F#F#x#36 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {|
  let #A#current_turn#40 = lambda (i) return ADD(i , +1) in
  let #A#other#41 = lambda (n) return let current_turn = (#A#current_turn#40)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (gen#35) return  match gen#35 with
                                      | ( _p , _s ) ->
                                      ( LIST_EMPTY() , (#A#other#41)@(+2) ) in
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
