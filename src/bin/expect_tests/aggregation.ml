open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    let ##A#a1 = 42 in
    let ##B#b2 = 1 in
    let x = ##A#a1 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {|
    let ##A#a1 = 40 in
    let ##B#b4 = let ##LOCAL#inA#ba2 = 1 in
    let ##LOCAL#inA#baa3 = ##LOCAL#inA#ba2 in
    ADD(##LOCAL#inA#ba2 ,
    ##LOCAL#inA#baa3) in
    let x = ADD(##A#a1 , ##B#b4) in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {|
    let ##A#a1 = 1 in
    let ##A_s#as2 = 42 in
    let ##B#x3 = ##A#a1 in
    let ##B#b4 = ##A_s#as2 in
    let x = ##A_s#as2 in
    unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
  let ##A_s#as1 = 20 in
  let ##A#s_as2 = 22 in
  let x = ADD(##A_s#as1 , ##A#s_as2) in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {|
  let ##A#a1 = 1 in
  let ##A#A_s#as2 = 42 in
  let ##A#A_s#as3 = 3 in
  let x = ##A#A_s#as2 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {|
  let ##Foo#x1 = 1 in
  let foo = let x = 20 in
  let ##LOCAL#inFoo#x2 = x in
  let ##LOCAL#inFoo#y3 = ##Foo#x1 in
  let ##LOCAL#inFoo#z4 = ##LOCAL#inFoo#y3 in
  ADD(ADD(ADD(##LOCAL#inFoo#x2 , ##LOCAL#inFoo#y3) , x) ,
  ##LOCAL#inFoo#z4) in
  let x = foo in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
  let ##A#v1 = 40 in
  let ##A#B#v2 = ADD(##A#v1 , 1) in
  let ##A#B#C#v3 = ADD(##A#B#v2 , 1) in
  let x = ##A#B#C#v3 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {|
  let ##Foo#x1 = 41 in
  let x = 1 in
  let ##TFoo#x2 = x in
  let ##TFoo#y3 = ##Foo#x1 in
  let u = ADD(##TFoo#x2 , ##TFoo#y3) in
  let x = u in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {|
  let ##A#B#x1 = 41 in
  let ##A#B#x2 = ADD(##A#B#x1 , 1) in
  let x = ##A#B#x2 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {|
  let ##A#B#x1 = 42 in
  let ##A#B#x2 = 2 in
  let ##A#y3 = ##A#B#x1 in
  let x = ##A#y3 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {|
  let ##Foo#x1 = 19 in
  let ##Foo#y2 = 22 in
  let x = let x = 1 in
  let u = ##Foo#x1 in
  let v = ##Foo#y2 in
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
  let ##F#F#a1 = 42 in
  let ##F#F#x2 = ##F#F#a1 in
  let x = ##F#F#x2 in
  unit |}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {|
  let ##A#current_turn5 = lambda (i) return ADD(i , +1) in
  let ##A#other6 = lambda (n) return let current_turn = (##A#current_turn5)@(+1) in
  ASSERTION(EQ(n ,
  current_turn)) in
  let main = lambda (#1) return let #3 = #1 in
   match #3 with
    | ( _p , _s ) ->
    ( LIST_EMPTY() , (##A#other6)@(+2) ) in
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
