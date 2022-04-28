open Cli_expect

let contract basename =
  "../../test/contracts/aggregation/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias.mligo" ] ;
  [%expect {xxx|
    let #A#a#135 : int = 42 in
    let #B#b#136 : int = 1 in
    let x : int = #A#a#135 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias2.mligo" ] ;
  [%expect {xxx|
    let #A#a#135 : int = 40 in
    let #B#b#138 : int = let #LOCAL#inA#ba#136 : int = 1 in
    let #LOCAL#inA#baa#137 : int = #LOCAL#inA#ba#136 in
    ADD(#LOCAL#inA#ba#136 ,
    #LOCAL#inA#baa#137) in
    let x : int = ADD(#A#a#135 , #B#b#138) in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias2.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias3.mligo" ] ;
  [%expect {xxx|
    let #A#a#135 : int = 1 in
    let #A_s#as#136 : int = 42 in
    let #B#x#137 : int = #A#a#135 in
    let #B#b#138 : int = #A_s#as#136 in
    let x : int = #A_s#as#136 in
    unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias3.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias4.mligo" ] ;
  [%expect {xxx|
  let #A_s#as#135 : int = 20 in
  let #A#s_as#136 : int = 22 in
  let x : int = ADD(#A_s#as#135 , #A#s_as#136) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias4.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias5.mligo" ] ;
  [%expect {xxx|
  let #A#a#135 : int = 1 in
  let #A#A_s#as#136 : int = 42 in
  let #A#A_s#as#137 : int = 3 in
  let x : int = #A#A_s#as#136 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias5.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias6.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#135 : int = 1 in
  let foo : int = let x = 20 in
  let #LOCAL#inFoo#x#136 : int = x in
  let #LOCAL#inFoo#y#137 : int = #Foo#x#135 in
  let #LOCAL#inFoo#z#138 : int = #LOCAL#inFoo#y#137 in
  ADD(ADD(ADD(#LOCAL#inFoo#x#136 , #LOCAL#inFoo#y#137) , x) ,
  #LOCAL#inFoo#z#138) in
  let x : int = foo in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias6.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias7.mligo" ] ;
  [%expect {xxx|
  let #A#v#135 : int = 40 in
  let #A#B#v#136 : int = ADD(#A#v#135 , 1) in
  let #A#B#C#v#137 : int = ADD(#A#B#v#136 , 1) in
  let x : int = #A#B#C#v#137 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias7.mligo" ] ;
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias8.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#135 : int = 41 in
  let x : int = 1 in
  let #TFoo#x#136 : int = x in
  let #TFoo#y#137 : int = #Foo#x#135 in
  let u : int = ADD(#TFoo#x#136 , #TFoo#y#137) in
  let x : int = u in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias8.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias9.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#135 : int = 41 in
  let #A#B#x#136 : int = ADD(#A#B#x#135 , 1) in
  let x : int = #A#B#x#136 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias9.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias10.mligo" ] ;
  [%expect {xxx|
  let #A#B#x#135 : int = 42 in
  let #A#B#x#136 : int = 2 in
  let #A#y#137 : int = #A#B#x#135 in
  let x : int = #A#y#137 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias10.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias11.mligo" ] ;
  [%expect {xxx|
  let #Foo#x#135 : int = 19 in
  let #Foo#y#136 : int = 22 in
  let x : int = let x = 1 in
  let u = #Foo#x#135 in
  let v = #Foo#y#136 in
  ADD(ADD(u , v) ,
  x) in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias11.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias12.mligo" ] ;
  [%expect {xxx|
  let #F#F#a#135 : int = 42 in
  let #F#F#x#136 : int = #F#F#a#135 in
  let x : int = #F#F#x#136 in
  unit |xxx}]
let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "bug_alias12.mligo" ] ;
  [%expect{| 42 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-aggregated" ; contract "bug_alias13.mligo" ] ;
  [%expect {xxx|
  let #A#current_turn#137 : nat -> nat = lambda (i : nat) return ADD(i , +1) in
  let #A#other#138 : nat -> unit = lambda (n : nat) return let current_turn = (#A#current_turn#137)@(+1) in
  (assert)@(EQ(n ,
  current_turn)) in
  let main : ( unit * unit ) -> ( list (operation) * unit ) = lambda (gen#2 : ( unit * unit )) return  match
                                                                      gen#2 with
                                                                      | ( _p , _s ) ->
                                                                      ( LIST_EMPTY() , (#A#other#138)@(+2) ) in
  unit |xxx}]
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
  [%expect{|
    { parameter int ;
      storage int ;
      code { CDR ; PUSH string "foo" ; FAILWITH } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "bug_module_record.ligo" ] ;
  [%expect {|
    L(unit) |}]
