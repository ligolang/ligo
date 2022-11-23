open Cli_expect

let%expect_test _ =
  run_ligo_good [ "run"; "interpret" ; "t1" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect{| 1 |}] ;
  run_ligo_good [ "run"; "interpret" ; "t2" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect{| "7" |}] ;
  run_ligo_good [ "run"; "interpret" ; "t3" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect{| ( 3 , +3 , "7" ) |}] ;
  run_ligo_good [ "run"; "interpret" ; "t4" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect{| ( 4 , +3 ) |}] ;
  run_ligo_good [ "run"; "interpret" ; "t5" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect{| +1 |}] ;
  run_ligo_good [ "run"; "interpret" ; "t6" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect{| ( 3 , +2 ) |}] ;
  run_ligo_good [ "run"; "interpret" ; "t7" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect{| ( 2 , +3 ) |}] ;
  run_ligo_good [ "run"; "interpret" ; "t8" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect{| ( 2 , +2 ) |}] ;
  run_ligo_good [ "run"; "interpret" ; "t9" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect{| 2 |}] ;
  run_ligo_bad [ "run"; "interpret" ; "t1" ; "--init-file";(bad_test "let_destructuring.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/let_destructuring.mligo", line 4, characters 6-23:
      3 | let t1 =
      4 |   let { a = a ; f = b }  = { a = 1 ; b = 1n } in
      5 |   (a,b)

    Pattern not of the expected type "foo". |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret" ; "t1"; "--init-file";(test "let_destructuring.religo") ] ;
  [%expect{| 1 |}] ;
  run_ligo_good [ "run"; "interpret" ; "t2"; "--init-file";(test "let_destructuring.religo") ] ;
  [%expect{| "7" |}] ;
  run_ligo_good [ "run"; "interpret" ; "t3"; "--init-file";(test "let_destructuring.religo") ] ;
  [%expect{| ( 3 , +3 , "7" ) |}] ;
  run_ligo_good [ "run"; "interpret" ; "t4"; "--init-file";(test "let_destructuring.religo") ] ;
   [%expect{| ( 4 , +3 ) |}]

let%expect_test _ =
  run_ligo_good ["run"; "interpret" ; "t1" ; "--init-file";(test "let_destructuring.ligo") ] ;
  [%expect{| 1 |}] ;
  run_ligo_good ["run"; "interpret" ; "t2" ; "--init-file";(test "let_destructuring.ligo") ] ;
  [%expect{| "7" |}] ;
  run_ligo_good ["run"; "interpret" ; "t3" ; "--init-file";(test "let_destructuring.ligo") ] ;
  [%expect{| ( 3 , +3 , "7" ) |}] ;
  run_ligo_good ["run"; "interpret" ; "t4" ; "--init-file";(test "let_destructuring.ligo") ] ;
  [%expect{| ( 4 , +3 ) |}] ;
  run_ligo_good ["run"; "interpret" ; "t5" ; "--init-file";(test "let_destructuring.ligo") ] ;
  [%expect{| +1 |}] ;
  run_ligo_good ["run"; "interpret" ; "t6" ; "--init-file";(test "let_destructuring.ligo") ] ;
  [%expect{| ( 3 , +2 ) |}] ;
  run_ligo_good ["run"; "interpret" ; "t7" ; "--init-file";(test "let_destructuring.ligo") ] ;
  [%expect{| ( 2 , +3 ) |}] ;
  run_ligo_good ["run"; "interpret" ; "t8" ; "--init-file";(test "let_destructuring.ligo") ] ;
  [%expect{| ( 2 , +2 ) |}] ;
  run_ligo_good ["run"; "interpret" ; "t9" ; "--init-file";(test "let_destructuring.ligo") ] ;
  [%expect{| 2 |}] ;
  run_ligo_bad ["run"; "interpret" ; "t1" ; "--init-file";(bad_test "let_destructuring.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/let_destructuring.ligo", line 4, characters 6-30:
      3 | const t1 = {
      4 |   var record [ a = a ; f = b ] := record [ a = 1 ; b = 1n ] ;
      5 | } with (a,b)

    Pattern not of the expected type "foo". |}] ;
  run_ligo_bad ["run"; "interpret" ; "type t = {a:int;b:int} in let x = {a=2;b=3} in let {a} = x in a" ; "--syntax" ; "cameligo" ] ;
  [%expect{|
    Invalid type(s)
    Cannot unify "record[a -> ^a]" with "t".
    Hint: "^a" represent placeholder type(s). |}] ;
  run_ligo_bad ["run"; "interpret" ; "type t = {a:int;b:int} in let x = {a=2;b=3} in let {a ; b ; c} = x in a" ; "--syntax" ; "cameligo" ] ;
  [%expect{|
    Invalid type(s)
    Cannot unify "record[a -> ^a , b -> ^b , c -> ^c]" with "t".
    Hint: "^a", "^b", "^c" represent placeholder type(s). |}];

  run_ligo_good ["run"; "interpret" ; "t1" ; "--init-file";(test "let_destructuring.jsligo") ] ;
  [%expect{| 1 |}] ;
  run_ligo_good ["run"; "interpret" ; "t2" ; "--init-file";(test "let_destructuring.jsligo") ] ;
  [%expect{| 1 |}] ;
  run_ligo_good ["run"; "interpret" ; "t3" ; "--init-file";(test "let_destructuring.jsligo") ] ;
  [%expect{| +3 |}] ;
