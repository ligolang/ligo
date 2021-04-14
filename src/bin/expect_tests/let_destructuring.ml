open Cli_expect

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t1" ] ;
  [%expect{| 1 |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t2" ] ;
  [%expect{| "7" |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t3" ] ;
  [%expect{| ( 3 , +3 , "7" ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t4" ] ;
  [%expect{| ( 4 , +3 ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t5" ] ;
  [%expect{| +1 |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t6" ] ;
  [%expect{| ( 3 , +2 ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t7" ] ;
  [%expect{| ( 2 , +3 ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t8" ] ;
  [%expect{| ( 2 , +2 ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.mligo") ; "t9" ] ;
  [%expect{| 2 |}] ;
  run_ligo_bad [ "interpret" ; "--init="^(bad_test "let_destructuring.mligo") ; "t1" ] ;
  [%expect{|
    File "../../test/contracts/negative/let_destructuring.mligo", line 4, characters 6-23:
      3 | let t1 =
      4 |   let { a = a ; f = b }  = { a = 1 ; b = 1n } in
      5 |   (a,b)

    Pattern do not conform type record[a -> int , b -> nat] |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.religo") ; "t1" ] ;
  [%expect{| 1 |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.religo") ; "t2" ] ;
  [%expect{| "7" |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.religo") ; "t3" ] ;
  [%expect{| ( 3 , +3 , "7" ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.religo") ; "t4" ] ;
   [%expect{| ( 4 , +3 ) |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.ligo") ; "t1" ] ;
  [%expect{| 1 |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.ligo") ; "t2" ] ;
  [%expect{| "7" |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.ligo") ; "t3" ] ;
  [%expect{| ( 3 , +3 , "7" ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.ligo") ; "t4" ] ;
  [%expect{| ( 4 , +3 ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.ligo") ; "t5" ] ;
  [%expect{| +1 |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.ligo") ; "t6" ] ;
  [%expect{| ( 3 , +2 ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.ligo") ; "t7" ] ;
  [%expect{| ( 2 , +3 ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.ligo") ; "t8" ] ;
  [%expect{| ( 2 , +2 ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.ligo") ; "t9" ] ;
  [%expect{| 2 |}] ;
  run_ligo_bad [ "interpret" ; "--init="^(bad_test "let_destructuring.ligo") ; "t1" ] ;
  [%expect{|
    File "../../test/contracts/negative/let_destructuring.ligo", line 4, characters 6-30:
      3 | const t1 = block {
      4 |   var record [ a = a ; f = b ] := record [ a = 1 ; b = 1n ] ;
      5 | } with (a,b)

    Pattern do not conform type record[a -> int , b -> nat] |}]