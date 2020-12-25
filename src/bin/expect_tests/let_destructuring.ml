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
  run_ligo_bad [ "interpret" ; "--init="^(bad_test "let_destructuring.mligo") ; "t1" ] ;
  [%expect{|
    in file "../../test/contracts/negative/let_destructuring.mligo", line 4, characters 6-23
      3 | let t1 =
      4 |   let { a = a ; f = b }  = { a = 1 ; b = 1n } in
      5 |   (a,b)

    Labels do not match: Expected b but got f . |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.religo") ; "t1" ] ;
  [%expect{| 1 |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.religo") ; "t2" ] ;
  [%expect{| "7" |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.religo") ; "t3" ] ;
  [%expect{| ( 3 , +3 , "7" ) |}] ;
  run_ligo_good [ "interpret" ; "--init="^(test "let_destructuring.religo") ; "t4" ] ;
   [%expect{| ( 4 , +3 ) |}] ;