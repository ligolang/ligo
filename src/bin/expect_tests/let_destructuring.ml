open Cli_expect

let%expect_test _ =
  run_ligo_good [ "run"; "interpret" ; "t1" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect.unreachable] ;
  run_ligo_good [ "run"; "interpret" ; "t2" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect.unreachable] ;
  run_ligo_good [ "run"; "interpret" ; "t3" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect.unreachable] ;
  run_ligo_good [ "run"; "interpret" ; "t4" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect.unreachable] ;
  run_ligo_good [ "run"; "interpret" ; "t5" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect.unreachable] ;
  run_ligo_good [ "run"; "interpret" ; "t6" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect.unreachable] ;
  run_ligo_good [ "run"; "interpret" ; "t7" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect.unreachable] ;
  run_ligo_good [ "run"; "interpret" ; "t8" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect.unreachable] ;
  run_ligo_good [ "run"; "interpret" ; "t9" ; "--init-file";(test "let_destructuring.mligo") ] ;
  [%expect.unreachable] ;
  run_ligo_bad [ "run"; "interpret" ; "t1" ; "--init-file";(bad_test "let_destructuring.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 27, characters 7-29
  Called from Cli_expect_tests__Let_destructuring.(fun) in file "src/bin/expect_tests/let_destructuring.ml", line 4, characters 2-94
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts/let_destructuring.mligo", line 39, characters 8-20:
   38 | let t9 =
   39 |   let ((OneCase (av)), { a ; b = _ }) = (OneCase (1), { a = 1 ; b = 1n }) in
   40 |   av + a

  Invalid pattern.
  Can't match on values. |}]

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
      3 | const t1 = block {
      4 |   var record [ a = a ; f = b ] := record [ a = 1 ; b = 1n ] ;
      5 | } with (a,b)

    Pattern do not conform type record[a -> int , b -> nat] |}] ;
  run_ligo_bad ["run"; "interpret" ; "type t = {a:int;b:int} in let x = {a=2;b=3} in let {a} = x in a" ; "--syntax" ; "cameligo" ] ;
  [%expect{|
    Pattern {a = a} do not conform type record[a -> int , b -> int] |}] ;
  run_ligo_bad ["run"; "interpret" ; "type t = {a:int;b:int} in let x = {a=2;b=3} in let {a ; b ; c} = x in a" ; "--syntax" ; "cameligo" ] ;
  [%expect{|
    Pattern {a = a ; b = b ; c = c} do not conform type record[a -> int , b -> int] |}]