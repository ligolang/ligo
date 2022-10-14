open Cli_expect

(* evaluate-expr *)
let%expect_test _ =
  run_ligo_good ["run"; "evaluate-expr" ; "../../test/contracts/evaluation_tests.ligo" ; "--entry-point" ; "a" ] ;
  [%expect {|
    record[bar -> "bar" , foo -> +0] |} ];

  run_ligo_good ["run"; "evaluate-expr" ; "../../test/contracts/evaluation_tests.ligo" ; "--entry-point" ; "b" ] ;
  [%expect {|
    2 |} ]

(* list-declarations *)
let%expect_test _ =
  run_ligo_good [ "info"; "list-declarations" ; "../../test/contracts/loop.ligo" ] ;
  [%expect.unreachable ];

  run_ligo_good [ "info"; "list-declarations" ; "../../test/contracts/loop.mligo" ; "--format" ;"json" ] ;
  [%expect.unreachable ];


  run_ligo_good [ "info"; "list-declarations" ; "../../test/contracts/loop.mligo" ] ;
  [%expect.unreachable ];

  run_ligo_good ["info"; "list-declarations" ; "../../test/contracts/loop.religo" ] ;
  [%expect.unreachable ];

  run_ligo_bad ["run" ; "interpret" ; "1" ; "--syntax"; "cameligo" ; "--protocol"; "do_not_exist" ] ;
  [%expect.unreachable] ;

  run_ligo_bad [ "repl" ; "camelig0" ] ;
  [%expect.unreachable] ;

  run_ligo_bad [ "repl" ; "cameligo" ; "--protocol" ; "h" ] ;
  [%expect.unreachable] ;

  run_ligo_bad [ "repl" ; "cameligo" ; "--sender" ; "foo" ] ;
  [%expect.unreachable] ;
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 39, characters 7-29
  Called from Cli_expect_tests__Misc_cli_commands.(fun) in file "src/bin/expect_tests/misc_cli_commands.ml", line 15, characters 2-82
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts/loop.ligo", line 26, characters 18-21:
   25 |     var acc : int := 0;
   26 |     for i := 1 to int (n)
   27 |       {

  Variable "int" not found. |}]
