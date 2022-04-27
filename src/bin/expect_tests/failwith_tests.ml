open Cli_expect

let contract basename =
  "../../test/contracts/" ^ basename

let%expect_test _ =
  run_ligo_good ["run"; "evaluate-call" ; contract "failwith.ligo"; "1" ; "-e"; "failer"; "--no-warn" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Failwith_tests.(fun) in file "src/bin/expect_tests/failwith_tests.ml", line 7, characters 2-103
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  An internal error ocurred. Please, contact the developers.
  Hypothesis 3 don't hold. |}]

let%expect_test _ =
  run_ligo_good ["run"; "evaluate-call" ; contract "failwith.ligo" ; "1" ; "-e" ; "failer" ; "--format";"json" ; "--no-warn" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Failwith_tests.(fun) in file "src/bin/expect_tests/failwith_tests.ml", line 25, characters 2-126
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  An internal error ocurred. Please, contact the developers.
  Hypothesis 3 don't hold. |}]

let%expect_test _ =
  run_ligo_good ["run"; "dry-run" ; contract "subtle_nontail_fail.mligo" ; "()" ; "()" ] ;
  [%expect {|
    File "../../test/contracts/subtle_nontail_fail.mligo", line 1, characters 10-12:
      1 | let main (ps : unit * unit) : operation list * unit =
      2 |   if true
    :
    Warning: unused variable "ps".
    Hint: replace it by "_ps" to prevent this warning.

    failwith("This contract always fails") |}]

let%expect_test _ =
  run_ligo_good ["run"; "interpret" ; "assert(1=1)" ; "--syntax";"pascaligo" ] ;
  [%expect {|
    unit |}]

let%expect_test _ =
  run_ligo_good ["run"; "interpret" ; "assert(1=2)" ; "--syntax";"pascaligo" ] ;
  [%expect {|
    failwith("failed assertion") |}]

let%expect_test _ =
  run_ligo_good ["run"; "interpret" ; "assert(1=1)" ; "--syntax";"cameligo" ] ;
  [%expect {|
    unit |}]

let%expect_test _ =
  run_ligo_good ["run"; "interpret" ; "assert(1=2)" ; "--syntax";"cameligo" ] ;
  [%expect {|
    failwith("failed assertion") |}]