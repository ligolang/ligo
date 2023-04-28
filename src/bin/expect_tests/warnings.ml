open Cli_expect

let contract = test

(* warning unused variables example *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_unused.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Warnings.(fun) in file "src/bin/expect_tests/warnings.ml", line 7, characters 2-74
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  An internal error ocurred. Please, contact the developers.
  Error typing the stdlib:
  Can't infer the type of this value, please add a type annotation.. |}]

(* warning non-duplicable variable used examples *)
let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; contract "warning_duplicate.mligo"
    ];
  [%expect
    {|
  An internal error ocurred. Please, contact the developers.
  Error typing the stdlib:
  Can't infer the type of this value, please add a type annotation.. |}]

let%expect_test _ =
  run_ligo_bad
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "x"
    ; "--init-file"
    ; contract "warning_duplicate2.mligo"
    ];
  [%expect
    {|
  An internal error ocurred. Please, contact the developers.
  Error typing the stdlib:
  Can't infer the type of this value, please add a type annotation.. |}]

let%expect_test _ =
  run_ligo_bad [ "compile"; "contract"; contract "duplicate_ticket_local_module.mligo" ];
  [%expect
    {|
  An internal error ocurred. Please, contact the developers.
  Error typing the stdlib:
  Can't infer the type of this value, please add a type annotation.. |}]

(* some check about the warnings of the E_constructor cases *)
let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_ambiguous_ctor.mligo" ];
  [%expect.unreachable];
  run_ligo_good [ "compile"; "contract"; contract "not_ambiguous_ctor.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Warnings.(fun) in file "src/bin/expect_tests/warnings.ml", line 126, characters 2-82
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  An internal error ocurred. Please, contact the developers.
  Error typing the stdlib:
  Can't infer the type of this value, please add a type annotation.. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_sum_types.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Warnings.(fun) in file "src/bin/expect_tests/warnings.ml", line 147, characters 2-77
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  An internal error ocurred. Please, contact the developers.
  Error typing the stdlib:
  Can't infer the type of this value, please add a type annotation.. |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "warning_sum_types_shadowed.mligo" ];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 34, characters 25-47
  Called from Cli_expect_tests__Warnings.(fun) in file "src/bin/expect_tests/warnings.ml", line 243, characters 2-86
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  An internal error ocurred. Please, contact the developers.
  Error typing the stdlib:
  Can't infer the type of this value, please add a type annotation.. |}]