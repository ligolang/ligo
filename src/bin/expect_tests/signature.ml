open Cli_expect

let contract basename = "../../test/contracts/signature/" ^ basename

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "BI.f 42n"
    ; "--init-file"
    ; contract "simple.mligo"
    ];
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "cameligo"
    ; "Bar.Foo.x"
    ; "--init-file"
    ; contract "spath.mligo"
    ];
  [%expect {|
    42 |}]

let%expect_test _ =
  run_ligo_good
    [ "compile"
    ; "expression"
    ; "jsligo"
    ; "X.x"
    ; "--init-file"
    ; contract "interface.jsligo"
    ];
  [%expect {| 42 |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; contract "FA0.impl.jsligo"; "-m"; "Impl0" ];
  [%expect.unreachable];
  run_ligo_good [ "compile"; "contract"; contract "FA0.impl.jsligo"; "-m"; "Impl1" ];
  [%expect.unreachable];
  run_ligo_good [ "compile"; "contract"; contract "FA0.impl.jsligo"; "-m"; "Impl2" ];
  [%expect.unreachable];
  run_ligo_good [ "compile"; "contract"; contract "FA0.impl.jsligo"; "-m"; "Impl3" ];
  [%expect.unreachable];
  run_ligo_good [ "compile"; "contract"; contract "FA0.impl.jsligo"; "-m"; "Impl4" ];
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 42, characters 25-47
  Called from Cli_expect_tests__Signature.(fun) in file "src/bin/expect_tests/signature.ml", line 41, characters 2-84
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../test/contracts/signature/FA0.impl.jsligo", line 6, character 0 to line 10, character 1:
    5 |
    6 | namespace Impl0 implements INT0.FA0 {
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    7 |   type storage = nat;
        ^^^^^^^^^^^^^^^^^^^^^
    8 |

    9 |   @entry const add = (s : int, k : storage) : [list<operation>, storage] => [list([]), abs (s + k)];
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   10 | }
        ^
   11 |

  Type "storage" declared in signature but not found. |}]
