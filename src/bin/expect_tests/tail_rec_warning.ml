open Cli_expect

let contract = test

let%expect_test _ =
  run_ligo_good ["compile"; "contract" ; contract "unused_recursion.mligo" ] ;
  [%expect {|
    { parameter unit ;
      storage (pair (int %bar) (int %foo)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             SWAP ;
             CDR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |} ]

let%expect_test _ =
  run_ligo_good ["compile"; "contract" ; contract "unused_recursion.mligo" ; "--warn-unused-rec" ] ;
  [%expect {|
    File "../../test/contracts/unused_recursion.mligo", line 15, characters 10-13:
     14 |   (* fun_name shadowed in body *)
     15 |   let rec bar : int -> t = fun (x : int) ->
     16 |     let bar = x in
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "bar" to prevent this warning.

    File "../../test/contracts/unused_recursion.mligo", line 12, characters 10-13:
     11 |   (* parameter shadows fun_name: complex *)
     12 |   let rec foo : (int -> int) -> int = fun (foo : (int -> int)) -> let foo = foo 0 in foo in
     13 |
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "foo" to prevent this warning.

    File "../../test/contracts/unused_recursion.mligo", line 9, characters 10-14:
      8 |   (* parameter shadows fun_name: simple *)
      9 |   let rec toto : int -> int = fun (toto:int) : int -> let number = toto in number + 1 in
     10 |
    :
    Warning: unused recursion .
    Hint: remove recursion from the function "toto" to prevent this warning.

    { parameter unit ;
      storage (pair (int %bar) (int %foo)) ;
      code { CDR ;
             PUSH int 0 ;
             PUSH int 1 ;
             PUSH int 2 ;
             ADD ;
             ADD ;
             SWAP ;
             CDR ;
             SWAP ;
             PAIR ;
             NIL operation ;
             PAIR } } |} ]

let%expect_test _ =
  run_ligo_good ["compile"; "contract" ; contract "unused_recursion.jsligo" ] ;
  [%expect.unreachable ]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Tail_rec_warning.(fun) in file "src/bin/expect_tests/tail_rec_warning.ml", line 66, characters 2-77
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  An internal error ocurred. Please, contact the developers.
  Hypothesis 2 failed. |}]

let%expect_test _ =
  run_ligo_good ["compile"; "contract" ; contract "unused_recursion.jsligo" ; "--warn-unused-rec" ] ;
  [%expect.unreachable ]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 29, characters 7-29
  Called from Cli_expect_tests__Tail_rec_warning.(fun) in file "src/bin/expect_tests/tail_rec_warning.ml", line 84, characters 2-99
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  An internal error ocurred. Please, contact the developers.
  Hypothesis 2 failed. |}]