(*
  This test suite stressed the disabling of coloring on CLI output.

  It can be done through the [--no-color] CLI option,
  or via setting the NO_COLOR environment variable.
  This choice is in accordance to the [no-color.org] standard.

  Disabling the coloring is done through disabling ANSI codes
  (for bold and colorful text) in CLI output.

*)
open Cli_expect

let contract = "../../test/contracts/negative/colour.mligo"
let contract_test = "../../test/contracts/negative/colour_test.mligo"

(* Test of [compile contract] with default config (--no-color flag unset, NO_COLOR env var unset) *)
let%expect_test _ =
  (* By default, the "TERM" value is set to "dumb" to disable coloring in expect tests,
     however, here we want to test enabling/disabling of colors through the [--no-color] option,
     to test this, colors have to be enabled by default. *)
  (* Erasing the "dumb" value to re-enable coloring by default *)
  Ligo_unix.putenv ~key:"TERM" ~data:"";
  run_ligo_bad [ "compile"; "contract"; contract ];
  [%expect
    {|
    File "../../test/contracts/negative/colour.mligo", line 6, characters 16-25:
      5 | let main (p : unit) (s : storage) : return =
      6 |   let x : nat = [1m[31m1 + 2 + 3[0m (* int *) in
      7 |   [],s

    Invalid type(s)
    Cannot unify "int" with "nat". |}];
  Ligo_unix.putenv ~key:"TERM" ~data:"dumb"

(* Test of [compile contract] with --no-color set *)
let%expect_test _ =
  (* By default, the "TERM" value is set to "dumb" to disable coloring in expect tests,
     however, here we want to test enabling/disabling of colors through the [--no-color option,
     to test this, colors have to be enabled by default. *)
  (* Erasing the "dumb" value to re-enable coloring by default *)
  Ligo_unix.putenv ~key:"TERM" ~data:"";
  run_ligo_bad [ "compile"; "contract"; "--no-color"; contract ];
  [%expect
    {|
      File "../../test/contracts/negative/colour.mligo", line 6, characters 16-25:
        5 | let main (p : unit) (s : storage) : return =
        6 |   let x : nat = 1 + 2 + 3 (* int *) in
                            ^^^^^^^^^
        7 |   [],s

      Invalid type(s)
      Cannot unify "int" with "nat". |}];
  Ligo_unix.putenv ~key:"TERM" ~data:"dumb"

(* Test of [run test] with --no-color unset *)
let%expect_test _ =
  (* By default, the "TERM" value is set to "dumb" to disable coloring in expect tests,
     however, here we want to test enabling/disabling of colors through the [--no-color] option,
     to test this, colors have to be enabled by default. *)
  (* Erasing the "dumb" value to re-enable coloring by default *)
  Ligo_unix.putenv ~key:"TERM" ~data:"";
  run_ligo_bad [ "run"; "test"; contract_test ];
  [%expect
    {|
    File "../../test/contracts/negative/colour_test.mligo", line 3, characters 12-31:
      2 | let test1 = Test.assert (1 = 1)
      3 | let test2 = [1m[31mTest.assert (1 = 2)[0m

    Test failed with "failed assertion"
    Trace:
    File "../../test/contracts/negative/colour_test.mligo", line 3, characters 12-31 ,
    File "../../test/contracts/negative/colour_test.mligo", line 3, characters 12-31 |}];
  Ligo_unix.putenv ~key:"TERM" ~data:"dumb"

(* Test of [run test] with --no-color set *)
let%expect_test _ =
  (* By default, the "TERM" value is set to "dumb" to disable coloring in expect tests,
     however, here we want to test enabling/disabling of colors through the [--no-color] option,
     to test this, colors have to be enabled by default. *)
  (* Erasing the "dumb" value to re-enable coloring by default *)
  Ligo_unix.putenv ~key:"TERM" ~data:"";
  run_ligo_bad [ "run"; "test"; "--no-color"; contract_test ];
  [%expect
    {|
    File "../../test/contracts/negative/colour_test.mligo", line 3, characters 12-31:
      2 | let test1 = Test.assert (1 = 1)
      3 | let test2 = Test.assert (1 = 2)
                      ^^^^^^^^^^^^^^^^^^^

    Test failed with "failed assertion"
    Trace:
    File "../../test/contracts/negative/colour_test.mligo", line 3, characters 12-31 ,
    File "../../test/contracts/negative/colour_test.mligo", line 3, characters 12-31 |}];
  Ligo_unix.putenv ~key:"TERM" ~data:"dumb"

(* Test of [compile contract] with NO_COLOR env var unset *)
let%expect_test _ =
  (* Erasing the "dumb" value to re-enable coloring by default *)
  Ligo_unix.putenv ~key:"TERM" ~data:"";
  (* Setting the NO_COLOR env var to nothing, we expect color on output *)
  Ligo_unix.putenv ~key:"NO_COLOR" ~data:"";
  run_ligo_bad [ "compile"; "contract"; contract ];
  [%expect
    {|
    File "../../test/contracts/negative/colour.mligo", line 6, characters 16-25:
      5 | let main (p : unit) (s : storage) : return =
      6 |   let x : nat = [1m[31m1 + 2 + 3[0m (* int *) in
      7 |   [],s

    Invalid type(s)
    Cannot unify "int" with "nat". |}];
  Ligo_unix.putenv ~key:"TERM" ~data:"dumb"

(* Test of [compile contract] with NO_COLOR env var set *)
let%expect_test _ =
  (* Erasing the "dumb" value to re-enable coloring by default *)
  Ligo_unix.putenv ~key:"TERM" ~data:"";
  (* Setting the NO_COLOR env var to something, we expect a colorless output *)
  (* According to the no-color.org standard,
     NO_COLOR is on as long as it doesn't contain en empty string *)
  Ligo_unix.putenv ~key:"NO_COLOR" ~data:"true";
  run_ligo_bad [ "compile"; "contract"; contract ];
  [%expect
    {|
    File "../../test/contracts/negative/colour.mligo", line 6, characters 16-25:
      5 | let main (p : unit) (s : storage) : return =
      6 |   let x : nat = 1 + 2 + 3 (* int *) in
                          ^^^^^^^^^
      7 |   [],s

    Invalid type(s)
    Cannot unify "int" with "nat". |}];
  Ligo_unix.putenv ~key:"TERM" ~data:"dumb"
