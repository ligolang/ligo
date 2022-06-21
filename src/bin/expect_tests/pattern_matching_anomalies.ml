open Cli_expect

let bad_missing_test s = (bad_test "")^"/pattern_matching_anomalies/missing_cases/"^s
let bad_redundant_test s = (bad_test "")^"/pattern_matching_anomalies/redundant_case/"^s

(* Missing Cases *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_missing_test "c.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/c.mligo", line 5, characters 4-15:
      4 |   match x with
      5 |     One _ -> ()

    Pattern matching anomaly (redundant, or non exhaustive).
    FOUND MISSING CASE(S)
    - Three(_)
    - Two(_) |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_missing_test "c_c.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/c_c.mligo", line 6, character 2 to line 9, character 18:
      5 | let s (x : t) =
      6 |   match x with
      7 |     Two _    -> ()
      8 |   | Three    -> ()
      9 |   | One Five -> ()

    Pattern matching anomaly (redundant, or non exhaustive).
    FOUND MISSING CASE(S)
    - One(Four(_))
    - One(Six(_)) |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_missing_test "t_c.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/t_c.mligo", line 7, character 2 to line 10, character 25:
      6 | let s (x : t) =
      7 |   match x with
      8 |     (n, One, Six) -> ()
      9 |   | (n, Two, Six) -> ()
     10 |   | (n, Three, Six) -> ()

    Pattern matching anomaly (redundant, or non exhaustive).
    FOUND MISSING CASE(S)
    - (_,One(_),Five(_))
    - (_,One(_),Four(_)) |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_missing_test "r_c.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/r_c.mligo", line 7, character 2 to line 10, character 36:
      6 | let s (x : t) =
      7 |   match x with
      8 |     { a ; b = One;   c = Six } -> ()
      9 |   | { a ; b = Two;   c = Six } -> ()
     10 |   | { a ; b = Three; c = Six } -> ()

    Pattern matching anomaly (redundant, or non exhaustive).
    FOUND MISSING CASE(S)
    - { a = _ ; b = One(_) ; c = Five(_) }
    - { a = _ ; b = One(_) ; c = Four(_) } |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_missing_test "c_t_c.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/c_t_c.mligo", line 5, character 2 to line 8, character 22:
      4 | let s (x : t) =
      5 |   match x with
      6 |     One (n, Six) -> ()
      7 |   | Two          -> ()
      8 |   | Three        -> ()

    Pattern matching anomaly (redundant, or non exhaustive).
    FOUND MISSING CASE(S)
    - One((_,Five(_)))
    - One((_,Four(_))) |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_missing_test "c_r_c.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/c_r_c.mligo", line 5, character 2 to line 8, character 29:
      4 | let s (x : t) =
      5 |   match x with
      6 |     One { a ; b = Six } -> ()
      7 |   | Two                 -> ()
      8 |   | Three               -> ()

    Pattern matching anomaly (redundant, or non exhaustive).
    FOUND MISSING CASE(S)
    - One({ a = _ ; b = Five(_) })
    - One({ a = _ ; b = Four(_) }) |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_missing_test "t_t_c.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/t_t_c.mligo", line 7, character 2 to line 10, character 35:
      6 | let s (x : t) =
      7 |   match x with
      8 |     ((_, _, _), (One, Six))   -> ()
      9 |   | ((_, _, _), (Two, Six))   -> ()
     10 |   | ((_, _, _), (Three, Six)) -> ()

    Pattern matching anomaly (redundant, or non exhaustive).
    FOUND MISSING CASE(S)
    - ((_,_,_),(One(_),Five(_)))
    - ((_,_,_),(One(_),Four(_))) |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_missing_test "t_r_c.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/t_r_c.mligo", line 7, character 2 to line 9, character 40:
      6 | let s (x : t) =
      7 |   match x with
      8 |     (n, ({ a ; c ; b = One }, s))  -> ()
      9 |   | (n, ({ a ; b = Four ; c }, s)) -> ()

    Pattern matching anomaly (redundant, or non exhaustive).
    FOUND MISSING CASE(S)
    - (_,({ a = _ ; b = Three(_) ; c = _ },_))
    - (_,({ a = _ ; b = Two(_) ; c = _ },_)) |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_missing_test "r_t_c.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/r_t_c.mligo", line 7, character 2 to line 9, character 44:
      6 | let s (x : t) =
      7 |   match x with
      8 |     { a ; c ; b = ((x, y), (One, z)) } -> ()
      9 |   | { a ; b = ((x, y), (Two, z)) ; c } -> ()

    Pattern matching anomaly (redundant, or non exhaustive).
    FOUND MISSING CASE(S)
    - { a = _ ; b = ((_,_),(Four(_),_)) ; c = _ }
    - { a = _ ; b = ((_,_),(Three(_),_)) ; c = _ } |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_missing_test "r_r_c.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/r_r_c.mligo", line 7, character 2 to line 9, character 67:
      6 | let s (x : t) =
      7 |   match x with
      8 |     { a ; b = { d = { e ; f } ; i = { g = Two ; h } } ; c }   -> ()
      9 |   | { a ; c ; b = { d = { e ; f } ; i = { g = Three ; h } } } -> ()

    Pattern matching anomaly (redundant, or non exhaustive).
    FOUND MISSING CASE(S)
    - { a = _ ; b = { d = { e = _ ; f = _ } ; i = { g = Four(_) ; h = _ } } ; c = _ }
    - { a = _ ; b = { d = { e = _ ; f = _ } ; i = { g = One(_) ; h = _ } } ; c = _ } |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_missing_test "last.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/last.mligo", line 2, character 2 to line 4, character 20:
      1 | let rec last (xs : int list) : int =
      2 |   match xs with
      3 |     x::[] -> x
      4 |   | _::xs -> last xs

    Pattern matching anomaly (redundant, or non exhaustive).
    FOUND MISSING CASE(S)
    - [] |}]

(* Redundant Case *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_redundant_test "c1_c1_c2_c3.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/c1_c1_c2_c3.mligo", line 4, character 2 to line 8, character 15:
      3 | let s (x : t) =
      4 |   match x with
      5 |     One a -> ()
      6 |   | One b -> ()
      7 |   | Two c -> ()
      8 |   | Three -> ()

    Redundant pattern matching
    FOUND REDUNDANT CASE(S): 2 |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_redundant_test "c1_c2_c1_c3.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/c1_c2_c1_c3.mligo", line 4, character 2 to line 8, character 15:
      3 | let s (x : t) =
      4 |   match x with
      5 |     One a -> ()
      6 |   | Two c -> ()
      7 |   | One b -> ()
      8 |   | Three -> ()

    Redundant pattern matching
    FOUND REDUNDANT CASE(S): 3 |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_redundant_test "c1_c2_c3_c1.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/c1_c2_c3_c1.mligo", line 4, character 2 to line 8, character 15:
      3 | let s (x : t) =
      4 |   match x with
      5 |     One a -> ()
      6 |   | Two c -> ()
      7 |   | Three -> ()
      8 |   | One b -> ()

    Redundant pattern matching
    FOUND REDUNDANT CASE(S): 4 |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_redundant_test "c1_c2_c3_w.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_bad)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 35, characters 7-28
  Called from Cli_expect_tests__Pattern_matching_anomalies.(fun) in file "src/bin/expect_tests/pattern_matching_anomalies.ml", line 210, characters 2-82
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  FOUND REDUNDANT CASE(S): 4type t = sum[One -> int , Three -> unit , Two -> nat]
  const s =
    lambda (x : sum[One -> int , Three -> unit , Two -> nat]) return  match
                                                                      x with
                                                                      | One a ->
                                                                      unit
                                                                      | Three unit_proj#3 ->
                                                                      unit
                                                                      | Two c ->
                                                                      unit |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_redundant_test "c1_w_c2_c3.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_bad)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 35, characters 7-28
  Called from Cli_expect_tests__Pattern_matching_anomalies.(fun) in file "src/bin/expect_tests/pattern_matching_anomalies.ml", line 236, characters 2-82
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  FOUND REDUNDANT CASE(S): 3type t = sum[One -> int , Three -> unit , Two -> nat]
  const s =
    lambda (x : sum[One -> int , Three -> unit , Two -> nat]) return  match
                                                                      x with
                                                                      | Three ctor_proj#5 ->
                                                                      unit
                                                                      | Two ctor_proj#6 ->
                                                                      unit
                                                                      | One a ->
                                                                      unit |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_redundant_test "w_c1_c2_c3.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_bad)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 35, characters 7-28
  Called from Cli_expect_tests__Pattern_matching_anomalies.(fun) in file "src/bin/expect_tests/pattern_matching_anomalies.ml", line 262, characters 2-82
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  FOUND REDUNDANT CASE(S): 2type t = sum[One -> int , Three -> unit , Two -> nat]
  const s =
    lambda (x : sum[One -> int , Three -> unit , Two -> nat]) return unit |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_redundant_test "tx_tx_ty.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/tx_tx_ty.mligo", line 6, character 2 to line 9, character 18:
      5 | let s (x : t) =
      6 |   match x with
      7 |     (n, One) -> ()
      8 |   | (n, One) -> ()
      9 |   | (n, Two) -> ()

    Redundant pattern matching
    FOUND REDUNDANT CASE(S): 2 |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_redundant_test "tx_w_ty.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_bad)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 35, characters 7-28
  Called from Cli_expect_tests__Pattern_matching_anomalies.(fun) in file "src/bin/expect_tests/pattern_matching_anomalies.ml", line 294, characters 2-79
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  FOUND REDUNDANT CASE(S): 3type p = sum[One -> unit , Two -> unit]
  type t = ( int * sum[One -> unit , Two -> unit] )
  const s =
    lambda (x : ( int * sum[One -> unit , Two -> unit] )) return  match
                                                                   x with
                                                                   | ( n , tuple_proj#3 ) ->
                                                                    match
                                                                     tuple_proj#3 with
                                                                     | Two ctor_proj#11 ->
                                                                      unit
                                                                     | One unit_proj#10 ->
                                                                      unit |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_redundant_test "w_tx_ty.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_bad)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 35, characters 7-28
  Called from Cli_expect_tests__Pattern_matching_anomalies.(fun) in file "src/bin/expect_tests/pattern_matching_anomalies.ml", line 322, characters 2-79
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  FOUND REDUNDANT CASE(S): 2type p = sum[One -> unit , Two -> unit]
  type t = ( int * sum[One -> unit , Two -> unit] )
  const s =
    lambda (x : ( int * sum[One -> unit , Two -> unit] )) return  match
                                                                   x with
                                                                   | ( n , tuple_proj#3 ) ->
                                                                   unit |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_redundant_test "rx_rx_ry.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/rx_rx_ry.mligo", line 6, character 2 to line 9, character 25:
      5 | let s (x : t) =
      6 |   match x with
      7 |     { a ; b = One } -> ()
      8 |   | { a ; b = One } -> ()
      9 |   | { a ; b = Two } -> ()

    Redundant pattern matching
    FOUND REDUNDANT CASE(S): 2 |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_redundant_test "rx_w_ry.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_bad)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 35, characters 7-28
  Called from Cli_expect_tests__Pattern_matching_anomalies.(fun) in file "src/bin/expect_tests/pattern_matching_anomalies.ml", line 358, characters 2-79
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  FOUND REDUNDANT CASE(S): 3type p = sum[One -> unit , Two -> unit]
  type t = record[a -> int , b -> sum[One -> unit , Two -> unit]]
  const s =
    lambda (x : record[a -> int , b -> sum[One -> unit , Two -> unit]]) return
     match x with
      | record[a -> a , b -> record_proj#3] ->
       match record_proj#3 with
        | Two ctor_proj#11 ->
          unit | One unit_proj#10 ->
                 unit |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_redundant_test "w_rx_ry.mligo") ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_bad)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_bad in file "src/bin/expect_tests/cli_expect.ml", line 35, characters 7-28
  Called from Cli_expect_tests__Pattern_matching_anomalies.(fun) in file "src/bin/expect_tests/pattern_matching_anomalies.ml", line 384, characters 2-79
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  FOUND REDUNDANT CASE(S): 2type p = sum[One -> unit , Two -> unit]
  type t = record[a -> int , b -> sum[One -> unit , Two -> unit]]
  const s =
    lambda (x : record[a -> int , b -> sum[One -> unit , Two -> unit]]) return
     match x with
      | record[a -> a , b -> record_proj#3] ->
      unit |}]