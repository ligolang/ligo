open Cli_expect

let bad_missing_test s = bad_test "" ^ "/pattern_matching_anomalies/missing_cases/" ^ s
let bad_redundant_test s = bad_test "" ^ "/pattern_matching_anomalies/redundant_case/" ^ s

(* Missing Cases *)

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "c.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/c.mligo", line 5, characters 4-15:
      4 |   match x with
      5 |     One _ -> ()

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - Three
    - Two _ |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "c_c.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/c_c.mligo", line 6, character 2 to line 9, character 18:
      5 | let s (x : t) =
      6 |   match x with
      7 |     Two _    -> ()
      8 |   | Three    -> ()
      9 |   | One Five -> ()

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - One Four
    - One Six |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "t_c.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/t_c.mligo", line 7, character 2 to line 10, character 25:
      6 | let s (x : t) =
      7 |   match x with
      8 |     (n, One, Six) -> ()
      9 |   | (n, Two, Six) -> ()
     10 |   | (n, Three, Six) -> ()

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - (_, One, Five)
    - (_, One, Four) |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "r_c.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/r_c.mligo", line 7, character 2 to line 10, character 36:
      6 | let s (x : t) =
      7 |   match x with
      8 |     { a ; b = One;   c = Six } -> ()
      9 |   | { a ; b = Two;   c = Six } -> ()
     10 |   | { a ; b = Three; c = Six } -> ()

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - {c = Five; b = One; a = _}
    - {c = Four; b = One; a = _} |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "c_t_c.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/c_t_c.mligo", line 5, character 2 to line 8, character 22:
      4 | let s (x : t) =
      5 |   match x with
      6 |     One (n, Six) -> ()
      7 |   | Two          -> ()
      8 |   | Three        -> ()

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - One (_, Five)
    - One (_, Four) |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "c_r_c.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/c_r_c.mligo", line 5, character 2 to line 8, character 29:
      4 | let s (x : t) =
      5 |   match x with
      6 |     One { a ; b = Six } -> ()
      7 |   | Two                 -> ()
      8 |   | Three               -> ()

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - One {b = Five; a = _}
    - One {b = Four; a = _} |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "t_t_c.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/t_t_c.mligo", line 7, character 2 to line 10, character 35:
      6 | let s (x : t) =
      7 |   match x with
      8 |     ((_, _, _), (One, Six))   -> ()
      9 |   | ((_, _, _), (Two, Six))   -> ()
     10 |   | ((_, _, _), (Three, Six)) -> ()

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - ((_, _, _), (One, Five))
    - ((_, _, _), (One, Four)) |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "t_r_c.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/t_r_c.mligo", line 7, character 2 to line 9, character 40:
      6 | let s (x : t) =
      7 |   match x with
      8 |     (n, ({ a ; c ; b = One }, s))  -> ()
      9 |   | (n, ({ a ; b = Four ; c }, s)) -> ()

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - (_, ({c = _; b = Three; a = _}, _))
    - (_, ({c = _; b = Two; a = _}, _)) |}]

(*
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "t_r_c.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/t_r_c.ligo", line 7, character 2 to line 10, character 3:
      6 | function s (const x : t) is
      7 |   case x of [
      8 |   | (n, (record [ a = _ ; c = _ ; b = One ], s))  -> Unit
      9 |   | (n, (record [ a = _ ; b = Four ; c = _ ], s)) -> Unit
     10 |   ]

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - (_, (record [c = _; b = Three; a = _], _))
    - (_, (record [c = _; b = Two; a = _], _)) |}]
*)

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "r_t_c.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/r_t_c.mligo", line 7, character 2 to line 9, character 44:
      6 | let s (x : t) =
      7 |   match x with
      8 |     { a ; c ; b = ((x, y), (One, z)) } -> ()
      9 |   | { a ; b = ((x, y), (Two, z)) ; c } -> ()

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - {c = _; b = ((_, _), (Four, _)); a = _}
    - {c = _; b = ((_, _), (Three, _)); a = _} |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "r_r_c.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/r_r_c.mligo", line 7, character 2 to line 9, character 67:
      6 | let s (x : t) =
      7 |   match x with
      8 |     { a ; b = { d = { e ; f } ; i = { g = Two ; h } } ; c }   -> ()
      9 |   | { a ; c ; b = { d = { e ; f } ; i = { g = Three ; h } } } -> ()

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - {c = _; b = {i = {h = _; g = Four}; d = {f = _; e = _}}; a = _}
    - {c = _; b = {i = {h = _; g = One}; d = {f = _; e = _}}; a = _} |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "last.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/last.mligo", line 2, character 2 to line 4, character 20:
      1 | let rec last (xs : int list) : int =
      2 |   match xs with
      3 |     x::[] -> x
      4 |   | _::xs -> last xs

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - [] |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "c.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/c.jsligo", line 4, character 11 to line 6, character 4:
      3 | let s = (x : t) : unit =>
      4 |   match(x, {
      5 |     Two: (_ : nat) => unit,
      6 |   })

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - One
    - Three |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_missing_test "c_c.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/c_c.jsligo", line 9, character 30 to line 11, character 6:
      8 |   Three: ()      => unit,
      9 |   One: (c : p)   => (match(c, {
     10 |       Four: () => unit
     11 |     }))
     12 |   })

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - Five
    - Six |}]

(* Redundant Case *)

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "c1_c1_c2_c3.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/c1_c1_c2_c3.mligo", line 4, character 2 to line 8, character 15:
      3 | let s (x : t) =
      4 |   match x with
      5 |     One a -> ()
      6 |   | One b -> ()
      7 |   | Two c -> ()
      8 |   | Three -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "c1_c2_c1_c3.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/c1_c2_c1_c3.mligo", line 4, character 2 to line 8, character 15:
      3 | let s (x : t) =
      4 |   match x with
      5 |     One a -> ()
      6 |   | Two c -> ()
      7 |   | One b -> ()
      8 |   | Three -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "c1_c2_c3_c1.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/c1_c2_c3_c1.mligo", line 4, character 2 to line 8, character 15:
      3 | let s (x : t) =
      4 |   match x with
      5 |     One a -> ()
      6 |   | Two c -> ()
      7 |   | Three -> ()
      8 |   | One b -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "c1_c2_c3_w.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/c1_c2_c3_w.mligo", line 4, character 2 to line 8, character 15:
      3 | let s (x : t) =
      4 |   match x with
      5 |     One a -> ()
      6 |   | Two c -> ()
      7 |   | Three -> ()
      8 |   | _     -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "c1_w_c2_c3.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/c1_w_c2_c3.mligo", line 4, character 2 to line 8, character 15:
      3 | let s (x : t) =
      4 |   match x with
      5 |     One a -> ()
      6 |   | _     -> ()
      7 |   | Two c -> ()
      8 |   | Three -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "w_c1_c2_c3.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/w_c1_c2_c3.mligo", line 4, character 2 to line 8, character 15:
      3 | let s (x : t) =
      4 |   match x with
      5 |     _     -> ()
      6 |   | One a -> ()
      7 |   | Two c -> ()
      8 |   | Three -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "tx_tx_ty.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/tx_tx_ty.mligo", line 6, character 2 to line 9, character 18:
      5 | let s (x : t) =
      6 |   match x with
      7 |     (n, One) -> ()
      8 |   | (n, One) -> ()
      9 |   | (n, Two) -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "tx_w_ty.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/tx_w_ty.mligo", line 6, character 2 to line 9, character 18:
      5 | let s (x : t) =
      6 |   match x with
      7 |     (n, One) -> ()
      8 |   | _        -> ()
      9 |   | (n, Two) -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "w_tx_ty.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/w_tx_ty.mligo", line 6, character 2 to line 9, character 18:
      5 | let s (x : t) =
      6 |   match x with
      7 |     _        -> ()
      8 |   | (n, One) -> ()
      9 |   | (n, Two) -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "rx_rx_ry.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/rx_rx_ry.mligo", line 6, character 2 to line 9, character 25:
      5 | let s (x : t) =
      6 |   match x with
      7 |     { a ; b = One } -> ()
      8 |   | { a ; b = One } -> ()
      9 |   | { a ; b = Two } -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "rx_w_ry.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/rx_w_ry.mligo", line 6, character 2 to line 9, character 25:
      5 | let s (x : t) =
      6 |   match x with
      7 |     { a ; b = One } -> ()
      8 |   | _               -> ()
      9 |   | { a ; b = Two } -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "w_rx_ry.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/w_rx_ry.mligo", line 6, character 2 to line 9, character 25:
      5 | let s (x : t) =
      6 |   match x with
      7 |     _               -> ()
      8 |   | { a ; b = One } -> ()
      9 |   | { a ; b = Two } -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "c_c.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/c_c.mligo", line 6, character 2 to line 11, character 18:
      5 | let s (x : t) =
      6 |   match x with
      7 |     One Five -> ()
      8 |   | One Four -> ()
      9 |   | Two c    -> ()
     10 |   | Three    -> ()
     11 |   | One Four -> ()

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "c1_c1_c2_c3.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/c1_c1_c2_c3.jsligo", line 4, character 11 to line 9, character 4:
      3 | let s = (x : t) : unit =>
      4 |   match(x, {
      5 |     One:   (a : int) => unit,
      6 |     One:   (b : int) => unit,
      7 |     Two:   (c : nat) => unit,
      8 |     Three: () => unit
      9 |   })

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "c1_c2_c1_c3.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/c1_c2_c1_c3.jsligo", line 4, character 11 to line 9, character 4:
      3 | let s = (x : t) : unit =>
      4 |   match(x, {
      5 |     One:   (a : int) => unit,
      6 |     Two:   (c : nat) => unit,
      7 |     One:   (b : int) => unit,
      8 |     Three: () => unit
      9 |   })

    Error : this match case is unused. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_redundant_test "c1_c2_c3_c1.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//pattern_matching_anomalies/redundant_case/c1_c2_c3_c1.jsligo", line 4, character 11 to line 9, character 4:
      3 | let s = (x : t) : unit =>
      4 |   match(x, {
      5 |     One:   (a : int) => unit,
      6 |     Two:   (c : nat) => unit,
      7 |     Three: ()        => unit,
      8 |     One:   (b : int) => unit,
      9 |   })

    Error : this match case is unused. |}]
