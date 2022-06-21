open Cli_expect

let bad_test s = (bad_test "")^"/pattern_matching_anomalies/missing_cases/"^s

(* Missing Cases *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "c.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//pattern_matching_anomalies/missing_cases/c.mligo", line 5, characters 4-15:
      4 |   match x with
      5 |     One _ -> ()

    Pattern matching anomaly (redundant, or non exhaustive).
    FOUND MISSING CASE(S)
    - Three(_)
    - Two(_) |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "c_c.mligo") ] ;
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
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "t_c.mligo") ] ;
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
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "r_c.mligo") ] ;
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
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "c_t_c.mligo") ] ;
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
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "c_r_c.mligo") ] ;
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
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "t_t_c.mligo") ] ;
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
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "t_r_c.mligo") ] ;
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
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "r_t_c.mligo") ] ;
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
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "r_r_c.mligo") ] ;
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