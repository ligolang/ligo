open Cli_expect

let bad_test s = bad_test "" ^ "/deep_pattern_matching/" ^ s
let good_test s = test "" ^ "/deep_pattern_matching/" ^ s

(* Negatives *)

(* testing that subtitution is stoping on resursive definitions *)
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail17.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail17.mligo", line 15, characters 42-43:
     14 |     (* testing that subtitution is stoping on resursive definitions *)
     15 |     let rec a (b : int) : int =let x = fo a in b + 1 in
                                                    ^
     16 |     (a 1) + (fo b)

    This expression has type "[b]int -> int", but an expression was expected of type
    "optioni".
    Type "[b]int -> int" is not compatible with type "optioni". |}]

(* wrong type on constructor argument pattern *)
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail16.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail16.mligo", line 6, characters 4-25:
      5 |   match action with
      6 |   | {one = _ ; three = _} -> 0
              ^^^^^^^^^^^^^^^^^^^^^

    Pattern not of the expected type "parameter". |}]

(* wrong type on constructor argument pattern *)
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail15.mligo"; "--no-color" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail15.mligo", line 7, character 2 to line 9, character 25:
      6 | let main (action : parameter) : int =
      7 |   match action with
            ^^^^^^^^^^^^^^^^^
      8 |   | Increment (n, m) -> 0
          ^^^^^^^^^^^^^^^^^^^^^^^^^
      9 |   | Reset            -> 0
          ^^^^^^^^^^^^^^^^^^^^^^^^^

    Can not unify the types "( ^a * ^b )" and "( int * int * int )".
    Type "( ^a * ^b )" is not compatible with type "( int * int * int )".
    Difference between the types:
    - ^a
    + int
    - ^b
    + int
    + int
    Hint: "^a", "^b" represent placeholder type(s). |}]

(* wrong unit pattern in a let destructuring *)
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail14.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail14.mligo", line 2, characters 11-14:
      1 | let main (_ : unit) (_ : unit) : operation list * unit =
      2 |   let () = 42n in
                     ^^^
      3 |   (([] : operation list), ())

    Invalid type(s).
    Expected "unit", but got: "nat". |}]

(* Trying to match on values *)
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail10.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail10.mligo", line 5, characters 8-9:
      4 |   match x with
      5 |   | One 1 -> 2
                  ^
      6 |   | Two -> 1

    Invalid pattern matching.
      If this is pattern matching over Booleans, then "true" or "false" is expected.
      If this is pattern matching on a list, then one of the following is expected:
        * an empty list pattern "[]";
        * a cons list pattern "[head, ...tail]".
      If this is pattern matching over variants, then a constructor of a variant is expected.

      Other forms of pattern matching are not (yet) supported. |}]

(* unbound variable *)

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail9.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail9.mligo", line 6, characters 11-12:
      5 |   | One a -> 2
      6 |   | Two -> a
                     ^

    Variable "a" not found. |}]

(* wrong patterns type *)
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail1.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail1.mligo", line 5, character 2 to line 8, character 44:
      4 | let t = fun (x: myt * myt) ->
      5 |   match x with
            ^^^^^^^^^^^^
      6 |   | Nil , {a = a ; b = b ; c = c} -> 1
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      7 |   | xs  , Nil -> 2
          ^^^^^^^^^^^^^^^^^^
      8 |   | Cons (a,b) , Cons (c,d) -> a + b + c + d
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Can not unify the types "record[a -> ^a , b -> ^b , c -> ^c]" and "myt".
    Type "record[a -> ^a , b -> ^b , c -> ^c]" is not compatible with type "myt".
    Hint: "^a", "^b", "^c" represent placeholder type(s). |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail2.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail2.mligo", line 4, character 2 to line 7, character 44:
      3 | let t = fun (x: myt * myt) ->
      4 |   match x with
            ^^^^^^^^^^^^
      5 |   | Nil , (a,b,c) -> 1
          ^^^^^^^^^^^^^^^^^^^^^^
      6 |   | xs  , Nil -> 2
          ^^^^^^^^^^^^^^^^^^
      7 |   | Cons (a,b) , Cons (c,d) -> a + b + c + d
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Can not unify the types "( ^a * ^b * ^c )" and "myt".
    Type "( ^a * ^b * ^c )" is not compatible with type "myt".
    Hint: "^a", "^b", "^c" represent placeholder type(s). |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail5.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail5.mligo", line 5, characters 4-15:
      4 |   match x with
      5 |   | Some_fake x -> x
              ^^^^^^^^^^^
      6 |   | None_fake -> 1

    Pattern not of the expected type "option (int)". |}]

(* wrong body type *)

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail7.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail7.mligo", line 6, characters 9-10:
      5 |   | A -> "hey"
      6 |   | B -> 2
                   ^

    This expression has type "int", but an expression was expected of type
    "string".
    Type "int" is not compatible with type "string". |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail8.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail8.mligo", line 19, characters 22-31:
     18 |         f (b+1)
     19 |       | Cons (a,b) -> "invalid"
                                ^^^^^^^^^
     20 |     in

    This expression has type "string", but an expression was expected of type
    "int".
    Type "string" is not compatible with type "int". |}]

(* rendundancy detected while compiling the pattern matching *)
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail3.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail3.mligo", line 4, character 2 to line 6, character 21:
      3 | let t = fun (x: myt * ( int * int * int)) ->
      4 |   match x with
            ^^^^^^^^^^^^
      5 |   | xs , (a,b,c) -> 1
          ^^^^^^^^^^^^^^^^^^^^^
      6 |   | xs , (c,b,a) -> 2
          ^^^^^^^^^^^^^^^^^^^^^

    Error : this match case is unused. |}]

(* anomaly detected in the pattern matching self_ast_typed pass *)

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail11.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail11.mligo", line 2, character 2 to line 4, character 11:
      1 | let t12 = fun (x : int list) ->
      2 |   match x with
            ^^^^^^^^^^^^
      3 |   | hd::(hd2::tl) -> hd + hd2
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      4 |   | [] -> 0
          ^^^^^^^^^^^

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - _ :: [] |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail12.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail12.mligo", line 4, character 2 to line 6, character 40:
      3 | let t13 = fun (x:recordi) ->
      4 |   match x with
            ^^^^^^^^^^^^
      5 |   | { a = Some ([]) ; b = (hd::tl) } -> hd
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      6 |   | { a = Some (hd::tl) ; b = [] } -> hd
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - {
     a = None;
     b = _
    } |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail13.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail13.mligo", line 7, characters 5-14:
      6 |    | Increment n -> s +1
      7 |    | Decrement -> s -1
               ^^^^^^^^^
      8 |  in ([] : operation list), stor

    Pattern not of the expected type "nat". |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "pm_fail4.mligo" ];
  [%expect
    {|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail4.mligo", line 4, character 2 to line 6, character 18:
      3 | let t = fun (x: myt * myt) ->
      4 |   match x with
            ^^^^^^^^^^^^
      5 |   | Nil , ys  -> 1
          ^^^^^^^^^^^^^^^^^^
      6 |   | xs  , Nil -> 2
          ^^^^^^^^^^^^^^^^^^

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - (Cons (_, _), Cons (_, _)) |}]

(* Positives *)

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t1 (Nil,Nil)"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t1 (Nil,Cons(1,2))"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t1 (Cons(1,2),Nil)"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 2 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t1 (Cons(1,2),Cons(3,4))"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 10 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t2 Nil Nil"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t2 Nil (Cons (1,2))"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 3 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t2 (Cons(1,2)) (Cons(1,2))"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 6 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t2 (Cons(1,2)) Nil"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 7 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t3 (One (Nil))"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t3 (One (Cons(1,2)))"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 3 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t3 (Two {a = 1 ; b = 2n ; c = \"tri\"})"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 6 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t2_3 (Cons(1,2)) Nil (One(Nil))"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 8 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t4 (One(Nil)) (One (Nil))"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t4 (One(Nil)) (Two {a=1;b=2n;c=\"tri\"})"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 2 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t4 (One(Cons(1,2))) (Two {a=1;b=2n;c=\"tri\"})"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 3 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t4 (Two {a=0;b=0n;c=\"\"}) (Two {a=1;b=2n;c=\"tri\"})"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 4 |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "t5 1"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good [ "run"; "interpret"; "t6 42"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 2 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t7 (Some 10)"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 10 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t7 (None: int option)"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t8 (Some (1,2)) 2"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 3 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t8 (None:(int * int) option) 2"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 2 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t9 (None:int option) (None:int option)"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t9 (None:int option) (Some 1)"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t9 (Some 1) (None:int option)"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 2 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t9 (Some 1) (Some 2)"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 3 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t10 (Consi(None:int option)) (Consi(Some 100))"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t11 (Consi(None:int option)) (Consi(Some 100))"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 4 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t12 ([]: int list)"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 0 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t12 [1]"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t12 [1;2]"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 3 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t12 [1;2;3]"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 6 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t12 [1;2;3;4]"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| -1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t13 none_a some_a"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| -1 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t13 some_a a_empty_b_not"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 111 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "t13 some_a b_empty_a_not"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 222 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"; "interpret"; "t13 some_a some_a"; "--init-file"; good_test "pm_test.mligo" ];
  [%expect {| 4 |}]

let%expect_test _ =
  run_ligo_good
    [ "run"
    ; "interpret"
    ; "nested_record_pm { a = 1 ; b = E }"
    ; "--init-file"
    ; good_test "pm_test.mligo"
    ];
  [%expect {| 5 |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; good_test "nested_record_sum.mligo" ];
  [%expect {| 148 bytes |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; good_test "edge_case_I.mligo" ];
  [%expect {| 354 bytes |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; good_test "edge_case_T.mligo" ];
  [%expect {| 448 bytes |}]

let%expect_test _ =
  run_ligo_bad [ "info"; "measure-contract"; good_test "edge_case_V.mligo" ];
  [%expect
    {|
    File "../../test/contracts//deep_pattern_matching/edge_case_V.mligo", line 10, character 3 to line 14, character 26:
      9 |   [],
     10 |   (match p with
             ^^^^^^^^^^^^
     11 |      A, A, A, _, _, _ -> 1
          ^^^^^^^^^^^^^^^^^^^^^^^^^^
     12 |    | B, _, _, A, A, _ -> 2
          ^^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |    | _, B, _, B, _, A -> 3
          ^^^^^^^^^^^^^^^^^^^^^^^^^^
     14 |    | _, _, B, _, B, B -> 4)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - (A, A, B, _, A, _) |}]

let%expect_test _ =
  run_ligo_bad [ "info"; "measure-contract"; good_test "edge_case_S.mligo" ];
  [%expect
    {|
    File "../../test/contracts//deep_pattern_matching/edge_case_S.mligo", line 10, character 3 to line 15, character 32:
      9 |   [],
     10 |   (match p with
             ^^^^^^^^^^^^
     11 |      A, A, _, _, _, _, _, _ -> 1
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     12 |    | _, _, A, A, _, _, _, _ -> 2
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     13 |    | _, _, _, _, A, A, _, _ -> 3
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     14 |    | _, _, _, _, _, _, A, A -> 4
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     15 |    | A, B, A, B, A, B, A, B -> 5)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - (B, _, B, _, B, _, B, _) |}]

let%expect_test _ =
  run_ligo_good [ "compile"; "contract"; good_test "pm_ticket.mligo" ];
  [%expect
    {|
    File "../../test/contracts//deep_pattern_matching/pm_ticket.mligo", line 9, characters 28-33:
      8 |     | { myt = myt ; mynat = mynat } , None -> (([]: operation list), mynat)
      9 |     | { myt = myt ; mynat = mynat } , Some x -> (([]: operation list), x)
                                      ^^^^^
    :
    Warning: unused variable "mynat".
    Hint: replace it by "_mynat" to prevent this warning.

    File "../../test/contracts//deep_pattern_matching/pm_ticket.mligo", line 9, characters 14-17:
      8 |     | { myt = myt ; mynat = mynat } , None -> (([]: operation list), mynat)
      9 |     | { myt = myt ; mynat = mynat } , Some x -> (([]: operation list), x)
                        ^^^
    :
    Warning: unused variable "myt".
    Hint: replace it by "_myt" to prevent this warning.

    File "../../test/contracts//deep_pattern_matching/pm_ticket.mligo", line 8, characters 14-17:
      7 |   match p with
      8 |     | { myt = myt ; mynat = mynat } , None -> (([]: operation list), mynat)
                        ^^^
      9 |     | { myt = myt ; mynat = mynat } , Some x -> (([]: operation list), x)
    :
    Warning: unused variable "myt".
    Hint: replace it by "_myt" to prevent this warning.

    File "../../test/contracts//deep_pattern_matching/pm_ticket.mligo", line 6, characters 32-33:
      5 | [@entry]
      6 | let main = fun (p : parameter) (s: storage) ->
                                          ^
      7 |   match p with
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter (pair (pair (ticket %myt int) (nat %mynat)) (option nat)) ;
      storage nat ;
      code { CAR ;
             UNPAIR ;
             CDR ;
             SWAP ;
             IF_NONE {} { SWAP ; DROP } ;
             NIL operation ;
             PAIR } } |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; good_test "bug_report.mligo" ];
  [%expect {| 468 bytes |}]

let%expect_test _ =
  run_ligo_good [ "info"; "measure-contract"; good_test "mini_shifumi.mligo" ];
  [%expect {| 368 bytes |}]
