open Cli_expect

let bad_test s = (bad_test "")^"/deep_pattern_matching/"^s
let good_test s = (test "")^"/deep_pattern_matching/"^s

(* Negatives *)

(* Trying to match on values *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail10.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail10.religo", line 5, characters 7-10:
      4 |   switch(x) {
      5 |   | One(1) => 2
      6 |   | Two    => 1

    Invalid pattern matching.
    Can't match on values. |}]

(* unbound variable *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail9.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail9.religo", line 6, characters 14-15:
      5 |   | One(a) => 2
      6 |   | Two    => a
      7 |   }

    Variable "a" not found. |}]

(* wrong patterns type *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail1.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail1.religo", line 6, characters 11-34:
      5 |   switch(x) {
      6 |   | (Nil , {a : a , b : b , c : c}) => 1
      7 |   | (xs  , Nil) => 2

    Pattern not of the expected type myt |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail2.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail2.religo", line 5, characters 12-17:
      4 |   switch(x) {
      5 |   | (Nil , (a,b,c)) => 1
      6 |   | (xs  , Nil) => 2

    Pattern not of the expected type myt |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail5.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail5.religo", line 5, characters 4-16:
      4 |   switch(x) {
      5 |   | Some_fake(x) => x
      6 |   | None_fake    => 1

    Pattern not of the expected type option (int) |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_test6.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_test6.religo", line 4, characters 5-14:
      3 |   | a           => 0
      4 |   | [hd, ...tl] => 0
      5 |   }

    Error : this match case is unused. |}]

(* wrong body type *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail7.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail7.religo", line 6, characters 9-10:
      5 |   | A => "hey"
      6 |   | B => 2
      7 |   }

    Invalid type(s)
    Cannot unify int with string. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail8.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail8.religo", line 20, characters 24-33:
     19 |         f (b+1)
     20 |       | Cons ((a,b)) => "invalid"
     21 |       };

    Invalid type(s)
    Cannot unify string with int. |}]


(* rendundancy detected while compiling the pattern matching *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail3.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail3.religo", line 6, characters 5-17:
      5 |   | (xs , (a,b,c)) => 1
      6 |   | (xs , (c,b,a)) => 2
      7 |   }

    Error : this match case is unused. |}]

(* anomaly detected in the pattern matching self_ast_typed pass *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail11.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail11.religo", line 2, character 2 to line 5, character 3:
      1 | let t12 = (x : list(int)) =>
      2 |   switch(x) {
      3 |   | [hd, ...[hd2, ...tl]] => hd + hd2
      4 |   | [] => 0
      5 |   }

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - [_, ...[]] |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail12.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail12.religo", line 4, character 2 to line 7, character 3:
      3 | let t13 = (x:recordi) =>
      4 |   switch(x) {
      5 |   | { a : Some ([])          , b : [hd, ...tl] } => hd
      6 |   | { a : Some ([hd, ...tl]) , b : [] }          => hd
      7 |   }

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - {a : None,b : _} |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail4.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail4.religo", line 4, character 2 to line 7, character 3:
      3 | let t = (x: (myt, myt)) =>
      4 |   switch(x) {
      5 |   | (Nil , ys)  => 1
      6 |   | (xs  , Nil) => 2
      7 |   }

    Error : this pattern-matching is not exhaustive.
    Here are examples of cases that are not matched:
    - (Cons(_, _), Cons(_, _)) |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail13.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail13.religo", line 8, characters 10-19:
      7 |         | Increment(n) => s + 1
      8 |         | Decrement    => s - 1
      9 |         };

    Pattern not of the expected type nat |}]

(* wrong unit pattern in a let destructuring *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail14.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail14.religo", line 2, characters 6-8:
      1 | let main = (_ : (unit, unit)) : (list(operation), unit) =>
      2 |   let () = 42n;
      3 |   ([] : list(operation), ())

    Invalid pattern matching.
    Can't match on values. |}]

(* wrong fields on record pattern *)
(* wrong type on constructor argument pattern *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail16.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail16.religo", line 6, characters 4-25:
      5 |   switch(action) {
      6 |   | {one : _ , three : _} => 0
      7 |   }

    Pattern not of the expected type parameter |}]

(* wrong type on constructor argument pattern *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail15.religo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail15.religo", line 8, characters 15-19:
      7 |   switch(action) {
      8 |   | Increment((n, m)) => 0
      9 |   | Reset             => 0

    Pattern not of the expected type ( int * int * int ) |}]

(* Positives *)

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1 (Nil,Nil)" ; "--init-file";(good_test "pm_test.religo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1 (Nil,Cons(1,2))" ; "--init-file";(good_test "pm_test.religo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1 (Cons(1,2),Nil)" ; "--init-file";(good_test "pm_test.religo") ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1 (Cons(1,2),Cons(3,4))" ; "--init-file";(good_test "pm_test.religo") ] ;
  [%expect{|
    10 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t2 (Nil))(Nil)" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t2 (Nil))(Cons (1,2))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 3 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t2 (Cons(1,2)))(Cons(1,2))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 6 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t2 (Cons(1,2)))(Nil)" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 7 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t3 (One (Nil))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t3 (One (Cons(1,2)))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t3 (Two ({a : 1 , b : 2n , c : \"tri\"}))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 6 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "((t2_3 (Cons(1,2)))(Nil))(One(Nil))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 8 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t4 (One(Nil)))(One (Nil))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t4 (One(Nil)))(Two ({a:1,b:2n,c:\"tri\"}))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 2 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t4 (One(Cons(1,2))))(Two ({a:1,b:2n,c:\"tri\"}))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 3 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t4 (Two ({a:0,b:0n,c:\"\"})))(Two ({a:1,b:2n,c:\"tri\"}))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 4 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t5 (1)" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t6 (42)" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 2 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t7 (Some (10))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 10 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t7 ((None: option(int)))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t8 (Some(1,2)))(2)" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 3 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t8 (None: option((int, int))))(2)" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 2 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t9 (None:option(int)))(None: option(int))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t9 (None: option(int)))(Some (1))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t9 (Some (1)))(None: option(int))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 2 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t9 (Some (1)))(Some (2))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 3 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t10 (Consi(None:option(int))))(Consi(Some (100)))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t11 (Consi(None:option(int))))(Consi(Some (100)))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 4 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 ([]: list(int))" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 0 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 ([1])" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 ([1,2])" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 3 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 ([1,2,3])" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 6 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret"  ; "t12 ([1,2,3,4])" ; "--init-file" ; (good_test "pm_test.religo")] ;
  [%expect{| -1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t13 (none_a))(some_a)" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| -1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t13 (some_a))(a_empty_b_not)" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 111 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t13 (some_a))(b_empty_a_not)" ; "--init-file" ; (good_test "pm_test.religo") ] ;
  [%expect{| 222 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "(t13 (some_a))(some_a)" ; "--init-file";(good_test "pm_test.religo") ] ;
  [%expect{| 4 |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (good_test "list_pattern.religo") ] ;
  [%expect{|
    const a =
       match CONS(1 , LIST_EMPTY()) with
        | [] -> 1
        | a::b::c::[] -> 2
        | gen#2 -> 3 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (good_test "pm_ticket.religo") ] ;
  [%expect{|
    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    Reasonligo is depreacted, support will be dropped in a few versions.

    { parameter (pair (pair (nat %mynat) (ticket %myt int)) (option nat)) ;
      storage nat ;
      code { CAR ;
             UNPAIR ;
             CAR ;
             SWAP ;
             IF_NONE {} { SWAP ; DROP } ;
             NIL operation ;
             PAIR } } |}]
