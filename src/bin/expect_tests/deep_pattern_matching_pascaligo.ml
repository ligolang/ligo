open Cli_expect

let bad_test s = (bad_test "")^"/deep_pattern_matching/"^s
let good_test s = (test "")^"/deep_pattern_matching/"^s

(* Negatives *)

(* Trying to match on values *)
let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail10.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail10.ligo", line 5, characters 9-10:
      4 |   case x of
      5 |   | One (1) -> 2
      6 |   | Two -> 1

    Invalid case pattern.
    Can't match on values. |}]

(* unbound variable *)

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail9.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail9.ligo", line 6, characters 11-12:
      5 |   | One (a) -> 2
      6 |   | Two -> a
      7 |   end

    Variable "a" not found. |}]

(* wrong patterns type *)
let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail1.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail1.ligo", line 6, characters 11-30:
      5 |   case x of
      6 |   | (Nil , record [a ; b ; c ]) -> 1
      7 |   | (xs  , Nil) -> 2

    Pattern do not conform type sum[Cons -> ( int * int ) , Nil -> unit] |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail2.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail2.ligo", line 5, characters 11-18:
      4 |   case x of
      5 |   | (Nil , (a,b,c)) -> 1
      6 |   | (xs  , Nil) -> 2

    Pattern do not conform type sum[Cons -> ( int * int ) , Nil -> unit] |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail5.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail5.ligo", line 6, characters 4-13:
      5 |   | Some_fake (x) -> x
      6 |   | None_fake -> 1
      7 |   end

    Pattern do not conform type option (int) |}]

(* wrong body type *)

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail7.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail7.ligo", line 6, characters 9-10:
      5 |   | A -> "hey"
      6 |   | B -> 2
      7 |   end

    Invalid type(s).
    Expected: "string", but got: "int". |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail8.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail8.ligo", line 21, characters 12-53:
     20 |           block {
     21 |             const f = function (const b:int) is b + a ;
     22 |           } with f (b+1)

    Invalid type(s).
    Expected: "string", but got: "int". |}]


(* rendundancy detected while compiling the pattern matching *)
let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail3.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail3.ligo", line 4, character 2 to line 7, character 5:
      3 | function t (const x: myt * ( int * int * int)) is
      4 |   case x of
      5 |   | (xs , (a,b,c)) -> 1
      6 |   | (xs , (c,b,a)) -> 2
      7 |   end

    Redundant pattern matching |}]

(* anomaly detected in the pattern matching self_ast_typed pass *)

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail11.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail11.ligo", line 2, character 2 to line 5, character 5:
      1 | function t (const x : list(int)) is
      2 |   case x of
      3 |   | hd#(hd2#tl) -> hd + hd2
      4 |   | nil -> 0
      5 |   end

    Pattern matching anomaly (redundant, or non exhaustive). |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail12.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail12.ligo", line 4, character 2 to line 7, character 5:
      3 | function t (const x:recordi) is
      4 |   case x of
      5 |   | record [ a = Some (nil) ; b = (hd#tl) ] -> hd
      6 |   | record [ a = Some ((hd#tl)) ; b = nil ] -> hd
      7 |   end

    Pattern matching anomaly (redundant, or non exhaustive). |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail4.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail4.ligo", line 4, character 2 to line 7, character 5:
      3 | function t (const x: myt * myt) is
      4 |   case x of
      5 |   | (Nil , ys)  -> 1
      6 |   | (xs  , Nil) -> 2
      7 |   end

    Pattern matching anomaly (redundant, or non exhaustive). |}]

let%expect_test _ =
  run_ligo_bad [ "print-ast-typed" ; (bad_test "pm_fail13.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail13.ligo", line 10, characters 8-17:
      9 |         Increment (n) -> s + 1
     10 |       | Decrement -> s - 1
     11 |     ]

    Variant pattern argument is expected of type nat but is of type unit. |}]

(* Positives *)

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t1 (Nil,Nil)" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t1 (Nil,Cons(1,2))" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t1 (Cons(1,2),Nil)" ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t1 (Cons(1,2),Cons(3,4))" ] ;
  [%expect{|
    10 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t2 (Nil, Nil)" ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t2 (Nil, (Cons (1,2)))" ] ;
  [%expect{| 3 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t2 (Cons(1,2) , Cons(1,2))" ] ;
  [%expect{| 6 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t2 (Cons(1,2) , Nil)" ] ;
  [%expect{| 7 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t3 (One (Nil))" ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t3 (One (Cons(1,2)))" ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t3 (Two (record [a = 1 ; b = 2n ; c = \"tri\"]))" ] ;
  [%expect{| 6 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t2_3 (Cons(1,2) , Nil, (One(Nil)))" ] ;
  [%expect{| 8 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t4 (One(Nil) , One (Nil))" ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t4 (One(Nil) , Two (record [a=1;b=2n;c=\"tri\"]))" ] ;
  [%expect{| 2 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t4 (One(Cons(1,2)) , Two (record [ a=1;b=2n;c=\"tri\"]))" ] ;
  [%expect{| 3 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t4 (Two (record [a=0;b=0n;c=\"\"]) , Two (record [ a=1;b=2n;c=\"tri\"]))" ] ;
  [%expect{| 4 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t5 (1)" ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t6 (42)" ] ;
  [%expect{| 2 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t7 (Some (10))" ] ;
  [%expect{| 10 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t7 ((None: option(int)))" ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t8 (Some (1,2), 2)" ] ;
  [%expect{| 3 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t8 ( (None: option(int * int)), 2)" ] ;
  [%expect{| 2 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t9 ((None:option(int)) , (None: option(int)))" ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t9 ((None: option(int)) , Some (1))" ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t9 (Some (1) , (None: option(int)))" ] ;
  [%expect{| 2 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t9 (Some (1) , Some (2))" ] ;
  [%expect{| 3 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t10 (Consi ((None:  option(int))) ,  Consi(Some (100)) )" ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t11 (Consi ((None: option(int))) , Consi (Some (100)))" ] ;
  [%expect{| 4 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t12 ((nil: list(int)))" ] ;
  [%expect{| 0 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t12 (list [1])" ] ;
  [%expect{| 1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t12 (list [1;2])" ] ;
  [%expect{| 3 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t12 (list [1;2;3])" ] ;
  [%expect{| 6 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t12 (list [1;2;3;4])" ] ;
  [%expect{| -1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t13 (none_a , some_a)" ] ;
  [%expect{| -1 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t13 (some_a , a_empty_b_not)" ] ;
  [%expect{| 111 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t13 (some_a,  b_empty_a_not)" ] ;
  [%expect{| 222 |}]

let%expect_test _ =
  run_ligo_good [ "interpret" ; "--init-file="^(good_test "pm_test.ligo") ; "t13 (some_a, some_a)" ] ;
  [%expect{| 4 |}]

let%expect_test _ =
  run_ligo_good [ "print-ast-core" ; (good_test "list_pattern.ligo") ] ;
  [%expect{|
    const a =
       match CONS(1 , LIST_EMPTY()) with
        | [  ] -> 1
        | a :: b :: c :: [  ] -> 2
        | _ -> 3 |}]
