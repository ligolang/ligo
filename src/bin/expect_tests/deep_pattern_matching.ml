open Cli_expect

let bad_test s = (bad_test "")^"/deep_pattern_matching/"^s
let good_test s = (test "")^"/deep_pattern_matching/"^s

(* Negatives *)

(* wrong fields on record pattern *)
(* wrong type on constructor argument pattern *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail16.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail16.mligo", line 6, characters 4-25:
      5 |   match action with
      6 |   | {one = _ ; three = _} -> 0

    Pattern do not conform type record[one -> int , two -> int] |}]

(* wrong type on constructor argument pattern *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail15.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail15.mligo", line 8, characters 15-19:
      7 |   match action with
      8 |   | Increment (n, m) -> 0
      9 |   | Reset            -> 0

    Pattern do not conform type ( int * int * int ) |}]

(* wrong unit pattern in a let destructuring *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail14.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail14.mligo", line 2, characters 6-8:
      1 | let main (_ : unit * unit) : operation list * unit =
      2 |   let () = 42n in
      3 |   (([] : operation list), ())

    Variant pattern argument is expected of type nat but is of type unit. |}]


(* Trying to match on values *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail10.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail10.mligo", line 5, characters 8-9:
      4 |   match x with
      5 |   | One 1 -> 2
      6 |   | Two -> 1

    Invalid pattern.
    Can't match on values. |}]

(* unbound variable *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail9.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail9.mligo", line 6, characters 11-12:
      5 |   | One a -> 2
      6 |   | Two -> a

    Variable "a" not found. |}]

(* wrong patterns type *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail1.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail1.mligo", line 6, characters 10-33:
      5 |   match x with
      6 |   | Nil , {a = a ; b = b ; c = c} -> 1
      7 |   | xs  , Nil -> 2

    Pattern do not conform type sum[Cons -> ( int * int ) , Nil -> unit] |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail2.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail2.mligo", line 5, characters 11-16:
      4 |   match x with
      5 |   | Nil , (a,b,c) -> 1
      6 |   | xs  , Nil -> 2

    Pattern do not conform type sum[Cons -> ( int * int ) , Nil -> unit] |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail5.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail5.mligo", line 6, characters 4-13:
      5 |   | Some_fake x -> x
      6 |   | None_fake -> 1

    Pattern do not conform type option (int) |}]

(* wrong body type *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail7.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail7.mligo", line 6, characters 9-10:
      5 |   | A -> "hey"
      6 |   | B -> 2

    Invalid type(s).
    Expected: "string", but got: "int". |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail8.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail8.mligo", line 17, character 8 to line 18, character 15:
     16 |       | Nil ->
     17 |         let f = fun (b:int) -> b + a in
     18 |         f (b+1)
     19 |       | Cons (a,b) -> "invalid"

    Invalid type(s).
    Expected: "string", but got: "int". |}]


(* rendundancy detected while compiling the pattern matching *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail3.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail3.mligo", line 4, character 2 to line 6, character 21:
      3 | let t = fun (x: myt * ( int * int * int)) ->
      4 |   match x with
      5 |   | xs , (a,b,c) -> 1
      6 |   | xs , (c,b,a) -> 2

    Redundant pattern matching |}]

(* anomaly detected in the pattern matching self_ast_typed pass *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail11.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail11.mligo", line 2, character 2 to line 4, character 11:
      1 | let t12 = fun (x : int list) ->
      2 |   match x with
      3 |   | hd::(hd2::tl) -> hd + hd2
      4 |   | [] -> 0

    Pattern matching anomaly (redundant, or non exhaustive). |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail12.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail12.mligo", line 4, character 2 to line 6, character 40:
      3 | let t13 = fun (x:recordi) ->
      4 |   match x with
      5 |   | { a = Some ([]) ; b = (hd::tl) } -> hd
      6 |   | { a = Some (hd::tl) ; b = [] } -> hd

    Pattern matching anomaly (redundant, or non exhaustive). |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail13.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail13.mligo", line 7, characters 5-14:
      6 |    | Increment n -> s +1
      7 |    | Decrement -> s -1
      8 |  in ([] : operation list), stor

    Variant pattern argument is expected of type nat but is of type unit. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail4.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail4.mligo", line 4, character 2 to line 6, character 18:
      3 | let t = fun (x: myt * myt) ->
      4 |   match x with
      5 |   | Nil , ys  -> 1
      6 |   | xs  , Nil -> 2

    Pattern matching anomaly (redundant, or non exhaustive). |}]

(* Positives *)

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1 (Nil,Nil)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1 (Nil,Cons(1,2))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1 (Cons(1,2),Nil)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t1 (Cons(1,2),Cons(3,4))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    10 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2 Nil Nil" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2 Nil (Cons (1,2))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2 (Cons(1,2)) (Cons(1,2))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    6 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2 (Cons(1,2)) Nil" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    7 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t3 (One (Nil))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t3 (One (Cons(1,2)))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t3 (Two {a = 1 ; b = 2n ; c = \"tri\"})" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    6 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t2_3 (Cons(1,2)) Nil (One(Nil))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    8 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t4 (One(Nil)) (One (Nil))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t4 (One(Nil)) (Two {a=1;b=2n;c=\"tri\"})" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t4 (One(Cons(1,2))) (Two {a=1;b=2n;c=\"tri\"})" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t4 (Two {a=0;b=0n;c=\"\"}) (Two {a=1;b=2n;c=\"tri\"})" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t5 1" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t6 42" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t7 (Some 10)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    10 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t7 (None: int option)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t8 (Some (1,2)) 2" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t8 (None:(int * int) option) 2" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t9 (None:int option) (None:int option)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t9 (None:int option) (Some 1)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t9 (Some 1) (None:int option)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    2 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t9 (Some 1) (Some 2)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t10 (Consi(None:int option)) (Consi(Some 100))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t11 (Consi(None:int option)) (Consi(Some 100))" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 ([]: int list)" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{| 0 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 [1]" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 [1;2]" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    3 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 [1;2;3]" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    6 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t12 [1;2;3;4]" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    -1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t13 none_a some_a" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    -1 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t13 some_a a_empty_b_not" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    111 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t13 some_a b_empty_a_not" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    222 |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "interpret" ; "t13 some_a some_a" ; "--init-file" ; (good_test "pm_test.mligo") ] ;
  [%expect{|
    4 |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; (good_test "pm_ticket.mligo") ] ;
  [%expect{|
    File "../../test/contracts//deep_pattern_matching/pm_ticket.mligo", line 7, characters 14-17:
      6 |   match p with
      7 |     | { myt = myt ; mynat = mynat } , None -> (([]: operation list), mynat)
      8 |     | { myt = myt ; mynat = mynat } , Some x -> (([]: operation list), x)
    :
    Warning: unused variable "myt".
    Hint: replace it by "_myt" to prevent this warning.

    File "../../test/contracts//deep_pattern_matching/pm_ticket.mligo", line 5, characters 18-19:
      4 |
      5 | let main = fun (p,s: parameter * storage) ->
      6 |   match p with
    :
    Warning: unused variable "s".
    Hint: replace it by "_s" to prevent this warning.

    { parameter (pair (pair (nat %mynat) (ticket %myt int)) (option nat)) ;
      storage nat ;
      code { CAR ;
             UNPAIR ;
             CAR ;
             SWAP ;
             IF_NONE { NIL operation ; PAIR } { SWAP ; DROP ; NIL operation ; PAIR } } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-core" ; (good_test "list_pattern.mligo") ] ;
  [%expect{|
    const a =
       match CONS(1 , LIST_EMPTY()) with
        | [  ] -> 1
        | a :: b :: c :: [  ] -> 2
        | #1 -> 3 |}]


let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (good_test "pm_test.religo") ] ;
  [%expect{|
    type myt = sum[Cons -> ( int * int ) , Nil -> unit]
    type myr = record[a -> int , b -> nat , c -> string]
    type myd = sum[One -> sum[Cons -> ( int * int ) , Nil -> unit] , Two -> record[a -> int , b -> nat , c -> string]]
    const t1 = lambda (x) return let fr = lambda (_x) return 1 in let fl = lambda (_x) return 2 in
     match x with
      | ( tuple_proj#39 , ys ) ->
       match tuple_proj#39 with
        | Cons ctor_proj#52 ->
           match ys with
            | Cons ctor_proj#50 ->
               match ctor_proj#52 with
                | ( a , b ) ->
                 match ctor_proj#50 with
                  | ( c , d ) ->
                  ADD(ADD(ADD(a , b) , c) , d)
            | Nil unit_proj#49 ->
              (fl)@(tuple_proj#39)
        | Nil unit_proj#51 ->
          (fr)@(ys)
    const t2 = lambda (x) return lambda (y) return  match x with
                                                     | Cons ctor_proj#53 ->
                                                        match ctor_proj#53 with
                                                         | ( a , b ) ->
                                                         let old_b = b in let b =
                                                          match y with
                                                           | Cons ctor_proj#56 ->
                                                             ADD(a ,
                                                             b)
                                                           | Nil unit_proj#55 ->
                                                             let f = lambda (b) return ADD(a ,
                                                             b) in (f)@(ADD(b ,
                                                             1)) in ADD(ADD(a ,
                                                         old_b) , b)
                                                     | Nil unit_proj#57 ->
                                                        match y with
                                                         | Cons ctor_proj#58 ->
                                                            match ctor_proj#58 with
                                                             | ( _a , b ) ->
                                                             let a = "a" in ADD(INT(SIZE(a)) ,
                                                             b)
                                                         | Nil unit_proj#60 ->
                                                           1
    const t3 = lambda (x) return  match x with
                                   | One ctor_proj#61 ->
                                      match ctor_proj#61 with
                                       | Cons ctor_proj#66 ->
                                          match ctor_proj#61 with
                                           | Cons ctor_proj#62 ->
                                              match ctor_proj#62 with
                                               | ( a , b ) ->
                                               ADD(a , b)
                                           | Nil unit_proj#64 ->
                                             2
                                       | Nil unit_proj#65 ->
                                         1
                                   | Two ctor_proj#67 ->
                                      match ctor_proj#67 with
                                       | record[a -> a , b -> b , c -> c] ->
                                       ADD(ADD(a , INT(b)) , INT(SIZE(c)))
    const t2_3 = lambda (x) return lambda (y) return lambda (x2) return let t2 =
     match x with
      | Cons ctor_proj#69 ->
         match ctor_proj#69 with
          | ( a , b ) ->
          let old_b = b in let b =  match y with
                                     | Cons ctor_proj#71 ->
                                        match ctor_proj#71 with
                                         | ( a , b ) ->
                                         ADD(a , b)
                                     | Nil unit_proj#73 ->
                                       let f = lambda (b) return ADD(a ,
                                       b) in (f)@(ADD(b ,
                                       1)) in ADD(ADD(a ,
          old_b) , b)
      | Nil unit_proj#74 ->
         match y with
          | Cons ctor_proj#75 ->
             match ctor_proj#75 with
              | ( _a , b ) ->
              let a = "a" in ADD(INT(SIZE(a)) , b)
          | Nil unit_proj#77 ->
            1 in let t3 =  match x2 with
                            | One ctor_proj#78 ->
                               match ctor_proj#78 with
                                | Cons ctor_proj#83 ->
                                   match ctor_proj#78 with
                                    | Cons ctor_proj#79 ->
                                       match ctor_proj#79 with
                                        | ( a , b ) ->
                                        ADD(a , b)
                                    | Nil unit_proj#81 ->
                                      2
                                | Nil unit_proj#82 ->
                                  1
                            | Two ctor_proj#84 ->
                               match ctor_proj#84 with
                                | record[a -> a , b -> b , c -> c] ->
                                ADD(ADD(a , b) , INT(SIZE(c))) in ADD(t2 ,
    t3)
    const t4 = lambda (x) return lambda (y) return let #86 = ( x , y ) in
     match #86 with
      | ( a , tuple_proj#87 ) ->
       match tuple_proj#87 with
        | Two ctor_proj#101 ->
           match a with
            | One ctor_proj#92 ->
               match ctor_proj#92 with
                | Cons ctor_proj#93 ->
                   match ctor_proj#93 with
                    | ( a , b ) ->
                    ADD(a , b)
                | Nil unit_proj#95 ->
                  2
            | Two ctor_proj#96 ->
               match ctor_proj#96 with
                | record[a -> a , b -> b , c -> c] ->
                 match ctor_proj#101 with
                  | record[a -> aa , b -> #4 , c -> cc] ->
                  ADD(ADD(ADD(ADD(a , INT(b)) , INT(SIZE(c))) , aa) ,
                  INT(SIZE(cc)))
        | One _x ->
          1
    const t5 = lambda (x) return let #102 = ( x , unit ) in  match #102 with
                                                              | ( a , tuple_proj#103 ) ->
                                                              a
    const t6 = lambda (x) return let #105 = ( x , unit ) in  match #105 with
                                                              | ( #5 , #6 ) ->
                                                              2
    const t7 = lambda (x) return  match x with
                                   | Some x ->
                                     x | None unit_proj#107 ->
                                         1
    const t8 = lambda (x) return lambda (y) return let #108 = ( x , y ) in
     match #108 with
      | ( tuple_proj#109 , x ) ->
       match tuple_proj#109 with
        | Some ctor_proj#112 ->
           match ctor_proj#112 with
            | ( x , y ) ->
            ADD(x , y)
        | None unit_proj#114 ->
          x
    const t9 = lambda (x) return lambda (y) return let #115 = ( x , y ) in
     match #115 with
      | ( tuple_proj#116 , ys ) ->
       match tuple_proj#116 with
        | Some ctor_proj#125 ->
           match ys with
            | Some ctor_proj#123 ->
              ADD(ctor_proj#125 ,
              ctor_proj#123)
            | None unit_proj#122 ->
              2
        | None unit_proj#124 ->
          1
    type optioni = option (int)
    type myti = sum[Consi -> option (int) , Nili -> unit]
    const fl = lambda (_x) return 1
    const fo = lambda (_x) return 2
    const t10 = lambda (x) return lambda (y) return let #126 = ( x , y ) in
     match #126 with
      | ( tuple_proj#127 , ys ) ->
       match tuple_proj#127 with
        | Consi ctor_proj#143 ->
           match ys with
            | Consi ctor_proj#141 ->
               match ctor_proj#143 with
                | Some ctor_proj#138 ->
                  ADD((fo)@(ctor_proj#143) ,
                  (fo)@(ctor_proj#141))
                | None unit_proj#134 ->
                   match ys with
                    | Nili ctor_proj#137 ->
                      ADD((fo)@(ctor_proj#143) ,
                      (fo)@(ctor_proj#141))
                    | Consi ctor_proj#135 ->
                       match ctor_proj#135 with
                        | None ctor_proj#136 ->
                          ADD((fo)@(ctor_proj#143) ,
                          (fo)@(ctor_proj#141))
                        | Some _b ->
                          let b = 1 in b
            | Nili unit_proj#140 ->
              (fl)@(tuple_proj#127)
        | Nili unit_proj#142 ->
          (fl)@(ys)
    const t11 = lambda (x) return lambda (y) return let #144 = ( x , y ) in
     match #144 with
      | ( tuple_proj#145 , ys ) ->
       match tuple_proj#145 with
        | Consi ctor_proj#161 ->
           match ys with
            | Consi ctor_proj#159 ->
               match ctor_proj#161 with
                | None ctor_proj#156 ->
                   match ctor_proj#161 with
                    | Some a ->
                      a
                    | None unit_proj#151 ->
                      ADD((fo)@(ctor_proj#161) ,
                      (fo)@(ctor_proj#159))
                | Some _a ->
                   match ys with
                    | Nili ctor_proj#155 ->
                       match ctor_proj#161 with
                        | Some a ->
                          a
                        | None unit_proj#151 ->
                          ADD((fo)@(ctor_proj#161) ,
                          (fo)@(ctor_proj#159))
                    | Consi ctor_proj#153 ->
                       match ctor_proj#153 with
                        | None ctor_proj#154 ->
                           match ctor_proj#161 with
                            | Some a ->
                              a
                            | None unit_proj#151 ->
                              ADD((fo)@(ctor_proj#161) ,
                              (fo)@(ctor_proj#159))
                        | Some b ->
                          let a = 1 in ADD(a ,
                          b)
            | Nili unit_proj#158 ->
              (fl)@(tuple_proj#145)
        | Nili unit_proj#160 ->
          (fl)@(ys)
    const t12 = lambda (x) return  match x with
                                    | Cons ctor_proj#162 ->
                                       match ctor_proj#162 with
                                        | ( hd , tuple_proj#163 ) ->
                                         match tuple_proj#163 with
                                          | Cons ctor_proj#168 ->
                                             match ctor_proj#168 with
                                              | ( hd2 , tuple_proj#169 ) ->
                                               match tuple_proj#169 with
                                                | Cons ctor_proj#172 ->
                                                   match ctor_proj#172 with
                                                    | ( hd3 , tuple_proj#173 ) ->
                                                     match tuple_proj#173 with
                                                      | Cons ctor_proj#176 ->
                                                        NEG(1)
                                                      | Nil unit_proj#175 ->
                                                        ADD(ADD(hd ,
                                                        hd2) ,
                                                        hd3)
                                                | Nil unit_proj#177 ->
                                                  ADD(hd ,
                                                  hd2)
                                          | Nil unit_proj#178 ->
                                            hd
                                    | Nil unit_proj#179 ->
                                      0
    type recordi = record[a -> option (list (int)) , b -> list (int)]
    const none_a = record[a -> NONE() , b -> CONS(42 , LIST_EMPTY())]
    const some_a = record[a -> SOME(CONS(1 , CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))))) , b -> CONS(42 , LIST_EMPTY())]
    const a_empty_b_not = record[a -> SOME(LIST_EMPTY()) , b -> CONS(111 , LIST_EMPTY())]
    const b_empty_a_not = record[a -> SOME(CONS(222 , LIST_EMPTY())) , b -> LIST_EMPTY()]
    const t13 = lambda (x) return lambda (y) return let #180 = ( x , y ) in
     match #180 with
      | ( tuple_proj#181 , tuple_proj#182 ) ->
       match tuple_proj#181 with
        | record[a -> record_proj#187 , b -> #8] ->
         match record_proj#187 with
          | Some ctor_proj#209 ->
             match tuple_proj#182 with
              | record[a -> record_proj#193 , b -> record_proj#194] ->
               match record_proj#193 with
                | None ctor_proj#206 ->
                  INT(SIZE(ctor_proj#209))
                | Some ctor_proj#197 ->
                   match ctor_proj#197 with
                    | Cons ctor_proj#198 ->
                       match ctor_proj#198 with
                        | ( hd , _tl ) ->
                         match record_proj#194 with
                          | Cons ctor_proj#201 ->
                            INT(SIZE(ctor_proj#209))
                          | Nil unit_proj#200 ->
                            hd
                    | Nil unit_proj#202 ->
                       match record_proj#194 with
                        | Nil ctor_proj#205 ->
                          INT(SIZE(ctor_proj#209))
                        | Cons ctor_proj#203 ->
                           match ctor_proj#203 with
                            | ( hd , _tl ) ->
                            hd
          | None unit_proj#207 ->
             match tuple_proj#182 with
              | record[a -> #10 , b -> #9] ->
              NEG(1) |}]
