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

    Pattern not of the expected type record[one -> int , two -> int] |}]

(* wrong type on constructor argument pattern *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail15.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail15.mligo", line 8, characters 15-19:
      7 |   match action with
      8 |   | Increment (n, m) -> 0
      9 |   | Reset            -> 0

    Pattern not of the expected type ( int * int * int ) |}]

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

    Pattern not of the expected type sum[Cons -> ( int * int ) , Nil -> unit] |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail2.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail2.mligo", line 5, characters 11-16:
      4 |   match x with
      5 |   | Nil , (a,b,c) -> 1
      6 |   | xs  , Nil -> 2

    Pattern not of the expected type sum[Cons -> ( int * int ) , Nil -> unit] |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail5.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail5.mligo", line 6, characters 4-13:
      5 |   | Some_fake x -> x
      6 |   | None_fake -> 1

    Pattern not of the expected type option (int) |}]

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
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail8.mligo", line 18, characters 8-15:
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
        | _#2 -> 3 |}]


let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (good_test "pm_test.religo") ] ;
  [%expect{|
    type myt = sum[Cons -> ( int * int ) , Nil -> unit]
    type myr = record[a -> int , b -> nat , c -> string]
    type myd =
      sum[One -> sum[Cons -> ( int * int ) , Nil -> unit] , Two -> record[a -> int , b -> nat , c -> string]]
    const t1 =
      lambda (x) return let fr = lambda (_x) return 1 in let fl = lambda (_x) return 2 in
       match x with
        | ( tuple_proj#38 , ys ) ->
         match tuple_proj#38 with
          | Cons ctor_proj#51 ->
             match ys with
              | Cons ctor_proj#49 ->
                 match ctor_proj#51 with
                  | ( a , b ) ->
                   match ctor_proj#49 with
                    | ( c , d ) ->
                    ADD(ADD(ADD(a , b) , c) , d)
              | Nil unit_proj#48 ->
                (fl)@(tuple_proj#38)
          | Nil unit_proj#50 ->
            (fr)@(ys)
    const t2 =
      lambda (x) return lambda (y) return  match x with
                                            | Cons ctor_proj#52 ->
                                               match ctor_proj#52 with
                                                | ( a , b ) ->
                                                let old_b = b in let b =
                                                 match y with
                                                  | Cons ctor_proj#55 ->
                                                    ADD(a ,
                                                    b)
                                                  | Nil unit_proj#54 ->
                                                    let f = lambda (b) return ADD(a ,
                                                    b) in (f)@(ADD(b ,
                                                    1)) in ADD(ADD(a ,
                                                old_b) , b)
                                            | Nil unit_proj#56 ->
                                               match y with
                                                | Cons ctor_proj#57 ->
                                                   match ctor_proj#57 with
                                                    | ( _a , b ) ->
                                                    let a = "a" in ADD(INT(SIZE(a)) ,
                                                    b)
                                                | Nil unit_proj#59 ->
                                                  1
    const t3 =
      lambda (x) return  match x with
                          | One ctor_proj#60 ->
                             match ctor_proj#60 with
                              | Cons ctor_proj#65 ->
                                 match ctor_proj#60 with
                                  | Cons ctor_proj#61 ->
                                     match ctor_proj#61 with
                                      | ( a , b ) ->
                                      ADD(a , b)
                                  | Nil unit_proj#63 ->
                                    2
                              | Nil unit_proj#64 ->
                                1
                          | Two ctor_proj#66 ->
                             match ctor_proj#66 with
                              | record[a -> a , b -> b , c -> c] ->
                              ADD(ADD(a , INT(b)) , INT(SIZE(c)))
    const t2_3 =
      lambda (x) return lambda (y) return lambda (x2) return let t2 =  match
                                                                        x with
                                                                        | Cons ctor_proj#68 ->
                                                                         match
                                                                        ctor_proj#68 with
                                                                        | ( a , b ) ->
                                                                        let old_b = b in let b =
                                                                         match
                                                                        y with
                                                                        | Cons ctor_proj#70 ->
                                                                         match
                                                                        ctor_proj#70 with
                                                                        | ( a , b ) ->
                                                                        ADD(a ,
                                                                        b)
                                                                        | Nil unit_proj#72 ->
                                                                        let f = lambda (b) return ADD(a ,
                                                                        b) in (f)@(ADD(b ,
                                                                        1)) in ADD(ADD(a ,
                                                                        old_b) ,
                                                                        b)
                                                                        | Nil unit_proj#73 ->
                                                                         match
                                                                        y with
                                                                        | Cons ctor_proj#74 ->
                                                                         match
                                                                        ctor_proj#74 with
                                                                        | ( _a , b ) ->
                                                                        let a = "a" in ADD(INT(SIZE(a)) ,
                                                                        b)
                                                                        | Nil unit_proj#76 ->
                                                                        1 in let t3 =
       match x2 with
        | One ctor_proj#77 ->
           match ctor_proj#77 with
            | Cons ctor_proj#82 ->
               match ctor_proj#77 with
                | Cons ctor_proj#78 ->
                   match ctor_proj#78 with
                    | ( a , b ) ->
                    ADD(a , b)
                | Nil unit_proj#80 ->
                  2
            | Nil unit_proj#81 ->
              1
        | Two ctor_proj#83 ->
           match ctor_proj#83 with
            | record[a -> a , b -> b , c -> c] ->
            ADD(ADD(a , b) , INT(SIZE(c))) in ADD(t2 ,
      t3)
    const t4 =
      lambda (x) return lambda (y) return let gen#85 = ( x , y ) in  match
                                                                      gen#85 with
                                                                      | ( a , tuple_proj#86 ) ->
                                                                       match
                                                                        tuple_proj#86 with
                                                                        | Two ctor_proj#100 ->
                                                                         match
                                                                        a with
                                                                        | One ctor_proj#91 ->
                                                                         match
                                                                        ctor_proj#91 with
                                                                        | Cons ctor_proj#92 ->
                                                                         match
                                                                        ctor_proj#92 with
                                                                        | ( a , b ) ->
                                                                        ADD(a ,
                                                                        b)
                                                                        | Nil unit_proj#94 ->
                                                                        2
                                                                        | Two ctor_proj#95 ->
                                                                         match
                                                                        ctor_proj#95 with
                                                                        | record[a -> a , b -> b , c -> c] ->
                                                                         match
                                                                        ctor_proj#100 with
                                                                        | record[a -> aa , b -> gen#3 , c -> cc] ->
                                                                        ADD(ADD(ADD(ADD(a ,
                                                                        INT(b)) ,
                                                                        INT(SIZE(c))) ,
                                                                        aa) ,
                                                                        INT(SIZE(cc)))
                                                                        | One _x ->
                                                                        1
    const t5 =
      lambda (x) return let gen#101 = ( x , unit ) in  match gen#101 with
                                                        | ( a , tuple_proj#102 ) ->
                                                        a
    const t6 =
      lambda (x) return let gen#104 = ( x , unit ) in  match gen#104 with
                                                        | ( gen#4 , gen#5 ) ->
                                                        2
    const t7 =
      lambda (x) return  match x with
                          | Some x ->
                            x | None unit_proj#106 ->
                                1
    const t8 =
      lambda (x) return lambda (y) return let gen#107 = ( x , y ) in  match
                                                                       gen#107 with
                                                                       |
                                                                       ( tuple_proj#108 , x ) ->
                                                                        match
                                                                        tuple_proj#108 with
                                                                        | Some ctor_proj#111 ->
                                                                         match
                                                                        ctor_proj#111 with
                                                                        | ( x , y ) ->
                                                                        ADD(x ,
                                                                        y)
                                                                        | None unit_proj#113 ->
                                                                        x
    const t9 =
      lambda (x) return lambda (y) return let gen#114 = ( x , y ) in  match
                                                                       gen#114 with
                                                                       |
                                                                       ( tuple_proj#115 , ys ) ->
                                                                        match
                                                                        tuple_proj#115 with
                                                                        | Some ctor_proj#124 ->
                                                                         match
                                                                        ys with
                                                                        | Some ctor_proj#122 ->
                                                                        ADD(ctor_proj#124 ,
                                                                        ctor_proj#122)
                                                                        | None unit_proj#121 ->
                                                                        2
                                                                        | None unit_proj#123 ->
                                                                        1
    type optioni = option (int)
    type myti = sum[Consi -> option (int) , Nili -> unit]
    const fl = lambda (_x) return 1
    const fo = lambda (_x) return 2
    const t10 =
      lambda (x) return lambda (y) return let gen#125 = ( x , y ) in  match
                                                                       gen#125 with
                                                                       |
                                                                       ( tuple_proj#126 , ys ) ->
                                                                        match
                                                                        tuple_proj#126 with
                                                                        | Consi ctor_proj#142 ->
                                                                         match
                                                                        ys with
                                                                        | Consi ctor_proj#140 ->
                                                                         match
                                                                        ctor_proj#142 with
                                                                        | Some ctor_proj#137 ->
                                                                        ADD((fo)@(ctor_proj#142) ,
                                                                        (fo)@(ctor_proj#140))
                                                                        | None unit_proj#133 ->
                                                                         match
                                                                        ys with
                                                                        | Nili ctor_proj#136 ->
                                                                        ADD((fo)@(ctor_proj#142) ,
                                                                        (fo)@(ctor_proj#140))
                                                                        | Consi ctor_proj#134 ->
                                                                         match
                                                                        ctor_proj#134 with
                                                                        | None ctor_proj#135 ->
                                                                        ADD((fo)@(ctor_proj#142) ,
                                                                        (fo)@(ctor_proj#140))
                                                                        | Some _b ->
                                                                        let b = 1 in b
                                                                        | Nili unit_proj#139 ->
                                                                        (fl)@(tuple_proj#126)
                                                                        | Nili unit_proj#141 ->
                                                                        (fl)@(ys)
    const t11 =
      lambda (x) return lambda (y) return let gen#143 = ( x , y ) in  match
                                                                       gen#143 with
                                                                       |
                                                                       ( tuple_proj#144 , ys ) ->
                                                                        match
                                                                        tuple_proj#144 with
                                                                        | Consi ctor_proj#160 ->
                                                                         match
                                                                        ys with
                                                                        | Consi ctor_proj#158 ->
                                                                         match
                                                                        ctor_proj#160 with
                                                                        | None ctor_proj#155 ->
                                                                         match
                                                                        ctor_proj#160 with
                                                                        | Some a ->
                                                                        a
                                                                        | None unit_proj#150 ->
                                                                        ADD((fo)@(ctor_proj#160) ,
                                                                        (fo)@(ctor_proj#158))
                                                                        | Some _a ->
                                                                         match
                                                                        ys with
                                                                        | Nili ctor_proj#154 ->
                                                                         match
                                                                        ctor_proj#160 with
                                                                        | Some a ->
                                                                        a
                                                                        | None unit_proj#150 ->
                                                                        ADD((fo)@(ctor_proj#160) ,
                                                                        (fo)@(ctor_proj#158))
                                                                        | Consi ctor_proj#152 ->
                                                                         match
                                                                        ctor_proj#152 with
                                                                        | None ctor_proj#153 ->
                                                                         match
                                                                        ctor_proj#160 with
                                                                        | Some a ->
                                                                        a
                                                                        | None unit_proj#150 ->
                                                                        ADD((fo)@(ctor_proj#160) ,
                                                                        (fo)@(ctor_proj#158))
                                                                        | Some b ->
                                                                        let a = 1 in ADD(a ,
                                                                        b)
                                                                        | Nili unit_proj#157 ->
                                                                        (fl)@(tuple_proj#144)
                                                                        | Nili unit_proj#159 ->
                                                                        (fl)@(ys)
    const t12 =
      lambda (x) return  match x with
                          | Cons ctor_proj#161 ->
                             match ctor_proj#161 with
                              | ( hd , tuple_proj#162 ) ->
                               match tuple_proj#162 with
                                | Cons ctor_proj#167 ->
                                   match ctor_proj#167 with
                                    | ( hd2 , tuple_proj#168 ) ->
                                     match tuple_proj#168 with
                                      | Cons ctor_proj#171 ->
                                         match ctor_proj#171 with
                                          | ( hd3 , tuple_proj#172 ) ->
                                           match tuple_proj#172 with
                                            | Cons ctor_proj#175 ->
                                              NEG(1)
                                            | Nil unit_proj#174 ->
                                              ADD(ADD(hd ,
                                              hd2) ,
                                              hd3)
                                      | Nil unit_proj#176 ->
                                        ADD(hd ,
                                        hd2)
                                | Nil unit_proj#177 ->
                                  hd
                          | Nil unit_proj#178 ->
                            0
    type recordi = record[a -> option (list (int)) , b -> list (int)]
    const none_a = record[a -> NONE() , b -> CONS(42 , LIST_EMPTY())]
    const some_a =
      record[a -> SOME(CONS(1 , CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))))) , b -> CONS(42 , LIST_EMPTY())]
    const a_empty_b_not =
      record[a -> SOME(LIST_EMPTY()) , b -> CONS(111 , LIST_EMPTY())]
    const b_empty_a_not =
      record[a -> SOME(CONS(222 , LIST_EMPTY())) , b -> LIST_EMPTY()]
    const t13 =
      lambda (x) return lambda (y) return let gen#179 = ( x , y ) in  match
                                                                       gen#179 with
                                                                       |
                                                                       ( tuple_proj#180 , tuple_proj#181 ) ->
                                                                        match
                                                                        tuple_proj#180 with
                                                                        |
                                                                        record[a -> record_proj#186 , b -> gen#7] ->
                                                                         match
                                                                        record_proj#186 with
                                                                        | Some ctor_proj#208 ->
                                                                         match
                                                                        tuple_proj#181 with
                                                                        | record[a -> record_proj#192 , b -> record_proj#193] ->
                                                                         match
                                                                        record_proj#192 with
                                                                        | None ctor_proj#205 ->
                                                                        INT(SIZE(ctor_proj#208))
                                                                        | Some ctor_proj#196 ->
                                                                         match
                                                                        ctor_proj#196 with
                                                                        | Cons ctor_proj#197 ->
                                                                         match
                                                                        ctor_proj#197 with
                                                                        | ( hd , _tl ) ->
                                                                         match
                                                                        record_proj#193 with
                                                                        | Cons ctor_proj#200 ->
                                                                        INT(SIZE(ctor_proj#208))
                                                                        | Nil unit_proj#199 ->
                                                                        hd
                                                                        | Nil unit_proj#201 ->
                                                                         match
                                                                        record_proj#193 with
                                                                        | Nil ctor_proj#204 ->
                                                                        INT(SIZE(ctor_proj#208))
                                                                        | Cons ctor_proj#202 ->
                                                                         match
                                                                        ctor_proj#202 with
                                                                        | ( hd , _tl ) ->
                                                                        hd
                                                                        | None unit_proj#206 ->
                                                                         match
                                                                        tuple_proj#181 with
                                                                        | record[a -> gen#9 , b -> gen#8] ->
                                                                        NEG(1) |}]
