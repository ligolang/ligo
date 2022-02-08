open Cli_expect

let bad_test s = (bad_test "")^"/deep_pattern_matching/"^s
let good_test s = (test "")^"/deep_pattern_matching/"^s

(* Negatives *)

(* testing that subtitution is stoping on resursive definitions *)
let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "pm_fail17.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail17.mligo", line 15, characters 39-43:
     14 |     (* testing that subtitution is stoping on resursive definitions *)
     15 |     let rec a (b : int) : int =let x = fo a in b + 1 in
     16 |     (a 1) + (fo b)

    Invalid type(s).
    Expected: "optioni", but got: "int -> int". |}]

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
    File "../../test/contracts/negative//deep_pattern_matching/pm_fail5.mligo", line 5, characters 4-15:
      4 |   match x with
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
        | _#7 -> 3 |}]


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
    | ( tuple_proj#188 , ys ) ->
     match tuple_proj#188 with
      | Cons ctor_proj#201 ->
         match ys with
          | Cons ctor_proj#199 ->
             match ctor_proj#201 with
              | ( a , b ) ->
               match ctor_proj#199 with
                | ( c , d ) ->
                ADD(ADD(ADD(a , b) , c) , d)
          | Nil unit_proj#198 ->
            (fl)@(tuple_proj#188)
      | Nil unit_proj#200 ->
        (fr)@(ys)
const t2 =
  lambda (x) return lambda (y) return  match x with
                                        | Cons ctor_proj#202 ->
                                           match ctor_proj#202 with
                                            | ( a , b ) ->
                                            let old_b = b in let b =
                                             match y with
                                              | Cons ctor_proj#205 ->
                                                ADD(a ,
                                                b)
                                              | Nil unit_proj#204 ->
                                                let f = lambda (b) return ADD(a ,
                                                b) in (f)@(ADD(b ,
                                                1)) in ADD(ADD(a ,
                                            old_b) , b)
                                        | Nil unit_proj#206 ->
                                           match y with
                                            | Cons ctor_proj#207 ->
                                               match ctor_proj#207 with
                                                | ( _a , b ) ->
                                                let a = "a" in ADD((int@{nat})@((String.length)@(a)) ,
                                                b)
                                            | Nil unit_proj#209 ->
                                              1
const t3 =
  lambda (x) return  match x with
                      | One ctor_proj#210 ->
                         match ctor_proj#210 with
                          | Cons ctor_proj#215 ->
                             match ctor_proj#210 with
                              | Cons ctor_proj#211 ->
                                 match ctor_proj#211 with
                                  | ( a , b ) ->
                                  ADD(a , b)
                              | Nil unit_proj#213 ->
                                2
                          | Nil unit_proj#214 ->
                            1
                      | Two ctor_proj#216 ->
                         match ctor_proj#216 with
                          | record[a -> a , b -> b , c -> c] ->
                          ADD(ADD(a , (int@{nat})@(b)) ,
                          (int@{nat})@((String.length)@(c)))
const t2_3 =
  lambda (x) return lambda (y) return lambda (x2) return let t2 =  match
                                                                    x with
                                                                    | Cons ctor_proj#218 ->
                                                                     match
                                                                    ctor_proj#218 with
                                                                    | ( a , b ) ->
                                                                    let old_b = b in let b =
                                                                     match
                                                                    y with
                                                                    | Cons ctor_proj#220 ->
                                                                     match
                                                                    ctor_proj#220 with
                                                                    | ( a , b ) ->
                                                                    ADD(a ,
                                                                    b)
                                                                    | Nil unit_proj#222 ->
                                                                    let f = lambda (b) return ADD(a ,
                                                                    b) in (f)@(ADD(b ,
                                                                    1)) in ADD(ADD(a ,
                                                                    old_b) ,
                                                                    b)
                                                                    | Nil unit_proj#223 ->
                                                                     match
                                                                    y with
                                                                    | Cons ctor_proj#224 ->
                                                                     match
                                                                    ctor_proj#224 with
                                                                    | ( _a , b ) ->
                                                                    let a = "a" in ADD((int@{nat})@((String.length)@(a)) ,
                                                                    b)
                                                                    | Nil unit_proj#226 ->
                                                                    1 in let t3 =
   match x2 with
    | One ctor_proj#227 ->
       match ctor_proj#227 with
        | Cons ctor_proj#232 ->
           match ctor_proj#227 with
            | Cons ctor_proj#228 ->
               match ctor_proj#228 with
                | ( a , b ) ->
                ADD(a , b)
            | Nil unit_proj#230 ->
              2
        | Nil unit_proj#231 ->
          1
    | Two ctor_proj#233 ->
       match ctor_proj#233 with
        | record[a -> a , b -> b , c -> c] ->
        ADD(ADD(a , b) , (int@{nat})@((String.length)@(c))) in ADD(t2 ,
  t3)
const t4 =
  lambda (x) return lambda (y) return let gen#235 = ( x , y ) in  match
                                                                   gen#235 with
                                                                   |
                                                                   ( a , tuple_proj#236 ) ->
                                                                    match
                                                                    tuple_proj#236 with
                                                                    | Two ctor_proj#250 ->
                                                                     match
                                                                    a with
                                                                    | One ctor_proj#241 ->
                                                                     match
                                                                    ctor_proj#241 with
                                                                    | Cons ctor_proj#242 ->
                                                                     match
                                                                    ctor_proj#242 with
                                                                    | ( a , b ) ->
                                                                    ADD(a ,
                                                                    b)
                                                                    | Nil unit_proj#244 ->
                                                                    2
                                                                    | Two ctor_proj#245 ->
                                                                     match
                                                                    ctor_proj#245 with
                                                                    | record[a -> a , b -> b , c -> c] ->
                                                                     match
                                                                    ctor_proj#250 with
                                                                    | record[a -> aa , b -> gen#106 , c -> cc] ->
                                                                    ADD(ADD(ADD(ADD(a ,
                                                                    (int@{nat})@(b)) ,
                                                                    (int@{nat})@((String.length)@(c))) ,
                                                                    aa) ,
                                                                    (int@{nat})@((String.length)@(cc)))
                                                                    | One _x ->
                                                                    1
const t5 =
  lambda (x) return let gen#251 = ( x , unit ) in  match gen#251 with
                                                    | ( a , tuple_proj#252 ) ->
                                                    a
const t6 =
  lambda (x) return let gen#254 = ( x , unit ) in  match gen#254 with
                                                    | ( gen#113 , gen#114 ) ->
                                                    2
const t7 =
  lambda (x) return  match x with
                      | Some x ->
                        x | None unit_proj#256 ->
                            1
const t8 =
  lambda (x) return lambda (y) return let gen#257 = ( x , y ) in  match
                                                                   gen#257 with
                                                                   |
                                                                   ( tuple_proj#258 , x ) ->
                                                                    match
                                                                    tuple_proj#258 with
                                                                    | Some ctor_proj#261 ->
                                                                     match
                                                                    ctor_proj#261 with
                                                                    | ( x , y ) ->
                                                                    ADD(x ,
                                                                    y)
                                                                    | None unit_proj#263 ->
                                                                    x
const t9 =
  lambda (x) return lambda (y) return let gen#264 = ( x , y ) in  match
                                                                   gen#264 with
                                                                   |
                                                                   ( tuple_proj#265 , ys ) ->
                                                                    match
                                                                    tuple_proj#265 with
                                                                    | Some ctor_proj#274 ->
                                                                     match
                                                                    ys with
                                                                    | Some ctor_proj#272 ->
                                                                    ADD(ctor_proj#274 ,
                                                                    ctor_proj#272)
                                                                    | None unit_proj#271 ->
                                                                    2
                                                                    | None unit_proj#273 ->
                                                                    1
type optioni = option (int)
type myti = sum[Consi -> option (int) , Nili -> unit]
const fl = lambda (_x) return 1
const fo = lambda (_x) return 2
const t10 =
  lambda (x) return lambda (y) return let gen#275 = ( x , y ) in  match
                                                                   gen#275 with
                                                                   |
                                                                   ( tuple_proj#276 , ys ) ->
                                                                    match
                                                                    tuple_proj#276 with
                                                                    | Consi ctor_proj#292 ->
                                                                     match
                                                                    ys with
                                                                    | Consi ctor_proj#290 ->
                                                                     match
                                                                    ctor_proj#292 with
                                                                    | Some ctor_proj#287 ->
                                                                    ADD((fo)@(ctor_proj#292) ,
                                                                    (fo)@(ctor_proj#290))
                                                                    | None unit_proj#283 ->
                                                                     match
                                                                    ys with
                                                                    | Nili ctor_proj#286 ->
                                                                    ADD((fo)@(ctor_proj#292) ,
                                                                    (fo)@(ctor_proj#290))
                                                                    | Consi ctor_proj#284 ->
                                                                     match
                                                                    ctor_proj#284 with
                                                                    | None ctor_proj#285 ->
                                                                    ADD((fo)@(ctor_proj#292) ,
                                                                    (fo)@(ctor_proj#290))
                                                                    | Some _b ->
                                                                    let b = 1 in b
                                                                    | Nili unit_proj#289 ->
                                                                    (fl)@(tuple_proj#276)
                                                                    | Nili unit_proj#291 ->
                                                                    (fl)@(ys)
const t11 =
  lambda (x) return lambda (y) return let gen#293 = ( x , y ) in  match
                                                                   gen#293 with
                                                                   |
                                                                   ( tuple_proj#294 , ys ) ->
                                                                    match
                                                                    tuple_proj#294 with
                                                                    | Consi ctor_proj#310 ->
                                                                     match
                                                                    ys with
                                                                    | Consi ctor_proj#308 ->
                                                                     match
                                                                    ctor_proj#310 with
                                                                    | None ctor_proj#305 ->
                                                                     match
                                                                    ctor_proj#310 with
                                                                    | Some a ->
                                                                    a
                                                                    | None unit_proj#300 ->
                                                                    ADD((fo)@(ctor_proj#310) ,
                                                                    (fo)@(ctor_proj#308))
                                                                    | Some _a ->
                                                                     match
                                                                    ys with
                                                                    | Nili ctor_proj#304 ->
                                                                     match
                                                                    ctor_proj#310 with
                                                                    | Some a ->
                                                                    a
                                                                    | None unit_proj#300 ->
                                                                    ADD((fo)@(ctor_proj#310) ,
                                                                    (fo)@(ctor_proj#308))
                                                                    | Consi ctor_proj#302 ->
                                                                     match
                                                                    ctor_proj#302 with
                                                                    | None ctor_proj#303 ->
                                                                     match
                                                                    ctor_proj#310 with
                                                                    | Some a ->
                                                                    a
                                                                    | None unit_proj#300 ->
                                                                    ADD((fo)@(ctor_proj#310) ,
                                                                    (fo)@(ctor_proj#308))
                                                                    | Some b ->
                                                                    let a = 1 in ADD(a ,
                                                                    b)
                                                                    | Nili unit_proj#307 ->
                                                                    (fl)@(tuple_proj#294)
                                                                    | Nili unit_proj#309 ->
                                                                    (fl)@(ys)
const t12 =
  lambda (x) return  match x with
                      | Cons ctor_proj#311 ->
                         match ctor_proj#311 with
                          | ( hd , tuple_proj#312 ) ->
                           match tuple_proj#312 with
                            | Cons ctor_proj#317 ->
                               match ctor_proj#317 with
                                | ( hd2 , tuple_proj#318 ) ->
                                 match tuple_proj#318 with
                                  | Cons ctor_proj#321 ->
                                     match ctor_proj#321 with
                                      | ( hd3 , tuple_proj#322 ) ->
                                       match tuple_proj#322 with
                                        | Cons ctor_proj#325 ->
                                          NEG(1)
                                        | Nil unit_proj#324 ->
                                          ADD(ADD(hd ,
                                          hd2) ,
                                          hd3)
                                  | Nil unit_proj#326 ->
                                    ADD(hd ,
                                    hd2)
                            | Nil unit_proj#327 ->
                              hd
                      | Nil unit_proj#328 ->
                        0
type recordi = record[a -> option (list (int)) , b -> list (int)]
const none_a = record[a -> None(unit) , b -> CONS(42 , LIST_EMPTY())]
const some_a =
  record[a -> Some(CONS(1 , CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))))) , b -> CONS(42 , LIST_EMPTY())]
const a_empty_b_not =
  record[a -> Some(LIST_EMPTY()) , b -> CONS(111 , LIST_EMPTY())]
const b_empty_a_not =
  record[a -> Some(CONS(222 , LIST_EMPTY())) , b -> LIST_EMPTY()]
const t13 =
  lambda (x) return lambda (y) return let gen#329 = ( x , y ) in  match
                                                                   gen#329 with
                                                                   |
                                                                   ( tuple_proj#330 , tuple_proj#331 ) ->
                                                                    match
                                                                    tuple_proj#330 with
                                                                    |
                                                                    record[a -> record_proj#336 , b -> gen#173] ->
                                                                     match
                                                                    record_proj#336 with
                                                                    | Some ctor_proj#358 ->
                                                                     match
                                                                    tuple_proj#331 with
                                                                    | record[a -> record_proj#342 , b -> record_proj#343] ->
                                                                     match
                                                                    record_proj#342 with
                                                                    | None ctor_proj#355 ->
                                                                    (int@{nat})@((List.length@{int})@(ctor_proj#358))
                                                                    | Some ctor_proj#346 ->
                                                                     match
                                                                    ctor_proj#346 with
                                                                    | Cons ctor_proj#347 ->
                                                                     match
                                                                    ctor_proj#347 with
                                                                    | ( hd , _tl ) ->
                                                                     match
                                                                    record_proj#343 with
                                                                    | Cons ctor_proj#350 ->
                                                                    (int@{nat})@((List.length@{int})@(ctor_proj#358))
                                                                    | Nil unit_proj#349 ->
                                                                    hd
                                                                    | Nil unit_proj#351 ->
                                                                     match
                                                                    record_proj#343 with
                                                                    | Nil ctor_proj#354 ->
                                                                    (int@{nat})@((List.length@{int})@(ctor_proj#358))
                                                                    | Cons ctor_proj#352 ->
                                                                     match
                                                                    ctor_proj#352 with
                                                                    | ( hd , _tl ) ->
                                                                    hd
                                                                    | None unit_proj#356 ->
                                                                     match
                                                                    tuple_proj#331 with
                                                                    | record[a -> gen#174 , b -> gen#175] ->
                                                                    NEG(1) |}]
