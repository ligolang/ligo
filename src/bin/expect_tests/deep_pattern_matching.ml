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
        | #generated1 -> 3 |}]


let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (good_test "pm_test.religo") ] ;
  [%expect{|
    type myt = sum[Cons -> ( int * int ) , Nil -> unit]
    type myr = record[a -> int , b -> nat , c -> string]
    type myd = sum[One -> sum[Cons -> ( int * int ) , Nil -> unit] , Two -> record[a -> int , b -> nat , c -> string]]
    const t1 = lambda (x) return let fr = lambda (_x) return 1 in let fl = lambda (_x) return 2 in let #generated37 = x in
     match #generated37 with
      | ( #tuple_proj38 , ys ) ->
       match #tuple_proj38 with
        | Cons #ctor_proj51 ->
           match ys with
            | Cons #ctor_proj49 ->
               match #ctor_proj51 with
                | ( a , b ) ->
                 match #ctor_proj49 with
                  | ( c , d ) ->
                  ADD(ADD(ADD(a , b) , c) , d)
            | Nil #unit_proj48 ->
              (fl)@(#tuple_proj38)
        | Nil #unit_proj50 ->
          (fr)@(ys)
    const t2 = lambda (x) return lambda (y) return let #generated52 = x in
     match #generated52 with
      | Cons #ctor_proj53 ->
         match #ctor_proj53 with
          | ( a , b ) ->
          let old_b = b in let b = let #generated55 = y in  match #generated55 with
                                                             | Cons #ctor_proj57 ->
                                                               ADD(a ,
                                                               b)
                                                             | Nil #unit_proj56 ->
                                                               let f = lambda (b) return ADD(a ,
                                                               b) in (f)@(ADD(b ,
                                                               1)) in ADD(ADD(a ,
          old_b) , b)
      | Nil #unit_proj58 ->
        let #generated59 = y in  match #generated59 with
                                  | Cons #ctor_proj60 ->
                                     match #ctor_proj60 with
                                      | ( _a , b ) ->
                                      let a = "a" in ADD(INT(SIZE(a)) , b)
                                  | Nil #unit_proj62 ->
                                    1
    const t3 = lambda (x) return let #generated63 = x in  match #generated63 with
                                                           | One #ctor_proj64 ->
                                                              match #ctor_proj64 with
                                                               | Cons #ctor_proj70 ->
                                                                 let #generated65 = #ctor_proj64 in
                                                                  match #generated65 with
                                                                   | Cons #ctor_proj66 ->
                                                                      match
                                                                       #ctor_proj66 with
                                                                       |
                                                                       ( a , b ) ->
                                                                       ADD(a , b)
                                                                   | Nil #unit_proj68 ->
                                                                     2
                                                               | Nil #unit_proj69 ->
                                                                 1
                                                           | Two #ctor_proj71 ->
                                                              match #ctor_proj71 with
                                                               | record[a -> a , b -> b , c -> c] ->
                                                               ADD(ADD(a ,
                                                               INT(b)) ,
                                                               INT(SIZE(c)))
    const t2_3 = lambda (x) return lambda (y) return lambda (x2) return let t2 = let #generated73 = x in
     match #generated73 with
      | Cons #ctor_proj74 ->
         match #ctor_proj74 with
          | ( a , b ) ->
          let old_b = b in let b = let #generated76 = y in  match #generated76 with
                                                             | Cons #ctor_proj77 ->
                                                                match #ctor_proj77 with
                                                                 | ( a , b ) ->
                                                                 ADD(a , b)
                                                             | Nil #unit_proj79 ->
                                                               let f = lambda (b) return ADD(a ,
                                                               b) in (f)@(ADD(b ,
                                                               1)) in ADD(ADD(a ,
          old_b) , b)
      | Nil #unit_proj80 ->
        let #generated81 = y in  match #generated81 with
                                  | Cons #ctor_proj82 ->
                                     match #ctor_proj82 with
                                      | ( _a , b ) ->
                                      let a = "a" in ADD(INT(SIZE(a)) , b)
                                  | Nil #unit_proj84 ->
                                    1 in let t3 = let #generated85 = x2 in
     match #generated85 with
      | One #ctor_proj86 ->
         match #ctor_proj86 with
          | Cons #ctor_proj92 ->
            let #generated87 = #ctor_proj86 in  match #generated87 with
                                                 | Cons #ctor_proj88 ->
                                                    match #ctor_proj88 with
                                                     | ( a , b ) ->
                                                     ADD(a , b)
                                                 | Nil #unit_proj90 ->
                                                   2
          | Nil #unit_proj91 ->
            1
      | Two #ctor_proj93 ->
         match #ctor_proj93 with
          | record[a -> a , b -> b , c -> c] ->
          ADD(ADD(a , b) , INT(SIZE(c))) in ADD(t2 ,
    t3)
    const t4 = lambda (x) return lambda (y) return let #generated95 = ( x , y ) in
     match #generated95 with
      | ( a , #tuple_proj96 ) ->
       match #tuple_proj96 with
        | Two #ctor_proj110 ->
           match a with
            | One #ctor_proj101 ->
               match #ctor_proj101 with
                | Cons #ctor_proj102 ->
                   match #ctor_proj102 with
                    | ( a , b ) ->
                    ADD(a , b)
                | Nil #unit_proj104 ->
                  2
            | Two #ctor_proj105 ->
               match #ctor_proj105 with
                | record[a -> a , b -> b , c -> c] ->
                 match #ctor_proj110 with
                  | record[a -> aa , b -> #generated2 , c -> cc] ->
                  ADD(ADD(ADD(ADD(a , INT(b)) , INT(SIZE(c))) , aa) ,
                  INT(SIZE(cc)))
        | One _x ->
          1
    const t5 = lambda (x) return let #generated111 = ( x , unit ) in  match
                                                                       #generated111 with
                                                                       |
                                                                       ( a , #tuple_proj112 ) ->
                                                                       a
    const t6 = lambda (x) return let #generated114 = ( x , unit ) in  match
                                                                       #generated114 with
                                                                       |
                                                                       ( #generated3 , #generated4 ) ->
                                                                       2
    const t7 = lambda (x) return let #generated116 = x in  match #generated116 with
                                                            | Some x ->
                                                              x
                                                            | None #unit_proj117 ->
                                                              1
    const t8 = lambda (x) return lambda (y) return let #generated118 = ( x , y ) in
     match #generated118 with
      | ( #tuple_proj119 , x ) ->
       match #tuple_proj119 with
        | Some #ctor_proj122 ->
           match #ctor_proj122 with
            | ( x , y ) ->
            ADD(x , y)
        | None #unit_proj124 ->
          x
    const t9 = lambda (x) return lambda (y) return let #generated125 = ( x , y ) in
     match #generated125 with
      | ( #tuple_proj126 , ys ) ->
       match #tuple_proj126 with
        | Some #ctor_proj135 ->
           match ys with
            | Some #ctor_proj133 ->
              ADD(#ctor_proj135 ,
              #ctor_proj133)
            | None #unit_proj132 ->
              2
        | None #unit_proj134 ->
          1
    type optioni = option (int)
    type myti = sum[Consi -> option (int) , Nili -> unit]
    const fl = lambda (_x) return 1
    const fo = lambda (_x) return 2
    const t10 = lambda (x) return lambda (y) return let #generated136 = ( x , y ) in
     match #generated136 with
      | ( #tuple_proj137 , ys ) ->
       match #tuple_proj137 with
        | Consi #ctor_proj153 ->
           match ys with
            | Consi #ctor_proj151 ->
               match #ctor_proj153 with
                | Some #ctor_proj148 ->
                  ADD((fo)@(#ctor_proj153) ,
                  (fo)@(#ctor_proj151))
                | None #unit_proj144 ->
                   match ys with
                    | Nili #ctor_proj147 ->
                      ADD((fo)@(#ctor_proj153) ,
                      (fo)@(#ctor_proj151))
                    | Consi #ctor_proj145 ->
                       match #ctor_proj145 with
                        | None #ctor_proj146 ->
                          ADD((fo)@(#ctor_proj153) ,
                          (fo)@(#ctor_proj151))
                        | Some _b ->
                          let b = 1 in b
            | Nili #unit_proj150 ->
              (fl)@(#tuple_proj137)
        | Nili #unit_proj152 ->
          (fl)@(ys)
    const t11 = lambda (x) return lambda (y) return let #generated154 = ( x , y ) in
     match #generated154 with
      | ( #tuple_proj155 , ys ) ->
       match #tuple_proj155 with
        | Consi #ctor_proj172 ->
           match ys with
            | Consi #ctor_proj170 ->
               match #ctor_proj172 with
                | None #ctor_proj167 ->
                  let #generated161 = #ctor_proj172 in  match #generated161 with
                                                         | Some a ->
                                                           a
                                                         | None #unit_proj162 ->
                                                           ADD((fo)@(#ctor_proj172) ,
                                                           (fo)@(#ctor_proj170))
                | Some _a ->
                   match ys with
                    | Nili #ctor_proj166 ->
                      let #generated161 = #ctor_proj172 in  match #generated161 with
                                                             | Some a ->
                                                               a
                                                             | None #unit_proj162 ->
                                                               ADD((fo)@(#ctor_proj172) ,
                                                               (fo)@(#ctor_proj170))
                    | Consi #ctor_proj164 ->
                       match #ctor_proj164 with
                        | None #ctor_proj165 ->
                          let #generated161 = #ctor_proj172 in  match #generated161 with
                                                                 | Some a ->
                                                                   a
                                                                 | None #unit_proj162 ->
                                                                   ADD((fo)@(#ctor_proj172) ,
                                                                   (fo)@(#ctor_proj170))
                        | Some b ->
                          let a = 1 in ADD(a ,
                          b)
            | Nili #unit_proj169 ->
              (fl)@(#tuple_proj155)
        | Nili #unit_proj171 ->
          (fl)@(ys)
    const t12 = lambda (x) return let #generated173 = x in  match #generated173 with
                                                             | Cons #ctor_proj174 ->
                                                                match #ctor_proj174 with
                                                                 | ( hd , #tuple_proj175 ) ->
                                                                  match #tuple_proj175 with
                                                                   | Cons #ctor_proj180 ->
                                                                      match
                                                                       #ctor_proj180 with
                                                                       |
                                                                       ( hd2 , #tuple_proj181 ) ->
                                                                        match
                                                                        #tuple_proj181 with
                                                                        | Cons #ctor_proj184 ->
                                                                         match
                                                                        #ctor_proj184 with
                                                                        | ( hd3 , #tuple_proj185 ) ->
                                                                         match
                                                                        #tuple_proj185 with
                                                                        | Cons #ctor_proj188 ->
                                                                        NEG(1)
                                                                        | Nil #unit_proj187 ->
                                                                        ADD(ADD(hd ,
                                                                        hd2) ,
                                                                        hd3)
                                                                        | Nil #unit_proj189 ->
                                                                        ADD(hd ,
                                                                        hd2)
                                                                   | Nil #unit_proj190 ->
                                                                     hd
                                                             | Nil #unit_proj191 ->
                                                               0
    type recordi = record[a -> option (list (int)) , b -> list (int)]
    const none_a = record[a -> NONE() , b -> CONS(42 , LIST_EMPTY())]
    const some_a = record[a -> SOME(CONS(1 , CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))))) , b -> CONS(42 , LIST_EMPTY())]
    const a_empty_b_not = record[a -> SOME(LIST_EMPTY()) , b -> CONS(111 , LIST_EMPTY())]
    const b_empty_a_not = record[a -> SOME(CONS(222 , LIST_EMPTY())) , b -> LIST_EMPTY()]
    const t13 = lambda (x) return lambda (y) return let #generated192 = ( x , y ) in
     match #generated192 with
      | ( #tuple_proj193 , #tuple_proj194 ) ->
       match #tuple_proj193 with
        | record[a -> #record_proj199 , b -> #generated6] ->
         match #record_proj199 with
          | Some #ctor_proj221 ->
             match #tuple_proj194 with
              | record[a -> #record_proj205 , b -> #record_proj206] ->
               match #record_proj205 with
                | None #ctor_proj218 ->
                  INT(SIZE(#ctor_proj221))
                | Some #ctor_proj209 ->
                   match #ctor_proj209 with
                    | Cons #ctor_proj210 ->
                       match #ctor_proj210 with
                        | ( hd , _tl ) ->
                         match #record_proj206 with
                          | Cons #ctor_proj213 ->
                            INT(SIZE(#ctor_proj221))
                          | Nil #unit_proj212 ->
                            hd
                    | Nil #unit_proj214 ->
                       match #record_proj206 with
                        | Nil #ctor_proj217 ->
                          INT(SIZE(#ctor_proj221))
                        | Cons #ctor_proj215 ->
                           match #ctor_proj215 with
                            | ( hd , _tl ) ->
                            hd
          | None #unit_proj219 ->
             match #tuple_proj194 with
              | record[a -> #generated8 , b -> #generated7] ->
              NEG(1) |}]
