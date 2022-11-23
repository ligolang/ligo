open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_function_annotation_1.mligo"];
  [%expect {|
    File "../../test/contracts/negative/error_function_annotation_1.mligo", line 1, characters 26-27:
      1 | let main (a:int) : unit = a

    Invalid type(s)
    Cannot unify "int" with "unit". |}];

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_function_annotation_2.mligo"; "--entry-point"; "f"];
  [%expect {|
    File "../../test/contracts/negative/error_function_annotation_2.mligo", line 1, characters 14-43:
      1 | let f : int = fun (x, y : int*int) -> x + y
      2 | let g (x, y : int * int) : int = f (x, y)

    Invalid type(s)
    Cannot unify "( int * int ) -> int" with "int". |}];

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_function_annotation_3.mligo"; "--entry-point"; "f"];
  [%expect {|
    File "../../test/contracts/negative/error_function_annotation_3.mligo", line 8, characters 14-20:
      7 |   match s with
      8 |   | Add si -> Add si
      9 |   | Sub si -> Sub si

    Invalid type(s)
    Cannot unify "op" with "( list (operation) * op )". |}];

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_no_tail_recursive_function.mligo"; "--entry-point"; "unvalid"];
  [%expect {|
    File "../../test/contracts/negative/error_no_tail_recursive_function.mligo", line 2, characters 14-21:
      1 | let rec unvalid (n:int):int =
      2 |     let res = unvalid (n) in
      3 |     res + 1

    Recursive call not in tail position.
    The value of a recursive call must be immediately returned by the defined function. |}];

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_type.ligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_type.ligo", line 3, characters 18-28:
      2 |
      3 | const foo : nat = 42 + "bar"

    Invalid type(s)
    Cannot unify "int" with "string". |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_type_record_access.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_type_record_access.mligo", line 6, characters 17-20:
      5 | let bar (x : foo) : int =
      6 |   let y : bool = x.i in
      7 |   42

    Invalid type(s)
    Cannot unify "int" with "bool". |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_type_record_update.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_type_record_update.mligo", line 7, characters 23-26:
      6 | let bar (x : foo) : foo =
      7 |   let x = { x with i = x.j } in
      8 |   x

    Invalid type(s)
    Cannot unify "bool" with "int". |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_typer_1.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_1.mligo", line 3, characters 19-27:
      2 |
      3 | let foo : string = 42 + 127
      4 |

    Invalid type(s)
    Cannot unify "int" with "string". |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_typer_2.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_2.mligo", line 3, characters 24-39:
      2 |
      3 | let foo : string list = Some (42 + 127)
      4 |

    Invalid type(s)
    Cannot unify "toto" with "list (string)". |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_typer_3.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_3.mligo", line 3, characters 34-53:
      2 |
      3 | let foo : (int * string * bool) = ((1, "foo") : toto)
      4 |

    Invalid type(s)
    Cannot unify "toto" with "( int * string * bool )". |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_typer_4.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_4.mligo", line 4, characters 17-56:
      3 |
      4 | let foo : tata = ({a = 1 ; b = "foo" ; c = true} : toto)
      5 |

    Invalid type(s)
    Cannot unify "toto" with "tata". |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_typer_5.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_5.mligo", line 1, characters 10-17:
      1 | let foo : boolean = 3
      2 |

    Type "boolean" not found. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_typer_6.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_6.mligo", line 1, characters 30-64:
      1 | let foo : (int, string) map = (Map.literal [] : (int, bool) map)
      2 | let main (p:int) (storage : int) =

    Invalid type(s)
    Cannot unify "bool" with "string". |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_typer_7.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_7.mligo", line 4, characters 18-48:
      3 |
      4 | let foo : tata = ({a = 1 ; b = "foo" ; c = true} : toto)
      5 |

    Mismatching record labels. Expected record of type "toto". |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/error_typer_1.jsligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_1.jsligo", line 9, character 0 to line 12, character 1:
      8 |
      9 | let main = ([param, oldStorage] : [action, storage]) : [list<operation>, storage] => {
     10 |     let newStorage : storage = addone (oldStorage, 1 as nat);
     11 |     return [list([]) as list<operation>, newStorage];
     12 | }

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/negative/error_typer_1.jsligo", line 5, character 0 to line 7, character 1:
      4 |
      5 | let addone = (oldStorage: nat) : nat => {
      6 |    return oldStorage + (1 as nat);
      7 | }
      8 |

    Toplevel let declaration are silently change to const declaration.

    File "../../test/contracts/negative/error_typer_1.jsligo", line 10, characters 38-60:
      9 | let main = ([param, oldStorage] : [action, storage]) : [list<operation>, storage] => {
     10 |     let newStorage : storage = addone (oldStorage, 1 as nat);
     11 |     return [list([]) as list<operation>, newStorage];

    Invalid type(s)
    Cannot unify "( nat * nat )" with "nat". |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/id.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/id.mligo", line 45, characters 26-40:
     44 |   let updated_identities: (id, id_details) big_map =
     45 |     Big_map.update new_id new_id_details identities
     46 |   in

    Invalid type(s)
    Cannot unify "id_details" with "option (^a)".
    Hint: "^a" represent placeholder type(s). |}]

(*
  This test is here to ensure compatibility with comparable pairs introduced in carthage
  note that only "comb pairs" are allowed to be compared (would be better if any pair would be comparable ?)
  EDIT: With EDO, all kind of pairs are comparable
*)
let%expect_test _ =
  run_ligo_good [ "run"; "interpret" ; "Set.literal [ (1,(2,3)) ; (2,(3,4)) ]" ; "--syntax"; "cameligo" ] ;
  [%expect {|
    SET_ADD(( 2 , ( 3 , 4 ) ) , SET_ADD(( 1 , ( 2 , 3 ) ) , SET_EMPTY())) |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/invalid_field_record_update.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/invalid_field_record_update.mligo", line 4, characters 29-36:
      3 | let main (p:int) (storage : abc) =
      4 |   (([] : operation list) , { storage with nofield=2048} )

    Invalid record field "nofield" in record. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/override_option.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/override_option.mligo", line 3, characters 53-57:
      2 |
      3 | let main (x,y:bool * bool) = ([] : operation list), (None : option)

    Constructor "None" not found. |} ]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ;  "../../test/contracts/negative/will_be_ignored.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/will_be_ignored.mligo", line 7, characters 47-62:
      6 |      let receiver : contract =
      7 |       match (Tezos.get_contract_opt(s.owner) : contract option) with
      8 |         Some (contract) -> contract

    Invalid type
    Ill formed type "contract".Hint: you might be missing some type arguments. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/double_for_each.ligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/double_for_each.ligo", line 19, characters 23-28:
     18 |       (* param was accidentally still in the typing context after this point *)
     19 |       s.some_map[0] := param;
     20 |     };

    Variable "param" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/wrong_return1.ligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/wrong_return1.ligo", line 3, character 71 to line 5, character 8:
      2 |
      3 | function updateAdmin(const _new_admin: address; var s: int): return is {
      4 |     const _ = 1;
      5 | } with s

    Invalid type(s)
    Cannot unify "int" with "return". |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/wrong_return2.ligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/wrong_return2.ligo", line 3, characters 71-72:
      2 |
      3 | function updateAdmin(const _new_admin: address; var s: int): return is s

    Invalid type(s)
    Cannot unify "int" with "return". |}]


(* Compiles due to inference ;) *)
(* let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_contract_type_inference.mligo" ] ;
  [%expect {|
      File "../../test/contracts/negative/error_contract_type_inference.mligo", line 8, characters 12-69:
        7 |     Some contract -> contract
        8 |   | None -> (failwith "The entrypoint does not exist" : int contract)
        9 |
  
      Invalid type(s).
      Expected: "contract ('a)", but got: "contract (int)". |}] *)

(* Note : Disabling color in below tests (through the [--no-colour] option) prevents
   the introduction of ANSI escape sequences in the expected output *) 


(* In this case, the types are not record types,
   no diff should be displayed *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ; "../../test/contracts/negative/typer_unify_error_diff/int_vs_nat.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/typer_unify_error_diff/int_vs_nat.mligo", line 4, characters 16-17:
      3 |   let x : int = 42 in
      4 |   let y : nat = x in
      5 |   ([] : operation list), s

    Invalid type(s)
    Cannot unify "int" with "nat". |}]

(* In this case, one of the types is a tuple but not the other
   no diff should be displayed *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ; "../../test/contracts/negative/typer_unify_error_diff/int_vs_tuple.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/typer_unify_error_diff/int_vs_tuple.mligo", line 4, characters 31-32:
      3 |   let x : int                = 42 in
      4 |   let y : nat * int * string = x in
      5 |   ([] : operation list), s

    Invalid type(s)
    Cannot unify "int" with "( nat * int * string )". |}]

(*
  Here, the two tuples have no types in common and different sizes.
  The diff should display deletion of all elements of first tuple
  and insertion of all elements of the second.

  TODO NP :
  Instead of display - + - +... :
    - string
    + tez
    - int
    + nat
    - int
    + tez
    - string
  we want to display instead :
    - string
    - int
    - int
    + tez
    + nat
    + tez
    - string
  i.e., consecutive changes
    CHANGE A1 TO B1; CHANGE A2 TO B2
  shouldn't appear as 
    DELETE A1; INSERT B1; DELETE A2; INSERT B2
  but instead :
    DELETE A1; DELETE A2; INSERT B1; INSERT B2

*)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ; "../../test/contracts/negative/typer_unify_error_diff/tuple_vs_tuple_1.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/typer_unify_error_diff/tuple_vs_tuple_1.mligo", line 4, characters 40-41:
      3 |   let y : string * int * int * string = "foo", 42, 24, "bar" in
      4 |   let x : tez    * nat * tez          = y in
      5 |   ([] : operation list), s

    Invalid type(s)
    Cannot unify "( string * int * int * string )" with "( tez * nat * tez )".
    Difference between the types:
    - string
    + tez
    - int
    + nat
    - int
    + tez
    - string |}]

(*
  Here, the two tuples have some changes (1 change, 1 addition, 1 deletion)
  but they have the *same size*, so the typer will only display an error
  on the first difference only (here, [string] vs. [tez])
*)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ; "../../test/contracts/negative/typer_unify_error_diff/tuple_vs_tuple_2.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/typer_unify_error_diff/tuple_vs_tuple_2.mligo", line 4, characters 53-54:
      3 |   let  x : string * int * nat * int *       string = "foo" , 42  , 24n , 42 ,        "bar" in
      4 |   let _y : tez    * int       * tez * nat * string = x in
      5 |   //       ^^^^^^         ^^^         ^^^

    Invalid type(s)
    Cannot unify "string" with "tez". |}]

(*
  Here, the two tuples have 4 changes and different sizes.
  The diff should display these changes.
*)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ; "../../test/contracts/negative/typer_unify_error_diff/tuple_vs_tuple_3.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/typer_unify_error_diff/tuple_vs_tuple_3.mligo", line 4, characters 59-60:
      3 |   let  x : string * int * nat * tez *       string * int =  "foo" , 42  , 24n , 42tez ,        "bar",  42 in
      4 |   let _y : tez    * int       * tez * nat * string       = x in
      5 |   //       ^^^^^^         ^^^         ^^^            ^^^

    Invalid type(s)
    Cannot unify "( string * int * nat * tez * string * int )" with "( tez * int * tez * nat * string )".
    Difference between the types:
    - string
    + tez
      int
    - nat
      tez
    + nat
      string
    - int |}]

(* Yet another example, with longer tuples this time *)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ; "../../test/contracts/negative/typer_unify_error_diff/tuple_vs_tuple_4.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/typer_unify_error_diff/tuple_vs_tuple_4.mligo", line 4, characters 72-73:
      3 |   let x  : int *                nat * int * nat     * int *       nat = 42 , 4n , 42 , 24n , 42 , 24n in
      4 |   let _y : int * tez * string * nat * int * address * int * tez * nat = x in
      5 | //               ^^^   ^^^^^^               ^^^^^^^         ^^^

    Invalid type(s)
    Cannot unify "( int * nat * int * nat * int * nat )" with "( int * tez * string * nat * int * address * int * tez * nat )".
    Difference between the types:
      int
    + tez
    + string
      nat
      int
    - nat
    + address
      int
    + tez
      nat |}]

(*
  Here we have a tuple nested inside another
  The diff should suggest a [REPLACE subtuple_a BY subtuple_b]
  
  For example :
    int * string * (nat * tez * nat) *          tez
  vs.
    int *          (nat * tez * int) * string * tez * address
          ^^^^^^                ^^^
  Here, we suppose the probable desired diff is :
    DELETE string
    CHANGE (nat * tez * nat) TO (nat * tez * int) (TODO NP : Ideally have diff of subtuples somehow)
    ADD    string
    keep   tez
    ADD    address
  But if all changes were considered equal, we would have :
    CHANGE string            TO (nat * tez * int)
    CHANGE (nat * tez * nat) TO string
    keep   tez
    ADD    address
  
  But weights are computed accordingly to the size of the types involved,
  so the first diff should be chosen over the second.
  In the first diff,
    weight DELETE string = 1
    weight CHANGE (nat * tez * nat) TO (nat * tez * int) = 0 + 0 + 1 = 1
    weight ADD string = 1
    total weight = 1 + 1 + 1 = 3
  In the second diff, however
    weight CHANGE string            TO (nat * tez * int) = 3
    weight CHANGE (nat * tez * nat) TO string = 3
    total weight = 3 + 3 = 6

  Because both subtuples are similar, the weight to change subtuple_a
  into subtuple_b is low (it's 0 + 0 + 1 = 1), so this diff is prefered.
*)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ; "../../test/contracts/negative/typer_unify_error_diff/subtuples_1.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/typer_unify_error_diff/subtuples_1.mligo", line 4, characters 71-72:
      3 |   let  x : int * string * (nat * tez * nat) *          tez           = 1, "a", (1n, 1tez, 1n), 1tez in
      4 |   let _y : int *          (nat * tez * int) * string * tez * address = x in
      5 |   //             ^^^^^^                ^^^    ^^^^^^         ^^^^^^^

    Invalid type(s)
    Cannot unify "( int * string * ( nat * tez * nat ) * tez )" with "( int * ( nat * tez * int ) * string * tez * address )".
    Difference between the types:
      int
    - string
    - ( nat * tez * nat )
    + ( nat * tez * int )
    + string
      tez
    + address |}]

(*
  In this case, the tuple is itself composed of several
  long sub-tuples.

  Since [s] and [s_close] are similar types,
  the weight to change one into another
  should be less than to change [s] to [s1] or [s2] or [s3] etc.
  So the diff should "match" [s] and [s_close] together,
  in a [REPLACE s BY s_close]

  TODO : Ideally we would like to get a more precise
         diff of the subtuples themselves.
*)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ; "../../test/contracts/negative/typer_unify_error_diff/subtuples_2.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/typer_unify_error_diff/subtuples_2.mligo", line 9, characters 48-49:
      8 |   let  x : int *           s       *      nat = 42, (1n, 1tez, 1tez, 1n), 1n in
      9 |   let _y : int * s1 * s2 * s_close * s2 * nat = x in
     10 |   ([] : operation list), s

    Invalid type(s)
    Cannot unify "( int * s * nat )" with "( int * s1 * s2 * s_close * s2 * nat )".
    Difference between the types:
      int
    - s
    + s1
    + s2
    + s_close
    + s2
      nat |}]

(*
  When two mismatching tuples are within lists,
  here [tuple_a list] vs [tuple_b list]
  the error should target the tuples themselves :
    cannot unify [tuple_a] with [tuple_b]
  and not the whole list types :
    cannot unify [tuple_a list] with [tuple_b list]
*)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ; "../../test/contracts/negative/typer_unify_error_diff/tuple_lists.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/typer_unify_error_diff/tuple_lists.mligo", line 4, characters 65-66:
      3 |   let x : (string * int *       nat * int * string * int) list = [ "foo" , 42  , 24n , 42 ,        "bar",  42 ] in
      4 |   let y : (tez    * int * tez * nat *       string)       list = x in
      5 |   //       ^^^^^^         ^^^         ^^^            ^^^

    Invalid type(s)
    Cannot unify "( string * int * nat * int * string * int )" with "( tez * int * tez * nat * string )".
    Difference between the types:
    - string
    + tez
      int
    + tez
      nat
    - int
      string
    - int |}]

(*
  In this case,
  the two records have the same field labels,
  but with a type mismatch in one of their fields,
  the typer will pinpoint the precise type mismatch :
  here, [string] vs. [nat]

  TODO : We should add location to show where is the
  [string] and [nat] in the source code,
  otherwise it can be difficult to see where is
  the mismatch when the types are long.
*)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ; "../../test/contracts/negative/typer_unify_error_diff/record_vs_record.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/typer_unify_error_diff/record_vs_record.mligo", line 4, characters 47-48:
      3 |   let y : {foo : int ; bar : (nat * string)} = {foo = 1 ; bar = (2n, "lol") } in
      4 |   let x : {foo : int ; bar : (nat * nat   )} = y in
      5 |   //                                ^^^^^^

    Invalid type(s)
    Cannot unify "string" with "nat". |}]

(*
  In this case, the two records DON'T have the same field labels.

  TODO : Add a diff for records, just like tuples,
  to clarify where is the mismatch.
*)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ; "../../test/contracts/negative/typer_unify_error_diff/record_vs_record_2.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/typer_unify_error_diff/record_vs_record_2.mligo", line 4, characters 67-68:
      3 |   let y : {foo : int ; bar : (nat * string) ; third_field : tez} = {foo = 1 ; bar = (2n, "lol") ; third_field = 42tez } in
      4 |   let x : {foo : int ; bar : (nat * nat   )}                     = y in
      5 |   //                                ^^^^^^    ^^^^^^^^^^^^^^^^^

    Invalid type(s)
    Cannot unify "record[bar -> ( nat * string ) , foo -> int , third_field -> tez]" with "
    record[bar -> ( nat * nat ) , foo -> int]". |}]

(*
  In this case, the typer will stop at the first mismatch
  between arrow components.
  In below example, it will fail at [nat] vs. [int].

  TODO : How can we make the error message more precise
  and pinpoint a clear diff between both arrow types ?
*)
let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "--no-colour" ; "../../test/contracts/negative/typer_unify_error_diff/arrow_vs_arrow.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/typer_unify_error_diff/arrow_vs_arrow.mligo", line 4, characters 45-46:
      3 |   let  x : int -> nat -> nat -> tez        = (fun _x _y _z -> 1tez) in
      4 |   let _y : int -> int -> int -> int -> nat = x in
      5 |   //              ^^^    ^^^    ^^^    ^^^

    Invalid type(s)
    Cannot unify "int" with "nat". |}]


