open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_annotation_1.mligo"; "main"];
  [%expect {|
    [1mFile "../../test/contracts/negative/error_function_annotation_1.mligo", line 1, characters 26-27:[0m
      1 | let main (a:int) : unit = [1m[31ma[0m

    [1m[31mError[0m: Invalid type(s).
    Expected: "unit", but got: "int". |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_annotation_2.mligo"; "f"];
  [%expect {|
    [1mFile "../../test/contracts/negative/error_function_annotation_2.mligo", line 1, characters 14-43:[0m
      1 | let f : int = [1m[31mfun (x, y : int*int) -> x + y[0m
      2 | let g (x, y : int * int) : int = f (x, y)

    [1m[31mError[0m: Invalid type(s).
    Expected: "int", but got: "( int * int ) -> int". |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_function_annotation_3.mligo"; "f"];
  [%expect {|
    Invalid type(s).
    Expected: "( list (operation) * sum[Add -> int , Sub -> int] )", but got: "
    sum[Add -> int , Sub -> int]". |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_no_tail_recursive_function.mligo"; "f"];
  [%expect {|
    [1mFile "../../test/contracts/negative/error_no_tail_recursive_function.mligo", line 2, characters 14-21:[0m
      1 | let rec unvalid (n:int):int =
      2 |     let res = [1m[31munvalid[0m (n) in
      3 |     res + 1

    [1m[31mError[0m: Recursive call not in tail position.
    The value of a recursive call must be immediately returned by the defined function. |}];

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_type.ligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/error_type.ligo", line 3, characters 18-28:[0m
      2 |
      3 | const foo : nat = [1m[31m42 + "bar"[0m

    [1m[31mError[0m: Invalid arguments.
    Expected an argument of type (nat, nat) or (int, int) or (mutez, mutez) or (nat, int) or (int, nat) or (timestamp, int) or (int, timestamp), but got an argument of type int, string. |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_1.mligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/error_typer_1.mligo", line 3, characters 19-27:[0m
      2 |
      3 | let foo : string = [1m[31m42 + 127[0m
      4 |

    [1m[31mError[0m: Invalid type(s).
    Expected: "string", but got: "int". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_2.mligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/error_typer_2.mligo", line 3, characters 24-39:[0m
      2 |
      3 | let foo : string list = [1m[31mSome (42 + 127)[0m
      4 |

    [1m[31mError[0m: Invalid type(s).
    Expected: "list (string)", but got: "option (int)". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_3.mligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/error_typer_3.mligo", line 3, characters 36-44:[0m
      2 |
      3 | let foo : (int * string * bool) = (([1m[31m1, "foo"[0m) : toto)
      4 |

    [1m[31mError[0m: Invalid type(s).
    Expected: "( int * string * sum[false -> unit , true -> unit] )", but got: "
    ( int * string )". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_4.mligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/error_typer_4.mligo", line 4, characters 18-48:[0m
      3 |
      4 | let foo : tata = ([1m[31m{a = 1 ; b = "foo" ; c = true}[0m : toto)
      5 |

    [1m[31mError[0m: Invalid type(s).
    Expected: "record[a -> int , c -> sum[false -> unit , true -> unit] , d -> string]", but got: "
    record[a -> int , b -> string , c -> sum[false -> unit , true -> unit]]". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_5.mligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/error_typer_5.mligo", line 1, characters 10-17:[0m
      1 | let foo : [1m[31mboolean[0m = 3
      2 |

    [1m[31mError[0m: Type "boolean" not found. |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_6.mligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/error_typer_6.mligo", line 1, characters 31-45:[0m
      1 | let foo : (int, string) map = ([1m[31mMap.literal [][0m : (int, bool) map)
      2 | let main (p:int) (storage : int) =

    [1m[31mError[0m: Invalid type(s).
    Expected: "Map (int , string)", but got: "Map (int ,
    sum[false -> unit , true -> unit])". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_typer_7.mligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/error_typer_7.mligo", line 4, characters 18-48:[0m
      3 |
      4 | let foo : tata = ([1m[31m{a = 1 ; b = "foo" ; c = true}[0m : toto)
      5 |

    [1m[31mError[0m: Invalid type(s).
    Expected: "record[a -> int , b -> string]", but got: "record[a -> int , b -> string , c -> sum[false -> unit , true -> unit]]". |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/id.mligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/id.mligo", line 45, characters 4-51:[0m
     44 |   let updated_identities: (id, id_details) big_map =
     45 |     [1m[31mBig_map.update new_id new_id_details identities[0m
     46 |   in

    [1m[31mError[0m: Incorrect argument.
    Expected an option, but got an argument of type "record[controller -> address , owner -> address , profile -> bytes]". |}]

(* 
  This test is here to ensure compatibility with comparable pairs introduced in carthage
  note that only "comb pairs" are allowed to be compared (would be beter if any pair would be comparable ?)
*)
let%expect_test _ =
  run_ligo_good [ "interpret" ; "Set.literal [ (1,(2,3)) ; (2,(3,4)) ]" ; "--syntax=cameligo" ] ;
  [%expect {|
    SET_ADD(( 2 , ( 3 , 4 ) ) , SET_ADD(( 1 , ( 2 , 3 ) ) , SET_EMPTY())) |}];

  run_ligo_bad [ "interpret" ; "Set.literal [ (1,2,3) ; (2,3,4) ]" ; "--syntax=cameligo" ] ;
  [%expect {|
    Error(s) occurred while parsing the Michelson input:
    At (unshown) location 1, comparable type expected.Type
                                                        pair (pair int int) int
                                                      is not comparable. |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/failwith_wrong_type.ligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/failwith_wrong_type.ligo", line 2, characters 19-46:[0m
      1 |
      2 | const bad : unit = [1m[31mfailwith((nil : list(int)))[0m

    [1m[31mError[0m: Invalid arguments.
    Expected an argument of type (string) or (nat) or (int), but got an argument of type list (int). |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/compare_sum_types.ligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/compare_sum_types.ligo", line 4, characters 29-36:[0m
      3 | function main (const p : foo; const s : bool) : list(operation) * bool is
      4 |   ((nil : list (operation)), [1m[31mp = Foo[0m)

    [1m[31mError[0m: Invalid arguments.
    These types cannot be compared: "sum[Bar -> unit , Foo -> unit]" and "
    sum[Bar -> unit , Foo -> unit]". |}]

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/invalid_field_record_update.mligo" ; "main" ] ;
  [%expect {|
    [1mFile "../../test/contracts/negative/invalid_field_record_update.mligo", line 4, characters 50-54:[0m
      3 | let main (p:int) (storage : abc) =
      4 |   (([] : operation list) , { storage with nofield=[1m[31m2048[0m} )

    [1m[31mError[0m: Invalid record field "nofield" in record "{ storage with { nofield = 2048 } }". |}]