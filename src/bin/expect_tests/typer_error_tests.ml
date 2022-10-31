open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_function_annotation_1.mligo"];
  [%expect {|
    File "../../test/contracts/negative/error_function_annotation_1.mligo", line 1, characters 26-27:
      1 | let main (a:int) : unit = a

    Invalid type(s)
    Cannot unify int with unit. |}];

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_function_annotation_2.mligo"; "--entry-point"; "f"];
  [%expect {|
    File "../../test/contracts/negative/error_function_annotation_2.mligo", line 1, characters 14-43:
      1 | let f : int = fun (x, y : int*int) -> x + y
      2 | let g (x, y : int * int) : int = f (x, y)

    Invalid type(s)
    Cannot unify ( int * int ) -> int with int. |}];

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_function_annotation_3.mligo"; "--entry-point"; "f"];
  [%expect {|
    File "../../test/contracts/negative/error_function_annotation_3.mligo", line 8, characters 14-20:
      7 |   match s with
      8 |   | Add si -> Add si
      9 |   | Sub si -> Sub si

    Invalid type(s)
    Cannot unify sum[Add -> int , Sub -> int] with ( list (operation) * sum[Add -> int , Sub -> int] ). |}];

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_no_tail_recursive_function.mligo"; "--entry-point"; "unvalid"];
  [%expect {|
    File "../../test/contracts/negative/error_no_tail_recursive_function.mligo", line 2, characters 14-21:
      1 | let rec unvalid (n:int):int =
      2 |     let res = unvalid (n) in
      3 |     res + 1

    Recursive call not in tail position.
    The value of a recursive call must be immediately returned by the defined function. |}];

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_type.ligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_type.ligo", line 3, characters 18-28:
      2 |
      3 | const foo : nat = 42 + "bar"

    Invalid type(s)
    Cannot unify int with string. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_type_record_access.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_type_record_access.mligo", line 6, characters 17-20:
      5 | let bar (x : foo) : int =
      6 |   let y : bool = x.i in
      7 |   42

    Invalid type(s)
    Cannot unify int with bool. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_type_record_update.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_type_record_update.mligo", line 7, characters 23-26:
      6 | let bar (x : foo) : foo =
      7 |   let x = { x with i = x.j } in
      8 |   x

    Invalid type(s)
    Cannot unify bool with int. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_1.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_1.mligo", line 3, characters 19-27:
      2 |
      3 | let foo : string = 42 + 127
      4 |

    Invalid type(s)
    Cannot unify int with string. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_2.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_2.mligo", line 3, characters 24-39:
      2 |
      3 | let foo : string list = Some (42 + 127)
      4 |

    Invalid type(s)
    Cannot unify option (int) with list (string). |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_3.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_3.mligo", line 3, characters 34-53:
      2 |
      3 | let foo : (int * string * bool) = ((1, "foo") : toto)
      4 |

    Invalid type(s)
    Cannot unify ( int * string ) with ( int * string * bool ). |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_4.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_4.mligo", line 4, characters 17-56:
      3 |
      4 | let foo : tata = ({a = 1 ; b = "foo" ; c = true} : toto)
      5 |

    Invalid type(s)
    Cannot unify record[a -> int , b -> string , c -> bool] with record[a -> int , c -> bool , d -> string]. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_5.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_5.mligo", line 1, characters 10-17:
      1 | let foo : boolean = 3
      2 |

    Type "boolean" not found. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_6.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_6.mligo", line 1, characters 30-64:
      1 | let foo : (int, string) map = (Map.literal [] : (int, bool) map)
      2 | let main (p:int) (storage : int) =

    Invalid type(s)
    Cannot unify bool with string. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_7.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/error_typer_7.mligo", line 4, characters 18-48:
      3 |
      4 | let foo : tata = ({a = 1 ; b = "foo" ; c = true} : toto)
      5 |

    Mismatching record labels. Expected record of type record[a -> int , b -> string]. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/error_typer_1.jsligo" ] ;
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
    Cannot unify ( nat * nat ) with nat. |} ] ;

  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/id.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/id.mligo", line 45, characters 26-40:
     44 |   let updated_identities: (id, id_details) big_map =
     45 |     Big_map.update new_id new_id_details identities
     46 |   in

    Invalid type(s)
    Cannot unify record[controller -> address , owner -> address , profile -> bytes] with option (^gen#494). |}]

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
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/invalid_field_record_update.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/invalid_field_record_update.mligo", line 4, characters 29-36:
      3 | let main (p:int) (storage : abc) =
      4 |   (([] : operation list) , { storage with nofield=2048} )

    Invalid record field "nofield" in record. |}]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/override_option.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/override_option.mligo", line 3, characters 53-57:
      2 |
      3 | let main (x,y:bool * bool) = ([] : operation list), (None : option)

    Constructor "None" not found. |} ]

let%expect_test _ =
  run_ligo_bad [ "compile" ; "contract" ; "../../test/contracts/negative/will_be_ignored.mligo" ] ;
  [%expect {|
    File "../../test/contracts/negative/will_be_ignored.mligo", line 7, characters 47-55:
      6 |      let receiver : contract =
      7 |       match (Tezos.get_contract_opt(s.owner) : contract option) with
      8 |         Some (contract) -> contract

    Type is applied to a wrong number of arguments, expected: 1 got: 0 |}]

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