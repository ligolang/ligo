module Location    = Simple_utils.Location
module List        = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
open Simple_utils
open Types
open Stage_common

let assert_eq = fun a b -> if Caml.(=) a b then Some () else None
let assert_same_size = fun a b -> if (List.length a = List.length b) then Some () else None
let rec assert_list_eq f = fun a b -> match (a,b) with
  | [], [] -> Some ()
  | [], _  -> None
  | _ , [] -> None
  | hda::tla, hdb::tlb -> Option.(
    let* () = f hda hdb in
    assert_list_eq f tla tlb
  )


let assert_type_expression_eq (a, b: (type_expression * type_expression)) : unit option =
  if equal_type_expression a b then Some () else None

and assert_literal_eq (a, b : Literal_types.t * Literal_types.t) : unit option =
  if Literal_types.equal a b then Some () else None
