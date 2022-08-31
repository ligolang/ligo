open Errors
open Ligo_prim
open Ast_typed
open Simple_utils.Trace

type contract_pass_data = Contract_passes.contract_pass_data

let rec check_no_nested_bigmap ~raise is_in_bigmap e =
  match e.type_content with
  | T_constant {injection=Big_map; _} when is_in_bigmap ->
    raise.error @@ nested_bigmap e.location
  | T_constant {injection=(Big_map | Map); parameters=[k ; v];_} ->
    let _ = check_no_nested_bigmap ~raise false k in
    let _ = check_no_nested_bigmap ~raise true  v in
    ()
  | T_constant {parameters;_} ->
    let _ = List.map ~f:(check_no_nested_bigmap ~raise is_in_bigmap) parameters in
    ()
  | T_sum s ->
    let es = List.map ~f:(fun {associated_type;_} -> associated_type) (Record.LMap.to_list s.fields) in
    let _ = List.map ~f:(fun l -> check_no_nested_bigmap ~raise is_in_bigmap l) es in
    ()
  | T_record {fields=elm;_} ->
    let _ = Record.map (fun ({associated_type;_}: row_element) -> check_no_nested_bigmap ~raise is_in_bigmap associated_type) elm in
    ()
  | T_arrow { type1; type2 } ->
    let _ = check_no_nested_bigmap ~raise false type1 in
    let _ = check_no_nested_bigmap ~raise false type2 in
    ()
  | T_variable _ -> ()
  | T_singleton _ -> ()
  | T_abstraction x -> check_no_nested_bigmap ~raise is_in_bigmap x.type_
  | T_for_all x -> check_no_nested_bigmap ~raise is_in_bigmap x.type_

let self_typing ~raise : contract_pass_data -> expression -> bool * contract_pass_data * expression = fun dat el ->
  let () = check_no_nested_bigmap ~raise false el.type_expression in
  (true, dat, el)
