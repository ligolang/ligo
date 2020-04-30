open Ast_typed
open Trace

type contract_pass_data = Contract_passes.contract_pass_data

module Errors = struct
  let no_nested_bigmap () =
    let title = (thunk ("It looks like you have nested a big map inside another big map. This is not supported. ")) in
    let message () = "" in
    let data = [
      (* ("location" , fun () -> Format.asprintf "%a" Location.pp loc) TODO once types have an actual location *)
    ] in
    error ~data title message ()
end

let rec check_no_nested_bigmap is_in_bigmap e = 
  match e.type_content with
  | T_operator (TC_big_map _) when is_in_bigmap  -> 
    fail @@ Errors.no_nested_bigmap
  | T_operator (TC_big_map {k ; v}) ->
    let%bind _ = check_no_nested_bigmap false k in
    let%bind _ = check_no_nested_bigmap true  v in
    ok ()
  | T_operator (TC_map_or_big_map {k ; v}) ->
    let%bind _ = check_no_nested_bigmap false k in
    let%bind _ = check_no_nested_bigmap true  v in
    ok ()
  | T_operator (TC_contract t)
  | T_operator (TC_option t)
  | T_operator (TC_list t)
  | T_operator (TC_set t) ->
    let%bind _ = check_no_nested_bigmap is_in_bigmap t in
    ok ()
  | T_operator (TC_map { k ; v }) ->
    let%bind _ = check_no_nested_bigmap is_in_bigmap k in
    let%bind _ = check_no_nested_bigmap is_in_bigmap v in
    ok ()
  | T_sum s -> 
    let es = List.map (fun {ctor_type;_} -> ctor_type) (CMap.to_list s) in
    let%bind _ = bind_map_list (fun l -> check_no_nested_bigmap is_in_bigmap l) es in
    ok ()
  | T_record elm -> 
    let es = LMap.to_list elm in
    let%bind _ = bind_map_list (fun {field_type;_} -> check_no_nested_bigmap is_in_bigmap field_type) es in
    ok ()
  | T_arrow { type1; type2 } -> 
    let%bind _ = check_no_nested_bigmap false type1 in    
    let%bind _ = check_no_nested_bigmap false type2 in
    ok ()
  | T_variable _
  | T_constant _ -> 
    ok ()

let self_typing : contract_pass_data -> expression -> (bool * contract_pass_data * expression) result = fun dat el ->
  let%bind _ = check_no_nested_bigmap false el.type_expression in 
  ok (true, dat, el)
