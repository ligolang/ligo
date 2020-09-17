open Errors
open Ast_typed
open Trace

type contract_pass_data = Contract_passes.contract_pass_data

let rec check_no_nested_bigmap is_in_bigmap e = 
  match e.type_content with
  | T_constant {type_constant=TC_big_map; _} when is_in_bigmap  -> 
    fail @@ nested_bigmap e.location
  | T_constant {type_constant=TC_big_map|TC_map_or_big_map; arguments=[k ; v]} ->
    let%bind _ = check_no_nested_bigmap false k in
    let%bind _ = check_no_nested_bigmap true  v in
    ok ()
  | T_constant {arguments;_} ->
    let%bind _ = bind_map_list (check_no_nested_bigmap is_in_bigmap) arguments in
    ok ()
  | T_sum s -> 
    let es = List.map (fun {associated_type;_} -> associated_type) (LMap.to_list s) in
    let%bind _ = bind_map_list (fun l -> check_no_nested_bigmap is_in_bigmap l) es in
    ok ()
  | T_record elm -> 
    let es = LMap.to_list elm in
    let%bind _ = bind_map_list (fun {associated_type;_} -> check_no_nested_bigmap is_in_bigmap associated_type) es in
    ok ()
  | T_arrow { type1; type2 } -> 
    let%bind _ = check_no_nested_bigmap false type1 in    
    let%bind _ = check_no_nested_bigmap false type2 in
    ok ()
  | T_variable _
  | T_wildcard -> 
    ok ()

let self_typing : contract_pass_data -> expression -> (bool * contract_pass_data * expression , self_ast_typed_error) result = fun dat el ->
  let%bind _ = check_no_nested_bigmap false el.type_expression in 
  ok (true, dat, el)
