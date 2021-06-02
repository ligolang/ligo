open Errors
open Ast_typed
open Trace
open Stage_common.Constant

type contract_pass_data = Contract_passes.contract_pass_data

let extract = Ligo_string.extract

let rec check_no_nested_bigmap is_in_bigmap e =
  match e.type_content with
  | T_constant {injection; _} when (String.equal (extract injection) big_map_name) && is_in_bigmap ->
    fail @@ nested_bigmap e.location
  | T_constant {injection; parameters=[k ; v];_} when String.equal (extract injection) big_map_name || String.equal (extract injection) map_name ->
    let* _ = check_no_nested_bigmap false k in
    let* _ = check_no_nested_bigmap true  v in
    ok ()
  | T_constant {parameters;_} ->
    let* _ = bind_map_list (check_no_nested_bigmap is_in_bigmap) parameters in
    ok ()
  | T_sum s ->
    let es = List.map ~f:(fun {associated_type;_} -> associated_type) (LMap.to_list s.content) in
    let* _ = bind_map_list (fun l -> check_no_nested_bigmap is_in_bigmap l) es in
    ok ()
  | T_record {content=elm;_} ->
    let* _ = Helpers.bind_map_lmap (fun {associated_type;_} -> check_no_nested_bigmap is_in_bigmap associated_type) elm in
    ok ()
  | T_arrow { type1; type2 } ->
    let* _ = check_no_nested_bigmap false type1 in
    let* _ = check_no_nested_bigmap false type2 in
    ok ()
  | T_variable _ -> ok ()
  | T_module_accessor _ -> ok ()
  | T_singleton _ -> ok ()

let self_typing : contract_pass_data -> expression -> (bool * contract_pass_data * expression , self_ast_typed_error) result = fun dat el ->
  let* _ = check_no_nested_bigmap false el.type_expression in
  ok (true, dat, el)
