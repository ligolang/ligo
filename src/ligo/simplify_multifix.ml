open Trace
open Function
module I = Multifix.Ast
module O = Ast_simplified

let unwrap = Location.unwrap

let expression : I.expression -> O.annotated_expression result = fun _ ->
  simple_fail (thunk "")

let type_expression : I.type_expression -> O.type_expression result = fun _ ->
  simple_fail (thunk "")

let statement : I.statement -> O.declaration result = fun s ->
  match s with
  | Statement_variable_declaration ([_], _) -> simple_fail (thunk "")
  (* | Statement_variable_declaration ([n], e) ->
   *     let%bind e' = bind_map_location expression e in
   *     let%bind (name, ty) =
   *       let%bind pattern =
   *         match unwrap n with
   *         | Param_restricted_pattern c -> ok @@ unwrap c
   *         | Param_implicit_named_param _ -> simple_fail (thunk "") in
   *       simple_fail (thunk "")
   *     in
   *     ok @@ O.Declaration_constant {name = unwrap n ; annotated_expression = unwrap e'} *)
  | Statement_variable_declaration _ -> simple_fail (thunk "no sugar-candy for fun declarations yet")
  | Statement_init_declaration _ -> simple_fail (thunk "no init declaration yet")
  | Statement_entry_declaration _ -> simple_fail (thunk "no entry declaration yet")
  | Statement_type_declaration (n, te) ->
      let%bind te' = bind_map_location type_expression te in
      ok @@ O.Declaration_type {type_name = unwrap n ; type_expression = unwrap te'}

let program : I.program -> O.program result = fun (Program lst) ->
  bind_map_list (apply Location.unwrap >| bind_map_location statement) lst

let main : I.entry_point -> O.program Location.wrap result =
  bind_map_location program
