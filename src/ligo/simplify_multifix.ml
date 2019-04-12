open Trace
open Function
module I = Multifix.Ast
module O = Ast_simplified

let statement : I.statement -> O.declaration result = fun s ->
  match s with
  (* | Statement_variable_declaration (s, [], expr) -> simple_fail (thunk "") *)
  | Statement_variable_declaration _ -> simple_fail ""
  | Statement_init_declaration _ -> simple_fail ""
  | Statement_entry_declaration _ -> simple_fail ""
  | Statement_type_declaration _ -> simple_fail ""

let program : I.program -> O.program result = fun (Program lst) ->
  bind_map_list (apply Location.unwrap >| bind_map_location statement) lst

let main : I.entry_point -> O.program Location.wrap result =
  bind_map_location program
