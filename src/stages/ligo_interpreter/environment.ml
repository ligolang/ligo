open Types
open Ligo_prim

let extend
    :  env -> Value_var.t -> ?inline:bool -> ?no_mutation:bool
    -> Ast_aggregated.type_expression * value -> env
  =
 fun env name ?(inline = false) ?(no_mutation = false) (ast_type, eval_term) ->
  (name, { item = { ast_type; eval_term }; no_mutation; inline }) :: env


let lookup : env -> Value_var.t -> (value_expr * bool * bool) option =
 fun env var ->
  Option.map ~f:(fun { item; no_mutation; inline } -> item, no_mutation, inline)
  @@ List.Assoc.find env ~equal:Value_var.equal var


let values : env -> value list =
 fun env -> List.map ~f:(fun (_, it) -> it.item.eval_term) env


let empty_env : env = []
