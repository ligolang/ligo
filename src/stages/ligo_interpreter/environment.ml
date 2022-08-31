open Types
open Ligo_prim

let extend :
  env -> ValueVar.t -> ?inline:bool -> ?no_mutation:bool -> (Ast_aggregated.type_expression * value) -> env
  = fun env name ?(inline = false) ?(no_mutation = false) (ast_type,eval_term) ->
  (name, { item = { ast_type = ast_type ; eval_term } ; no_mutation ; inline }) :: env

let lookup : env -> ValueVar.t -> (value_expr * bool * bool) option
  = fun env var ->
  Option.map ~f:(fun { item ; no_mutation ; inline } -> (item, no_mutation, inline)) @@ List.Assoc.find env ~equal:ValueVar.equal var

let empty_env : env = []
