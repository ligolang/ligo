val extend
  :  Types.env
  -> Ligo_prim.Value_var.t
  -> ?inline:bool
  -> ?no_mutation:bool
  -> Ast_aggregated.type_expression * Types.value
  -> Types.env

val lookup : Types.env -> Ligo_prim.Value_var.t -> (Types.value_expr * bool * bool) option
val values : Types.env -> Types.value list
val empty_env : Types.env
