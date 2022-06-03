open Types

let extend :
  env -> expression_variable -> ?inline:bool -> ?no_mutation:bool -> (Ast_aggregated.type_expression * value) -> env
  = fun env name ?(inline = false) ?(no_mutation = false) (ast_type,eval_term) ->
  Expression {name ; item = { ast_type = ast_type ; eval_term } ; no_mutation ; inline } :: env

let expressions :
  env -> (expression_variable * (value_expr * bool * bool)) list
  = fun env ->
  List.filter_map env ~f:(function | Expression {name;item;no_mutation;inline} -> Some (name, (item, no_mutation, inline)))

let lookup : env -> expression_variable -> (value_expr * bool * bool) option
  = fun env var ->
  List.Assoc.find (expressions env) ~equal:ValueVar.equal var

let empty_env = []
