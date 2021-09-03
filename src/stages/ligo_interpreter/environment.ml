open Types

let extend :
  env -> ?ast_type:Ast_typed.type_expression -> (expression_variable * value) -> env
  = fun env ?ast_type (var,eval_term) ->
  { env with expression_env = (var, {ast_type;eval_term}) :: env.expression_env }

let extend_mod :
  env -> (module_variable * env) -> env
  = fun env (var,eval_term) ->
  { env with module_env = (var, eval_term) :: env.module_env }

let lookup :
  env -> expression_variable -> value_expr option
    = fun env var ->
  let open Location in
  let equal a b = Var.compare a.wrap_content b.wrap_content = 0 in
  List.Assoc.find env.expression_env ~equal var

let empty_env = { expression_env = []; module_env = [] }

let to_kv_list v = v
let to_kv_list_rev v = List.rev v

let filter :
  env -> (value_expr -> bool) -> env
    = fun env pred ->
  { env with expression_env = List.filter ~f:(fun (_, v) -> pred v) env.expression_env }
