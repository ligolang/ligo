open Types

let extend :
  env -> expression_variable -> (Ast_typed.type_expression * value) -> env
  = fun env name (ast_type,eval_term) ->
  Expression {name ; item = { ast_type = ast_type ; eval_term }} :: env

let extend_mod :
  env -> module_variable -> env -> env
  = fun env name item ->
  Module {name; item} :: env

let expressions :
  env -> (expression_variable * value_expr) list
  = fun env ->
  List.filter_map env ~f:(function | Expression {name;item} -> Some (name, item) | Module _ -> None)

let modules :
  env -> (module_variable * env) list
  = fun env ->
  List.filter_map env ~f:(function | Module {name;item} -> Some (name, item) | Expression _ -> None)

let lookup :
  env -> expression_variable -> value_expr option
  = fun env var ->
  let open Location in
  let equal a b = Var.compare a.wrap_content b.wrap_content = 0 in
  List.Assoc.find (expressions env) ~equal var

let empty_env = []

let to_kv_list v = v
let to_kv_list_rev v = List.rev v

let filter :
  env -> (value_expr -> bool) -> env
    = fun env pred ->
  let rec aux = function
    | [] -> []
    | Expression {name = _; item} :: xs when not (pred item) -> aux xs
    | x :: xs -> x :: aux xs in
  aux env
