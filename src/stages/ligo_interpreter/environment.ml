open Types

let extend :
  env -> (expression_variable * value) -> env
    = fun env (var,exp) -> Env.add var exp env

let lookup :
  env -> expression_variable -> value option
    = fun env var -> Env.find_opt var env

let empty_env = Env.empty

let to_kv_list = Env.to_kv_list