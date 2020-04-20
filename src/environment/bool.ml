open Ast_typed

let environment = env_sum_type ~type_name:(Var.of_name "bool") @@ [(Constructor "true",t_unit ());(Constructor "false",t_unit ())]
