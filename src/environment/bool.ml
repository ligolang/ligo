open Ast_typed

let environment =
  Ast_typed.Environment.add_type
    Constant.t_bool
    (t_sum_ez [ ("true" ,t_unit ()); ("false",t_unit ()) ])
    Ast_typed.Environment.empty
