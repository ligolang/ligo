open Ast_typed

let environment = Ast_typed.Environment.add_ez_sum_type ~type_name:Ast_typed.Constant.t_bool @@ 
  [
    (Label "true" ,{associated_type=t_unit ();michelson_annotation=None;decl_pos=0});
    (Label "false",{associated_type=t_unit ();michelson_annotation=None;decl_pos=1});
  ]
