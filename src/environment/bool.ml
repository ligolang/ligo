open Ast_typed

let environment = Ast_typed.Environment.add_ez_sum_type ~type_name:Ast_typed.Constant.t_bool @@ 
  [
    (Constructor "true" ,{ctor_type=t_unit ();michelson_annotation=None;ctor_decl_pos=0});
    (Constructor "false",{ctor_type=t_unit ();michelson_annotation=None;ctor_decl_pos=1});
  ]
