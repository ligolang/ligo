open Ast_typed
open Stage_common.Constant

let environment = Ast_typed.Environment.add_ez_sum_type ~type_name:t_bool @@ 
  [
    (Constructor "true" ,{ctor_type=t_unit ();michelson_annotation=None;ctor_decl_pos=0});
    (Constructor "false",{ctor_type=t_unit ();michelson_annotation=None;ctor_decl_pos=1});
  ]
