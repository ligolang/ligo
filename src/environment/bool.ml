open Ast_typed
open Stage_common.Constant

let environment = env_sum_type ~type_name:t_bool @@ [(Constructor "true",{ctor_type=t_unit ();michelson_annotation=None});(Constructor "false",{ctor_type=t_unit ();michelson_annotation=None})]
