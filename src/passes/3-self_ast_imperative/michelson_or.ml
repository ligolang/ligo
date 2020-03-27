open Ast_imperative
open Trace

let peephole_type_expression : type_expression -> type_expression result = fun e ->
  let return type_content = ok { type_content } in
  match e.type_content with
  | T_operator (TC_michelson_or (l_ty,r_ty)) ->
    return @@ T_sum (CMap.of_list [ (Constructor "M_left", l_ty) ; (Constructor "M_right", r_ty) ])
  | e -> return e
