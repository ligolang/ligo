open Ast_aggregated
open Ligo_prim.Constant

let expression : expression -> expression = fun expr ->
  match expr.expression_content with
  | E_constant { cons_name = C_POLYMORPHIC_ADD ; arguments } ->
     let decide e = match e with
       | { type_expression ; _ } when is_t_string type_expression -> Some C_CONCAT
       | _ -> None in
     let cons_name =
       Option.value ~default:C_ADD @@ List.find_map arguments ~f:decide in
     { expr with expression_content = E_constant { cons_name ; arguments } }
  | E_constant { cons_name = C_POLYMORPHIC_SUB ; arguments } ->
     let decide e = match e with
       | { type_expression ; _ } when is_t_tez type_expression -> Some C_SUB_MUTEZ
       | _ -> None in
     let cons_name =
       Option.value ~default:C_SUB @@ List.find_map arguments ~f:decide in
     { expr with expression_content = E_constant { cons_name ; arguments } }
  | _ -> expr
