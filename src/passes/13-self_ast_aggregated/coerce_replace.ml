open Ast_aggregated
open Ligo_prim.Constant
open Trace

let expression ~raise : expression -> expression =
 fun expr ->
  match expr.expression_content with
  | E_coerce { anno_expr = value; _ }
    when is_t_bool expr.type_expression && is_t_nat value.type_expression ->
    let constant =
      { cons_name = C_LT
      ; arguments = [ e_a_nat ~loc:value.location Z.(of_int 0); value ]
      }
    in
    e_constant ~loc:expr.location constant expr.type_expression
  | E_coerce { anno_expr = value; _ }
    when is_t_bool expr.type_expression && is_t_int value.type_expression ->
    let constant =
      { cons_name = C_NEQ
      ; arguments = [ e_a_int ~loc:value.location Z.(of_int 0); value ]
      }
    in
    e_constant ~loc:expr.location constant expr.type_expression
  | E_coerce { anno_expr = value; _ }
    when is_t_bool expr.type_expression && is_t_tez value.type_expression ->
    let constant =
      { cons_name = C_LT
      ; arguments = [ e_a_mutez ~loc:value.location Z.(of_int 0); value ]
      }
    in
    e_constant ~loc:expr.location constant expr.type_expression
  | E_coerce { anno_expr = value; _ }
    when is_t_bool expr.type_expression && is_t_string value.type_expression ->
    let constant =
      { cons_name = C_LT
      ; arguments =
          [ e_a_string ~loc:value.location Simple_utils.Ligo_string.(standard ""); value ]
      }
    in
    e_constant ~loc:expr.location constant expr.type_expression
  | E_coerce { anno_expr = value; _ }
    when is_t_bool expr.type_expression && is_t_bytes value.type_expression ->
    let constant =
      { cons_name = C_LT
      ; arguments = [ e_a_bytes ~loc:value.location Bytes.(of_string ""); value ]
      }
    in
    e_constant ~loc:expr.location constant expr.type_expression
  | E_coerce { anno_expr = value; _ }
    when is_t_bool expr.type_expression && is_t_list value.type_expression ->
    let tcase : _ Match_expr.match_case =
      { pattern = Location.wrap ~loc:value.location Pattern.(P_list (List []))
      ; body = e_a_bool ~loc:value.location false
      }
    in
    let fcase : _ Match_expr.match_case =
      { pattern =
          Location.wrap
            ~loc:value.location
            Pattern.(
              P_var
                Ligo_prim.Binder.(
                  make
                    Ligo_prim.Value_var.(fresh ~loc:value.location ())
                    value.type_expression))
      ; body = e_a_bool ~loc:value.location true
      }
    in
    let cases : _ Match_expr.match_case list = [ tcase; fcase ] in
    let matching : (expression, type_expression) Match_expr.t =
      { matchee = value; cases }
    in
    e_matching ~loc:expr.location matching expr.type_expression
  | E_coerce { anno_expr = value; _ }
    when is_t_bool expr.type_expression && is_t_set value.type_expression ->
    let size =
      e_constant
        ~loc:value.location
        { cons_name = C_SET_SIZE; arguments = [ value ] }
        (t_nat ~loc:value.type_expression.location ())
    in
    let constant =
      { cons_name = C_LT; arguments = [ e_a_nat ~loc:value.location Z.(of_int 0); size ] }
    in
    e_constant ~loc:expr.location constant expr.type_expression
  | E_coerce { anno_expr = value; _ }
    when is_t_bool expr.type_expression && is_t_map value.type_expression ->
    let size =
      e_constant
        ~loc:value.location
        { cons_name = C_MAP_SIZE; arguments = [ value ] }
        (t_nat ~loc:value.type_expression.location ())
    in
    let constant =
      { cons_name = C_LT; arguments = [ e_a_nat ~loc:value.location Z.(of_int 0); size ] }
    in
    e_constant ~loc:expr.location constant expr.type_expression
  | E_coerce _ -> raise.error @@ Errors.unsolved_coerce expr.location
  | _ -> expr
