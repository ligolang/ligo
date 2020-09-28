open Trace
open Errors

module I = Ast_core
module O = Ast_typed

(*
  Transform a Ast_typed type_expression into an ast_core type_expression
 *)
let rec untype_type_expression (t:O.type_expression) : (I.type_expression, typer_error) result =
  let return t = ok @@ I.make_t t in
  match t.type_content with
  | O.T_sum {content ; layout} ->
     let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
       let%bind associated_type = untype_type_expression associated_type in
       let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
       ok @@ v' in
     let%bind x' = Stage_common.Helpers.bind_map_lmap aux content in
     return @@ I.T_sum { fields = x' ; layout = Some layout }
  | O.T_record {content;layout} -> (
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let%bind associated_type = untype_type_expression associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      ok @@ v' in
    let%bind x' = Stage_common.Helpers.bind_map_lmap aux content in
    return @@ I.T_record {fields = x' ; layout = Some layout}
  )
  | O.T_variable name -> return @@ I.T_variable (Var.todo_cast name)
  | O.T_arrow {type1;type2} ->
     let%bind type1 = untype_type_expression type1 in
     let%bind type2 = untype_type_expression type2 in
     return @@ I.T_arrow {type1;type2}
  | O.T_constant {type_constant;arguments} ->
     let%bind arguments = bind_map_list untype_type_expression arguments in
     return @@ I.T_constant {type_constant;arguments}

let untype_declaration_constant untype_expression O.{binder;expr;inline} =
  let attr = I.{inline} in
  let%bind ty = untype_type_expression expr.type_expression in
  let var = Location.map Var.todo_cast binder in
  let binder = var in
  let%bind expr = untype_expression expr in
  let expr = I.e_annotation expr ty in
  ok @@ I.{binder;attr;expr;type_opt=Some(ty)}

let untype_declaration_type O.{type_binder; type_expr} =
  let%bind type_expr = untype_type_expression type_expr in
  let type_binder = Var.todo_cast type_binder in
  ok @@ I.{type_binder; type_expr}

let untype_declaration untype_expression = function
  | O.Declaration_constant dc ->
     let%bind dc = untype_declaration_constant untype_expression dc in
     ok @@ I.Declaration_constant dc
  | O.Declaration_type dt ->
     let%bind dt = untype_declaration_type dt in
     ok @@ I.Declaration_type dt

let untype_program untype_expression : O.program -> (I.program, _) result =
  let untype_declaration = untype_declaration untype_expression in
  bind_map_list (bind_map_location untype_declaration)
