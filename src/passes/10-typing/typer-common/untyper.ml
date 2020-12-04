open Trace
open Errors
open Stage_common.Maps

module I = Ast_core
module O = Ast_typed

(*
  Transform a Ast_typed type_expression into an ast_core type_expression
 *)
let rec untype_type_expression (t:O.type_expression) : (I.type_expression, typer_error) result =
  let self = untype_type_expression in
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
  | O.T_arrow arr ->
    let%bind arr = arrow self arr in
    return @@ T_arrow arr
  | O.T_constant {language;injection;parameters} ->
    ignore language ;
    let%bind arguments = bind_map_list untype_type_expression parameters in
    let type_operator = Var.of_name (Ligo_string.extract injection) in
    return @@ I.T_app {type_operator;arguments}
  | O.T_variable name -> return @@ I.T_variable (Var.todo_cast name)
  | O.T_module_accessor ma ->
    let%bind ma = module_access self ma in
    return @@ I.T_module_accessor ma

let untype_declaration_constant untype_expression O.{binder;expr;inline} =
  let attr = I.{inline} in
  let%bind ty = untype_type_expression expr.type_expression in
  let var = Location.map Var.todo_cast binder in
  let binder = ({var;ascr= Some ty}: _ I.binder) in
  let%bind expr = untype_expression expr in
  let expr = I.e_annotation expr ty in
  ok @@ I.{binder;attr;expr;}

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

let untype_program untype_expression : O.program_fully_typed -> (I.program, _) result =
  fun (O.Program_Fully_Typed p) ->
  let untype_declaration = untype_declaration untype_expression in
  bind_map_list (bind_map_location untype_declaration) p
