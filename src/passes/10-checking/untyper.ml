module Ligo_string = Simple_utils.Ligo_string
module Location    = Simple_utils.Location
module I = Ast_core
module O = Ast_typed

(*
  Transform a Ast_typed type_expression into an ast_core type_expression
 *)
let rec untype_type_expression_nofail (t:O.type_expression) : I.type_expression =
  let self = untype_type_expression_nofail in
  let return t = I.make_t t in
  match t.type_content with
  | O.T_sum {content ; layout} ->
     let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
       let associated_type = untype_type_expression_nofail associated_type in
       let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
       v' in
     let x' = Stage_common.Types.LMap.map aux content in
     return @@ I.T_sum { fields = x' ; layout = Some layout }
  | O.T_record {content;layout} -> (
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let associated_type = untype_type_expression_nofail associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      v' in
    let x' = Stage_common.Types.LMap.map aux content in
    return @@ I.T_record {fields = x' ; layout = Some layout}
  )
  | O.T_arrow {type1;type2} ->
    let arr = O.{type1 = self type1; type2 = self type2} in
    return @@ T_arrow arr
  | O.T_constant {language;injection;parameters} ->
    ignore language ;
    let arguments = List.map ~f:untype_type_expression_nofail parameters in
    let type_operator = I.TypeVar.fresh ~name:(Stage_common.Constant.to_string injection) () in
    return @@ I.T_app {type_operator;arguments}
  | O.T_variable name -> return @@ I.T_variable name
  | O.T_module_accessor ma -> return @@ I.T_module_accessor ma
  | O.T_singleton x -> return @@ I.T_singleton x
  | O.T_abstraction x ->
    let type_ = untype_type_expression_nofail x.type_ in
    return @@ I.T_abstraction {x with type_}
  | O.T_for_all x ->
    let type_ = untype_type_expression_nofail x.type_ in
    return @@ I.T_for_all {x with type_}

let untype_type_expression (t:O.type_expression) : I.type_expression =
  untype_type_expression_nofail t
