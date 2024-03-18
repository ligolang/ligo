open Ligo_prim
open Types

module Declaration_mapper = struct
  type mapper = declaration -> declaration

  let rec map_expression : mapper -> expression -> expression =
   fun f expr ->
    let self = map_expression f in
    let return expression_content = { expr with expression_content } in
    match expr.expression_content with
    | E_application app -> return @@ E_application (Application.map self app)
    | E_lambda lamb -> return @@ E_lambda (Lambda.map self Fun.id lamb)
    | E_recursive rec_ -> return @@ E_recursive (Recursive.map self Fun.id rec_)
    | E_type_abstraction type_abs ->
      return @@ E_type_abstraction (Type_abs.map self type_abs)
    | E_let_in let_in -> return @@ E_let_in (Let_in.map self Fun.id let_in)
    | E_type_in type_in -> return @@ E_type_in (Type_in.map self Fun.id type_in)
    | E_mod_in mod_in -> return @@ E_mod_in (Mod_in.map self Fun.id mod_in)
    | E_raw_code raw_code -> return @@ E_raw_code (Raw_code.map self raw_code)
    | E_constructor constr -> return @@ E_constructor (Constructor.map self constr)
    | E_matching match_ -> return @@ E_matching (Match_expr.map self Fun.id match_)
    | E_record record -> return @@ E_record (Record.map ~f:self record)
    | E_accessor accessor -> return @@ E_accessor (Accessor.map self accessor)
    | E_update update -> return @@ E_update (Update.map self update)
    | E_ascription ascr -> return @@ E_ascription (Ascription.map self Fun.id ascr)
    | E_let_mut_in let_mut_in ->
      return @@ E_let_mut_in (Let_in.map self Fun.id let_mut_in)
    | E_assign assign -> return @@ E_assign (Assign.map self Fun.id assign)
    | E_for for_ -> return @@ E_for (For_loop.map self for_)
    | E_for_each for_each -> return @@ E_for_each (For_each_loop.map self for_each)
    | E_while while_ -> return @@ E_while (While_loop.map self while_)
    | E_variable _ | E_contract _ | E_literal _ | E_constant _ | E_module_accessor _ ->
      expr


  and map_expression_in_module_expr
      : (declaration -> declaration) -> module_expr -> module_expr
    =
   fun f module_expr -> Location.map (Module_expr.map (map_declaration f)) module_expr


  and map_declaration f decl =
    let decl = f decl in
    decl
    |> Location.map
       @@ function
       | D_value val_decl -> D_value (Value_decl.map (map_expression f) Fun.id val_decl)
       | D_module mod_decl ->
         D_module (Module_decl.map (map_expression_in_module_expr f) Fun.id mod_decl)
       | D_irrefutable_match pat_decl ->
         D_irrefutable_match (Pattern_decl.map (map_expression f) Fun.id pat_decl)
       | D_module_include module_ ->
         D_module_include (map_expression_in_module_expr f module_)
       | (D_type _ | D_signature _ | D_import _) as decl -> decl


  and map_decl m d = map_declaration m d
  and map_module : mapper -> module_ -> module_ = fun m -> List.map ~f:(map_decl m)
end
