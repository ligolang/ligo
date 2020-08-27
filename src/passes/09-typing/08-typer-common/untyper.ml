open Trace

module I = Ast_core
module O = Ast_typed

let untype_declaration_constant untype_expression untype_type_value O.{binder;expr;inline} =
  let attr = I.{inline} in
  let%bind ty = untype_type_value expr.type_expression in
  let var = Location.map Var.todo_cast binder in
  let binder = I.{var; ty} in
  let%bind expr = untype_expression expr in
  let expr = I.e_annotation expr ty in
  ok @@ I.{binder;attr;expr}

let untype_declaration_type untype_type_value O.{type_binder; type_expr} =
  let%bind type_expr = untype_type_value type_expr in
  let type_binder = Var.todo_cast type_binder in
  ok @@ I.{type_binder; type_expr}

let untype_declaration untype_expression untype_type_value = function
  | O.Declaration_constant dc ->
     let%bind dc = untype_declaration_constant untype_expression untype_type_value dc in
     ok @@ I.Declaration_constant dc
  | O.Declaration_type dt ->
     let%bind dt = untype_declaration_type untype_type_value dt in
     ok @@ I.Declaration_type dt

let untype_program untype_expression untype_type_value : O.program -> (I.program, _) result =
  let untype_declaration = untype_declaration untype_expression untype_type_value in
  bind_map_list (bind_map_location untype_declaration)
