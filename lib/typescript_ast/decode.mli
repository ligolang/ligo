module Region = Simple_utils.Region
module Loc_map = Typescript_ast.Loc_map
module Ts_wrap = Typescript_ast.Ts_wrap
module Ast = Typescript_ast.Ast

val dec_program :
    ?debug_arg:bool ->
    filename:string ->
    file:string ->
    Loc_map.t ->
    Ts_wrap.ts_tree ->
    (Ast.t, string Region.reg) result

val dec_standalone_expression :
  file:string ->
  Loc_map.t ->
  Ts_wrap.ts_tree ->
  (Ast.expression, string Region.reg) result

val dec_standalone_type_expr :
  Loc_map.t ->
  Ts_wrap.ts_tree ->
  (Ast.type_expr, string Region.reg) result
