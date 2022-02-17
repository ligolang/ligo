open Ast_sugar
open Purification

let decompile (m : module_) : Ast_imperative.module_  =
  decompile_module m

let decompile_expression (e : expression) : Ast_imperative.expression  =
  decompile_expression e

let decompile_type_expression (e : type_expression) : Ast_imperative.type_expression  =
  decompile_type_expression e
