open Ast_core
open Desugaring

let decompile (m : module_) : Ast_sugar.module_  =
  decompile_module m

let decompile_expression (e : expression) : Ast_sugar.expression  =
  decompile_expression e

let decompile_type_expression (e : type_expression) : Ast_sugar.type_expression =
  decompile_type_expression e
