[@@@warning "-45"]

open Trace

module CST = Cst.Cameligo
module AST = Ast_imperative
module Errors = Errors


val compile_expression : CST.expr -> (AST.expr, Errors.abs_error) result
val compile_module : CST.ast -> (AST.module_, Errors.abs_error) result

val decompile_expression : AST.expr -> (CST.expr, _) result
val decompile_module    : AST.module_ -> (CST.ast, _) result
