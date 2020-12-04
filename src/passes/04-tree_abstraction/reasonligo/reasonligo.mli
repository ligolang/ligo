[@@@warning "-45"]

open Trace

module CST = Cst.Reasonligo
module AST = Ast_imperative
module Errors = Errors


val compile_expression : CST.expr -> (AST.expr, Errors.abs_error) result
val compile_program : CST.ast -> (AST.program, Errors.abs_error) result

val decompile_expression : AST.expr -> (CST.expr, _) result
val decompile_program    : AST.program -> (CST.ast, _) result
