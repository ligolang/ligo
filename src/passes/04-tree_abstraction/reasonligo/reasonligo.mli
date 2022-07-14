[@@@warning "-45"]

module CST = Cst.Reasonligo
module AST = Ast_imperative
module Errors = Errors


val compile_expression : raise:(Errors.abs_error,Main_warnings.all) Simple_utils.Trace.raise -> ?fun_rec:AST.expression_variable -> CST.expr -> AST.expr
val compile_program    : raise:(Errors.abs_error list,Main_warnings.all) Simple_utils.Trace.raise -> CST.ast -> AST.program

val decompile_expression : AST.expr -> CST.expr
val decompile_module    : AST.module_ -> CST.ast
