[@@@warning "-45"]

open Simple_utils.Trace

module CST = Cst.Cameligo
module AST = Ast_imperative
module Errors = Errors


val compile_expression : raise:(Errors.abs_error,Main_warnings.all) raise -> CST.expr -> AST.expr
val compile_program    : raise:(Errors.abs_error list,Main_warnings.all) raise -> CST.ast -> AST.program

val decompile_expression : AST.expr -> CST.expr
val decompile_module     : AST.module_ -> CST.ast
val decompile_pattern    : AST.type_expression AST.pattern -> CST.pattern