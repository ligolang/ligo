[@@@warning "-45"]

open Simple_utils.Trace

module CST = Cst.Cameligo
module AST = Ast_imperative
module Errors = Errors


val compile_expression : raise:(Errors.abs_error,Main_warnings.all) raise -> CST.expr -> AST.expr
val compile_module     : raise:(Errors.abs_error,Main_warnings.all) raise -> CST.ast -> AST.module_

val decompile_expression : AST.expr -> CST.expr
val decompile_module     : AST.module_ -> CST.ast
