[@@@warning "-45"]


module CST = Cst.Jsligo
module AST = Ast_imperative
module Errors = Errors

val compile_expression : add_warning:(Main_warnings.all -> unit) -> raise:Errors.abs_error Simple_utils.Trace.raise -> CST.expr -> AST.expr
val compile_module     : add_warning:(Main_warnings.all -> unit) -> raise:Errors.abs_error Simple_utils.Trace.raise -> CST.ast -> AST.module_

val decompile_expression: AST.expr -> CST.expr list
val decompile_module: AST.module_ -> CST.ast
