[@@@warning "-45"]

module CST = Cst.Reasonligo
module AST = Ast_imperative
module Errors = Errors
open Ligo_prim

val compile_expression
  :  raise:(Errors.abs_error, Main_warnings.all) Simple_utils.Trace.raise
  -> ?fun_rec:Value_var.t
  -> CST.expr
  -> AST.expr

val compile_program
  :  raise:(Errors.abs_error list, Main_warnings.all) Simple_utils.Trace.raise
  -> CST.ast
  -> AST.program

val decompile_expression : AST.expr -> CST.expr
val decompile_program : AST.program -> CST.ast
val decompile_pattern_to_string : AST.type_expression option AST.Pattern.t -> string
