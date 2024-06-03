module Trace = Simple_utils.Trace
module AST = Ast_expanded
module Append_tree = Errors.Tree.Append
module Errors = Errors
module Layout = Layout
open Mini_c

val compile_expression
  :  raise:(Errors.spilling_error, _) Trace.raise
  -> AST.expression
  -> Mini_c.expression

val compile_type
  :  raise:(Errors.spilling_error, _) Trace.raise
  -> AST.type_expression
  -> Mini_c.type_expression

val decompile
  :  raise:(Errors.spilling_error, _) Trace.raise
  -> value
  -> AST.type_expression
  -> AST.expression
