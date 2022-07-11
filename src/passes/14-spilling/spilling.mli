open Simple_utils.Trace
open Errors

module AST = Ast_aggregated
module Append_tree = Tree.Append
module Errors = Errors
open Mini_c

val compile_expression : raise:(spilling_error, _) raise -> AST.expression -> Mini_c.expression
val compile_type       : raise:(spilling_error, _) raise -> AST.type_expression -> Mini_c.type_expression
val decompile          : raise:(spilling_error, _) raise -> value -> AST.type_expression -> AST.expression
