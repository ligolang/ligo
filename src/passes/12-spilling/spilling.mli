open Trace
open Errors

module AST = Ast_typed
module Append_tree = Tree.Append
module Errors = Errors
open Mini_c

val compile_expression : AST.expression -> (expression, spilling_error) result
val compile_program : AST.program_fully_typed -> (program, spilling_error) result

val decompile : value -> AST.type_expression -> (AST.expression , spilling_error) result
