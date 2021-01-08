open Trace
open Errors

module AST = Ast_typed
module Append_tree = Tree.Append
module Errors = Errors
open Mini_c


val compile_expression : ?module_env: AST.type_expression Compiler.SMap.t -> AST.expression -> (Mini_c.expression, spilling_error) result
val compile_module : AST.module_fully_typed -> (program, spilling_error) result

val decompile : value -> AST.type_expression -> (AST.expression , spilling_error) result
