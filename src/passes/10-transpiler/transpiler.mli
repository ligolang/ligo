open Trace
open Errors

module AST = Ast_typed
module Append_tree = Tree.Append
module Errors = Errors
open Mini_c

val temp_unwrap_loc : 'a Location.wrap -> 'a

val transpile_annotated_expression : AST.expression -> (expression, transpiler_error) result

val transpile_program : AST.program -> (program, transpiler_error) result

val extract_constructor : value -> ( string * AST.type_expression ) Append_tree.t' -> ((string * value * AST.type_expression) , transpiler_error) result
val extract_tuple : value -> AST.type_expression Append_tree.t' -> ((value * AST.type_expression) list , transpiler_error) result
val extract_record : value -> ( string * AST.type_expression ) Append_tree.t' -> (( string * ( value * AST.type_expression)) list , transpiler_error) result

val untranspile : value -> AST.type_expression -> (AST.expression , transpiler_error) result