module Errors = Errors

let compile ~raise : Ast_aggregated.expression -> Ast_expanded.expression =
 fun e -> Compiler.compile_expression ~raise e


let decompile : Ast_expanded.expression -> Ast_aggregated.expression =
  Decompiler.decompile_expression


let compile_type_expression ~raise
    : Ast_aggregated.type_expression -> Ast_expanded.type_expression
  =
 fun x -> Compiler.compile_type ~raise x


let compile_type ~raise : Ast_aggregated.type_expression -> Ast_expanded.type_expression =
  Compiler.compile_type ~raise
