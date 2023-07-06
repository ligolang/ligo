module Errors = Errors

let compile_expression ~raise : Ast_typed.expression -> Ast_aggregated.expression =
 fun x ->
  ignore raise;
  Compiler.compile_expression Compiler.Data.empty_env x


let compile_program ~raise
    : Ast_typed.expression -> Ast_typed.program -> Ast_aggregated.program
  =
 fun hole program ->
  ignore raise;
  Compiler.compile Compiler.Data.empty hole program


let decompile : Ast_aggregated.expression -> Ast_typed.expression = Decompiler.decompile
