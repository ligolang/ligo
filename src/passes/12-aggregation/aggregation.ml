module Errors = Errors

let compile_expression ~raise : Ast_typed.expression -> Ast_aggregated.expression =
 fun x -> Compiler.compile_expression ~raise Compiler.Data.empty_env x


let compile_type_expression ~raise
    : Ast_typed.type_expression -> Ast_aggregated.type_expression
  =
 fun x -> Compiler.compile_type ~raise x


let compile_program ~raise
    : Ast_typed.expression -> Ast_typed.program -> Ast_aggregated.program
  =
 fun hole program -> Compiler.compile ~raise Compiler.Data.empty hole program


let compile_type ~raise : Ast_typed.type_expression -> Ast_aggregated.type_expression =
  Compiler.compile_type ~raise


let decompile : Ast_aggregated.expression -> Ast_typed.expression = Decompiler.decompile
