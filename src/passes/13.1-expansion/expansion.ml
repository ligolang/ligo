let compile : Ast_aggregated.expression -> Ast_expanded.expression =
 fun e -> Compiler.compile_expression e


let decompile : Ast_expanded.expression -> Ast_aggregated.expression =
  Decompiler.decompile_expression


let compile_type_expression
    : Ast_aggregated.type_expression -> Ast_expanded.type_expression
  =
 fun x -> Compiler.compile_type x


let compile_type : Ast_aggregated.type_expression -> Ast_expanded.type_expression =
  Compiler.compile_type
