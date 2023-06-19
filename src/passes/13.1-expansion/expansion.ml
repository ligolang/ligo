let compile : Ast_aggregated.expression -> Ast_expanded.expression =
 fun e -> Compiler.compile_expression e


let decompile : Ast_expanded.expression -> Ast_aggregated.expression =
  Decompiler.decompile_expression
