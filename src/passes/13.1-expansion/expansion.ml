let compile : Ast_aggregated.expression -> Ast_expanded.expression =
 fun e ->
  let e = Compiler.compile_expression e in
  let e = Pattern_simpl.peephole_expression e in
  e


let decompile : Ast_expanded.expression -> Ast_aggregated.expression =
  Decompiler.decompile_expression
