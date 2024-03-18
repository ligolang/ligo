let decompile_expression ~raise (e : Ast_typed.expression) : Ast_core.expression =
  Checking.untype_expression ~raise e
