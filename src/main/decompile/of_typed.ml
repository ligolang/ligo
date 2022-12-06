let decompile_expression (e : Ast_typed.expression) : Ast_core.expression =
  Checking.untype_expression e
