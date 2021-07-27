
let decompile (m : Ast_typed.module_fully_typed) : Ast_core.module_  =
  Checking.untype_module_fully_typed m
