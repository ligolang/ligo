module AST = Ast_typed

type raise = (Errors.typer_error, Main_warnings.all) Simple_utils.Trace.raise

val check_anomalies
  :  raise:raise
  -> syntax:Syntax_types.t option
  -> loc:Simple_utils.Location.t
  -> (AST.type_expression Ligo_prim.Linear_pattern.t * AST.type_expression) list
  -> AST.type_expression
  -> unit
