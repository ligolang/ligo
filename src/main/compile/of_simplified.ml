open Trace

let compile (program : Ast_simplified.program) : (Ast_typed.program * Typer.Solver.state) result =
  let%bind (prog_typed , state) = Typer.type_program program in
  let () = Typer.Solver.discard_state state in
  ok @@ (prog_typed, state)

let compile_expression ?(env = Ast_typed.Environment.full_empty) ~(state : Typer.Solver.state) (ae : Ast_simplified.expression)
    : (Ast_typed.value * Typer.Solver.state) result =
  let () = Typer.Solver.discard_state state in
  Typer.type_expression_subst env state ae

let apply (entry_point : string) (param : Ast_simplified.expression) : Ast_simplified.expression result =
  let name = Var.of_name entry_point in
  let entry_point_var : Ast_simplified.expression =
    { expression = Ast_simplified.E_variable name ;
      location = Virtual "generated entry-point variable" } in
  let applied : Ast_simplified.expression =
    { expression = Ast_simplified.E_application (entry_point_var, param) ;
      location = Virtual "generated application" } in
  ok applied

let pretty_print formatter (program : Ast_simplified.program) = 
  Ast_simplified.PP.program formatter program