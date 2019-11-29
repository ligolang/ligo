open Trace

let compile (program : Ast_simplified.program) : (Ast_typed.program * Typer.Solver.state) result =
  let%bind (prog_typed , state) = Typer.type_program program in
  let () = Typer.Solver.discard_state state in
  ok @@ (prog_typed, state)

let compile_expression ?(env = Ast_typed.Environment.full_empty) ~(state : Typer.Solver.state) (ae : Ast_simplified.expression) : (Ast_typed.value * Typer.Solver.state) result =
  Typer.type_expression env state ae