open Trace

let compile (program : Ast_simplified.program) : (Ast_typed.program * Typer.Solver.state) result =
  let%bind (prog_typed , state) = Typer.type_program program in
  let () = Typer.Solver.discard_state state in
  ok @@ (prog_typed, state)

let compile_expression ?(env = Ast_typed.Environment.full_empty) ~(state : Typer.Solver.state) (ae : Ast_simplified.expression)
    : (Ast_typed.expression * Typer.Solver.state) result =
  let () = Typer.Solver.discard_state state in
  Typer.type_expression_subst env state ae

let apply (entry_point : string) (param : Ast_simplified.expression) : Ast_simplified.expression result =
  let name = Var.of_name entry_point in
  let entry_point_var : Ast_simplified.expression =
    { expression_content = Ast_simplified.E_variable name ;
      location = Virtual "generated entry-point variable" } in
  let applied : Ast_simplified.expression = 
    { expression_content = Ast_simplified.E_application {expr1=entry_point_var; expr2=param} ;
      location = Virtual "generated application" } in
  ok applied

let pretty_print formatter (program : Ast_simplified.program) = 
  Ast_simplified.PP.program formatter program

let list_declarations (program : Ast_simplified.program) : string list =
  List.fold_left
    (fun prev el -> 
      let open Location in
      let open Ast_simplified in
      match el.wrap_content with
      | Declaration_constant (var,_,_,_) -> (Var.to_name var)::prev
      | _ -> prev) 
    [] program
