open Trace

type form = 
  | Contract of string
  | Env

let compile (cform: form) (program : Ast_core.program) : (Ast_typed.program * Typesystem.Solver_types.typer_state) result =
  let%bind (prog_typed , state) = Typer.type_program program in
  let () = Typer.Solver.discard_state state in
  let%bind applied = Self_ast_typed.all_program prog_typed in
  let%bind applied' = match cform with
    | Contract entrypoint -> Self_ast_typed.all_contract entrypoint applied
    | Env -> ok applied in
  ok @@ (applied', state)

let compile_expression ?(env = Ast_typed.Environment.empty) ~(state : Typesystem.Solver_types.typer_state) (e : Ast_core.expression)
    : (Ast_typed.expression * Typesystem.Solver_types.typer_state) result =
  let%bind (ae_typed,state) = Typer.type_expression_subst env state e in
  let () = Typer.Solver.discard_state state in
  let%bind ae_typed' = Self_ast_typed.all_expression ae_typed in
  ok @@ (ae_typed',state)

let apply (entry_point : string) (param : Ast_core.expression) : Ast_core.expression result =
  let name = Var.of_name entry_point in
  let entry_point_var : Ast_core.expression =
    { expression_content = Ast_core.E_variable name ;
      location = Virtual "generated entry-point variable" } in
  let applied : Ast_core.expression = 
    { expression_content = Ast_core.E_application {lamb=entry_point_var; args=param} ;
      location = Virtual "generated application" } in
  ok applied

let pretty_print formatter (program : Ast_core.program) = 
  Ast_core.PP.program formatter program

let list_declarations (program : Ast_core.program) : string list =
  List.fold_left
    (fun prev el -> 
      let open Location in
      let open Ast_core in
      match el.wrap_content with
      | Declaration_constant (var,_,_,_) -> (Var.to_name var)::prev
      | _ -> prev) 
    [] program
