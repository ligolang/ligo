open Main_errors
open Trace

type form = 
  | Contract of string
  | Env

let compile (cform: form) (program : Ast_core.program) : (Ast_typed.program * _ Typesystem.Solver_types.typer_state , _) result =
  let%bind (prog_typed , state) = trace typer_tracer @@ Typer.type_program program in
  let () = Typer.Solver.discard_state state in
  let%bind applied = trace self_ast_typed_tracer @@
    let%bind selfed = Self_ast_typed.all_program prog_typed in
    match cform with
    | Contract entrypoint -> Self_ast_typed.all_contract entrypoint selfed
    | Env -> ok selfed in
  ok @@ (applied, state)

let compile_expression ?(env = Ast_typed.Environment.empty) ~(state : _ Typesystem.Solver_types.typer_state) (e : Ast_core.expression)
    : (Ast_typed.expression * _ Typesystem.Solver_types.typer_state , _) result =
  let%bind (ae_typed,state) = trace typer_tracer @@ Typer.type_expression_subst env state e in
  let%bind ae_typed' = trace self_ast_typed_tracer @@ Self_ast_typed.all_expression ae_typed in
  ok @@ (ae_typed',state)

let apply (entry_point : string) (param : Ast_core.expression) : (Ast_core.expression , _) result =
  let name = Location.wrap @@ Var.of_name entry_point in
  let entry_point_var : Ast_core.expression =
    { content  = Ast_core.E_variable name ;
      sugar    = None ;
      location = Virtual "generated entry-point variable" } in
  let applied : Ast_core.expression = 
    { content  = Ast_core.E_application {lamb=entry_point_var; args=param} ;
      sugar    = None ;
      location = Virtual "generated application" } in
  ok applied

let list_declarations (program : Ast_core.program) : string list =
  List.fold_left
    (fun prev el -> 
      let open Location in
      let open Ast_core in
      match el.wrap_content with
      | Declaration_constant {binder;_} -> (Var.to_name binder.wrap_content)::prev
      | _ -> prev) 
    [] program

let evaluate_type (env : Ast_typed.Environment.t) (t: Ast_core.type_expression) = trace typer_tracer @@ Typer.evaluate_type env t