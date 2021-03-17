open Main_errors
open Trace

type form =
  | Contract of string
  | Env

let compile ~(typer_switch : Ast_typed.typer_switch) ~(init_env:Ast_typed.environment) (cform: form) (m : Ast_core.module_) : (Ast_typed.module_fully_typed * Ast_typed.environment * _ Typer.Solver.typer_state , _) result =
  let%bind (e, prog_typed , state) = trace typer_tracer @@ Typer.type_module typer_switch ~init_env m in
  let () = Typer.Solver.discard_state state in
  let%bind applied = trace self_ast_typed_tracer @@
    let%bind selfed = Self_ast_typed.all_module prog_typed in
    match cform with
    | Contract entrypoint -> Self_ast_typed.all_contract entrypoint selfed
    | Env -> ok selfed in
  ok @@ (applied, e, state)

let compile_expression ~(typer_switch : Ast_typed.typer_switch) ~(env : Ast_typed.environment) ~(state : _ Typer.Solver.typer_state) (e : Ast_core.expression)
    : (Ast_typed.expression * Ast_typed.environment * _ Typer.Solver.typer_state , _) result =
  let%bind (e,ae_typed,state) = trace typer_tracer @@ Typer.type_expression_subst typer_switch env state e in
  let%bind ae_typed' = trace self_ast_typed_tracer @@ Self_ast_typed.all_expression ae_typed in
  ok @@ (ae_typed', e, state)

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

let list_declarations (m : Ast_core.module_) : string list =
  List.fold_left
    (fun prev el ->
      let open Location in
      let open Ast_core in
      match (el.wrap_content : Ast_core.declaration) with
      | Declaration_constant {binder;_} -> (Var.to_name binder.var.wrap_content)::prev
      | _ -> prev)
    [] m

let list_type_declarations (m : Ast_core.module_) : string list =
  List.fold_left
    (fun prev el ->
      let open Location in
      let open Ast_core in
      match (el.wrap_content : Ast_core.declaration) with
      | Declaration_type {type_binder;_} -> (Var.to_name type_binder)::prev
      | _ -> prev)
    [] m

let list_mod_declarations (m : Ast_core.module_) : string list =
  List.fold_left
    (fun prev el ->
      let open Location in
      let open Ast_core in
      match (el.wrap_content : Ast_core.declaration) with
      | Declaration_module {module_binder;_} -> (module_binder)::prev
      | Module_alias {alias;_} -> (alias)::prev
      | _ -> prev)
    [] m

let evaluate_type ~(typer_switch : Ast_typed.typer_switch) (env : Ast_typed.Environment.t) (t: Ast_core.type_expression) = trace typer_tracer @@ Typer.evaluate_type typer_switch env t
