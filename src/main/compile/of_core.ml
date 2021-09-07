open Main_errors
open Trace

type form =
  | Contract of string
  | Env

let infer ~raise ~(options: Compiler_options.t) (m : Ast_core.module_) =
  let env_inf = Checking.decompile_env options.init_env in
  match options.infer with
    | true  -> let (_,e,_,_) = trace ~raise inference_tracer @@ Inference.type_module ~init_env:env_inf m in e
    | false -> m

let typecheck ~raise ~add_warning ~(options: Compiler_options.t) (cform : form) (m : Ast_core.module_) : Ast_typed.module_fully_typed * Ast_typed.environment =
  let e,typed = trace ~raise checking_tracer @@ Checking.type_module ~test:options.test ~init_env:options.init_env m in
  let applied = trace ~raise self_ast_typed_tracer @@
    fun ~raise ->
    let selfed = Self_ast_typed.all_module ~raise ~add_warning typed in
    match cform with
    | Contract entrypoint -> Self_ast_typed.all_contract ~raise entrypoint selfed
    | Env -> selfed in
  (applied,e)

let compile_expression ~raise ?(infer = false) ~(env : Ast_typed.environment) (e : Ast_core.expression)
    : Ast_typed.expression * Ast_typed.environment =
  let env_inf = Checking.decompile_env env in
  let inferred = match infer with
    | true  -> let (_,e,_,_) =
      trace ~raise inference_tracer @@ Inference.type_expression_subst env_inf Inference.Solver.initial_state e in
      e
    | false -> e
  in
  let e,typed = trace ~raise checking_tracer @@ Checking.type_expression ~test:false env inferred in
  let applied = trace ~raise self_ast_typed_tracer @@ Self_ast_typed.all_expression typed in
  (applied, e)

let apply (entry_point : string) (param : Ast_core.expression) : Ast_core.expression  =
  let name = Location.wrap @@ Var.of_name entry_point in
  let entry_point_var : Ast_core.expression =
    { expression_content  = Ast_core.E_variable name ;
      sugar    = None ;
      location = Virtual "generated entry-point variable" } in
  let applied : Ast_core.expression =
    { expression_content  = Ast_core.E_application {lamb=entry_point_var; args=param} ;
      sugar    = None ;
      location = Virtual "generated application" } in
  applied

let list_declarations (m : Ast_core.module_) : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      let open Ast_core in
      match (el.wrap_content : Ast_core.declaration) with
      | Declaration_constant {binder;_} -> (Var.to_name binder.var.wrap_content)::prev
      | _ -> prev)
    ~init:[] m

let list_type_declarations (m : Ast_core.module_) : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      let open Ast_core in
      match (el.wrap_content : Ast_core.declaration) with
      | Declaration_type {type_binder;_} -> (Var.to_name type_binder)::prev
      | _ -> prev)
    ~init:[] m

let list_mod_declarations (m : Ast_core.module_) : string list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      let open Ast_core in
      match (el.wrap_content : Ast_core.declaration) with
      | Declaration_module {module_binder;_} -> (module_binder)::prev
      | Module_alias {alias;_} -> (alias)::prev
      | _ -> prev)
    ~init:[] m

let evaluate_type ~raise (env : Ast_typed.Environment.t) (t: Ast_core.type_expression) = trace ~raise checking_tracer @@ Checking.evaluate_type env t
