open Main_errors
open Simple_utils.Trace
module Var = Simple_utils.Var
module Location = Simple_utils.Location

type form =
  | Contract of string
  | View of string * string
  | Env

let infer ~raise:_ ~(options: Compiler_options.t) (m : Ast_core.module_) =
  match options.infer with
    | true  -> m
    | false -> m

let typecheck ~raise ~add_warning ~(options: Compiler_options.t) (cform : form) (m : Ast_core.module_) : Ast_typed.program = 
  let typed = trace ~raise checking_tracer @@ Checking.type_program ~test:options.test ~env:options.init_env ~protocol_version:options.protocol_version m in
  let applied = trace ~raise self_ast_typed_tracer @@
    fun ~raise ->
    let selfed = Self_ast_typed.all_module ~raise ~add_warning typed in
    match cform with
    | Contract entrypoint -> Self_ast_typed.all_contract ~raise entrypoint selfed
    | View (view_name,main_name) -> Self_ast_typed.all_view ~raise view_name main_name selfed
    | Env -> selfed in
  applied

let compile_expression ~raise ~(options: Compiler_options.t) ~(init_prog : Ast_typed.program) (expr : Ast_core.expression)
    : Ast_typed.expression =
  let inferred = match options.infer with
    | true  -> expr
    | false -> expr
  in
  let env = Environment.append init_prog options.init_env in
  let typed = trace ~raise checking_tracer @@ Checking.type_expression ~test:false ~protocol_version:options.protocol_version ~env inferred in
  let applied = trace ~raise self_ast_typed_tracer @@ Self_ast_typed.all_expression typed in
  applied

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

