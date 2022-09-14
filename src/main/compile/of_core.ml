open Main_errors
open Simple_utils.Trace
open Ligo_prim
module Location = Simple_utils.Location

type form =
  | Contract of Value_var.t
  | View of {
      command_line_views : string list option ;      (* views declared as command line arguments if any *)
      contract_entry : Value_var.t (* contract main function name                     *)
    }
  | Env

let specific_passes ~raise cform prg =
  match cform with
  | Contract entrypoint -> Self_ast_typed.all_contract ~raise entrypoint prg
  | View { command_line_views ; contract_entry } -> Self_ast_typed.all_view ~raise command_line_views contract_entry prg
  | Env -> prg

let typecheck ~raise ~(options: Compiler_options.t) (cform : form) (p : Ast_core.program) : Ast_typed.program =
  let typed = trace ~raise checking_tracer @@ Checking.type_program ~options:options.middle_end ~env:options.middle_end.init_env p in
  let applied = trace ~raise self_ast_typed_tracer @@
    fun ~raise ->
    let selfed = Self_ast_typed.all_program ~raise ~warn_unused_rec:options.middle_end.warn_unused_rec typed in
    specific_passes ~raise cform selfed
  in
  applied

let compile_expression ~raise ~(options: Compiler_options.t) ~(init_prog : Ast_typed.program) (expr : Ast_core.expression)
    : Ast_typed.expression =
  let Compiler_options.{ init_env ; _ } = options.middle_end in
  let env = Environment.append init_prog init_env in

  let typed = trace ~raise checking_tracer @@ Checking.type_expression ~options:options.middle_end ~env expr in
  let applied = trace ~raise self_ast_typed_tracer
    @@ Self_ast_typed.all_expression ~warn_unused_rec:options.middle_end.warn_unused_rec typed in
  applied

let apply (entry_point : string) (param : Ast_core.expression) : Ast_core.expression  =
  let name = Value_var.of_input_var entry_point in
  let entry_point_var : Ast_core.expression =
    { expression_content  = Ast_core.E_variable name ;
      sugar    = None ;
      location = Virtual "generated entry-point variable" } in
  let applied : Ast_core.expression =
    { expression_content  = Ast_core.E_application {lamb=entry_point_var; args=param} ;
      sugar    = None ;
      location = Virtual "generated application" } in
  applied

let list_declarations (m : Ast_core.program) : Value_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_core.declaration_content) with
      | D_value {binder;_} -> (Binder.get_var binder)::prev
      | _ -> prev)
    ~init:[] m

let list_type_declarations (m : Ast_core.program) : Type_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_core.declaration_content) with
      | D_type {type_binder;type_attr;_} when type_attr.public -> type_binder::prev
      | _ -> prev)
    ~init:[] m

let list_mod_declarations (m : Ast_core.program) : Module_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_core.declaration_content) with
      | D_module {module_binder;_} -> module_binder::prev
      | _ -> prev)
    ~init:[] m

