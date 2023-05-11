open Main_errors
open Simple_utils.Trace
open Ligo_prim
module Location = Simple_utils.Location

type form =
  | Contract of
      { entrypoints : string list
      ; module_path : Module_var.t list
      }
  | View of
      { (* views declared as command line arguments if any *)
        command_line_views : string list option
      ; (* contract main function name *)
        contract_entry : Value_var.t
      ; contract_type : Self_ast_typed.Helpers.contract_type
      ; module_path : Module_var.t list
      }

let specific_passes ~raise ?(remove = true) cform prg =
  match cform with
  | Contract { entrypoints; module_path } ->
    Self_ast_typed.all_contract ~raise ~remove entrypoints module_path prg
  | View { command_line_views; contract_entry; module_path; contract_type } ->
    let prg =
      Self_ast_typed.all_view
        ~raise
        command_line_views
        contract_entry
        module_path
        contract_type
        prg
    in
    (contract_entry, contract_type), prg


let typecheck_with_signature
    ~raise
    ~(options : Compiler_options.t)
    ?(cform : form option)
    ?(context : Ast_typed.signature option)
    (p : Ast_core.program)
    : Ast_typed.program * Ast_typed.signature
  =
  let typed, signature =
    trace ~raise checking_tracer
    @@ Checking.type_program_with_signature ~options:options.middle_end ?env:context p
  in
  let typed =
    trace ~raise self_ast_typed_tracer
    @@ fun ~raise ->
    Self_ast_typed.all_program
      ~raise
      ~warn_unused_rec:options.middle_end.warn_unused_rec
      typed
  in
  let applied =
    match cform with
    | None -> typed
    | Some cform ->
      trace ~raise self_ast_typed_tracer
      @@ fun ~raise -> snd @@ specific_passes ~raise cform typed
  in
  applied, signature


let typecheck
    ~raise
    ~(options : Compiler_options.t)
    ?(cform : form option)
    ?(context : Ast_typed.signature option)
    (p : Ast_core.program)
    : Ast_typed.program
  =
  let typed =
    trace ~raise checking_tracer
    @@ Checking.type_program ~options:options.middle_end ?env:context p
  in
  let typed =
    trace ~raise self_ast_typed_tracer
    @@ fun ~raise ->
    Self_ast_typed.all_program
      ~raise
      ~warn_unused_rec:options.middle_end.warn_unused_rec
      typed
  in
  let applied =
    match cform with
    | None -> typed
    | Some cform ->
      trace ~raise self_ast_typed_tracer
      @@ fun ~raise -> snd @@ specific_passes ~raise cform typed
  in
  applied


let compile_expression
    ~raise
    ~(options : Compiler_options.t)
    ~(context : Ast_typed.signature)
    (expr : Ast_core.expression)
    : Ast_typed.expression
  =
  let typed =
    trace ~raise checking_tracer
    @@ Checking.type_expression ~options:options.middle_end ~env:context expr
  in
  let applied =
    trace ~raise self_ast_typed_tracer
    @@ Self_ast_typed.all_expression
         ~warn_unused_rec:options.middle_end.warn_unused_rec
         typed
  in
  applied


let apply (entry_point : Value_var.t) (param : Ast_core.expression) : Ast_core.expression =
  let entry_point_var : Ast_core.expression =
    { expression_content = Ast_core.E_variable entry_point
    ; sugar = None
    ; location = Virtual "generated entry-point variable"
    }
  in
  let applied : Ast_core.expression =
    { expression_content = Ast_core.E_application { lamb = entry_point_var; args = param }
    ; sugar = None
    ; location = Virtual "generated application"
    }
  in
  applied


let apply_twice
    (entry_point : string)
    (param1 : Ast_core.expression)
    (param2 : Ast_core.expression)
    : Ast_core.expression
  =
  let name = Value_var.of_input_var ~loc:Location.dummy entry_point in
  let entry_point_var : Ast_core.expression =
    { expression_content = Ast_core.E_variable name
    ; sugar = None
    ; location = Virtual "generated entry-point variable"
    }
  in
  let applied : Ast_core.expression =
    { expression_content =
        Ast_core.E_application { lamb = entry_point_var; args = param1 }
    ; sugar = None
    ; location = Virtual "generated application"
    }
  in
  let applied : Ast_core.expression =
    { expression_content = Ast_core.E_application { lamb = applied; args = param2 }
    ; sugar = None
    ; location = Virtual "generated application"
    }
  in
  applied


let list_declarations (m : Ast_core.program) : Value_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_core.declaration_content) with
      | D_value { binder; _ } -> Binder.get_var binder :: prev
      | D_irrefutable_match _ | D_type _ | D_module _ -> prev)
    ~init:[]
    m


let list_lhs_pattern_declarations (m : Ast_core.program) : Value_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      match Location.unwrap el with
      | D_irrefutable_match { pattern; _ } ->
        let binders = Ast_core.Pattern.binders pattern in
        let vars = List.map binders ~f:Binder.get_var in
        vars @ prev
      | D_value _ | D_type _ | D_module _ -> prev)
    ~init:[]
    m


let list_type_declarations (m : Ast_core.program) : Type_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_core.declaration_content) with
      | D_type { type_binder; type_attr; _ } when type_attr.public -> type_binder :: prev
      | D_value _ | D_irrefutable_match _ | D_module _ | D_type _ -> prev)
    ~init:[]
    m


let list_mod_declarations (m : Ast_core.program) : Module_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_core.declaration_content) with
      | D_module { module_binder; _ } -> module_binder :: prev
      | D_value _ | D_irrefutable_match _ | D_type _ -> prev)
    ~init:[]
    m
