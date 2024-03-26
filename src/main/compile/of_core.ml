open Main_errors
open Simple_utils.Trace
open Ligo_prim
module Location = Simple_utils.Location

let typecheck_with_signature
    ~raise
    ~(options : Compiler_options.t)
    ?(context : Ast_typed.signature option)
    ?(self_pass : bool = false)
    (p : Ast_core.program)
    : Ast_typed.program
  =
  let prg =
    trace ~raise checking_tracer
    @@ Checking.type_program ~options:options.middle_end ?env:context p
  in
  if self_pass
  then
    trace ~raise self_ast_typed_tracer
    @@ fun ~raise -> Self_ast_typed.all_program ~raise prg
  else prg


let typecheck
    ~raise
    ~(options : Compiler_options.t)
    ?(context : Ast_typed.signature option)
    (p : Ast_core.program)
    : Ast_typed.program
  =
  let typed =
    trace ~raise checking_tracer
    @@ Checking.type_program ~options:options.middle_end ?env:context p
  in
  trace ~raise self_ast_typed_tracer (Self_ast_typed.all_program typed)


let compile_expression
    ~raise
    ~(options : Compiler_options.t)
    ~(context : Ast_typed.signature)
    (expr : Ast_core.expression)
    : Ast_typed.expression
  =
  let typed =
    trace ~raise checking_tracer
    @@ Checking.type_expression ~options:options.middle_end ~env:context expr ~path:[]
  in
  typed


let compile_type_expression
    ~raise
    ~(options : Compiler_options.t)
    ~(context : Ast_typed.signature)
    (ty : Ast_core.type_expression)
    : Ast_typed.type_expression
  =
  trace ~raise checking_tracer
  @@ Checking.type_type_expression ~options:options.middle_end ~env:context ty ~path:[]


let apply (entry_point : Value_var.t) (param : Ast_core.expression) : Ast_core.expression =
  let entry_point_var : Ast_core.expression =
    { expression_content = Ast_core.E_variable entry_point
    ; location = Virtual "generated entry-point variable"
    }
  in
  let applied : Ast_core.expression =
    { expression_content = Ast_core.E_application { lamb = entry_point_var; args = param }
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
    ; location = Virtual "generated entry-point variable"
    }
  in
  let applied : Ast_core.expression =
    { expression_content =
        Ast_core.E_application { lamb = entry_point_var; args = param1 }
    ; location = Virtual "generated application"
    }
  in
  let applied : Ast_core.expression =
    { expression_content = Ast_core.E_application { lamb = applied; args = param2 }
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
      | D_irrefutable_match _
      | D_type _
      | D_module _
      | D_signature _
      | D_module_include _
      | D_import _ -> prev)
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
      | D_value _
      | D_type _
      | D_module _
      | D_signature _
      | D_module_include _
      | D_import _ -> prev)
    ~init:[]
    m


let list_type_declarations (m : Ast_core.program) : Type_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_core.declaration_content) with
      | D_type { type_binder; type_attr; _ } when type_attr.public -> type_binder :: prev
      | D_value _
      | D_irrefutable_match _
      | D_module _
      | D_type _
      | D_signature _
      | D_module_include _
      | D_import _ -> prev)
    ~init:[]
    m


let list_mod_declarations (m : Ast_core.program) : Module_var.t list =
  List.fold_left
    ~f:(fun prev el ->
      let open Location in
      match (el.wrap_content : Ast_core.declaration_content) with
      | D_module { module_binder; _ } -> module_binder :: prev
      | D_import { import_name; _ } ->
        (* CR: Is this correct? Don't we want to hide import bindings? *)
        import_name :: prev
      | D_value _ | D_irrefutable_match _ | D_type _ | D_signature _ | D_module_include _
        -> prev)
    ~init:[]
    m
