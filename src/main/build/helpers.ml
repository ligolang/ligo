module Location = Simple_utils.Location
open Ligo_prim

let loc = Location.env

(* This pass turns 'sap_' prefixed outer type quantification into Singleton kind *)
let sap_for_all (b : Ast_core.type_expression option Binder.t) =
  let open Ast_core in
  let f t =
    match t.type_content with
    | T_for_all { ty_binder; type_; kind = _ } when not (Type_var.is_generated ty_binder)
      ->
      let name = Type_var.to_name_exn ty_binder in
      if String.is_prefix name ~prefix:"sap_"
      then (
        let type_content = T_for_all { ty_binder; type_; kind = Kind.Singleton } in
        { t with type_content })
      else t
    | _ -> t
  in
  let b = Binder.map (Option.map ~f) b in
  b


let internalize_core (ds : Ast_core.program) : Ast_core.program =
  let open Ast_core in
  let rec module_decl
      ({ module_binder; module_; module_attr; annotation } :
        (module_expr, _) Module_decl.t)
    =
    let module_ =
      match module_ with
      | { wrap_content = M_struct x; _ } ->
        { module_ with wrap_content = Module_expr.M_struct (module' x) }
      | _ -> module_
    in
    let module_attr = { module_attr with hidden = true } in
    Module_decl.{ module_binder; module_; module_attr; annotation }
  and value_decl (value_decl : _ Value_decl.t) =
    let binder = sap_for_all value_decl.binder in
    let var = Binder.get_var binder in
    let loc = Value_var.get_location var in
    let name = Value_var.to_name_exn var in
    let binder = Binder.set_var binder @@ Value_var.of_input_var ~loc name in
    let attr : ValueAttr.t =
      { value_decl.attr with inline = true; hidden = true; no_mutation = true }
    in
    Value_decl.{ value_decl with binder; attr }
  and type_decl (type_decl : _ Type_decl.t) =
    let type_attr : TypeOrModuleAttr.t = { type_decl.type_attr with hidden = true } in
    Type_decl.{ type_decl with type_attr }
  and pattern_decl (pattern_decl : _ Pattern_decl.t) =
    let attr : ValueAttr.t = { pattern_decl.attr with hidden = true } in
    Pattern_decl.{ pattern_decl with attr }
  and declaration : declaration -> declaration =
   fun decl ->
    Location.map
      (function
        | D_module module_decl' ->
          let module_decl' = module_decl module_decl' in
          D_module module_decl'
        | D_value value_decl' ->
          let value_decl' = value_decl value_decl' in
          D_value value_decl'
        | D_type type_decl' ->
          let type_decl' = type_decl type_decl' in
          D_type type_decl'
        | D_irrefutable_match pattern_decl' ->
          let pattern_decl' = pattern_decl pattern_decl' in
          D_irrefutable_match pattern_decl'
        | D_module_include x -> D_module_include x
        | D_signature signature' ->
          (* TODO *)
          D_signature signature'
        | D_import import ->
          (* Imports are hidden by default *)
          D_import import)
      decl
  and module' module_ = List.map ~f:declaration module_ in
  List.map ~f:declaration ds


(* [inject_declaration] [syntax] [module_] fetch expression argument passed through CLI options and inject a declaration `let cli_arg = [options.cli_expr_inj]`
         on top of an existing core program
*)
let inject_declaration ~options ~raise
    : Syntax_types.t -> Ast_core.program -> Ast_core.program
  =
 fun syntax prg ->
  let inject_arg_declaration arg =
    let open Ast_core in
    let expr = Ligo_compile.Utils.core_expression_string ~raise ~options syntax arg in
    let attr = ValueAttr.{ default_attributes with no_mutation = true; public = false } in
    let d =
      Location.wrap ~loc
      @@ D_value
           { binder = Binder.make (Value_var.of_input_var ~loc "cli_arg") None
           ; expr
           ; attr
           }
    in
    d :: prg
  in
  Option.value_map
    Compiler_options.(options.test_framework.cli_expr_inj)
    ~default:prg
    ~f:inject_arg_declaration


let process_imports ~f prg : Ast_core.program =
  prg |> Ast_core.Helpers.Declaration_mapper.map_module
  @@ fun decl ->
    let loc = decl.location in
    Location.wrap ~loc @@
    match decl.wrap_content with
    | Ast_core.D_import decl ->
      Ast_core.D_import
        (match decl with
        | Import_rename _ -> decl
        | Import_all_as { alias; module_str; import_attr; original_module_str } ->
          let module_str = f module_str in
          Import_all_as { alias; module_str; import_attr; original_module_str }
        | Import_selected { module_str; imported; import_attr; original_module_str } ->
          let module_str = f module_str in
          Import_selected { module_str; imported; import_attr; original_module_str })
    | x -> x

(* let add_module_aliases ~mangle imports c_unit : Ast_core.program = *)
(*   let make_decl BuildSystem.{ file_name; original_module; _ } = *)
(*     let loc = original_module.location in *)
(*     let alias = Location.unwrap original_module in *)
(*     let module_binder = Module_var.of_input_var ~loc alias in *)
(*     let module_attr = Ligo_prim.Type_or_module_attr.default_attributes in *)
(*     let annotation = None in *)
(*     let module_name = mangle file_name in *)
(*     let module_ = *)
(*       Location.wrap ~loc *)
(*       @@ Module_expr.M_variable (Module_var.of_input_var ~loc module_name) *)
(*     in *)
(*     Location.wrap ~loc *)
(*     @@ Ast_core.D_module { module_binder; module_attr; annotation; module_ } *)
(*   in *)
(*   List.map ~f:make_decl imports @ c_unit *)
