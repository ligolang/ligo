open Types

(** Given some signature:

    {[
      module type I = sig
        (* In no particular order: *)
        include sig ... end (* let's call it I1 *)
        include sig ... end (* I2 *)
        ...
        include I3
        include I4
        ...
      end
    ]}

    This function will inline every signature RECURSIVELY into [I] so it will look like
    this:

    {[
      module type I = sig
        (* contents of I1 and all its nested includes *)
        (* contents of I2 and all its nested includes *)
        include I3
        include I4
      end
    ]}

    Note that this function will NOT visit nested modules or module types. *)
let flatten_includes : Ast_core.signature -> Ast_core.signature =
 fun { items } ->
  let rec go acc =
    List.fold_right ~init:acc ~f:(fun item acc ->
        match Location.unwrap item with
        | Ast_core.S_include sig_expr ->
          (match Location.unwrap sig_expr with
          | S_sig { items } -> go acc items
          | S_path _mod_path ->
            let loc = Location.get_location item in
            Location.wrap ~loc (Ast_core.S_include sig_expr) :: acc)
        | S_value _ | S_type _ | S_type_var _ | S_module _ | S_module_type _ ->
          item :: acc)
  in
  { items = go [] items }


let map_core_type_content_in_type_expression
    :  (Ast_core.type_content -> Ast_core.type_content) -> Ast_core.type_expression
    -> Ast_core.type_expression
  =
 fun f ->
  let open Ligo_prim in
  let rec type_content : Ast_core.type_content -> Ast_core.type_content = function
    | T_sum (t, label) -> T_sum (row t, label)
    | T_record t -> T_record (row t)
    | T_arrow { type1; type2; param_names } ->
      T_arrow
        { type1 = type_expression type1; type2 = type_expression type2; param_names }
    | T_app { type_operator; arguments } ->
      T_app { type_operator; arguments = List.map arguments ~f:type_expression }
    | T_abstraction { ty_binder; kind; type_ } ->
      T_abstraction { ty_binder; kind; type_ = type_expression type_ }
    | T_for_all { ty_binder; kind; type_ } ->
      T_for_all { ty_binder; kind; type_ = type_expression type_ }
    | ( T_variable _
      | T_constant _
      | T_contract_parameter _
      | T_module_accessor _
      | T_singleton _ ) as t -> t
  and row : Ast_core.row -> Ast_core.row =
   fun { fields; layout } -> { fields = Label.Map.map fields ~f:type_expression; layout }
  and type_expression : Ast_core.type_expression -> Ast_core.type_expression =
   fun { type_content = t; location } -> { type_content = f @@ type_content t; location }
  in
  type_expression


let map_core_type_expression_module_path
    :  (Ligo_prim.Module_var.t list -> Ligo_prim.Module_var.t list)
    -> (Ligo_prim.Module_var.t List.Ne.t -> Ligo_prim.Module_var.t List.Ne.t)
    -> Ast_core.type_expression -> Ast_core.type_expression
  =
 fun f_list f_ne_list ->
  let module_access : 'a Ligo_prim.Module_access.t -> 'a Ligo_prim.Module_access.t =
   fun { module_path; element } -> { module_path = f_list module_path; element }
  in
  map_core_type_content_in_type_expression (function
      | T_contract_parameter m -> T_contract_parameter (f_ne_list m)
      | T_app { type_operator; arguments } ->
        T_app { type_operator = module_access type_operator; arguments }
      | T_module_accessor t ->
        let module_access = module_access t in
        (match module_access.module_path with
        | [] -> T_variable t.element
        | _ :: _ -> T_module_accessor module_access)
      | ( T_sum _
        | T_record _
        | T_arrow _
        | T_abstraction _
        | T_for_all _
        | T_variable _
        | T_constant _
        | T_singleton _ ) as t -> t)


let map_typed_type_expression_module_path
    :  (Ligo_prim.Module_var.t list -> Ligo_prim.Module_var.t list)
    -> Ast_typed.type_expression -> Ast_typed.type_expression
  =
 fun f ->
  let open Ligo_prim in
  let rec type_content : Ast_typed.type_content -> Ast_typed.type_content = function
    | T_constant { language; injection; parameters } ->
      T_constant
        { language; injection; parameters = List.map parameters ~f:type_expression }
    | T_sum (t, orig_label) -> T_sum (row t, orig_label)
    | T_record t -> T_record (row t)
    | T_arrow { type1; type2; param_names } ->
      T_arrow
        { type1 = type_expression type1; type2 = type_expression type2; param_names }
    | T_abstraction { ty_binder; kind; type_ } ->
      T_abstraction { ty_binder; kind; type_ = type_expression type_ }
    | T_for_all { ty_binder; kind; type_ } ->
      T_for_all { ty_binder; kind; type_ = type_expression type_ }
    | (T_variable _ | T_singleton _) as t -> t
  and row : Ast_typed.row -> Ast_typed.row =
   fun { fields; layout } -> { fields = Label.Map.map fields ~f:type_expression; layout }
  and type_expression : Ast_typed.type_expression -> Ast_typed.type_expression =
   fun { type_content = t; orig_var; location } ->
    { type_content = type_content t
    ; orig_var = Option.map orig_var ~f:(Tuple2.map_fst ~f)
    ; location
    }
  in
  type_expression


let map_core_signature_module_path
    :  (Ligo_prim.Module_var.t list -> Ligo_prim.Module_var.t list)
    -> (Ligo_prim.Module_var.t List.Ne.t -> Ligo_prim.Module_var.t List.Ne.t)
    -> Ast_core.signature -> Ast_core.signature
  =
 fun f_list f_ne_list ->
  let type_expression = map_core_type_expression_module_path f_list f_ne_list in
  let rec sig_item_content : Ast_core.sig_item_content -> Ast_core.sig_item_content
    = function
    | S_value (binder, ty_expr, attr) -> S_value (binder, type_expression ty_expr, attr)
    | S_type (binder, ty_expr, attr) -> S_type (binder, type_expression ty_expr, attr)
    | S_type_var _ as item -> item
    | S_module (binder, sig') -> S_module (binder, signature sig')
    | S_module_type (binder, sig') -> S_module_type (binder, signature sig')
    | S_include sig_expr -> S_include (signature_expr sig_expr)
  and sig_item : Ast_core.sig_item -> Ast_core.sig_item =
   fun { wrap_content; location } ->
    { wrap_content = sig_item_content wrap_content; location }
  and signature : Ast_core.signature -> Ast_core.signature =
   fun { items } -> { items = List.map items ~f:sig_item }
  and signature_expr : Ast_core.signature_expr -> Ast_core.signature_expr =
   fun { wrap_content; location } ->
    { wrap_content = signature_content wrap_content; location }
  and signature_content : Ast_core.signature_content -> Ast_core.signature_content
    = function
    | S_sig sig' -> S_sig (signature sig')
    | S_path path -> S_path (f_ne_list path)
  in
  signature
