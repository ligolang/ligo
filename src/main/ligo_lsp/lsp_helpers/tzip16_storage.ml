open Scopes.Types

(** Detects all contracts appearing in the module *)
let rec get_all_contract_sigs (env : Ast_typed.signature) : Ast_typed.contract_sig list =
  let this_sig =
    match env.sig_sort with
    | Ss_contract s -> Some s
    | Ss_module -> None
  in
  let child_sigs =
    List.concat_map env.sig_items ~f:(fun sig_item ->
        match sig_item.wrap_content with
        | S_module (_, s) | S_module_type (_, s) -> get_all_contract_sigs s
        | S_value _ | S_type _ | S_type_var _ -> [])
  in
  Option.to_list this_sig @ child_sigs


type var_declaration_info =
  { binders : Ast_typed.type_expression Ligo_prim.Binder.t list
  ; has_tzip16_compatible_attr : bool
  }

let rec get_all_variables_info (prg : Ast_typed.declaration list)
    : var_declaration_info Sequence.t
  =
  Sequence.concat_map (Sequence.of_list prg) ~f:(fun decl ->
      match decl.wrap_content with
      | D_value { binder; expr = _; attr } ->
        Sequence.singleton
          { binders = [ binder ]; has_tzip16_compatible_attr = attr.tzip16_compatible }
      | D_irrefutable_match { pattern; expr = _; attr } ->
        Sequence.singleton
          { binders = Ast_core.Pattern.binders pattern
          ; has_tzip16_compatible_attr = attr.tzip16_compatible
          }
      | D_module { module_; _ } ->
        (match module_.module_content with
        | M_struct decls -> get_all_variables_info decls
        | M_variable _ | M_module_path _ -> Sequence.empty)
      | D_module_include _ | D_type _ | D_signature _ -> Sequence.empty)


(** Makes best effort to tell whether this type is some storage type or not. *)
let is_potential_storage_type
    (type_expr : Ast_typed.type_expression)
    (known_storage_types : Ast_typed.type_expression list)
    : bool
  =
  let has_metadata =
    match type_expr.type_content with
    | T_record r -> Map.mem r.fields (Ligo_prim.Label.create "metadata")
    | _ -> false
  in
  let has_known_type () =
    List.mem known_storage_types type_expr ~equal:Ast_typed.equal_ty_expr
  in
  has_metadata && has_known_type ()


let location_is_from_file (path : Path.t) : Location.t -> bool = function
  | Virtual _ -> false
  | File reg -> String.equal (Path.to_string path) reg#file


(** Extract all potential storages from a file *)
let vars_to_mark_as_tzip16_compatible
    (cur_file : Path.t)
    (env : Ast_typed.signature)
    (prg : Ast_typed.declaration list)
    : Ast_typed.expression_variable list
  =
  let all_storage_types =
    List.map (get_all_contract_sigs env) ~f:(fun contract_sig -> contract_sig.storage)
  in
  get_all_variables_info prg
  |> Sequence.filter_map ~f:(fun decl_info ->
         match decl_info.binders with
         | [] -> None
         | _ :: _ :: _ ->
           (* Multiple binders may mean that several variables are declared
              at the same line, and such case is hard to process when "Add
              TZIP-16-compatible attribute" code lens will be invoked. Let's
              skip this rare case *)
           None
         | [ binder ] ->
           if (not decl_info.has_tzip16_compatible_attr)
              && location_is_from_file
                   cur_file
                   (Ligo_prim.Value_var.get_location binder.var)
              && is_potential_storage_type binder.ascr all_storage_types
           then Some binder.var
           else None)
  |> Sequence.to_list
