open Core
open Common
open Lsp_helpers
open Ligo_prim
module Row = Row.With_optional_layout

(** Recursively resolves a projection path, taking the current field's path and its type.

    We look into the record's known fields and check for the presence of the current
    field. If resolved, the algorithm recursively looks up this field's type and proceeds
    to try to resolve it as a record with the remainder of the field path. Once there are
    no more fields, we return the current record. *)
let rec find_record_from_path
    (struct_type : Ast_core.type_expression)
    (field_path : string option list)
    (definitions : Def.definitions)
    : Ast_core.row option
  =
  let rec find_record_in_core
      : Ast_core.type_content -> (Ast_core.row * Type_var.t list) option
    = function
    | T_record row -> Some (row, [])
    | T_variable var ->
      Option.bind
        (Def.find_map definitions ~f:(function
            | Type tdef ->
              if Ligo_prim.Type_var.is_name var tdef.name then Some tdef else None
            | Variable _ | Module _ | Label _ -> None))
        ~f:(fun tdef ->
          match tdef.content with
          | None -> None
          | Some content -> find_record_in_core content.type_content)
    | T_for_all { type_; _ } -> find_record_in_core type_.type_content
    | T_abstraction { type_; ty_binder; _ } ->
      Option.map ~f:(fun (record, ty_binders) -> record, ty_binder :: ty_binders)
      @@ find_record_in_core type_.type_content
    | T_app { type_operator; arguments } ->
      (match find_record_in_core @@ T_variable type_operator.element with
      | Some (row, ty_binders) ->
        let new_row =
          match List.zip ty_binders arguments with
          | Unequal_lengths -> row
          | Ok var_value_pairs ->
            Row.map (Ast_core.Helpers.subst_type var_value_pairs) row
        in
        Some (new_row, [])
      | None -> None)
    | T_sum _
    | T_arrow _
    | T_constant _
    | T_singleton _
    | T_module_accessor _
    | T_contract_parameter _ -> None
  in
  let open Option.Monad_infix in
  find_record_in_core struct_type.type_content
  >>= fun (struct_type, _) ->
  match field_path with
  | [] -> Some struct_type
  | selection :: field_path ->
    selection
    >>= fun name ->
    Ligo_prim.(Record.find_opt struct_type.fields (Label.of_string name))
    >>= fun struct_type -> find_record_from_path struct_type field_path definitions

(** Helper function to create completion item from record fields. *)
let core_record_to_completion_items ~syntax (row : Ast_core.row) : CompletionItem.t list =
  List.map (Map.to_alist row.fields) ~f:(fun (Label (label, _), texp) ->
      let detail = Pretty.show_type ~syntax texp in
      let sortText = completion_context_priority Record_field in
      CompletionItem.create ~label ~kind:CompletionItemKind.Field ~detail ~sortText ())

(** Provides completions for the fields from the provided reference position of a record
    variable name. *)
let projection_impl
    ~(normalize : Path.normalization)
    ({ syntax; path; definitions; _ } : _ Common.input)
    (struct_pos : Position.t)
    (proj_fields_before_cursor : string option list)
    : CompletionItem.t list option
  =
  match Def.get_definition ~normalize struct_pos path definitions with
  | Some (Variable { t; _ }) ->
    let mk_completions t =
      Option.map ~f:(core_record_to_completion_items ~syntax)
      @@ find_record_from_path t proj_fields_before_cursor definitions
    in
    (match t with
    | Core t -> mk_completions t
    | Resolved t ->
      let%bind.Option t =
        try
          Simple_utils.Trace.to_option ~fast_fail:false
          @@ Checking.untype_type_expression t
        with
        | _exn -> None
      in
      mk_completions t
    | Unresolved -> None)
  | None | Some (Type _ | Module _ | Label _) -> None
