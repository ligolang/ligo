open Common
open Lsp_helpers

let rec find_record_from_path
    (struct_type : Ast_core.type_expression)
    (field_path : string option list)
    (definitions : Def.t list)
    : Ast_core.row option
  =
  let rec find_record_in_core : Ast_core.type_content -> Ast_core.row option = function
    | T_record row -> Some row
    | T_variable var ->
      Option.bind
        (List.find_map definitions ~f:(function
            | Type tdef ->
              if Ligo_prim.Type_var.is_name var tdef.name then Some tdef else None
            | Variable _ | Module _ -> None))
        ~f:(fun tdef ->
          match tdef.content with
          | None -> None
          | Some content -> find_record_in_core content.type_content)
    | _ -> None
  in
  let open Option.Monad_infix in
  find_record_in_core struct_type.type_content
  >>= fun struct_type ->
  match field_path with
  | [] -> Some struct_type
  | selection :: field_path ->
    selection
    >>= fun name ->
    Ligo_prim.(Record.find_opt struct_type.fields (Label.of_string name))
    >>= fun struct_type -> find_record_from_path struct_type field_path definitions


(* Recursively resolve a projection path. This is done with
   [find_record_from_path], which takes the current field's path and its type.
   We look into the record's known fields and check for the presence of the
   current field. If resolved, the algorithm recursively looks up this field's
   type and proceeds to try to resolve it as a record with the remainder of
   the field path. Once there is no more fields, we return the current record. *)
let core_record_to_completion_items ~syntax (row : Ast_core.row) : CompletionItem.t list =
  List.map (Map.to_alist row.fields) ~f:(fun (Label label, texp) ->
      let detail = Pretty.show_type ~syntax texp in
      let sortText = completion_context_priority Record_field in
      CompletionItem.create ~label ~kind:CompletionItemKind.Field ~detail ~sortText ())


let projection_impl
    ({ syntax; path; definitions; _ } : _ Common.input)
    (struct_pos : Position.t)
    (proj_fields_before_cursor : string option list)
    : CompletionItem.t list option
  =
  match Def.get_definition struct_pos path definitions with
  | Some (Variable { t; _ }) ->
    let mk_completions t =
      Option.map ~f:(core_record_to_completion_items ~syntax)
      @@ find_record_from_path t proj_fields_before_cursor definitions
    in
    (match t with
    | Core t -> mk_completions t
    | Resolved t -> mk_completions (Checking.untype_type_expression t)
    | Unresolved -> None)
  | None | Some (Type _ | Module _) -> None
