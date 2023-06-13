(**
    This module contains functions operating on the Ast_typed
    in order to check contract and storage compliance
    with the TZIP-16 standard.
*)

open Ast_typed.Types
open Simple_utils.Trace

(** Looks up the expression corresponding to the contract storage
    to find the TZIP-16 contract metadata type, if it exists.

    If there is a 'metadata' field, it returns the field's type.
    Otherwise, it returns [None]. *)
let find_storage_metadata_opt (storage : Ast_typed.type_expression)
    : Ast_typed.type_expression option
  =
  match storage.type_content with
  | Ast_typed.T_record rows ->
    let metadata_string = "metadata" in
    let metadata_label = match Ligo_prim.Layout.find_annot rows.layout metadata_string with
    | Some l -> l
    | _ ->  Ligo_prim.Label.of_string metadata_string in
    let fields : Ast_typed.type_expression Ligo_prim.Record.t = rows.fields in
    Ligo_prim.Record.find_opt fields metadata_label
  | _ -> None


(** Verifies that the type of the [metadata] field in the storage
    is a [(string, bytes) big_map], as required by the TZIP-16 standard. *)
let check_metadata_tzip16_type_compliance ~raise ?syntax (storage_metadata : type_expression)
    : unit
  =
  let pass =
    match Ast_typed.get_t_big_map storage_metadata with
    | None -> false
    | Some (p1, p2) ->
      (match Ast_typed.get_t_string p1, Ast_typed.get_t_bytes p2 with
      | Some _, Some _ -> true
      | _ -> false)
  in
  let suggested_type = match syntax with
    (* TODO: improve this with decompilation? *)
    | Some Syntax_types.JsLIGO -> "big_map<string, bytes>"
    | Some CameLIGO -> "(string, bytes) big_map"
    | _ -> "big_map string bytes" in
  let warning = `Self_ast_typed_metadata_invalid_type (storage_metadata.location, suggested_type) in
  if not pass then raise.warning @@ warning
