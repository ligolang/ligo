open Handler
open Lsp_helpers

(* TODO: escape identifiers with @ if there is a collision with a keyword. Also, handle
   collisions with identifiers of the same name (probably in the prepare rename request). *)

(** Helper to create a text edit from the provided string and range. *)
let rename_reference : string -> Range.t -> TextEdit.t =
 fun newText range -> TextEdit.create ~range ~newText


(** Runs the handler of the rename request. This is normally invoked when the user presses
    the rename button and if the prepare rename request didn't fail. *)
let on_req_rename : string -> Position.t -> Path.t -> WorkspaceEdit.t Handler.t =
 fun new_name pos file ->
  let open Handler.Let_syntax in
  let%bind get_scope_buffers = ask_docs_cache in
  with_cached_doc file ~default:(WorkspaceEdit.create ())
  @@ fun { definitions; _ } ->
  let%bind value =
    let%bind normalize = ask_normalize in
    when_some' (Def.get_definition ~normalize pos file definitions)
    @@ fun definition ->
    let%bind files = References.get_reverse_dependencies file in
    let%bind all_definitions =
      References.get_all_reverse_dependencies_definitions files
    in
    let references =
      let locations =
        References.get_all_linked_locations_or_def ~normalize definition all_definitions
      in
      References.get_all_references_grouped_by_file
        ~normalize
        (Sequence.of_list locations)
        get_scope_buffers
    in
    let changes =
      Sequence.to_list
      @@ Sequence.map
           ~f:(fun (file, ranges) ->
             ( DocumentUri.of_path file
             , Sequence.to_list @@ Sequence.map ~f:(rename_reference new_name) ranges ))
           references
    in
    return @@ Some (WorkspaceEdit.create ~changes ())
  in
  return @@ Option.value value ~default:(WorkspaceEdit.create ())
