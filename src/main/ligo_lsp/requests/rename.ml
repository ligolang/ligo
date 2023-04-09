open Handler
open Lsp_helpers

let rename_reference : string -> Range.t -> TextEdit.t =
 fun newText range -> TextEdit.create ~range ~newText


let on_req_rename : string -> Position.t -> DocumentUri.t -> WorkspaceEdit.t Handler.t =
 fun new_name pos uri ->
  let@ get_scope_buffers = ask_docs_cache in
  with_cached_doc uri (WorkspaceEdit.create ())
  @@ fun { get_scope_info; _ } ->
  let@ value =
    when_some' (Go_to_definition.get_definition pos uri get_scope_info.definitions)
    @@ fun definition ->
    let references =
      References.get_all_references (Def.get_location definition) get_scope_buffers
    in
    let changes =
      List.map
        ~f:(fun (file, ranges) -> file, List.map ~f:(rename_reference new_name) ranges)
        references
    in
    return @@ Some (WorkspaceEdit.create ~changes ())
  in
  return @@ Option.value value ~default:(WorkspaceEdit.create ())
