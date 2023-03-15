open Handler
open Utils

let rename_reference : string -> Lsp.Types.Range.t -> Lsp.Types.TextEdit.t =
 fun newText range -> Lsp.Types.TextEdit.create ~range ~newText


let on_req_rename
    :  string -> Lsp.Types.Position.t -> Lsp.Types.DocumentUri.t
    -> Lsp.Types.WorkspaceEdit.t Handler.t
  =
 fun new_name pos uri ->
  let@ get_scope_buffers = ask_docs_cache in
  with_cached_doc uri (Lsp.Types.WorkspaceEdit.create ())
  @@ fun { get_scope_info; _ } ->
  let@ value =
    when_some' (Definition.get_definition pos uri get_scope_info.definitions)
    @@ fun definition ->
    let references =
      References.get_all_references (get_location definition) get_scope_buffers
    in
    let changes =
      List.map
        ~f:(fun (file, ranges) -> file, List.map ~f:(rename_reference new_name) ranges)
        references
    in
    return @@ Some (Lsp.Types.WorkspaceEdit.create ~changes ())
  in
  return @@ Option.value value ~default:(Lsp.Types.WorkspaceEdit.create ())
