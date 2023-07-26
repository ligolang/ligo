open Handler
open Lsp_helpers

let rename_reference : string -> Range.t -> TextEdit.t =
 fun newText range -> TextEdit.create ~range ~newText


let on_req_rename : string -> Position.t -> Path.t -> WorkspaceEdit.t Handler.t =
 fun new_name pos file ->
  let@ get_scope_buffers = ask_docs_cache in
  with_cached_doc file (WorkspaceEdit.create ())
  @@ fun { definitions; _ } ->
  let@ value =
    when_some' (Go_to_definition.get_definition pos file definitions)
    @@ fun definition ->
    let references =
      References.get_all_references_grouped_by_file
        (Def.get_location definition)
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
