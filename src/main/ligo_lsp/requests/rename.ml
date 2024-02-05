open Handler
open Lsp_helpers

let rename_reference : string -> Range.t -> TextEdit.t =
 fun newText range -> TextEdit.create ~range ~newText


let on_req_rename : string -> Position.t -> Path.t -> WorkspaceEdit.t Handler.t =
 fun new_name pos file ->
  let@ get_scope_buffers = ask_docs_cache in
  with_cached_doc file ~default:(WorkspaceEdit.create ())
  @@ fun { definitions; _ } ->
  let@ value =
    when_some' definitions
    @@ fun definitions ->
    when_some' (Def.get_definition pos file definitions)
    @@ fun definition ->
    let@ files = References.get_reverse_dependencies file in
    let@ () = References.prepare_caches_for_references files in
    let@ all_definitions = References.get_all_reverse_dependencies_definitions files in
    let locations =
      References.get_all_linked_locations_or_def definition all_definitions
    in
    let references =
      References.get_all_references_grouped_by_file
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
