open Handler
open Lsp_helpers

let prepare_rename : Position.t -> DocumentUri.t -> Scopes.def list -> Range.t option =
 fun pos uri defs ->
  let open Option in
  Go_to_definition.get_definition pos uri defs
  >>= fun def ->
  Option.some_if
    (match Def.get_location def with
    | File reg -> not Helpers_file.(is_stdlib reg#file || is_packaged reg#file)
    | Virtual _ -> false)
    ()
  >>= fun () ->
  let loc = Def.get_location def in
  let regs = References.get_references loc @@ Caml.List.to_seq defs in
  Seq.find_map
    (fun reg ->
      let range = Range.of_region reg in
      if Range.contains_position pos range then Some range else None)
    regs


let on_req_prepare_rename : Position.t -> DocumentUri.t -> Range.t option Handler.t =
 fun pos uri ->
  with_cached_doc uri None
  @@ fun { get_scope_info; _ } ->
  return @@ prepare_rename pos uri get_scope_info.definitions
