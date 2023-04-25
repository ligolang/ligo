open Handler
open Lsp_helpers

let prepare_rename : Position.t -> DocumentUri.t -> Scopes.def list -> Range.t option =
 fun pos uri defs ->
  let open Option in
  Go_to_definition.get_definition pos uri defs
  >>= fun def ->
  Option.some_if
    String.(
      match Def.get_location def with
      (* stdlib ranges have an empty file name. We don't wish to rename them. *)
      | File loc -> loc#file <> ""
      | Virtual _ -> false)
    def
  >>= fun def ->
  let loc = Def.get_location def in
  let refs = References.get_references uri loc defs in
  List.find ~f:(Range.contains_position pos) refs


let on_req_prepare_rename : Position.t -> DocumentUri.t -> Range.t option Handler.t =
 fun pos uri ->
  with_cached_doc uri None
  @@ fun { get_scope_info; _ } ->
  return @@ prepare_rename pos uri get_scope_info.definitions
