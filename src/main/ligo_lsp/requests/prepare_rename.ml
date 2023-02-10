module Loc = Simple_utils.Location
open Lsp.Types
open Handler

let prepare_rename : Position.t -> DocumentUri.t -> Scopes.def list -> Range.t option =
 fun pos uri defs ->
  let open Option in
  Definition.get_definition pos uri defs
  >>= fun def ->
  let loc = Utils.get_location def in
  let refs = References.get_references uri loc defs in
  List.find ~f:(Utils.is_position_in_range pos) refs


let on_req_prepare_rename : Position.t -> DocumentUri.t -> Range.t option Handler.t =
 fun pos uri ->
  with_cached_doc uri None
  @@ fun get_scope_info -> return @@ prepare_rename pos uri get_scope_info.definitions
