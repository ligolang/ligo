module Loc = Simple_utils.Location
open Handler

let prepare_rename
    :  Lsp.Types.Position.t -> Lsp.Types.DocumentUri.t -> Scopes.def list
    -> Lsp.Types.Range.t option
  =
 fun pos uri defs ->
  let open Option in
  Definition.get_definition pos uri defs
  >>= fun def ->
  let loc = Utils.get_location def in
  let refs = References.get_references uri loc defs in
  List.find ~f:(Utils.is_position_in_range pos) refs


let on_req_prepare_rename
    :  Lsp.Types.Position.t -> Lsp.Types.DocumentUri.t
    -> Lsp.Types.Range.t option Handler.t
  =
 fun pos uri ->
  with_cached_doc uri None
  @@ fun { get_scope_info; _ } ->
  return @@ prepare_rename pos uri get_scope_info.definitions
