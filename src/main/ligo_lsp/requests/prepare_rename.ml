module Loc = Simple_utils.Location
open Lsp.Types

let prepare_rename
    : Position.t -> DocumentUri.t -> Ligo_interface.get_scope_info -> Range.t option
  =
 fun pos uri (_, _, scope_info) ->
  let open Option in
  scope_info
  >>= fun (defs, _) ->
  Definition.get_definition pos uri defs
  >>= fun def ->
  let loc = Utils.get_location def in
  let refs = References.get_references uri loc defs in
  List.find ~f:(Utils.is_position_in_range pos) refs
