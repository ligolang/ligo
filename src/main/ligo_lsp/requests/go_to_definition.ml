open Handler
open Lsp_helpers

let get_definition : Position.t -> Path.t -> Scopes.def list -> Scopes.def option =
 fun pos uri definitions -> List.find ~f:(Def.is_reference pos uri) definitions


let on_req_definition : Position.t -> Path.t -> Locations.t option Handler.t =
 fun pos file ->
  with_cached_doc file None
  @@ fun { get_scope_info; _ } ->
  when_some' (get_definition pos file get_scope_info.definitions)
  @@ fun definition ->
  let def_location = Def.get_location definition in
  return
  @@
  match def_location with
  | File { range; path } ->
    Some (`Location [ Location.create ~range ~uri:(DocumentUri.of_path path) ])
  | StdLib _ | Virtual _ -> None
