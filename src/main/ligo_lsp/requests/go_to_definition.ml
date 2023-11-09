open Handler
open Lsp_helpers

let on_req_definition : Position.t -> Path.t -> Locations.t option Handler.t =
 fun pos file ->
  with_cached_doc file ~default:None
  @@ fun { definitions; _ } ->
  when_some' (Def.get_definition pos file definitions)
  @@ fun definition ->
  let def_location = Def.get_location definition in
  return
  @@
  match def_location with
  | File { range; path } ->
    Some (`Location [ Location.create ~range ~uri:(DocumentUri.of_path path) ])
  | StdLib _ | Virtual _ -> None
