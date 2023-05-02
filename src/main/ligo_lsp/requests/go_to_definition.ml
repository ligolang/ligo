open Handler
open Lsp_helpers

let get_definition : Position.t -> DocumentUri.t -> Scopes.def list -> Scopes.def option =
  fun pos uri definitions ->
  List.find ~f:(Def.is_reference pos uri) definitions


let on_req_definition : Position.t -> DocumentUri.t -> Locations.t option Handler.t =
 fun pos uri ->
  with_cached_doc uri None
  @@ fun { get_scope_info; _ } ->
  when_some' (get_definition pos uri get_scope_info.definitions)
  @@ fun definition ->
  let region = Def.get_location definition in
  return
  @@
  match region with
  | File region ->
    (* stdlib ranges have an empty file name. They have no definition location. *)
    Option.some_if
      String.(region#file <> "")
      (`Location [ Location.of_region region ])
  | Virtual _ -> None
