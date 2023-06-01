open Handler
open Lsp_helpers

let prepare_rename : Position.t -> Path.t -> Scopes.def list -> Range.t option =
 fun pos file defs ->
  let open Option in
  Go_to_definition.get_definition pos file defs
  >>= fun def ->
  Option.some_if
    (match Def.get_location def with
    | File { path; range = _ } -> not (Helpers_file.is_packaged path)
    | StdLib _ | Virtual _ -> false)
    ()
  >>= fun () ->
  let loc = Def.get_location def in
  let locations = References.get_references loc @@ Sequence.of_list defs in
  Option.map
    ~f:(fun loc -> loc.range)
    (Sequence.find
       ~f:(fun { path; range } ->
         Range.contains_position pos range && Path.equal file path)
       (* Checking if user's cursor is located on some reference *)
       locations)


let on_req_prepare_rename : Position.t -> Path.t -> Range.t option Handler.t =
 fun pos file ->
  with_cached_doc file None
  @@ fun { get_scope_info; _ } ->
  return @@ prepare_rename pos file get_scope_info.definitions
