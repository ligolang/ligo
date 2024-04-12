open Handler
open Lsp_helpers

(** Returns the declaration range of the symbol over the provided position and path (if
    any). Returns [None] if the declaration was not found or the symbol can't be renamed. *)
let prepare_rename
    :  normalize:Path.normalization -> Position.t -> Path.t -> Scopes.definitions
    -> Range.t option
  =
 fun ~normalize pos file defs ->
  let open Option in
  Def.get_definition ~normalize pos file defs
  >>= fun def ->
  Option.some_if
    (match Def.get_location ~normalize def with
    | File { path; range = _ } -> not (Helpers_file.is_packaged @@ Path.to_string path)
    | StdLib _ | Virtual _ -> false)
    ()
  >>= fun () ->
  let locations = References.get_all_linked_locations_or_def ~normalize def defs in
  let locations =
    References.get_references
      ~normalize
      (Sequence.of_list locations)
      (Sequence.of_list (Scopes.Types.flatten_defs defs))
  in
  Option.map
    ~f:(fun loc -> loc.range)
    (Sequence.find
       ~f:(fun { path; range } ->
         Range.contains_position pos range && Path.equal file path)
       (* Checking if user's cursor is located on some reference *)
       locations)


(** Runs the handler for prepare rename. This is normally called when the user presses
    the rename button. *)
let on_req_prepare_rename : Position.t -> Path.t -> Range.t option Handler.t =
 fun pos file ->
  let@ normalize = ask_normalize in
  with_cached_doc file ~default:None
  @@ fun { definitions; _ } -> return @@ prepare_rename ~normalize pos file definitions
