open Handler
open Lsp_helpers
open Ligo_interface
open References
module Def_location = Def.Def_location
module Def_locations = Def.Def_locations
module Loc_in_file = Def.Loc_in_file
module PathMap = Map.Make (Path)
module Ranges = Set.Make (Range)

let get_references_in_file : Def_location.t list -> Path.t -> Docs_cache.t -> Range.t list
  =
 fun locations file cache ->
  let definitions : Def.definitions =
    match Docs_cache.find cache file with
    | Some { definitions = Some definitions; _ } -> definitions
    | _ -> { definitions = [] }
  in
  get_references
    (Sequence.of_list locations)
    (Sequence.of_list (Scopes.Types.flatten_defs definitions))
  |> Sequence.filter_map ~f:(fun Loc_in_file.{ range; path } ->
         Option.some_if (Path.equal path file) range)
  |> Ranges.of_sequence
  |> Ranges.to_list


let on_req_highlight
    : Position.t -> Path.t -> Lsp.Types.DocumentHighlight.t list option Handler.t
  =
 fun pos file ->
  with_cached_doc file ~default:None
  @@ fun { definitions; _ } ->
  when_some (Def.get_definition pos file definitions)
  @@ fun definition ->
  let@ cache = ask_docs_cache in
  let locations = get_all_linked_locations_or_def definition definitions in
  let references = get_references_in_file locations file cache in
  let@ () =
    send_debug_msg
    @@ Format.asprintf
         "On highlight request on %a\n%a"
         Path.pp
         file
         (Format.pp_print_list ~pp_sep:Format.pp_force_newline Range.pp)
         references
  in
  return
  @@ List.map references ~f:(fun range ->
         (* TODO: in #2123 account for highlight kinds *)
         Lsp.Types.DocumentHighlight.create ~range ())
