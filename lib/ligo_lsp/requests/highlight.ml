open Core
open Handler
open Lsp_helpers
open Ligo_interface
open References
module Def_location = Def.Def_location
module Def_locations = Def.Def_locations
module Loc_in_file = Def.Loc_in_file
module PathMap = Map.Make (Path)
module Ranges = Set.Make (Range)

(* Gets all references of the symbol in the provided location in a given file. *)
let get_references_in_file
    :  normalize:Path.normalization -> Def_location.t list -> Path.t -> Docs_cache.t
    -> Range.t list
  =
 fun ~normalize locations file cache ->
  let definitions : Def.definitions =
    match Hashtbl.find cache file with
    | Some { definitions = Some definitions; _ } -> definitions
    | _ -> { definitions = [] }
  in
  get_references
    ~normalize
    (Sequence.of_list locations)
    (Sequence.of_list (Scopes.Types.flatten_defs definitions))
  |> Sequence.filter_map ~f:(fun Loc_in_file.{ range; path } ->
         Option.some_if (Path.equal path file) range)
  |> Ranges.of_sequence
  |> Set.to_list

(** Runs the handler for document highlight. This is normally called when the user clicks
    some symbol. *)
let on_req_highlight
    : Position.t -> Path.t -> Lsp.Types.DocumentHighlight.t list option Handler.t
  =
 fun pos file ->
  let open Handler.Let_syntax in
  with_cached_doc file ~default:None
  @@ fun { definitions; _ } ->
  let%bind normalize = ask_normalize in
  when_some (Def.get_definition ~normalize pos file definitions)
  @@ fun definition ->
  let%bind cache = ask_docs_cache in
  let locations = get_all_linked_locations_or_def ~normalize definition definitions in
  let references = get_references_in_file ~normalize locations file cache in
  let%bind () =
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
