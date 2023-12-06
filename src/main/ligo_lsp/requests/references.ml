open Handler
open Lsp_helpers
open Ligo_interface
module Def_location = Def.Def_location
module Def_locations = Def.Def_locations
module Loc_in_file = Def.Loc_in_file
module PathMap = Map.Make (Path)
module Ranges = Set.Make (Range)

let get_references
    : Def_location.t Sequence.t -> Scopes.def Sequence.t -> Loc_in_file.t Sequence.t
  =
 fun locations defs ->
  defs
  |> Sequence.concat_map ~f:(fun def ->
         Sequence.concat_map locations ~f:(fun location ->
             if Def_location.equal (Def.get_location def) location
             then Def_locations.to_sequence @@ Def.references_getter def
             else Sequence.empty))
  |> Sequence.filter_map ~f:(function
         | Def_location.File loc -> Some loc
         | Virtual _ | StdLib _ -> None)


let partition_references : Loc_in_file.t Sequence.t -> Ranges.t PathMap.t =
  Sequence.fold
    ~f:(fun acc loc ->
      let open Loc_in_file in
      PathMap.update acc loc.path ~f:(function
          | None -> Ranges.singleton loc.range
          | Some others -> Ranges.add others loc.range))
    ~init:PathMap.empty


(* This version is useful for rename request *)
let get_all_references_grouped_by_file
    :  Def_location.t Sequence.t -> Docs_cache.t
    -> (Path.t * Range.t Sequence.t) Sequence.t
  =
 fun locations get_scope_buffers ->
  let go (_path, { definitions; _ }) =
    get_references locations @@ Sequence.of_list definitions
  in
  get_scope_buffers
  |> Docs_cache.to_alist
  |> Sequence.of_list
  |> Sequence.concat_map ~f:go
  |> partition_references
  |> PathMap.to_sequence
  |> Sequence.map ~f:(fun (file, refs) -> file, Ranges.to_sequence refs)


let get_all_references : Def_location.t list -> Docs_cache.t -> Loc_in_file.t list =
 fun locations get_scope_buffers ->
  get_all_references_grouped_by_file (Sequence.of_list locations) get_scope_buffers
  |> Sequence.concat_map ~f:(fun (path, ranges) ->
         Sequence.map ~f:(fun range -> Loc_in_file.{ path; range }) ranges)
  |> Sequence.to_list


let try_to_get_all_linked_locations : Def.t -> Def.t list -> Def_location.t list option =
 fun definition definitions ->
  let open Go_to_definition in
  let mdefs = filter_mdefs definitions in
  let mod_ids = mdefs_to_identifiers mdefs in
  let%bind.Option def_mod_id = try_to_get_mdef_uid definition mdefs mod_ids in
  let mod_graph = build_mod_graph mdefs in
  let%map.Option defining_mods =
    List.find (Mod_graph.wcc mod_graph) ~f:(Mod_graph.mem def_mod_id)
  in
  List.concat_map ~f:(List.map ~f:Def.get_location)
  @@ List.filter_map (Mod_graph.to_vertices defining_mods) ~f:(fun id ->
         let%map.Option defs =
           match%bind.Option Mod_map.find_opt id mod_ids with
           | Ad_hoc_signature defs -> Some defs
           | Standalone_signature_or_module { module_path; resolved_module } ->
             let%bind.Option uid =
               try_to_resolve_standalone mdefs ~module_path ~resolved_module
             in
             let%bind.Option mdef =
               List.find mdefs ~f:(fun mdef -> Scopes.Uid.equal uid mdef.uid)
             in
             try_to_resolve_mod_case mdefs mdef.mod_case
         in
         find_defs_by_name_and_level definition defs)


let get_all_linked_locations_or_def : Def.t -> Def.t list -> Def_location.t list =
 fun definition definitions ->
  Option.value
    (try_to_get_all_linked_locations definition definitions)
    ~default:[ Def.get_location definition ]


let on_req_references : Position.t -> Path.t -> Location.t list option Handler.t =
 fun pos file ->
  with_cached_doc file ~default:None
  @@ fun { definitions; _ } ->
  when_some (Def.get_definition pos file definitions)
  @@ fun definition ->
  let@ get_scope_buffers = ask_docs_cache in
  let locations = get_all_linked_locations_or_def definition definitions in
  let references = get_all_references locations get_scope_buffers in
  let@ () = send_debug_msg @@ "On references request on " ^ Path.to_string file in
  let show_reference Loc_in_file.{ path; range } =
    Format.asprintf "%a\n%a" Path.pp path Range.pp range
  in
  let@ () =
    send_debug_msg @@ String.concat ~sep:"\n" @@ List.map ~f:show_reference references
  in
  return
  @@ List.map
       ~f:(fun { range; path } -> Location.create ~range ~uri:(DocumentUri.of_path path))
       references
