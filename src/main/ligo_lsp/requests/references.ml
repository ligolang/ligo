open Handler
open Lsp_helpers
open Ligo_interface
module Def_location = Def.Def_location
module Def_locations = Def.Def_locations
module Loc_in_file = Def.Loc_in_file
module PathMap = Map.Make (Path)
module Ranges = Set.Make (Range)

let get_references
    :  normalize:(string -> Path.t) -> Def_location.t Sequence.t -> Scopes.def Sequence.t
    -> Loc_in_file.t Sequence.t
  =
 fun ~normalize locations defs ->
  defs
  |> Sequence.concat_map ~f:(fun def ->
         Sequence.concat_map locations ~f:(fun location ->
             if Def_location.equal (Def.get_location ~normalize def) location
             then Def_locations.to_sequence @@ Def.references_getter ~normalize def
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
    :  normalize:(string -> Path.t) -> Def_location.t Sequence.t -> Docs_cache.t
    -> (Path.t * Range.t Sequence.t) Sequence.t
  =
 fun ~normalize locations cache ->
  let go (_path, ({ definitions; _ } : Ligo_interface.unprepared_file_data)) =
    Option.value_map
      definitions
      ~default:Sequence.empty
      ~f:
        (get_references ~normalize locations
        <@ Sequence.of_list
        <@ Scopes.Types.flatten_defs)
  in
  cache
  |> Docs_cache.to_alist
  |> Sequence.of_list
  |> Sequence.concat_map ~f:go
  |> partition_references
  |> PathMap.to_sequence
  |> Sequence.map ~f:(fun (file, refs) -> file, Ranges.to_sequence refs)


let get_all_references
    :  normalize:(string -> Path.t) -> Def_location.t list -> Docs_cache.t
    -> Loc_in_file.t list
  =
 fun ~normalize locations cache ->
  get_all_references_grouped_by_file ~normalize (Sequence.of_list locations) cache
  |> Sequence.concat_map ~f:(fun (path, ranges) ->
         Sequence.map ~f:(fun range -> Loc_in_file.{ path; range }) ranges)
  |> Sequence.to_list


let try_to_get_all_linked_locations
    :  normalize:(string -> Path.t) -> Def.t -> Def.definitions
    -> Def_location.t list option
  =
 fun ~normalize definition definitions ->
  let open Go_to_definition in
  let mdefs = filter_mdefs definitions in
  let mod_ids = mdefs_to_identifiers mdefs in
  let%bind.Option def_mod_id = try_to_get_mdef_uid definition mdefs mod_ids in
  let%map.Option defining_mods =
    List.find (Mod_graph.wcc @@ build_mod_graph mdefs) ~f:(Mod_graph.mem def_mod_id)
  in
  List.filter_map (Mod_graph.to_vertices defining_mods) ~f:(fun id ->
      let%bind.Option defs =
        match%bind.Option Mod_map.find_opt id mod_ids with
        | Ad_hoc_signature defs -> Some defs
        | Standalone_signature_or_module path ->
          let%bind.Option uid = try_to_resolve_path mdefs path in
          let%bind.Option mdef =
            List.find mdefs ~f:(fun mdef -> Scopes.Uid.equal uid mdef.uid)
          in
          try_to_resolve_mod_case mdefs mdef.mod_case
      in
      let%map.Option def = find_last_def_by_name_and_level definition defs in
      Def.get_location ~normalize def)


(** Tries to get all locations from definitions that relate to the given definition by
    name, or returns just the location of the given definition instead ([None] case from
    [try_to_get_all_linked_locations]). We say that the definition is related to another
    definition if their names are equal and one is a definition, implementation, or
    reference (or reference of implementation/definition) of the other.
      In other words, linked locations are those that, if you rename one of them, all of
    the others should be rename as well. *)
let get_all_linked_locations_or_def
    : normalize:(string -> Path.t) -> Def.t -> Def.definitions -> Def_location.t list
  =
 fun ~normalize definition definitions ->
  Option.value
    (try_to_get_all_linked_locations ~normalize definition definitions)
    ~default:[ Def.get_location ~normalize definition ]


let get_reverse_dependencies : Path.t -> Path.t list Handler.t =
 fun file ->
  let module File_graph = On_doc.File_graph in
  let@ file_graph = On_doc.build_file_graph in
  let reverse_deps =
    Option.value_map
      ~default:[ file ]
      ~f:File_graph.to_vertices
      (File_graph.reachable file
      @@ File_graph.transpose
      @@ Option.value ~default:File_graph.empty file_graph)
  in
  return reverse_deps


let get_all_reverse_dependencies_definitions : Path.t list -> definitions Handler.t =
  fmap Scopes.Types.wrap_definitions
  <@ concat_map ~f:(fun file ->
         with_cached_doc_pure file ~default:[]
         @@ fun { definitions = { definitions }; _ } -> definitions)


let on_req_references : Position.t -> Path.t -> Location.t list option Handler.t =
 fun pos file ->
  with_cached_doc file ~default:None
  @@ fun { definitions; _ } ->
  let@ normalize = ask_normalize in
  when_some (Def.get_definition ~normalize pos file definitions)
  @@ fun definition ->
  let@ cache = ask_docs_cache in
  let@ files = get_reverse_dependencies file in
  let@ all_definitions = get_all_reverse_dependencies_definitions files in
  let locations = get_all_linked_locations_or_def ~normalize definition all_definitions in
  let references = get_all_references ~normalize locations cache in
  let show_reference fmt Loc_in_file.{ path; range } =
    Format.fprintf fmt "%a\n%a" Path.pp path Range.pp range
  in
  let@ () =
    let@ project_root = ask_last_project_dir in
    send_debug_msg
    @@ Format.asprintf
         "On references request on %a (project root: %a)\n%a"
         Path.pp
         file
         (Fmt.option Path.pp)
         !project_root
         (Format.pp_print_list ~pp_sep:Format.pp_force_newline show_reference)
         references
  in
  return
  @@ List.map references ~f:(fun { range; path } ->
         Location.create ~range ~uri:(DocumentUri.of_path path))
