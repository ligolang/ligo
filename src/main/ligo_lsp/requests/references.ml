open Handler
open Lsp_helpers
open Ligo_interface
module Def_location = Def.Def_location
module Def_locations = Def.Def_locations
module Loc_in_file = Def.Loc_in_file
module PathMap = Map.Make (Path)
module Ranges = Set.Make (Range)

let get_references : Def_location.t -> Scopes.def Sequence.t -> Loc_in_file.t Sequence.t =
 fun location defs ->
  defs
  |> Sequence.concat_map ~f:(fun def ->
         if Def_location.equal (Def.get_location def) location
         then Def_locations.to_sequence @@ Def.references_getter def
         else Sequence.empty)
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
    : Def_location.t -> Docs_cache.t -> (Path.t * Range.t Sequence.t) Sequence.t
  =
 fun location get_scope_buffers ->
  let go (_path, { definitions; _ }) =
    get_references location @@ Sequence.of_list definitions
  in
  get_scope_buffers
  |> Docs_cache.to_alist
  |> Sequence.of_list
  |> Sequence.concat_map ~f:go
  |> partition_references
  |> PathMap.to_sequence
  |> Sequence.map ~f:(fun (file, refs) -> file, Ranges.to_sequence refs)


let get_all_references : Def_location.t -> Docs_cache.t -> Loc_in_file.t list =
 fun location get_scope_buffers ->
  get_all_references_grouped_by_file location get_scope_buffers
  |> Sequence.concat_map ~f:(fun (path, ranges) ->
         Sequence.map ~f:(fun range -> Loc_in_file.{ path; range }) ranges)
  |> Sequence.to_list


let on_req_references : Position.t -> Path.t -> Location.t list option Handler.t =
 fun pos file ->
  let@ get_scope_buffers = ask_docs_cache in
  with_cached_doc file ~default:None
  @@ fun { definitions; _ } ->
  when_some (Def.get_definition pos file definitions)
  @@ fun definition ->
  let references = get_all_references (Def.get_location definition) get_scope_buffers in
  let@ () = send_debug_msg @@ "On references request on " ^ Path.to_string file in
  let show_reference Loc_in_file.{ path; range } =
    Path.to_string path ^ "\n" ^ Range.to_string range
  in
  let@ () =
    send_debug_msg @@ String.concat ~sep:"\n" @@ List.map ~f:show_reference references
  in
  return
  @@ List.map
       ~f:(fun { range; path } -> Location.create ~range ~uri:(DocumentUri.of_path path))
       references
