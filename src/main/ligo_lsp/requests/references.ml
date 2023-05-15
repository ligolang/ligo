(* TODO: use Set, List & Hashtbl from Core *)
module SMap = Caml.Map.Make (String)
module LSet = Caml.Set.Make (Simple_utils.Location_ordered)
module Hashtbl = Caml.Hashtbl
open Handler
open Lsp_helpers
open Ligo_interface

module RSet = Caml.Set.Make (struct
  include Range

  let compare = Caml.compare
end)

let get_references : Loc.t -> Scopes.def Seq.t -> Region.t Seq.t =
 fun location defs ->
  defs
  |> Seq.concat_map (fun def ->
         if Loc.equal (Def.get_location def) location
         then LSet.to_seq @@ Def.references_getter def
         else Seq.empty)
  |> Seq.filter_map (function
         | Loc.File region -> Some region
         | Loc.Virtual _ -> None)


let partition_references : Region.t Seq.t -> RSet.t SMap.t =
  Seq.fold_left
    (fun acc reg ->
      let range = Range.of_region reg in
      SMap.update
        reg#file
        (function
          | None -> Some (RSet.singleton range)
          | Some ranges -> Some (RSet.add range ranges))
        acc)
    SMap.empty


let get_all_references
    : Loc.t -> (DocumentUri.t, file_data) Hashtbl.t -> (DocumentUri.t * Range.t list) list
  =
 fun location get_scope_buffers ->
  let go { get_scope_info; _ } =
    let defs = get_scope_info.definitions in
    get_references location @@ Caml.List.to_seq defs
  in
  get_scope_buffers
  |> Hashtbl.to_seq_values
  |> Seq.concat_map go
  |> partition_references
  |> SMap.to_seq
  |> Seq.map (fun (file, refs) -> DocumentUri.of_path file, RSet.elements refs)
  |> Caml.List.of_seq


let on_req_references : Position.t -> DocumentUri.t -> Location.t list option Handler.t =
 fun pos uri ->
  let@ get_scope_buffers = ask_docs_cache in
  with_cached_doc uri None
  @@ fun { get_scope_info; _ } ->
  when_some (Go_to_definition.get_definition pos uri get_scope_info.definitions)
  @@ fun definition ->
  let references = get_all_references (Def.get_location definition) get_scope_buffers in
  let@ () = send_debug_msg @@ "On references request on " ^ DocumentUri.to_path uri in
  let show_reference (uri, ranges) =
    DocumentUri.to_path uri
    ^ "\n"
    ^ String.concat ~sep:"\n"
    @@ List.map ~f:Range.to_string ranges
  in
  let@ () =
    send_debug_msg @@ String.concat ~sep:"\n" @@ List.map ~f:show_reference references
  in
  references
  |> List.concat_map ~f:(fun (file, ranges) ->
         let file =
           if Sys.unix then file else
           file
           |> DocumentUri.to_path
           |> Lsp_helpers.Path.normalise
           |> DocumentUri.of_path
         in
         List.map ~f:(fun range -> Location.create ~uri:file ~range) ranges)
  |> return
