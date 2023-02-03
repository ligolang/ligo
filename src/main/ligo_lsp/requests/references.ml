open Ligo_interface
open Linol_lwt
open Linol_lwt.Jsonrpc2
module Loc = Simple_utils.Location

(* TODO: use Set, List & Hashtbl from Core *)
module LSet = Caml.Set.Make (Loc)
module Hashtbl = Caml.Hashtbl
module List = Caml.List

let get_references : DocumentUri.t -> Loc.t -> Scopes.def list -> Range.t list =
 fun uri location defs ->
  defs
  |> List.filter_map (fun def ->
         if Loc.equal (Utils.get_location def) location
         then Some (LSet.elements @@ Utils.get_references_of_file uri def)
         else None)
  |> List.flatten
  |> List.filter_map (function
         | Loc.File region -> Some (Utils.region_to_range region)
         | Loc.Virtual _ -> None)


let get_all_references
    :  Loc.t -> (DocumentUri.t, get_scope_info) Hashtbl.t
    -> (DocumentUri.t * Range.t list) list
  =
 fun location get_scope_buffers ->
  let go (file, (_, _, defs_opt)) =
    let open Option in
    defs_opt
    >>= fun (defs, _) ->
    match get_references file location defs with
    | [] -> None
    | l -> Some (file, l)
  in
  let filter_map res value =
    let value = go value in
    match value with
    | None -> res
    | Some ref -> ref :: res
  in
  get_scope_buffers |> Hashtbl.to_seq |> List.of_seq |> List.fold_left filter_map []
