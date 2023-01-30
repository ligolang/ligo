open Ligo_interface
open Utils
open Linol_lwt
open Linol_lwt.Jsonrpc2
module Loc = Simple_utils.Location

(* TODO: use Set, List & Hashtbl from Core *)
module LSet = Caml.Set.Make (Loc)
module Hashtbl = Caml.Hashtbl
module List = Caml.List

let get_references
    : notify_back -> DocumentUri.t -> Loc.t -> Scopes.def list -> Range.t list IO.t
  =
 fun _notify_back uri location defs ->
  IO.return
  @@ (defs
     |> List.filter_map (fun def ->
            if Loc.equal (Utils.get_location def) location
            then Some (LSet.elements @@ Utils.get_references_of_file uri def)
            else None)
     |> List.flatten
     |> List.filter_map (function
            | Loc.File region -> Some (Utils.region_to_range region)
            | Loc.Virtual _ -> None))


let get_all_references
    :  notify_back -> Loc.t -> (DocumentUri.t, get_scope_info) Hashtbl.t
    -> (DocumentUri.t * Range.t list) list IO.t
  =
 fun notify_back location get_scope_buffers ->
  let go (file, (_, _, defs)) =
    let open Maybe in
    let< defs, _ = defs in
    let> l = from_list @@ get_references notify_back file location defs in
    return (file, l)
  in
  let filter_map res value =
    let* value = go value in
    let* res in
    IO.return
    @@
    match value with
    | None -> res
    | Some ref -> ref :: res
  in
  get_scope_buffers
  |> Hashtbl.to_seq
  |> List.of_seq
  |> List.fold_left filter_map (IO.return [])
