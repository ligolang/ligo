open Utils
open Linol_lwt
open Linol_lwt.Jsonrpc2
module Loc = Simple_utils.Location

(* TODO: use Set from Core *)
module LSet = Caml.Set.Make (Loc)

let get_definition
    :  notify_back -> Position.t -> DocumentUri.t -> Scopes.def list
    -> Scopes.def option IO.t
  =
 fun _notify_back pos uri definitions ->
  let open Maybe in
  let< definition = List.find ~f:(Utils.is_reference pos uri) definitions in
  (* let* () = notify_back#send_log_msg ~type_:Info (defintion_to_string definition) in *)
  return definition
