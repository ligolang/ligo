open Linol_lwt
open Linol_lwt.Jsonrpc2
module Loc = Simple_utils.Location

(* TODO: use Set from Core *)
module LSet = Caml.Set.Make (Loc)

let get_definition : Position.t -> DocumentUri.t -> Scopes.def list -> Scopes.def option =
 fun pos uri definitions -> List.find ~f:(Utils.is_reference pos uri) definitions
