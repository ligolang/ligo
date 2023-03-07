(* TODO: use String, Option, Set, List & Hashtbl from Core *)
open Linol_lwt
module Hashtbl = Caml.Hashtbl
module List = Caml.List
open Handler
open Utils

(* FIXME: add a configuration field for width, so user can choose max line size *)
let on_req_formatting : DocumentUri.t -> TextEdit.t list option Handler.t =
 fun uri ->
  let@ () = send_debug_msg @@ "Formatting request on " ^ DocumentUri.to_path uri in
  let on_error _err =
    send_message ~type_:Error @@ "Can not format a file with syntax errors"
  in
  with_cst ~strict:true ~on_error uri None
  @@ fun cst ->
  let result = Ligo_interface.pretty_print_cst ~width:80 ~dialect_cst:cst in
  return @@ Some [ TextEdit.create ~newText:result ~range:whole_file_range ]
