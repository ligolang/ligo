open Handler
open Lsp_helpers

(* FIXME: add a configuration field for width, so user can choose max line size *)
let on_req_formatting : Path.t -> TextEdit.t list option Handler.t =
 fun file ->
  let@ () = send_debug_msg @@ "Formatting request on " ^ Path.to_string file in
  if Helpers_file.is_packaged file
  then
    let@ () =
      send_message ~type_:Error @@ "Can not format a file from an imported package."
    in
    return None
  else (
    let on_error _err =
      send_message ~type_:Error @@ "Can not format a file with syntax errors"
    in
    with_cst ~strict:true ~on_error file None
    @@ fun cst ->
    let result = Ligo_interface.pretty_print_cst ~width:80 ~dialect_cst:cst in
    return @@ Some [ TextEdit.create ~newText:result ~range:Range.whole_file ])
