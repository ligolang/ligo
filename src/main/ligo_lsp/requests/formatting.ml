module Make (Ligo_api : Ligo_interface.LIGO_API) = struct
  (* TODO: use String, Option, Set, List & Hashtbl from Core *)
  open Linol_lwt
  module Hashtbl = Caml.Hashtbl
  open Ligo_interface.Make (Ligo_api)
  module List = Caml.List
  open Handler
  open Utils

  let on_req_formatting : DocumentUri.t -> TextEdit.t list option Handler.t =
   fun uri ->
    let@ () = send_debug_msg @@ "Formatting request on " ^ DocumentUri.to_path uri in
    let formatted_text = formatting uri in
    match formatted_text with
    | Ok (result, _) ->
      let formatted_text = TextEdit.create ~newText:result ~range:whole_file_range in
      return (Some [ formatted_text ])
    | Error _ -> return None
end
