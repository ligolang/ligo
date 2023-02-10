module Make (Ligo_api : Ligo_interface.LIGO_API) = struct
  (* TODO: use String, Option, Set, List & Hashtbl from Core *)
  open Linol_lwt
  module Hashtbl = Caml.Hashtbl
  open Ligo_interface.Make (Ligo_api)
  module List = Caml.List
  open Handler

  (* We define here a helper that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
  let on_doc : DocumentUri.t -> string -> unit Handler.t =
   fun uri contents ->
    let@ get_scope_buffers = ask_docs_cache in
    let new_state = Ligo_interface.unfold_get_scope @@ get_scope uri contents in
    Hashtbl.replace get_scope_buffers uri new_state;
    let@ () = send_debug_msg ("Updating DOC :" ^ DocumentUri.to_string uri) in
    let@ () =
      match new_state.errors with
      | [] -> send_debug_msg "No errors"
      | errors ->
        let value = List.map Main_errors.Formatter.error_json errors |> List.concat in
        send_debug_msg
          ("There are errors:\n *"
          ^ String.concat ~sep:"\n *"
          @@ List.map (fun (x : Simple_utils.Error.t) -> x.content.message) value)
    in
    (* FIXME and add Warnings*)
    let extract_error_information : Main_errors.all -> (Range.t * string) list =
     fun e ->
      let errs = Main_errors.Formatter.error_json e in
      List.map
        (fun ({ content = { message; location; _ }; _ } : Simple_utils.Error.t) ->
          match location with
          | Some location ->
            ( Option.value ~default:Utils.dummy_range (Utils.location_to_range location)
            , message )
          | None -> Utils.dummy_range, message)
        errs
    in
    let diagnostics =
      new_state.errors
      |> List.map extract_error_information
      |> List.concat
      |> List.map (fun (range, message) -> Diagnostic.create ~message ~range ())
    in
    let@ () = send_diagnostic diagnostics in
    let@ () =
      match new_state.warnings with
      | [] -> send_debug_msg "No warnings"
      | _ -> send_debug_msg "There are warnings"
    in
    if new_state.has_info
    then send_debug_msg "There is result"
    else send_debug_msg "No result"
end
