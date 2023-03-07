module Requests = Ligo_lsp.Server.Requests
open Linol_lwt
open Requests.Handler
open Utils

let default_test_config : config =
  { max_number_of_problems = Int.max_value
  ; logging_verbosity = Log
  ; disabled_features = []
  }


let test_run_session ?(config = default_test_config) (session : 'a Handler.t)
    : 'a * Jsonrpc2.Diagnostic.t list
  =
  let mocked_notify_back = ref [] in
  let result =
    run_handler
      { notify_back = Mock mocked_notify_back; config; docs_cache = Hashtbl.create 32 }
      session
  in
  Lwt_main.run result, !mocked_notify_back


(** File path is expected to be absolute *)
let open_file (file_path : string) : DocumentUri.t Handler.t =
  let uri = DocumentUri.of_path file_path in
  let@ () = Requests.on_doc uri (In_channel.read_all file_path) in
  return uri


let to_absolute : string -> string = Filename.concat @@ Ligo_unix.getcwd ()
let rel_path_to_uri : string -> DocumentUri.t = DocumentUri.of_path @. to_absolute
