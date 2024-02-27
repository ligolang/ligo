module Requests = Ligo_lsp.Server.Requests
open Requests.Handler
open Lsp_helpers

let default_test_config : config =
  { max_number_of_problems = Int.max_value
  ; logging_verbosity = Log
  ; disabled_features = []
  ; max_line_width = None
  ; completion_implementation = `With_scopes
  ; diagnostics_pull_mode = `OnDocUpdate
  }


let test_run_session ?(config = default_test_config) (session : 'a Handler.t)
    : 'a * Diagnostic.t list Path_hashtbl.t
  =
  let mocked_notify_back = Path_hashtbl.create () in
  let result =
    run_handler
      { notify_back = Mock mocked_notify_back
      ; config
      ; docs_cache = Docs_cache.create ()
      ; last_project_dir = ref None
      ; mod_res = ref None
      }
      session
  in
  Lwt_main.run result, mocked_notify_back


let open_file (file : Path.t) : Path.t Handler.t =
  let@ () =
    Requests.on_doc
      ~process_immediately:true
      file
      (In_channel.read_all @@ Path.to_string file)
  in
  return file


let to_absolute : string -> string = Path.to_string <@ Path.from_relative
