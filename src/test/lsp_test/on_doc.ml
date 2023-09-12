module Requests = Ligo_lsp.Server.Requests
open Lsp_helpers

type on_doc_test =
  { test_name : string
  ; file_name : string
  ; changes : TextDocumentContentChangeEvent.t list
  ; predicate : string Lexing_shared.Wrap.wrap -> bool
  }

let get_top_level_change_test { test_name; file_name; changes; predicate }
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let normalized_file_name = Path.from_relative file_name in
  let _cst, _diagnostics =
    Handlers.test_run_session
    @@
    let open Requests.Handler in
    let@ _uri = Handlers.open_file normalized_file_name in
    with_cst normalized_file_name None (fun cst -> return (Some cst))
  in
  ()


let matches_name name node = String.(node#payload = name)
let test_cases = []
let tests = "on_doc", List.map ~f:get_top_level_change_test test_cases
