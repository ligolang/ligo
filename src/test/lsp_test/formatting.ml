module Requests = Ligo_lsp.Server.Requests
open Handlers
open Alcotest_extras
open Lsp_helpers
open Requests.Handler

type formatting_test =
  { file_path : string
  ; expected : string option
  }

let get_formatting_test ({ file_path; expected } : formatting_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case file_path `Quick
  @@ fun () ->
  let result, _diagnostics =
    test_run_session
    @@ let@ uri = open_file (Path.from_relative file_path) in
       Requests.on_req_formatting uri
  in
  match result, expected with
  | None, None -> ()
  | None, Some _ -> fail "Could not format."
  | Some _, None -> fail "Can format, but expected to fail and return None."
  | Some [ { range; newText } ], Some path_to_expected ->
    check Range.testable "Expected a whole_file_range." Range.whole_file range;
    check
      Alcotest.string
      "Formatted file does not match the expected."
      (In_channel.read_all path_to_expected)
      newText
  | Some _, _ -> fail "Formatting returned multiple edits."


let test_cases =
  [ { file_path = "contracts/lsp/format_me.mligo"
    ; expected = Some "contracts/lsp/formatted.mligo"
    }
  ; { file_path = "contracts/lsp/syntax_error.mligo"
    ; expected = None (* No ghost_idents, please *)
    }
  ; { file_path = "contracts/lsp/format_me.jsligo"
    ; expected = Some "contracts/lsp/formatted.jsligo"
    }
  ]


let tests = "formatting", List.map ~f:get_formatting_test test_cases
