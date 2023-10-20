module Requests = Ligo_lsp.Server.Requests
open Lsp_helpers

module Top_level_change_test = struct
  type test_info =
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
end

module Project_root_test = struct
  type test_info =
    { test_name : string
    ; file_name : string
    ; expected_project_root : Path.t option
    }

  let get_project_root_test { test_name; file_name; expected_project_root }
      : unit Alcotest.test_case
    =
    Alcotest.test_case test_name `Quick
    @@ fun () ->
    let normalized_file_name = Path.from_relative file_name in
    let project_root, diagnostics =
      Handlers.test_run_session
      @@
      let open Requests.Handler in
      let@ _uri = Handlers.open_file normalized_file_name in
      ask_last_project_file
    in
    let all_diags = Requests.Handler.Path_hashtbl.to_alist diagnostics in
    Alcotest.(check' (option Path.testable))
      ~msg:"Project root doesn't match the expectation."
      ~expected:expected_project_root
      ~actual:!project_root;
    (* Expect no diagnostics as everything should be fine. *)
    Alcotest.(check (list (pair Path.testable (list Diagnostic.testable))))
      "Expected no diagnostics."
      [ normalized_file_name, [] ]
      all_diags


  let test_cases =
    [ { test_name = "Should find project root in parent directory"
      ; file_name = "contracts/lsp/project_tests/project_file_in_parent/nested/test.mligo"
      ; expected_project_root =
          Some (Path.from_relative "contracts/lsp/project_tests/project_file_in_parent")
      }
    ; { test_name = "Should find innermost project root"
      ; file_name = "contracts/lsp/project_tests/two_project_files/nested/test.jsligo"
      ; expected_project_root =
          Some (Path.from_relative "contracts/lsp/project_tests/two_project_files/nested")
      }
    ]
end

let tests =
  ( "on_doc"
  , Top_level_change_test.(List.map ~f:get_top_level_change_test test_cases)
    @ Project_root_test.(List.map ~f:get_project_root_test test_cases) )
