module Requests = Ligo_lsp.Server.Requests
module Handlers = Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common

module Project_root_test = struct
  let get_project_root_test file_name : unit =
    let normalized_file_name = normalize_path file_name in
    let project_root, diagnostics =
      Handlers.test_run_session
      @@
      let open Requests.Handler in
      let@ _uri = Handlers.open_file normalized_file_name in
      ask_last_project_dir
    in
    let all_diags = Requests.Handler.Path_hashtbl.to_alist diagnostics in
    (* Expect no diagnostics as everything should be fine. *)
    assert (Caml.(all_diags = [ normalized_file_name, [] ]));
    let project_root = Option.map !project_root ~f:path_to_relative in
    Format.printf "%a" (Fmt.Dump.option String.pp) project_root


  let%expect_test "Should find project root in parent directory" =
    get_project_root_test
      "contracts/lsp/project_tests/project_file_in_parent/nested/test.mligo";
    [%expect
      {|
      Some
        "../../../../../default/src/test/contracts/lsp/project_tests/project_file_in_parent" |}]

  let%expect_test "Should find innermost project root" =
    get_project_root_test
      "contracts/lsp/project_tests/two_project_files/nested/test.jsligo";
    [%expect
      {|
      Some
        "../../../../../default/src/test/contracts/lsp/project_tests/two_project_files/nested" |}]
end
