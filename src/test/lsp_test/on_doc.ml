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
      let open Let_syntax in
      let%bind _uri = Handlers.open_file normalized_file_name in
      ask_last_project_dir
    in
    let all_diags = Hashtbl.to_alist diagnostics in
    (* Expect no diagnostics as everything should be fine. *)
    assert (
      List.equal
        (Tuple2.equal
           ~eq1:Lsp_helpers.Path.equal
           ~eq2:(List.equal Lsp_helpers.Diagnostic.eq))
        all_diags
        [ normalized_file_name, [] ]);
    let project_root = Option.map !project_root ~f:path_to_relative in
    Format.printf "%a" (Fmt.Dump.option String.pp) project_root


  let%expect_test "Should find project root in parent directory" =
    get_project_root_test
      "contracts/lsp/project_tests/project_file_in_parent/nested/test.mligo";
    [%expect
      {|
      ((stack ((Ident _#645))) "3=before tuple")
      ((stack ((Ident _#645))) "2=before tuple")
      ((stack (Value (Ident _#645))) "2=after tuple")
      ((stack (Value (Ident _#645))) "1=before tuple")
      ((stack (Value (Ident _#645)))
        "0=before (Nil\
       \n ((desc Operation)\
       \n  (range\
       \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
      ((stack (Value Value (Ident _#645)))
        "0=after (Nil\
       \n ((desc Operation)\
       \n  (range\
       \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
      ((stack (Value Value (Ident _#645))) "1=after tuple")
      ((stack (Value (Ident _#645))) "3=after tuple")
      Some
        "../../../../../default/src/test/contracts/lsp/project_tests/project_file_in_parent" |}]

  let%expect_test "Should find innermost project root" =
    get_project_root_test
      "contracts/lsp/project_tests/two_project_files/nested/test.jsligo";
    [%expect
      {|
      ((stack ((Ident _#1068))) "7=before tuple")
      ((stack ((Ident _#1068))) "6=before tuple")
      ((stack (Value (Ident _#1068))) "6=after tuple")
      ((stack (Value (Ident _#1068))) "5=before tuple")
      ((stack (Value (Ident _#1068)))
        "4=before (Nil\
       \n ((desc Operation)\
       \n  (range\
       \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
      ((stack (Value Value (Ident _#1068)))
        "4=after (Nil\
       \n ((desc Operation)\
       \n  (range\
       \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
      ((stack (Value Value (Ident _#1068))) "5=after tuple")
      ((stack (Value (Ident _#1068))) "7=after tuple")
      Some
        "../../../../../default/src/test/contracts/lsp/project_tests/two_project_files/nested" |}]
end
