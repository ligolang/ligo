module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Lsp_helpers
open Range.Construct
open Requests.Handler

type range_formatting_test =
  { file_path : string
  ; range : Range.t
  }

let get_formatting_test ({ file_path; range } : range_formatting_test) : unit =
  let result, _diagnostics =
    test_run_session
    @@ let@ uri = open_file @@ normalize_path file_path in
       Requests.on_req_range_formatting uri range FormattingOptions.default
  in
  match result with
  | None -> ()
  | Some [ text_edit ] ->
    Format.printf "%a" (Helpers_pretty.pp_with_yojson TextEdit.yojson_of_t) text_edit
  | Some _ -> failwith "Returned multiple edits, which was not expected"


let%expect_test "CameLIGO no statements" =
  let tests =
    List.map
      [ range (1, 0) (2, 0); interval 4 16 39; interval 6 16 39 ]
      ~f:(fun r -> { file_path = "contracts/lsp/format_me.mligo"; range = r })
  in
  run_multiple_tests tests ~test_runner:get_formatting_test;
  [%expect {| [; ; ] |}]

let%expect_test "CameLIGO one directive" =
  let tests =
    List.map
      [ range (0, 0) (1, 0); interval 0 0 34 ]
      ~f:(fun r -> { file_path = "contracts/lsp/format_me.mligo"; range = r })
    @ List.map
        [ range (1, 0) (3, 0); interval 2 0 39; range (1, 0) (4, 12) ]
        ~f:(fun r -> { file_path = "contracts/lsp/format_me.mligo"; range = r })
  in
  run_multiple_tests tests ~test_runner:get_formatting_test;
  [%expect
    {|
    [{
       "newText": "#include \"simple.mligo\"",
       "range": {
         "end": { "character": 34, "line": 0 },
         "start": { "character": 0, "line": 0 }
       }
     };
     {
       "newText": "#include \"simple.mligo\"",
       "range": {
         "end": { "character": 34, "line": 0 },
         "start": { "character": 0, "line": 0 }
       }
     };
     {
       "newText": "#import \"local_module.mligo\" \"A\"",
       "range": {
         "end": { "character": 39, "line": 2 },
         "start": { "character": 0, "line": 2 }
       }
     };
     {
       "newText": "#import \"local_module.mligo\" \"A\"",
       "range": {
         "end": { "character": 39, "line": 2 },
         "start": { "character": 0, "line": 2 }
       }
     };
     {
       "newText": "#import \"local_module.mligo\" \"A\"",
       "range": {
         "end": { "character": 39, "line": 2 },
         "start": { "character": 0, "line": 2 }
       }
     }] |}]

let%expect_test "CameLIGO one declaration" =
  let tests =
    List.map
      [ range (4, 0) (5, 0); interval 4 0 39 ]
      ~f:(fun r -> { file_path = "contracts/lsp/format_me.mligo"; range = r })
    @ List.map
        [ range (5, 0) (6, 0); interval 5 0 40 ]
        ~f:(fun r -> { file_path = "contracts/lsp/format_me.mligo"; range = r })
  in
  run_multiple_tests tests ~test_runner:get_formatting_test;
  [%expect
    {|
    [{
       "newText": "let format_me =\n  let x = 20 in\n  x * 2",
       "range": {
         "end": { "character": 39, "line": 4 },
         "start": { "character": 0, "line": 4 }
       }
     };
     {
       "newText": "let format_me =\n  let x = 20 in\n  x * 2",
       "range": {
         "end": { "character": 39, "line": 4 },
         "start": { "character": 0, "line": 4 }
       }
     };
     {
       "newText": "let format_me_2 =\n  let q = A.A.s in\n  q ^ q",
       "range": {
         "end": { "character": 40, "line": 5 },
         "start": { "character": 0, "line": 5 }
       }
     };
     {
       "newText": "let format_me_2 =\n  let q = A.A.s in\n  q ^ q",
       "range": {
         "end": { "character": 40, "line": 5 },
         "start": { "character": 0, "line": 5 }
       }
     }] |}]

let%expect_test "CameLIGO two declarations" =
  let tests =
    List.map
      [ range (4, 0) (6, 0); range (4, 0) (6, 0); range (4, 0) (7, 0) ]
      ~f:(fun r -> { file_path = "contracts/lsp/format_me.mligo"; range = r })
  in
  run_multiple_tests tests ~test_runner:get_formatting_test;
  (* FIXME there should be a newline between decls *)
  [%expect
    {|
    [{
       "newText": "let format_me =\n  let x = 20 in\n  x * 2\nlet format_me_2 =\n  let q = A.A.s in\n  q ^ q",
       "range": {
         "end": { "character": 40, "line": 5 },
         "start": { "character": 0, "line": 4 }
       }
     };
     {
       "newText": "let format_me =\n  let x = 20 in\n  x * 2\nlet format_me_2 =\n  let q = A.A.s in\n  q ^ q",
       "range": {
         "end": { "character": 40, "line": 5 },
         "start": { "character": 0, "line": 4 }
       }
     };
     {
       "newText": "let format_me =\n  let x = 20 in\n  x * 2\nlet format_me_2 =\n  let q = A.A.s in\n  q ^ q",
       "range": {
         "end": { "character": 40, "line": 5 },
         "start": { "character": 0, "line": 4 }
       }
     }] |}]

(* FIXME #2024 <- dont forget to uncomment in line below *)
(* let%expect_test "CameLIGO whole file" =
  get_formatting_test
    { file_path = "contracts/lsp/format_me.mligo"; range = Range.whole_file };
  [%expect {||}] *)

let%expect_test "JsLIGO no statements" =
  let tests =
    List.map
      [ range (1, 0) (2, 0); interval 8 0 1; interval 11 0 10 ]
      ~f:(fun r -> { file_path = "contracts/lsp/format_me.jsligo"; range = r })
  in
  run_multiple_tests tests ~test_runner:get_formatting_test;
  [%expect {| [; ; ] |}]

let%expect_test "JsLIGO one declaration" =
  let tests =
    List.map
      [ range (1, 0) (5, 0); range (2, 0) (3, 62) ]
      ~f:(fun r -> { file_path = "contracts/lsp/format_me.jsligo"; range = r })
    @ List.map
        [ range (4, 0) (6, 0); interval 5 0 52 ]
        ~f:(fun r -> { file_path = "contracts/lsp/format_me.jsligo"; range = r })
    @ List.map
        [ range (6, 0) (13, 0); range (7, 0) (12, 27) ]
        ~f:(fun r -> { file_path = "contracts/lsp/format_me.jsligo"; range = r })
  in
  run_multiple_tests tests ~test_runner:get_formatting_test;
  [%expect
    {|
    [{
       "newText": "const increment = (b: int): int => ((a: int): int => a + 1)(b)",
       "range": {
         "end": { "character": 62, "line": 3 },
         "start": { "character": 0, "line": 2 }
       }
     };
     {
       "newText": "const increment = (b: int): int => ((a: int): int => a + 1)(b)",
       "range": {
         "end": { "character": 62, "line": 3 },
         "start": { "character": 0, "line": 2 }
       }
     };
     {
       "newText": "const with_semicolon = [1 + 2 * 3, M.fold_test]",
       "range": {
         "end": { "character": 51, "line": 5 },
         "start": { "character": 0, "line": 5 }
       }
     };
     {
       "newText": "const with_semicolon = [1 + 2 * 3, M.fold_test]",
       "range": {
         "end": { "character": 51, "line": 5 },
         "start": { "character": 0, "line": 5 }
       }
     };
     {
       "newText": "const incr_map = (l: list<int>): list<int> => List.map((i: int) => i + 1, l)",
       "range": {
         "end": { "character": 27, "line": 12 },
         "start": { "character": 0, "line": 7 }
       }
     };
     {
       "newText": "const incr_map = (l: list<int>): list<int> => List.map((i: int) => i + 1, l)",
       "range": {
         "end": { "character": 27, "line": 12 },
         "start": { "character": 0, "line": 7 }
       }
     }] |}]

let%expect_test "JsLIGO two declarations" =
  let tests =
    List.map
      [ range (1, 0) (6, 0); range (2, 0) (5, 52) ]
      ~f:(fun r -> { file_path = "contracts/lsp/format_me.jsligo"; range = r })
    @ List.map
        [ range (5, 0) (13, 0); range (5, 0) (12, 27) ]
        ~f:(fun r -> { file_path = "contracts/lsp/format_me.jsligo"; range = r })
  in
  run_multiple_tests tests ~test_runner:get_formatting_test;
  [%expect
    {|
    [{
       "newText": "const increment = (b: int): int => ((a: int): int => a + 1)(b)\nconst with_semicolon = [1 + 2 * 3, M.fold_test]",
       "range": {
         "end": { "character": 51, "line": 5 },
         "start": { "character": 0, "line": 2 }
       }
     };
     {
       "newText": "const increment = (b: int): int => ((a: int): int => a + 1)(b)\nconst with_semicolon = [1 + 2 * 3, M.fold_test]",
       "range": {
         "end": { "character": 51, "line": 5 },
         "start": { "character": 0, "line": 2 }
       }
     };
     {
       "newText": "const with_semicolon = [1 + 2 * 3, M.fold_test]\nconst incr_map = (l: list<int>): list<int> => List.map((i: int) => i + 1, l)",
       "range": {
         "end": { "character": 27, "line": 12 },
         "start": { "character": 0, "line": 5 }
       }
     };
     {
       "newText": "const with_semicolon = [1 + 2 * 3, M.fold_test]\nconst incr_map = (l: list<int>): list<int> => List.map((i: int) => i + 1, l)",
       "range": {
         "end": { "character": 27, "line": 12 },
         "start": { "character": 0, "line": 5 }
       }
     }] |}]

(* FIXME #2024 <- dont forget to uncomment in line below *)
(* let%expect_test "JsLIGO whole file" =
  get_formatting_test
    { file_path = "contracts/lsp/format_me.jsligo"; range = Range.whole_file };
  [%expect {||}] *)
