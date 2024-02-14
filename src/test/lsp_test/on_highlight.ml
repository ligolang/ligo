module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Lsp_helpers
open Requests.Handler

type highlight_test =
  { test_file : string
  ; position : Position.t
  }

let get_highlight_test ({ test_file; position } : highlight_test) : unit =
  let actual_highlights, _diagnostics =
    test_run_session
    @@ let@ uri = open_file @@ normalize_path test_file in
       Requests.on_req_highlight position uri
  in
  match actual_highlights with
  | None ->
    failwith
    @@ Format.asprintf
         "Expected to find Some highlights for %s, but found None."
         test_file
  | Some actual_highlights ->
    Format.printf "%a" (Fmt.Dump.list DocumentHighlight.pp) actual_highlights


let%expect_test "references of term with sig items and top level ref" =
  get_highlight_test
    { test_file = "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; position = Position.create ~line:9 ~character:6
    };
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 7, "line": 3 },
         "start": { "character": 6, "line": 3 }
       }
     };
     {
       "range": {
         "end": { "character": 7, "line": 9 },
         "start": { "character": 6, "line": 9 }
       }
     };
     {
       "range": {
         "end": { "character": 14, "line": 12 },
         "start": { "character": 13, "line": 12 }
       }
     }] |}]

let%expect_test "references of type with sig items and top level ref" =
  get_highlight_test
    { test_file = "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; position = Position.create ~line:9 ~character:10
    };
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 8, "line": 1 },
         "start": { "character": 7, "line": 1 }
       }
     };
     {
       "range": {
         "end": { "character": 11, "line": 3 },
         "start": { "character": 10, "line": 3 }
       }
     };
     {
       "range": {
         "end": { "character": 8, "line": 7 },
         "start": { "character": 7, "line": 7 }
       }
     };
     {
       "range": {
         "end": { "character": 11, "line": 9 },
         "start": { "character": 10, "line": 9 }
       }
     }] |}]

let%expect_test "references from inline and standalone sigs and mods" =
  get_highlight_test
    { test_file = "contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
    ; position = Position.create ~line:8 ~character:70
    };
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 8, "line": 1 },
         "start": { "character": 7, "line": 1 }
       }
     };
     {
       "range": {
         "end": { "character": 8, "line": 5 },
         "start": { "character": 7, "line": 5 }
       }
     };
     {
       "range": {
         "end": { "character": 39, "line": 8 },
         "start": { "character": 38, "line": 8 }
       }
     };
     {
       "range": {
         "end": { "character": 71, "line": 8 },
         "start": { "character": 70, "line": 8 }
       }
     };
     {
       "range": {
         "end": { "character": 8, "line": 9 },
         "start": { "character": 7, "line": 9 }
       }
     }] |}]

let%expect_test "references from multiple impls" =
  get_highlight_test
    { test_file = "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
    ; position = Position.create ~line:6 ~character:7
    };
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 8, "line": 1 },
         "start": { "character": 7, "line": 1 }
       }
     };
     {
       "range": {
         "end": { "character": 13, "line": 2 },
         "start": { "character": 12, "line": 2 }
       }
     };
     {
       "range": {
         "end": { "character": 8, "line": 6 },
         "start": { "character": 7, "line": 6 }
       }
     };
     {
       "range": {
         "end": { "character": 8, "line": 11 },
         "start": { "character": 7, "line": 11 }
       }
     }] |}]

let%expect_test "signature and include" =
  get_highlight_test
    { test_file = "contracts/lsp/go_to_implementations/signature_and_include.mligo"
    ; position = Position.create ~line:12 ~character:11
    };
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 7, "line": 1 },
         "start": { "character": 6, "line": 1 }
       }
     };
     {
       "range": {
         "end": { "character": 11, "line": 7 },
         "start": { "character": 10, "line": 7 }
       }
     };
     {
       "range": {
         "end": { "character": 11, "line": 12 },
         "start": { "character": 10, "line": 12 }
       }
     }] |}]

let%expect_test "highlights from included type" =
  get_highlight_test
    { test_file = "contracts/lsp/go_to_implementations/include.mligo"
    ; position = Position.create ~line:22 ~character:12
    };
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 8, "line": 1 },
         "start": { "character": 7, "line": 1 }
       }
     };
     {
       "range": {
         "end": { "character": 8, "line": 5 },
         "start": { "character": 7, "line": 5 }
       }
     };
     {
       "range": {
         "end": { "character": 8, "line": 9 },
         "start": { "character": 7, "line": 9 }
       }
     };
     {
       "range": {
         "end": { "character": 8, "line": 15 },
         "start": { "character": 7, "line": 15 }
       }
     };
     {
       "range": {
         "end": { "character": 13, "line": 22 },
         "start": { "character": 12, "line": 22 }
       }
     }] |}]

let%expect_test "highlights of references to the same sig from different file" =
  get_highlight_test
    { test_file = "contracts/lsp/go_to_implementations/implementation.mligo"
    ; position = Position.create ~line:3 ~character:7
    };
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 8, "line": 3 },
         "start": { "character": 7, "line": 3 }
       }
     };
     {
       "range": {
         "end": { "character": 8, "line": 8 },
         "start": { "character": 7, "line": 8 }
       }
     }] |}]

let%expect_test "highlights of references to the same sig from different file" =
  get_highlight_test
    { test_file = "contracts/lsp/go_to_implementations/implementation.mligo"
    ; position = Position.create ~line:4 ~character:6
    };
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 7, "line": 4 },
         "start": { "character": 6, "line": 4 }
       }
     };
     {
       "range": {
         "end": { "character": 7, "line": 9 },
         "start": { "character": 6, "line": 9 }
       }
     }] |}]

let%expect_test "highlights of references to the same sig from different file" =
  get_highlight_test
    { test_file = "contracts/lsp/go_to_implementations/implementation.mligo"
    ; position = Position.create ~line:2 ~character:12
    };
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 13, "line": 2 },
         "start": { "character": 12, "line": 2 }
       }
     };
     {
       "range": {
         "end": { "character": 13, "line": 7 },
         "start": { "character": 12, "line": 7 }
       }
     }] |}]

let%expect_test "highlights of shadowed type" =
  get_highlight_test
    { test_file = "contracts/lsp/go_to_implementations/shadow.mligo"
    ; position = Position.create ~line:13 ~character:11
    };
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 8, "line": 1 },
         "start": { "character": 7, "line": 1 }
       }
     };
     {
       "range": {
         "end": { "character": 8, "line": 10 },
         "start": { "character": 7, "line": 10 }
       }
     };
     {
       "range": {
         "end": { "character": 12, "line": 13 },
         "start": { "character": 11, "line": 13 }
       }
     }] |}]

let%expect_test "highlights of different but same-named local variables" =
  get_highlight_test
    { test_file = "contracts/lsp/shadow_var.mligo"
    ; position = Position.create ~line:5 ~character:14
    };
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 18, "line": 5 },
         "start": { "character": 11, "line": 5 }
       }
     };
     {
       "range": {
         "end": { "character": 34, "line": 5 },
         "start": { "character": 27, "line": 5 }
       }
     }] |}]

let%expect_test "highlights of same-named variables in different functions" =
  get_highlight_test
    { test_file = "contracts/lsp/shadow_var.mligo"
    ; position = Position.create ~line:0 ~character:18
    };
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 8, "line": 0 },
         "start": { "character": 7, "line": 0 }
       }
     };
     {
       "range": {
         "end": { "character": 19, "line": 0 },
         "start": { "character": 18, "line": 0 }
       }
     }] |}]
