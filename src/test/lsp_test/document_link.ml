module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Requests.Handler
open Lsp_helpers

let get_document_link_test file_path : unit =
  let links_opt, _diagnostics =
    test_run_session
    @@ let@ uri = open_file @@ normalize_path file_path in
       Requests.on_req_document_link uri
  in
  match links_opt with
  | Some links ->
    let links =
      List.map links ~f:(fun link ->
          let target = Option.map link.target ~f:to_relative_uri in
          { link with target })
    in
    Format.printf "%a" (Fmt.Dump.list DocumentLink.pp) links
  | None -> failwith "Expected some list of document links, got None"


let%expect_test _ =
  get_document_link_test "contracts/includer.mligo";
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 25, "line": 2 },
         "start": { "character": 9, "line": 2 }
       },
       "target": "file:///../../../../../default/src/test/contracts/included.mligo"
     }] |}]

let%expect_test _ =
  get_document_link_test "contracts/includer.jsligo";
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 26, "line": 2 },
         "start": { "character": 9, "line": 2 }
       },
       "target": "file:///../../../../../default/src/test/contracts/included.jsligo"
     }] |}]

(* with #import *)
let%expect_test _ =
  get_document_link_test "contracts/build/E.mligo";
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 17, "line": 1 },
         "start": { "character": 8, "line": 1 }
       },
       "target": "file:///../../../../../default/src/test/contracts/build/F.mligo"
     };
     {
       "range": {
         "end": { "character": 17, "line": 4 },
         "start": { "character": 8, "line": 4 }
       },
       "target": "file:///../../../../../default/src/test/contracts/build/G.mligo"
     }] |}]

(* with #import *)
let%expect_test _ =
  get_document_link_test "contracts/build/B.jsligo";
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 18, "line": 0 },
         "start": { "character": 8, "line": 0 }
       },
       "target": "file:///../../../../../default/src/test/contracts/build/A.jsligo"
     }] |}]

(* with #import *)
let%expect_test _ =
  get_document_link_test "contracts/lsp/registry.jsligo";
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 43, "line": 0 },
         "start": { "character": 8, "line": 0 }
       },
       "target": "file:///../../../../../default/src/test/contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/lib/bigarray.mligo"
     }] |}]

(* with #include *)
let%expect_test _ =
  get_document_link_test "contracts/lsp/registry_include.jsligo";
  [%expect
    {|
    [{
       "range": {
         "end": { "character": 44, "line": 0 },
         "start": { "character": 9, "line": 0 }
       },
       "target": "file:///../../../../../default/src/test/contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/lib/bigarray.mligo"
     }] |}]
