module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Lsp_helpers
open Requests.Handler

let get_folding_range_test file_path : unit =
  let folds_opt, _diagnostics =
    test_run_session
    @@ let@ uri = open_file @@ normalize_path file_path in
       Requests.on_req_folding_range uri
  in
  match folds_opt with
  | Some actual_folds -> Format.printf "%a" (Fmt.Dump.list FoldingRange.pp) actual_folds
  | None -> failwith "Expected some list of folding ranges, got None"


let%expect_test _ =
  get_folding_range_test "contracts/lsp/folding_range.mligo";
  [%expect
    {|
    [{
       "endCharacter": 4,
       "endLine": 17,
       "kind": "region",
       "startCharacter": 3,
       "startLine": 15
     };
     {
       "endCharacter": 4,
       "endLine": 17,
       "kind": "region",
       "startCharacter": 1,
       "startLine": 14
     };
     {
       "endCharacter": 4,
       "endLine": 12,
       "kind": "region",
       "startCharacter": 3,
       "startLine": 10
     };
     {
       "endCharacter": 4,
       "endLine": 12,
       "kind": "region",
       "startCharacter": 1,
       "startLine": 9
     };
     {
       "endCharacter": 20,
       "endLine": 7,
       "kind": "region",
       "startCharacter": 5,
       "startLine": 6
     };
     {
       "endCharacter": 20,
       "endLine": 7,
       "kind": "region",
       "startCharacter": 5,
       "startLine": 6
     };
     {
       "endCharacter": 20,
       "endLine": 7,
       "kind": "region",
       "startCharacter": 3,
       "startLine": 5
     };
     {
       "endCharacter": 20,
       "endLine": 7,
       "kind": "region",
       "startCharacter": 1,
       "startLine": 4
     };
     {
       "endCharacter": 12,
       "endLine": 2,
       "kind": "region",
       "startCharacter": 5,
       "startLine": 1
     };
     {
       "endCharacter": 12,
       "endLine": 2,
       "kind": "region",
       "startCharacter": 1,
       "startLine": 0
     }] |}]

(* TODO: delete the first one on folding range fix *)
let%expect_test _ =
  get_folding_range_test "contracts/lsp/folding_range.jsligo";
  [%expect
    {|
  [{
     "endCharacter": 2,
     "endLine": 19,
     "kind": "region",
     "startCharacter": 24,
     "startLine": 16
   };
   {
     "endCharacter": 2,
     "endLine": 19,
     "kind": "region",
     "startCharacter": 7,
     "startLine": 16
   };
   {
     "endCharacter": 2,
     "endLine": 19,
     "kind": "region",
     "startCharacter": 1,
     "startLine": 16
   };
   {
     "endCharacter": 2,
     "endLine": 14,
     "kind": "region",
     "startCharacter": 15,
     "startLine": 11
   };
   {
     "endCharacter": 2,
     "endLine": 14,
     "kind": "region",
     "startCharacter": 1,
     "startLine": 11
   };
   {
     "endCharacter": 4,
     "endLine": 8,
     "kind": "region",
     "startCharacter": 23,
     "startLine": 5
   };
   {
     "endCharacter": 4,
     "endLine": 8,
     "kind": "region",
     "startCharacter": 3,
     "startLine": 5
   };
   {
     "endCharacter": 2,
     "endLine": 9,
     "kind": "region",
     "startCharacter": 42,
     "startLine": 4
   };
   {
     "endCharacter": 2,
     "endLine": 9,
     "kind": "region",
     "startCharacter": 19,
     "startLine": 4
   };
   {
     "endCharacter": 2,
     "endLine": 9,
     "kind": "region",
     "startCharacter": 7,
     "startLine": 4
   };
   {
     "endCharacter": 2,
     "endLine": 9,
     "kind": "region",
     "startCharacter": 1,
     "startLine": 4
   };
   {
     "endCharacter": 24,
     "endLine": 2,
     "kind": "region",
     "startCharacter": 3,
     "startLine": 1
   };
   {
     "endCharacter": 24,
     "endLine": 2,
     "kind": "region",
     "startCharacter": 1,
     "startLine": 0
   }] |}]

(* TODO: delete the last one on folding range fix *)
let%expect_test _ =
  get_folding_range_test "contracts/lsp/import.jsligo";
  [%expect
    {|
    [{
       "endCharacter": 2,
       "endLine": 3,
       "kind": "region",
       "startCharacter": 8,
       "startLine": 0
     };
     {
       "endCharacter": 14,
       "endLine": 3,
       "kind": "imports",
       "startCharacter": 1,
       "startLine": 0
     }] |}]

let%expect_test _ =
  get_folding_range_test "contracts/lsp/folding_range_for_loop.jsligo";
  [%expect
    {|
    [{
       "endCharacter": 10,
       "endLine": 17,
       "kind": "region",
       "startCharacter": 40,
       "startLine": 14
     };
     {
       "endCharacter": 10,
       "endLine": 17,
       "kind": "region",
       "startCharacter": 9,
       "startLine": 14
     };
     {
       "endCharacter": 6,
       "endLine": 18,
       "kind": "region",
       "startCharacter": 36,
       "startLine": 13
     };
     {
       "endCharacter": 6,
       "endLine": 18,
       "kind": "region",
       "startCharacter": 5,
       "startLine": 13
     };
     {
       "endCharacter": 16,
       "endLine": 12,
       "kind": "region",
       "startCharacter": 9,
       "startLine": 10
     };
     {
       "endCharacter": 16,
       "endLine": 12,
       "kind": "region",
       "startCharacter": 5,
       "startLine": 10
     };
     {
       "endCharacter": 6,
       "endLine": 8,
       "kind": "region",
       "startCharacter": 17,
       "startLine": 6
     };
     {
       "endCharacter": 16,
       "endLine": 6,
       "kind": "region",
       "startCharacter": 9,
       "startLine": 4
     };
     {
       "endCharacter": 6,
       "endLine": 8,
       "kind": "region",
       "startCharacter": 5,
       "startLine": 4
     };
     {
       "endCharacter": 6,
       "endLine": 3,
       "kind": "region",
       "startCharacter": 36,
       "startLine": 1
     };
     {
       "endCharacter": 6,
       "endLine": 3,
       "kind": "region",
       "startCharacter": 5,
       "startLine": 1
     };
     {
       "endCharacter": 2,
       "endLine": 19,
       "kind": "region",
       "startCharacter": 21,
       "startLine": 0
     };
     {
       "endCharacter": 2,
       "endLine": 19,
       "kind": "region",
       "startCharacter": 15,
       "startLine": 0
     };
     {
       "endCharacter": 3,
       "endLine": 19,
       "kind": "region",
       "startCharacter": 14,
       "startLine": 0
     };
     {
       "endCharacter": 5,
       "endLine": 19,
       "kind": "region",
       "startCharacter": 14,
       "startLine": 0
     };
     {
       "endCharacter": 5,
       "endLine": 19,
       "kind": "region",
       "startCharacter": 7,
       "startLine": 0
     };
     {
       "endCharacter": 5,
       "endLine": 19,
       "kind": "region",
       "startCharacter": 1,
       "startLine": 0
     }] |}]
