module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Lsp_helpers

let get_tzip16_code_lens_test (file_path : string) : unit =
  let file_path_normalized = normalize_path file_path in
  let code_lens_response, _ =
    test_run_session
    @@
    let ( let@ ) = Requests.Handler.bind in
    let@ _ = open_file file_path_normalized in
    let@ docs_cache = Requests.Handler.ask_docs_cache in
    Requests.on_code_lens file_path_normalized
  in
  let code_lens_response_ =
    List.map code_lens_response ~f:(fun lens ->
        match lens.command with
        | None -> lens
        | Some command ->
          { lens with
            command =
              Some
                { command with
                  arguments =
                    (match command.arguments with
                    | Some (`String arg1 :: args) ->
                      Some (`String (string_path_to_relative arg1) :: args)
                    | _ -> command.arguments)
                }
          })
  in
  Format.printf "%a" Fmt.Dump.(list CodeLens.pp) code_lens_response_


let%expect_test "Suggestions for adding tzip16_compatible attributes in CameLIGO" =
  get_tzip16_code_lens_test "contracts/contract_metadata/metadata_tzip16_detection.mligo";
  [%expect
    {|
    [{
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.mligo",
           65, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 23, "line": 65 },
         "start": { "character": 4, "line": 65 }
       }
     };
     {
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.mligo",
           71, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 33, "line": 71 },
         "start": { "character": 4, "line": 71 }
       }
     };
     {
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.mligo",
           86, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 33, "line": 86 },
         "start": { "character": 4, "line": 86 }
       }
     };
     {
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.mligo",
           100, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 29, "line": 100 },
         "start": { "character": 8, "line": 100 }
       }
     }]
      |}]

let%expect_test "Suggestions for adding tzip16_compatible attributes in JsLIGO" =
  get_tzip16_code_lens_test "contracts/contract_metadata/metadata_tzip16_detection.jsligo";
  [%expect
    {|
    [{
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.jsligo",
           36, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 25, "line": 36 },
         "start": { "character": 6, "line": 36 }
       }
     };
     {
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.jsligo",
           41, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 35, "line": 41 },
         "start": { "character": 6, "line": 41 }
       }
     };
     {
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.jsligo",
           54, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 35, "line": 54 },
         "start": { "character": 6, "line": 54 }
       }
     };
     {
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.jsligo",
           66, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 31, "line": 66 },
         "start": { "character": 10, "line": 66 }
       }
     }]
      |}]
