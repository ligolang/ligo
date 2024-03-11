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
           63, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 23, "line": 63 },
         "start": { "character": 4, "line": 63 }
       }
     };
     {
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.mligo",
           69, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 33, "line": 69 },
         "start": { "character": 4, "line": 69 }
       }
     };
     {
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.mligo",
           84, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 33, "line": 84 },
         "start": { "character": 4, "line": 84 }
       }
     };
     {
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.mligo",
           98, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 29, "line": 98 },
         "start": { "character": 8, "line": 98 }
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
           34, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 25, "line": 34 },
         "start": { "character": 6, "line": 34 }
       }
     };
     {
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.jsligo",
           39, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 35, "line": 39 },
         "start": { "character": 6, "line": 39 }
       }
     };
     {
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.jsligo",
           52, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 35, "line": 52 },
         "start": { "character": 6, "line": 52 }
       }
     };
     {
       "command": {
         "arguments": [
           "../../../../../default/src/test/contracts/contract_metadata/metadata_tzip16_detection.jsligo",
           64, 1
         ],
         "command": "add-tzip16-addr",
         "title": "Mark as TZIP-16 compatible storage"
       },
       "range": {
         "end": { "character": 31, "line": 64 },
         "start": { "character": 10, "line": 64 }
       }
     }]
      |}]
