module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Lsp_helpers

let get_tzip16_code_lens_test (file_path : string) : unit =
  let file_path_normalized = normalize_path file_path in
  let code_lens_response, _ =
    test_run_session
    @@
    let open Handler.Let_syntax in
    let%bind _ = open_file file_path_normalized in
    let%bind docs_cache = Requests.Handler.ask_docs_cache in
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
    ((stack ((Ident _#724))) "3=before tuple")
    ((stack ((Ident _#724))) "2=before tuple")
    ((stack (Value (Ident _#724))) "2=after tuple")
    ((stack (Value (Ident _#724))) "1=before tuple")
    ((stack (Value (Ident _#724)))
      "0=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#724)))
      "0=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#724))) "1=after tuple")
    ((stack (Value (Ident _#724))) "3=after tuple")
    ((stack ()) "7=before tuple")
    ((stack ()) "6=before tuple")
    ((stack ())
      "5=before (Empty_bigmap\
     \n ((desc String)\
     \n  (range\
     \n   ((start 17) (stop 23)\
     \n    (source\
     \n     (File\
     \n      /home/eduardo/y/ligo/_build/default/src/test/contracts/contract_metadata/metadata_tzip16_detection.mligo)))))\
     \n ((desc Bytes)\
     \n  (range\
     \n   ((start 25) (stop 30)\
     \n    (source\
     \n     (File\
     \n      /home/eduardo/y/ligo/_build/default/src/test/contracts/contract_metadata/metadata_tzip16_detection.mligo))))))")
    ((stack (Value))
      "5=after (Empty_bigmap\
     \n ((desc String)\
     \n  (range\
     \n   ((start 17) (stop 23)\
     \n    (source\
     \n     (File\
     \n      /home/eduardo/y/ligo/_build/default/src/test/contracts/contract_metadata/metadata_tzip16_detection.mligo)))))\
     \n ((desc Bytes)\
     \n  (range\
     \n   ((start 25) (stop 30)\
     \n    (source\
     \n     (File\
     \n      /home/eduardo/y/ligo/_build/default/src/test/contracts/contract_metadata/metadata_tzip16_detection.mligo))))))")
    ((stack (Value)) "6=after tuple")
    ((stack (Value)) "4=before tuple")
    ((stack (Value Value)) "4=after tuple")
    ((stack (Value)) "7=after tuple")
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
    ((stack ((Ident _#1730))) "11=before tuple")
    ((stack ((Ident _#1730))) "10=before tuple")
    ((stack (Value (Ident _#1730))) "10=after tuple")
    ((stack (Value (Ident _#1730))) "9=before tuple")
    ((stack (Value (Ident _#1730)))
      "8=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1730)))
      "8=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1730))) "9=after tuple")
    ((stack (Value (Ident _#1730))) "11=after tuple")
    ((stack ()) "15=before tuple")
    ((stack ()) "14=before tuple")
    ((stack ())
      "13=before (Empty_bigmap\
     \n ((desc String)\
     \n  (range\
     \n   ((start 55) (stop 61)\
     \n    (source\
     \n     (File\
     \n      /home/eduardo/y/ligo/_build/default/src/test/contracts/contract_metadata/metadata_tzip16_detection.jsligo)))))\
     \n ((desc Bytes)\
     \n  (range\
     \n   ((start 63) (stop 68)\
     \n    (source\
     \n     (File\
     \n      /home/eduardo/y/ligo/_build/default/src/test/contracts/contract_metadata/metadata_tzip16_detection.jsligo))))))")
    ((stack (Value))
      "13=after (Empty_bigmap\
     \n ((desc String)\
     \n  (range\
     \n   ((start 55) (stop 61)\
     \n    (source\
     \n     (File\
     \n      /home/eduardo/y/ligo/_build/default/src/test/contracts/contract_metadata/metadata_tzip16_detection.jsligo)))))\
     \n ((desc Bytes)\
     \n  (range\
     \n   ((start 63) (stop 68)\
     \n    (source\
     \n     (File\
     \n      /home/eduardo/y/ligo/_build/default/src/test/contracts/contract_metadata/metadata_tzip16_detection.jsligo))))))")
    ((stack (Value)) "14=after tuple")
    ((stack (Value)) "12=before tuple")
    ((stack (Value Value)) "12=after tuple")
    ((stack (Value)) "15=after tuple")
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
