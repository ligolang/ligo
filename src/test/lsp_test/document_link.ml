module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Requests.Handler
open Lsp_helpers

let get_document_link_test file_path : unit =
  let links_opt, _diagnostics =
    test_run_session
    @@
    let open Handler.Let_syntax in
    let%bind uri = open_file @@ normalize_path file_path in
    let%bind links = Requests.on_req_document_link uri in
    let%bind normalize = ask_normalize in
    return
    @@ Option.map
         links
         ~f:
           (List.map ~f:(fun link ->
                let target =
                  Option.map link.DocumentLink.target ~f:(to_relative_uri ~normalize)
                in
                { link with target }))
  in
  match links_opt with
  | Some links -> Format.printf "%a" (Fmt.Dump.list DocumentLink.pp) links
  | None -> failwith "Expected some list of document links, got None"


let%expect_test _ =
  get_document_link_test "contracts/includer.mligo";
  [%expect
    {|
    ((stack ((Ident _#612))) "3=before tuple")
    ((stack ((Ident _#612))) "2=before tuple")
    ((stack (Value (Ident _#612))) "2=after tuple")
    ((stack (Value (Ident _#612))) "1=before tuple")
    ((stack (Value (Ident _#612)))
      "0=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#612)))
      "0=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#612))) "1=after tuple")
    ((stack (Value (Ident _#612))) "3=after tuple")
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
    ((stack ((Ident _#1002))) "7=before tuple")
    ((stack ((Ident _#1002))) "6=before tuple")
    ((stack (Value (Ident _#1002))) "6=after tuple")
    ((stack (Value (Ident _#1002))) "5=before tuple")
    ((stack (Value (Ident _#1002)))
      "4=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1002)))
      "4=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1002))) "5=after tuple")
    ((stack (Value (Ident _#1002))) "7=after tuple")
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
    ((stack ((Ident _#1400))) "11=before tuple")
    ((stack ((Ident _#1400))) "10=before tuple")
    ((stack (Value (Ident _#1400))) "10=after tuple")
    ((stack (Value (Ident _#1400))) "9=before tuple")
    ((stack (Value (Ident _#1400)))
      "8=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1400)))
      "8=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1400))) "9=after tuple")
    ((stack (Value (Ident _#1400))) "11=after tuple")
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
    ((stack ((Ident _#1793))) "15=before tuple")
    ((stack ((Ident _#1793))) "14=before tuple")
    ((stack (Value (Ident _#1793))) "14=after tuple")
    ((stack (Value (Ident _#1793))) "13=before tuple")
    ((stack (Value (Ident _#1793)))
      "12=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1793)))
      "12=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1793))) "13=after tuple")
    ((stack (Value (Ident _#1793))) "15=after tuple")
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
    ((stack ((Ident _#2252))) "19=before tuple")
    ((stack ((Ident _#2252))) "18=before tuple")
    ((stack (Value (Ident _#2252))) "18=after tuple")
    ((stack (Value (Ident _#2252))) "17=before tuple")
    ((stack (Value (Ident _#2252)))
      "16=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2252)))
      "16=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2252))) "17=after tuple")
    ((stack (Value (Ident _#2252))) "19=after tuple")
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
