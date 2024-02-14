open Lsp_helpers
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
module Requests = Ligo_lsp.Server.Requests
open Requests.Handler

let document_symbol_test ?(config : config option) file_name : unit =
  let actual_symbols, _diagnostics =
    test_run_session ?config
    @@ let@ uri = open_file @@ normalize_path file_name in
       Requests.on_req_document_symbol uri
  in
  match actual_symbols with
  | None -> failwith "Expected some symbol list, got None"
  | Some (`DocumentSymbol actual_symbols) ->
    Format.printf "%a" (Fmt.Dump.list DocumentSymbol.pp) actual_symbols


let%expect_test "Simple document symbols" =
  document_symbol_test "contracts/lsp/hovers.jsligo";
  [%expect
    {|
    [{
       "detail": "<p, s>(_: (_: p) => (_: s) => [list<operation>, s]) => (\n  _: option<key_hash>\n) => (_: tez) => (_: s) => [operation, address]",
       "kind": 13,
       "name": "z",
       "range": {
         "end": { "character": 31, "line": 0 },
         "start": { "character": 6, "line": 0 }
       },
       "selectionRange": {
         "end": { "character": 7, "line": 0 },
         "start": { "character": 6, "line": 0 }
       }
     };
     {
       "detail": "Test.Proxy_ticket.proxy_address",
       "kind": 23,
       "name": "t",
       "range": {
         "end": { "character": 40, "line": 2 },
         "start": { "character": 0, "line": 2 }
       },
       "selectionRange": {
         "end": { "character": 6, "line": 2 },
         "start": { "character": 5, "line": 2 }
       }
     }] |}]

let%expect_test "Nested defs in body and constructors" =
  document_symbol_test "contracts/lsp/patterns.mligo";
  [%expect
    {|
    [{
       "children": [
         {
           "detail": "int * string",
           "kind": 22,
           "name": "Foo",
           "range": {
             "end": { "character": 28, "line": 0 },
             "start": { "character": 9, "line": 0 }
           },
           "selectionRange": {
             "end": { "character": 12, "line": 0 },
             "start": { "character": 9, "line": 0 }
           }
         }
       ],
       "detail": "Foo of (int * string)",
       "kind": 10,
       "name": "v",
       "range": {
         "end": { "character": 28, "line": 0 },
         "start": { "character": 0, "line": 0 }
       },
       "selectionRange": {
         "end": { "character": 6, "line": 0 },
         "start": { "character": 5, "line": 0 }
       }
     };
     {
       "children": [
         {
           "children": [
             {
               "detail": "int",
               "kind": 13,
               "name": "a",
               "range": {
                 "end": { "character": 33, "line": 1 },
                 "start": { "character": 19, "line": 1 }
               },
               "selectionRange": {
                 "end": { "character": 24, "line": 1 },
                 "start": { "character": 23, "line": 1 }
               }
             },
             {
               "detail": "string",
               "kind": 13,
               "name": "b",
               "range": {
                 "end": { "character": 52, "line": 1 },
                 "start": { "character": 33, "line": 1 }
               },
               "selectionRange": {
                 "end": { "character": 38, "line": 1 },
                 "start": { "character": 37, "line": 1 }
               }
             }
           ],
           "detail": "int",
           "kind": 13,
           "name": "i",
           "range": {
             "end": { "character": 62, "line": 1 },
             "start": { "character": 0, "line": 1 }
           },
           "selectionRange": {
             "end": { "character": 11, "line": 1 },
             "start": { "character": 10, "line": 1 }
           }
         },
         {
           "children": [
             {
               "detail": "int",
               "kind": 13,
               "name": "a",
               "range": {
                 "end": { "character": 33, "line": 1 },
                 "start": { "character": 19, "line": 1 }
               },
               "selectionRange": {
                 "end": { "character": 24, "line": 1 },
                 "start": { "character": 23, "line": 1 }
               }
             },
             {
               "detail": "string",
               "kind": 13,
               "name": "b",
               "range": {
                 "end": { "character": 52, "line": 1 },
                 "start": { "character": 33, "line": 1 }
               },
               "selectionRange": {
                 "end": { "character": 38, "line": 1 },
                 "start": { "character": 37, "line": 1 }
               }
             }
           ],
           "detail": "string",
           "kind": 13,
           "name": "j",
           "range": {
             "end": { "character": 62, "line": 1 },
             "start": { "character": 0, "line": 1 }
           },
           "selectionRange": {
             "end": { "character": 14, "line": 1 },
             "start": { "character": 13, "line": 1 }
           }
         }
       ],
       "kind": 9,
       "name": "i, j",
       "range": {
         "end": { "character": 62, "line": 1 },
         "start": { "character": 0, "line": 1 }
       },
       "selectionRange": {
         "end": { "character": 14, "line": 1 },
         "start": { "character": 10, "line": 1 }
       }
     }] |}]

let%expect_test "Signature and module items" =
  document_symbol_test "contracts/lsp/go_to_implementations/inline.mligo";
  [%expect
    {|
    [{
       "children": [
         {
           "kind": 26,
           "name": "t",
           "range": {
             "end": { "character": 21, "line": 0 },
             "start": { "character": 15, "line": 0 }
           },
           "selectionRange": {
             "end": { "character": 21, "line": 0 },
             "start": { "character": 20, "line": 0 }
           }
         },
         {
           "detail": "int",
           "kind": 23,
           "name": "t",
           "range": {
             "end": { "character": 14, "line": 1 },
             "start": { "character": 2, "line": 1 }
           },
           "selectionRange": {
             "end": { "character": 8, "line": 1 },
             "start": { "character": 7, "line": 1 }
           }
         }
       ],
       "detail": "sig\n  type t =  int\n  end",
       "kind": 2,
       "name": "M",
       "range": {
         "end": { "character": 3, "line": 2 },
         "start": { "character": 0, "line": 0 }
       },
       "selectionRange": {
         "end": { "character": 8, "line": 0 },
         "start": { "character": 7, "line": 0 }
       }
     }] |}]

let%expect_test "Filters ghost identifiers" =
  document_symbol_test "contracts/lsp/hover/missing_type.mligo";
  [%expect
    {|
    [{
       "kind": 23,
       "name": "a",
       "range": {
         "end": { "character": 8, "line": 0 },
         "start": { "character": 0, "line": 0 }
       },
       "selectionRange": {
         "end": { "character": 6, "line": 0 },
         "start": { "character": 5, "line": 0 }
       }
     }] |}]

let%expect_test "Missing module name still shows hierarchy" =
  document_symbol_test "contracts/lsp/missing_module_name.mligo";
  [%expect
    {|
    [{
       "children": [
         {
           "detail": "int",
           "kind": 13,
           "name": "y",
           "range": {
             "end": { "character": 12, "line": 1 },
             "start": { "character": 2, "line": 1 }
           },
           "selectionRange": {
             "end": { "character": 7, "line": 1 },
             "start": { "character": 6, "line": 1 }
           }
         }
       ],
       "detail": "sig\n  val y : int\n  end",
       "kind": 2,
       "name": "?",
       "range": {
         "end": { "character": 3, "line": 2 },
         "start": { "character": 0, "line": 0 }
       },
       "selectionRange": {
         "end": { "character": 6, "line": 0 },
         "start": { "character": 6, "line": 0 }
       }
     }] |}]

let%expect_test "Type hierarchy" =
  document_symbol_test "contracts/lsp/hover/ctors_and_fields.mligo";
  [%expect
    {|
    [{
       "children": [
         {
           "kind": 22,
           "name": "Foo",
           "range": {
             "end": { "character": 7, "line": 1 },
             "start": { "character": 4, "line": 1 }
           },
           "selectionRange": {
             "end": { "character": 7, "line": 1 },
             "start": { "character": 4, "line": 1 }
           }
         },
         {
           "detail": "int",
           "kind": 22,
           "name": "Bar",
           "range": {
             "end": { "character": 14, "line": 2 },
             "start": { "character": 4, "line": 2 }
           },
           "selectionRange": {
             "end": { "character": 7, "line": 2 },
             "start": { "character": 4, "line": 2 }
           }
         },
         {
           "detail": "unit",
           "kind": 22,
           "name": "Baz",
           "range": {
             "end": { "character": 15, "line": 3 },
             "start": { "character": 4, "line": 3 }
           },
           "selectionRange": {
             "end": { "character": 7, "line": 3 },
             "start": { "character": 4, "line": 3 }
           }
         },
         {
           "children": [
             {
               "detail": "int",
               "kind": 8,
               "name": "a",
               "range": {
                 "end": { "character": 20, "line": 4 },
                 "start": { "character": 13, "line": 4 }
               },
               "selectionRange": {
                 "end": { "character": 14, "line": 4 },
                 "start": { "character": 13, "line": 4 }
               }
             },
             {
               "detail": "bool",
               "kind": 8,
               "name": "b",
               "range": {
                 "end": { "character": 30, "line": 4 },
                 "start": { "character": 22, "line": 4 }
               },
               "selectionRange": {
                 "end": { "character": 23, "line": 4 },
                 "start": { "character": 22, "line": 4 }
               }
             }
           ],
           "detail": "{\n a : int;\n b : bool\n}",
           "kind": 22,
           "name": "Aaa",
           "range": {
             "end": { "character": 32, "line": 4 },
             "start": { "character": 4, "line": 4 }
           },
           "selectionRange": {
             "end": { "character": 7, "line": 4 },
             "start": { "character": 4, "line": 4 }
           }
         }
       ],
       "detail": "| Aaa of {\n   a : int;\n   b : bool\n  }\n| Bar of int\n| Baz of unit\n| Foo",
       "kind": 10,
       "name": "t",
       "range": {
         "end": { "character": 32, "line": 4 },
         "start": { "character": 0, "line": 0 }
       },
       "selectionRange": {
         "end": { "character": 6, "line": 0 },
         "start": { "character": 5, "line": 0 }
       }
     }] |}]
