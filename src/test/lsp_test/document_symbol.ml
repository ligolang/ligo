open Alcotest_extras
open Lsp_helpers
open Handlers
module Requests = Ligo_lsp.Server.Requests
open Requests.Handler

type document_symbol_test =
  { test_name : string
  ; file_name : string
  ; symbols : DocumentSymbol.t list
  }

let document_symbol_test ?(config : config option) { test_name; file_name; symbols }
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let actual_symbols, _diagnostics =
    test_run_session ?config
    @@ let@ uri = open_file (Path.from_relative file_name) in
       Requests.on_req_document_symbol uri
  in
  match actual_symbols with
  | None -> fail "Expected some symbol list, got None"
  | Some (`DocumentSymbol actual_symbols) ->
    should_match_list DocumentSymbol.testable ~expected:symbols ~actual:actual_symbols


let test_cases =
  let open Range.Construct in
  [ { test_name = "Simple document symbols"
    ; file_name = "contracts/lsp/hovers.jsligo"
    ; symbols =
        [ DocumentSymbol.create
            ~detail:
              "<p, s>(_: (_: p) => (_: s) => [list<operation>, s]) => (\n\
              \  _: option<key_hash>\n\
               ) => (_: tez) => (_: s) => [operation, address]"
            ~kind:SymbolKind.Variable
            ~name:"z"
            ~range:(interval 0 6 31)
            ~selectionRange:(interval 0 6 7)
            ()
        ; DocumentSymbol.create
            ~detail:"Test.Proxy_ticket.proxy_address"
            ~kind:SymbolKind.Struct
            ~name:"t"
            ~range:(interval 2 0 40)
            ~selectionRange:(interval 2 5 6)
            ()
        ]
    }
  ; { test_name = "Nested defs in body and constructors"
    ; file_name = "contracts/lsp/patterns.mligo"
    ; symbols =
        [ DocumentSymbol.create
            ~detail:"Foo of (int * string)"
            ~kind:SymbolKind.Enum
            ~name:"v"
            ~range:(interval 0 0 28)
            ~selectionRange:(interval 0 5 6)
            ()
        ; DocumentSymbol.create
            ~children:
              (let a_b =
                 [ DocumentSymbol.create
                     ~detail:"int"
                     ~kind:SymbolKind.Variable
                     ~name:"a"
                     ~range:(interval 1 19 33)
                     ~selectionRange:(interval 1 23 24)
                     ()
                 ; DocumentSymbol.create
                     ~detail:"string"
                     ~kind:SymbolKind.Variable
                     ~name:"b"
                     ~range:(interval 1 33 52)
                     ~selectionRange:(interval 1 37 38)
                     ()
                 ]
               in
               [ DocumentSymbol.create
                   ~children:a_b
                   ~detail:"int"
                   ~kind:SymbolKind.Variable
                   ~name:"i"
                   ~range:(interval 1 0 62)
                   ~selectionRange:(interval 1 10 11)
                   ()
               ; DocumentSymbol.create
                   ~children:a_b
                   ~detail:"string"
                   ~kind:SymbolKind.Variable
                   ~name:"j"
                   ~range:(interval 1 0 62)
                   ~selectionRange:(interval 1 13 14)
                   ()
               ])
            ~kind:SymbolKind.Constructor
            ~name:"i, j"
            ~range:(interval 1 0 62)
            ~selectionRange:(interval 1 10 14)
            ()
        ]
    }
  ; { test_name = "Signature and module items"
    ; file_name = "contracts/lsp/go_to_implementations/inline.mligo"
    ; symbols =
        [ DocumentSymbol.create
            ~children:
              [ DocumentSymbol.create
                  ~kind:SymbolKind.TypeParameter
                  ~name:"t"
                  ~range:(interval 0 15 21)
                  ~selectionRange:(interval 0 20 21)
                  ()
              ; DocumentSymbol.create
                  ~detail:"int"
                  ~kind:SymbolKind.Struct
                  ~name:"t"
                  ~range:(interval 1 2 14)
                  ~selectionRange:(interval 1 7 8)
                  ()
              ]
            ~detail:"sig\n  type t =  int\n  end"
            ~kind:SymbolKind.Module
            ~name:"M"
            ~range:(range (0, 0) (2, 3))
            ~selectionRange:(interval 0 7 8)
            ()
        ]
    }
  ; { test_name = "Filters ghost identifiers"
    ; file_name = "contracts/lsp/hover/missing_type.mligo"
    ; symbols =
        [ DocumentSymbol.create
            ~kind:SymbolKind.Struct
            ~name:"a"
            ~range:(interval 0 0 8)
            ~selectionRange:(interval 0 5 6)
            ()
        ]
    }
  ]


let tests = "document_symbol", List.map ~f:document_symbol_test test_cases
