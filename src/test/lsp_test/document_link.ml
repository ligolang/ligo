module Requests = Ligo_lsp.Server.Requests
open Handlers
open Alcotest_extras
open Requests.Handler
open Lsp_helpers
open Range.Construct

type document_link_test =
  { file_path : string
  ; document_links : DocumentLink.t list
  }

let get_document_link_test ({ file_path; document_links } : document_link_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case file_path `Quick
  @@ fun () ->
  let links_opt, _diagnostics =
    test_run_session
    @@ let@ uri = open_file (Path.from_relative file_path) in
       Requests.on_req_document_link uri
  in
  match links_opt with
  | Some links ->
    check
      Alcotest.(list DocumentLink.testable)
      (Format.asprintf "Document links mismatch for %s:" file_path)
      links
      document_links
  | None -> fail "Expected some list of document links, got None"


let test_cases =
  let relative x = Some (DocumentLink.path_to_target (Path.from_relative x)) in
  [ { file_path = "contracts/includer.mligo"
    ; document_links =
        [ { range = interval 2 9 25
          ; target = relative "contracts/included.mligo"
          ; tooltip = None
          ; data = None
          }
        ]
    }
  ; { file_path = "contracts/includer.jsligo"
    ; document_links =
        [ { range = interval 2 9 26
          ; target = relative "contracts/included.jsligo"
          ; tooltip = None
          ; data = None
          }
        ]
    }
  ; { file_path = "contracts/build/E.mligo" (* with #import *)
    ; document_links =
        [ { range = interval 0 8 17
          ; target = relative "contracts/build/F.mligo"
          ; tooltip = None
          ; data = None
          }
        ; { range = interval 1 8 17
          ; target = relative "contracts/build/G.mligo"
          ; tooltip = None
          ; data = None
          }
        ]
    }
  ; { file_path = "contracts/build/B.jsligo" (* with #import *)
    ; document_links =
        [ { range = interval 0 8 18
          ; target = relative "contracts/build/A.jsligo"
          ; tooltip = None
          ; data = None
          }
        ]
    }
  ; { file_path = "contracts/lsp/registry.jsligo" (* with #import *)
    ; document_links =
        [ { range = interval 0 8 43
          ; target =
              relative
                "contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/lib/bigarray.mligo"
          ; tooltip = None
          ; data = None
          }
        ]
    }
  ; { file_path = "contracts/lsp/registry_include.jsligo" (* with #include *)
    ; document_links =
        [ { range = interval 0 9 44
          ; target =
              relative
                "contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/lib/bigarray.mligo"
          ; tooltip = None
          ; data = None
          }
        ]
    }
  ]


let tests = "document_link", List.map ~f:get_document_link_test test_cases
