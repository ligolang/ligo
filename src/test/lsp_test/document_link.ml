open Linol_lwt
open Handlers
open Common
module Requests = Ligo_lsp.Server.Requests
open Requests.Handler

type document_link_test =
  { file_path : string
  ; document_links : DocumentLink.t list
  }

let eq_document_link : DocumentLink.t -> DocumentLink.t -> bool = Caml.( = )

let pp_document_link : DocumentLink.t Fmt.t =
 fun formatter link -> Yojson.Safe.pretty_print formatter @@ DocumentLink.yojson_of_t link


let testable_document_link : DocumentLink.t Alcotest.testable =
  Alcotest.testable pp_document_link eq_document_link


let get_document_link_test ({ file_path; document_links } : document_link_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case file_path `Quick
  @@ fun () ->
  let links_opt, _diagnostics =
    test_run_session
    @@ let@ uri = open_file (to_absolute file_path) in
       Requests.on_req_document_link uri
  in
  match links_opt with
  | Some links ->
    check
      Alcotest.(list testable_document_link)
      (Format.asprintf "Document links mismatch for %s:" file_path)
      links
      document_links
  | None -> fail "Expected some list of document links, got None"


let test_cases =
  [ { file_path = "contracts/includer.mligo"
    ; document_links =
        [ { range = Utils.interval 2 9 25
          ; target = Some (to_absolute "contracts/included.mligo")
          ; tooltip = None
          ; data = None
          }
        ]
    }
  ; { file_path = "contracts/includer.jsligo"
    ; document_links =
        [ { range = Utils.interval 2 9 26
          ; target = Some (to_absolute "contracts/included.jsligo")
          ; tooltip = None
          ; data = None
          }
        ]
    }
  ; { file_path = "contracts/build/E.mligo" (* with #import *)
    ; document_links =
        [ { range = Utils.interval 0 8 17
          ; target = Some (to_absolute "contracts/build/F.mligo")
          ; tooltip = None
          ; data = None
          }
        ; { range = Utils.interval 1 8 17
          ; target = Some (to_absolute "contracts/build/G.mligo")
          ; tooltip = None
          ; data = None
          }
        ]
    }
  ; { file_path = "contracts/build/B.jsligo" (* with #import *)
    ; document_links =
        [ { range = Utils.interval 0 8 18
          ; target = Some (to_absolute "contracts/build/A.jsligo")
          ; tooltip = None
          ; data = None
          }
        ]
    }
  ]


let tests = "document_link", List.map ~f:get_document_link_test test_cases
