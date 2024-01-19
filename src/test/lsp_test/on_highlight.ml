module Requests = Ligo_lsp.Server.Requests
open Alcotest_extras
open Handlers
open Lsp_helpers
open Requests.Handler

type highlight_test =
  { test_name : string
  ; test_file : string
  ; position : Position.t
  ; highlights : Range.t list
  }

let get_highlight_test ({ test_name; test_file; position; highlights } : highlight_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let actual_highlights, _diagnostics =
    test_run_session
    @@ let@ uri = open_file (Path.from_relative test_file) in
       Requests.on_req_highlight position uri
  in
  let expected_highlights =
    List.map ~f:(fun range -> Lsp.Types.DocumentHighlight.create ~range ()) highlights
  in
  match actual_highlights with
  | None ->
    Alcotest.fail
    @@ Format.asprintf
         "Expected to find Some highlights for %s, but found None."
         test_file
  | Some actual_highlights ->
    should_match_list
      DocumentHighlight.testable
      ~msg:
        (Format.asprintf "Highlights mismatch for: %s, %a" test_file Position.pp position)
      ~expected:expected_highlights
      ~actual:actual_highlights


let test_cases =
  let open Range.Construct in
  [ { test_name = "references of term with sig items and top level ref"
    ; test_file = "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; position = Position.create ~line:9 ~character:6
    ; highlights = [ interval 3 6 7; interval 9 6 7; interval 12 13 14 ]
    }
  ; { test_name = "references of type with sig items and top level ref"
    ; test_file = "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; position = Position.create ~line:9 ~character:10
    ; highlights = [ interval 1 7 8; interval 3 10 11; interval 7 7 8; interval 9 10 11 ]
    }
  ; { test_name = "references from inline and standalone sigs and mods"
    ; test_file = "contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
    ; position = Position.create ~line:8 ~character:70
    ; highlights =
        [ interval 1 7 8
        ; interval 5 7 8
        ; interval 8 38 39
        ; interval 8 70 71
        ; interval 9 7 8
        ]
    }
  ; { test_name = "references from multiple impls"
    ; test_file = "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
    ; position = Position.create ~line:6 ~character:7
    ; highlights = [ interval 1 7 8; interval 2 12 13; interval 6 7 8; interval 11 7 8 ]
    }
  ; { test_name = "signature and include"
    ; test_file = "contracts/lsp/go_to_implementations/signature_and_include.mligo"
    ; position = Position.create ~line:12 ~character:11
    ; highlights = [ interval 1 6 7; interval 7 10 11; interval 12 10 11 ]
    }
  ; { test_name = "highlights from included type"
    ; test_file = "contracts/lsp/go_to_implementations/include.mligo"
    ; position = Position.create ~line:22 ~character:12
    ; highlights =
        [ interval 1 7 8
        ; interval 5 7 8
        ; interval 9 7 8
        ; interval 15 7 8
        ; interval 22 12 13
        ]
    }
  ; { test_name = "highlights of references to the same sig from different file"
    ; test_file = "contracts/lsp/go_to_implementations/implementation.mligo"
    ; position = Position.create ~line:3 ~character:7
    ; highlights = [ interval 3 7 8; interval 8 7 8 ]
    }
  ; { test_name = "highlights of references to the same sig from different file"
    ; test_file = "contracts/lsp/go_to_implementations/implementation.mligo"
    ; position = Position.create ~line:4 ~character:6
    ; highlights = [ interval 4 6 7; interval 9 6 7 ]
    }
  ; { test_name = "highlights of references to the same sig from different file"
    ; test_file = "contracts/lsp/go_to_implementations/implementation.mligo"
    ; position = Position.create ~line:2 ~character:12
    ; highlights = [ interval 2 12 13; interval 7 12 13 ]
    }
  ; { test_name = "highlights of shadowed type"
    ; test_file = "contracts/lsp/go_to_implementations/shadow.mligo"
    ; position = Position.create ~line:13 ~character:11
    ; highlights = [ interval 1 7 8; interval 10 7 8; interval 13 11 12 ]
    }
  ; { test_name = "highlights of different but same-named local variables"
    ; test_file = "contracts/lsp/shadow_var.mligo"
    ; position = Position.create ~line:5 ~character:14
    ; highlights = [ interval 5 11 18; interval 5 27 34 ]
    }
  ; { test_name = "highlights of same-named variables in different functions"
    ; test_file = "contracts/lsp/shadow_var.mligo"
    ; position = Position.create ~line:0 ~character:18
    ; highlights = [ interval 0 7 8; interval 0 18 19 ]
    }
  ]


let tests = "highlight", List.map ~f:get_highlight_test test_cases
