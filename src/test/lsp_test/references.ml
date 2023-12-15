module Requests = Ligo_lsp.Server.Requests
open Alcotest_extras
open Handlers
open Lsp_helpers
open Requests.Handler

type references_test =
  { test_name : string
  ; test_file : string
  ; reference : Position.t
  ; references : (string * Range.t) list
  }

let get_references_test
    ({ test_name; test_file; reference; references } : references_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let actual_references, _diagnostics =
    test_run_session
    @@ let@ uri = open_file (Path.from_relative test_file) in
       Requests.on_req_references reference uri
  in
  let expected_references =
    List.map
      ~f:(fun (file, range) ->
        Location.create ~uri:(DocumentUri.of_path @@ Path.from_relative file) ~range)
      references
  in
  match actual_references with
  | None ->
    Alcotest.fail
    @@ Format.asprintf
         "Expected to find Some references for %s, but found None."
         test_file
  | Some actual_references ->
    should_match_list
      Location.testable
      ~msg:
        (Format.asprintf
           "References mismatch for: %s, %a"
           test_file
           Position.pp
           reference)
      ~expected:expected_references
      ~actual:actual_references


let test_cases =
  let open Range.Construct in
  let intervals file = List.map ~f:(fun range -> file, range) in
  [ { test_name = "references in included file"
    ; test_file = "contracts/lsp/includer/includer.mligo"
    ; reference = Position.create ~line:1 ~character:8
    ; references =
        [ "contracts/lsp/includer/includer.mligo", interval 1 8 9
        ; "contracts/lsp/included.mligo", interval 0 4 5
        ]
    }
  ; { test_name = "references in a file with michelson injections"
    ; test_file = "contracts/lsp/references_michelson_inj.mligo"
    ; reference = Position.create ~line:4 ~character:8
    ; references =
        intervals
          "contracts/lsp/references_michelson_inj.mligo"
          [ interval 0 4 5; interval 4 8 9; interval 4 11 12 ]
    }
  ; { test_name = "references in a file with michelson injections"
    ; test_file = "contracts/lsp/references_michelson_inj.mligo"
    ; reference = Position.create ~line:4 ~character:8
    ; references =
        intervals
          "contracts/lsp/references_michelson_inj.mligo"
          [ interval 0 4 5; interval 4 8 9; interval 4 11 12 ]
    }
  ; { test_name = "references of term with sig items and top level ref"
    ; test_file = "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; reference = Position.create ~line:9 ~character:6
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
          [ interval 3 6 7; interval 9 6 7; interval 12 13 14 ]
    }
  ; { test_name = "references of type with sig items and top level ref"
    ; test_file = "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; reference = Position.create ~line:9 ~character:10
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
          [ interval 1 7 8; interval 3 10 11; interval 7 7 8; interval 9 10 11 ]
    }
  ; { test_name = "references from inline and standalone sigs and mods"
    ; test_file = "contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
    ; reference = Position.create ~line:8 ~character:70
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
          [ interval 1 7 8
          ; interval 5 7 8
          ; interval 8 38 39
          ; interval 8 70 71
          ; interval 9 7 8
          ]
    }
  ; { test_name = "references from multiple impls"
    ; test_file = "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
    ; reference = Position.create ~line:6 ~character:7
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
          [ interval 1 7 8; interval 2 12 13; interval 6 7 8; interval 11 7 8 ]
    }
  ; { test_name = "signature and include"
    ; test_file = "contracts/lsp/go_to_implementations/signature_and_include.mligo"
    ; reference = Position.create ~line:12 ~character:11
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/signature_and_include.mligo"
          [ interval 1 6 7; interval 7 10 11; interval 12 10 11 ]
    }
  ; { test_name = "references from included type"
    ; test_file = "contracts/lsp/go_to_implementations/include.mligo"
    ; reference = Position.create ~line:22 ~character:12
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/include.mligo"
          [ interval 1 7 8
          ; interval 5 7 8
          ; interval 9 7 8
          ; interval 15 7 8
          ; interval 22 12 13
          ]
    }
  ; { test_name = "references of included module"
    ; test_file = "contracts/lsp/go_to_implementations/include.mligo"
    ; reference = Position.create ~line:19 ~character:10
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/include.mligo"
          [ interval 12 7 9; interval 19 10 12 ]
    }
  ]


let tests = "references", List.map ~f:get_references_test test_cases
