open Lsp.Types
open Handlers
open Common
module Requests = Ligo_lsp.Server.Requests
open Requests.Handler

type definition_test =
  { test_name : string
  ; file_with_reference : string
  ; reference : Position.t
  ; file_with_definition : string
  ; definition : Range.t
  }

let get_definition_test
    ({ test_name; file_with_reference; reference; file_with_definition; definition } :
      definition_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let actual_definition, _diagnostics =
    test_run_session
    @@ let@ uri = open_file (to_absolute file_with_reference) in
       Requests.on_req_definition reference uri
  in
  let expected_definition =
    Location.create ~uri:(rel_path_to_uri file_with_definition) ~range:definition
  in
  Alcotest.(check (option testable_locations))
    (Format.asprintf
       "Definition position mismatch for: %s, %a"
       file_with_reference
       pp_position
       reference)
    (Some (`Location [ expected_definition ]))
    actual_definition


let test_cases =
  [ { test_name = "Identifier"
    ; file_with_reference = "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:1 ~character:8
    ; file_with_definition = "contracts/lsp/simple.mligo"
    ; definition = Utils.interval 0 4 5
    }
  ; { test_name = "Imported identifier"
    ; file_with_reference = "contracts/build/B.mligo"
    ; reference = Position.create ~line:6 ~character:19
    ; file_with_definition = "contracts/build/A.mligo"
    ; definition = Utils.interval 0 4 8
    }
  ; { test_name = "Identifier (local module)"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:6 ~character:18
    ; file_with_definition = "contracts/lsp/local_module.mligo"
    ; definition = Utils.interval 2 4 5
    }
  ; { test_name = "Type"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:9 ~character:8
    ; file_with_definition = "contracts/lsp/local_module.mligo"
    ; definition = Utils.interval 8 5 9
    }
  ; { test_name = "Type (local module)"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:8 ~character:14
    ; file_with_definition = "contracts/lsp/local_module.mligo"
    ; definition = Utils.interval 1 5 8
    }
  ; { test_name = "Local module"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:6 ~character:8
    ; file_with_definition = "contracts/lsp/local_module.mligo"
    ; definition = Utils.interval 0 7 8
    }
  ]


let tests = "definition", List.map ~f:get_definition_test test_cases
