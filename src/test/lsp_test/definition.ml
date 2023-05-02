module Requests = Ligo_lsp.Server.Requests
open Alcotest_extras
open Handlers
open Lsp_helpers
open Range.Construct
open Requests.Handler

type definition_test =
  { test_name : string
  ; file_with_reference : string
  ; reference : Position.t
  ; file_with_definition : string
  ; definition : Range.t option
  ; type_definition : bool
  }

let get_definition_test
    ({ test_name
     ; file_with_reference
     ; reference
     ; file_with_definition
     ; definition
     ; type_definition
     } :
      definition_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let get_definition =
    Requests.(if type_definition then on_req_type_definition else on_req_definition)
  in
  let actual_definition, _diagnostics =
    test_run_session
    @@ let@ uri = open_file (to_absolute file_with_reference) in
       get_definition reference uri
  in
  let expected_definition =
    Option.map definition ~f:(fun def ->
        `Location
          [ Location.create ~uri:(rel_path_to_uri file_with_definition) ~range:def ])
  in
  check
    Alcotest.(option Locations.testable)
    (Format.asprintf
       "Definition position mismatch for: %s, %a"
       file_with_reference
       Position.pp
       reference)
    expected_definition
    actual_definition


let test_cases =
  [ { test_name = "Identifier"
    ; file_with_reference = "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:1 ~character:8
    ; file_with_definition = "contracts/lsp/simple.mligo"
    ; definition = Some (interval 0 4 5)
    ; type_definition = false
    }
  ; { test_name = "Imported identifier"
    ; file_with_reference = "contracts/build/B.mligo"
    ; reference = Position.create ~line:6 ~character:19
    ; file_with_definition = "contracts/build/A.mligo"
    ; definition = Some (interval 0 4 8)
    ; type_definition = false
    }
  ; { test_name = "Identifier (local module)"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:6 ~character:18
    ; file_with_definition = "contracts/lsp/local_module.mligo"
    ; definition = Some (interval 2 4 5)
    ; type_definition = false
    }
  ; { test_name = "Type"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:9 ~character:8
    ; file_with_definition = "contracts/lsp/local_module.mligo"
    ; definition = Some (interval 8 5 9)
    ; type_definition = false
    }
  ; { test_name = "Type (local module)"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:8 ~character:14
    ; file_with_definition = "contracts/lsp/local_module.mligo"
    ; definition = Some (interval 1 5 8)
    ; type_definition = false
    }
  ; { test_name = "Local module"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:6 ~character:8
    ; file_with_definition = "contracts/lsp/local_module.mligo"
    ; definition = Some (interval 0 7 8)
    ; type_definition = false
    }
  ; { test_name = "stdlib definition"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:5 ~character:11
    ; file_with_definition = "contracts/lsp/local_module.mligo"
    ; definition = None
    ; type_definition = false
    }
  ; { test_name = "stdlib type definition"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:5 ~character:11
    ; file_with_definition = "contracts/lsp/local_module.mligo"
    ; definition = None
    ; type_definition = true
    }
  ]


let tests = "definition", List.map ~f:get_definition_test test_cases
