module Requests = Ligo_lsp.Server.Requests
open Alcotest_extras
open Handlers
open Lsp_helpers
open Requests.Handler

type prepare_rename_test =
  { test_name : string
  ; file_path : string
  ; reference : Position.t
  ; can_rename : bool
  }

let get_prepare_rename_test
    ({ test_name; file_path; reference; can_rename } : prepare_rename_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let result, _diagnostics =
    test_run_session
    @@ let@ uri = open_file file_path in
       Requests.on_req_prepare_rename reference uri
  in
  let error_prefix = Format.asprintf "In %s, %a: " file_path Position.pp reference in
  match result with
  | None ->
    if can_rename
    then fail @@ error_prefix ^ "Cannot prepare rename for this reference."
    else ()
  | Some actual_range ->
    if can_rename
    then
      if Range.contains_position reference actual_range
      then ()
      else fail @@ error_prefix ^ "Reference is not contained within the range."
    else
      fail @@ error_prefix ^ "Should not be able to rename this identifier, but we can."


let test_cases =
  [ { test_name = "Identifier (definition)"
    ; file_path = to_absolute "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:0 ~character:4
    ; can_rename = true
    }
  ; { test_name = "Identifier (reference)"
    ; file_path = to_absolute "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:1 ~character:8
    ; can_rename = true
    }
  ; { test_name = "Identifier from stdlib"
    ; file_path = to_absolute "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:5 ~character:8
    ; can_rename = false
    }
  ; { test_name = "Number"
    ; file_path = to_absolute "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:0 ~character:8
    ; can_rename = false
    }
  ; { test_name = "Type"
    ; file_path = to_absolute "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:2 ~character:8
    ; can_rename = true
    }
  ; { test_name = "Module"
    ; file_path = to_absolute "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:6 ~character:16
    ; can_rename = true
    }
  ; { test_name = "Built-in type"
    ; file_path = to_absolute "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:1 ~character:11
    ; can_rename = false
    }
  ; { test_name = "Type from stdlib"
    ; file_path = to_absolute "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:5 ~character:28
    ; can_rename = false
    }
  ; { test_name = "Keyword"
    ; file_path = to_absolute "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:0 ~character:0
    ; can_rename = false
    }
  ; { test_name = "Registry package imported identifier"
    ; file_path = to_absolute "contracts/lsp/registry.jsligo"
    ; reference = Position.create ~line:9 ~character:20
    ; can_rename = false
    }
  ; { test_name = "Registry package identifier"
    ; file_path =
        Option.value_exn
        @@ Lsp_test_helpers.Lib.resolve_lib_path
             ~project_root:"contracts/lsp"
             ~file:"contracts/lsp/registry.jsligo"
             ~lib_name:"bigarray"
             ~file_path:(Filename.concat "lib" "bigarray.mligo")
    ; reference = Position.create ~line:27 ~character:4
    ; can_rename = false
    }
  ]


let tests = "prepare_rename", List.map ~f:get_prepare_rename_test test_cases
