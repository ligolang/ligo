module Requests = Ligo_lsp.Server.Requests
open Alcotest_extras
open Handlers
open Lsp_helpers
open Requests.Handler

(* We use [Path.t] rather than [string] to support package files. *)
type references_test =
  { test_name : string
  ; test_file : Path.t
  ; reference : Position.t
  ; references : (Path.t * Range.t) list
  }

let get_references_test
    ({ test_name; test_file; reference; references } : references_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Slow
  @@ fun () ->
  let actual_references, _diagnostics =
    test_run_session
    @@ let@ uri = open_file test_file in
       Requests.on_req_references reference uri
  in
  let expected_references =
    List.map
      ~f:(fun (file, range) -> Location.create ~uri:(DocumentUri.of_path file) ~range)
      references
  in
  match actual_references with
  | None ->
    Alcotest.fail
    @@ Format.asprintf
         "Expected to find Some references for %a, but found None."
         Path.pp
         test_file
  | Some actual_references ->
    should_match_list
      Location.testable
      ~msg:
        (Format.asprintf
           "References mismatch for: %a, %a"
           Path.pp
           test_file
           Position.pp
           reference)
      ~expected:expected_references
      ~actual:actual_references


let test_cases =
  let open Range.Construct in
  let intervals file = List.map ~f:(fun range -> Path.from_relative file, range) in
  [ { test_name = "references in included file"
    ; test_file = Path.from_relative "contracts/lsp/includer/includer.mligo"
    ; reference = Position.create ~line:1 ~character:8
    ; references =
        [ Path.from_relative "contracts/lsp/includer/includer.mligo", interval 1 8 9
        ; Path.from_relative "contracts/lsp/included.mligo", interval 0 4 5
        ]
    }
  ; { test_name = "references in a file with michelson injections"
    ; test_file = Path.from_relative "contracts/lsp/references_michelson_inj.mligo"
    ; reference = Position.create ~line:4 ~character:8
    ; references =
        intervals
          "contracts/lsp/references_michelson_inj.mligo"
          [ interval 0 4 5; interval 4 8 9; interval 4 11 12 ]
    }
  ; { test_name = "references of term with sig items and top level ref"
    ; test_file =
        Path.from_relative "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; reference = Position.create ~line:9 ~character:6
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
          [ interval 3 6 7; interval 9 6 7; interval 12 13 14 ]
    }
  ; { test_name = "references of type with sig items and top level ref"
    ; test_file =
        Path.from_relative "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; reference = Position.create ~line:9 ~character:10
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
          [ interval 1 7 8; interval 3 10 11; interval 7 7 8; interval 9 10 11 ]
    }
  ; { test_name = "references from inline and standalone sigs and mods"
    ; test_file =
        Path.from_relative
          "contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
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
    ; test_file =
        Path.from_relative "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
    ; reference = Position.create ~line:6 ~character:7
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
          [ interval 1 7 8; interval 2 12 13; interval 6 7 8; interval 11 7 8 ]
    }
  ; { test_name = "signature and include"
    ; test_file =
        Path.from_relative
          "contracts/lsp/go_to_implementations/signature_and_include.mligo"
    ; reference = Position.create ~line:12 ~character:11
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/signature_and_include.mligo"
          [ interval 1 6 7; interval 7 10 11; interval 12 10 11 ]
    }
  ; { test_name = "references from included type"
    ; test_file = Path.from_relative "contracts/lsp/go_to_implementations/include.mligo"
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
    ; test_file = Path.from_relative "contracts/lsp/go_to_implementations/include.mligo"
    ; reference = Position.create ~line:19 ~character:10
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/include.mligo"
          [ interval 12 7 9; interval 19 10 12 ]
    }
  ; { test_name = "references of shadowed identifier"
    ; test_file = Path.from_relative "contracts/lsp/go_to_implementations/shadow.mligo"
    ; reference = Position.create ~line:13 ~character:11
    ; references =
        intervals
          "contracts/lsp/go_to_implementations/shadow.mligo"
          [ interval 1 7 8; interval 10 7 8; interval 13 11 12 ]
    }
  ; { test_name = "references of record field"
    ; test_file = Path.from_relative "contracts/lsp/references_ctors_and_fields.mligo"
    ; reference = Position.create ~line:19 ~character:28
    ; references =
        intervals
          "contracts/lsp/references_ctors_and_fields.mligo"
          [ interval 2 20 21
          ; interval 8 23 24
          ; interval 17 24 25
          ; interval 18 12 13
          ; interval 19 28 29
          ; interval 23 20 21
          ]
    }
  ; { test_name = "references of poly record field"
    ; test_file = Path.from_relative "contracts/lsp/references_ctors_and_fields.mligo"
    ; reference = Position.create ~line:6 ~character:33
    ; references =
        intervals
          "contracts/lsp/references_ctors_and_fields.mligo"
          [ interval 6 33 34; interval 20 40 41; interval 21 38 39; interval 22 13 14 ]
    }
  ; { test_name = "references of constructor"
    ; test_file = Path.from_relative "contracts/lsp/references_ctors_and_fields.mligo"
    ; reference = Position.create ~line:9 ~character:16
    ; references =
        intervals
          "contracts/lsp/references_ctors_and_fields.mligo"
          [ interval 0 12 15
          ; interval 8 6 9
          ; interval 9 7 10
          ; interval 9 16 19
          ; interval 10 11 14
          ; interval 10 20 23
          ; interval 23 2 5
          ]
    }
  ; { test_name = "references of poly constructor"
    ; test_file = Path.from_relative "contracts/lsp/references_ctors_and_fields.mligo"
    ; reference = Position.create ~line:11 ~character:31
    ; references =
        intervals
          "contracts/lsp/references_ctors_and_fields.mligo"
          [ interval 4 23 25; interval 11 31 33; interval 14 6 8 ]
    }
  ; { test_name = "Reference in reverse dependency"
    ; test_file = Path.from_relative "contracts/import_export/f.jsligo"
    ; reference = Position.create ~line:0 ~character:13
    ; references =
        [ Path.from_relative "contracts/import_export/f.jsligo", interval 0 13 14
        ; Path.from_relative "contracts/import_export/h.jsligo", interval 2 14 15
        ]
    }
  ; { test_name = "Reference of signature item in module in reverse dependency"
    ; test_file = Path.from_relative "contracts/lsp/go_to_implementations/interface.mligo"
    ; reference = Position.create ~line:2 ~character:6
    ; references =
        [ ( Path.from_relative "contracts/lsp/go_to_implementations/interface.mligo"
          , interval 2 6 7 )
        ; ( Path.from_relative "contracts/lsp/go_to_implementations/implementation.mligo"
          , interval 4 6 7 )
        ; ( Path.from_relative "contracts/lsp/go_to_implementations/implementation.mligo"
          , interval 9 6 7 )
        ]
    }
  ; { test_name = "Reference of item in reverse dependency in other directory"
    ; test_file = Path.from_relative "contracts/lsp/included.mligo"
    ; reference = Position.create ~line:0 ~character:4
    ; references =
        [ Path.from_relative "contracts/lsp/included.mligo", interval 0 4 5
        ; Path.from_relative "contracts/lsp/includer/includer.mligo", interval 1 8 9
        ]
    }
  ; (let package_path =
       Path.from_absolute
       @@ Option.value_exn
       @@ Lsp_test_helpers.Lib.resolve_lib_path
            ~project_root:"contracts/lsp"
            ~file:"contracts/lsp/registry.jsligo"
            ~lib_name:"bigarray"
            ~file_path:(Filename.concat "lib" "bigarray.mligo")
     in
     { test_name = "Reference of registry package in reverse dependency"
     ; test_file = package_path
     ; reference = Position.create ~line:27 ~character:4
     ; references =
         [ Path.from_relative "contracts/lsp/registry.jsligo", interval 8 20 27
         ; Path.from_relative "contracts/lsp/registry.jsligo", interval 14 26 33
         ; package_path, interval 27 4 11
         ]
     })
  ]


let tests = "references", List.map ~f:get_references_test test_cases
