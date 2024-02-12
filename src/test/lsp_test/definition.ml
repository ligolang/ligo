module Requests = Ligo_lsp.Server.Requests
open Alcotest_extras
open Handlers
open Lsp_helpers
open Range.Construct
open Requests.Handler

type def_type =
  | Decl
  | Def
  | Impl
  | Type_def

type definition_test =
  { test_name : string
  ; file_with_reference : string
  ; reference : Position.t
  ; file_with_definition : Path.t (* To support packaged files *)
  ; definitions : Range.t list option
  ; def_type : def_type
  }

let get_definition_test
    ({ test_name
     ; file_with_reference
     ; reference
     ; file_with_definition
     ; definitions
     ; def_type
     } :
      definition_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let get_definition =
    Requests.(
      match def_type with
      | Decl -> on_req_declaration
      | Def -> on_req_definition
      | Impl -> on_req_implementation
      | Type_def -> on_req_type_definition)
  in
  let actual_definitions, diagnostics =
    test_run_session
    @@ let@ uri = open_file (Path.from_relative file_with_reference) in
       get_definition reference uri
  in
  let expected_definitions =
    Option.bind definitions ~f:(function
        | [] -> None
        | _ :: _ as definitions ->
          Some
            (`Location
              (List.map definitions ~f:(fun def ->
                   Location.create
                     ~uri:(DocumentUri.of_path file_with_definition)
                     ~range:def))))
  in
  check
    Alcotest.(option Locations.testable)
    (Format.asprintf
       "Definition position mismatch for: %s, %a.\nDiagnostics for this test: %a"
       file_with_reference
       Position.pp
       reference
       Fmt.Dump.(list (pair Path.pp (list Diagnostic.pp)))
       (Path_hashtbl.to_alist diagnostics))
    expected_definitions
    actual_definitions


let test_cases =
  [ { test_name = "Identifier"
    ; file_with_reference = "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:1 ~character:8
    ; file_with_definition = Path.from_relative "contracts/lsp/simple.mligo"
    ; definitions = Some [ interval 0 4 5 ]
    ; def_type = Def
    }
  ; { test_name = "Imported identifier"
    ; file_with_reference = "contracts/build/B.mligo"
    ; reference = Position.create ~line:7 ~character:19
    ; file_with_definition = Path.from_relative "contracts/build/A.mligo"
    ; definitions = Some [ interval 0 4 8 ]
    ; def_type = Def
    }
  ; { test_name = "Identifier (local module)"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:6 ~character:18
    ; file_with_definition = Path.from_relative "contracts/lsp/local_module.mligo"
    ; definitions = Some [ interval 2 4 5 ]
    ; def_type = Def
    }
  ; { test_name = "Type"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:9 ~character:8
    ; file_with_definition = Path.from_relative "contracts/lsp/local_module.mligo"
    ; definitions = Some [ interval 8 5 9 ]
    ; def_type = Def
    }
  ; { test_name = "Type (local module)"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:8 ~character:14
    ; file_with_definition = Path.from_relative "contracts/lsp/local_module.mligo"
    ; definitions = Some [ interval 1 5 8 ]
    ; def_type = Def
    }
  ; { test_name = "Local module"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:6 ~character:8
    ; file_with_definition = Path.from_relative "contracts/lsp/local_module.mligo"
    ; definitions = Some [ interval 0 7 8 ]
    ; def_type = Def
    }
  ; { test_name = "stdlib definition"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:5 ~character:11
    ; file_with_definition = Path.from_relative "contracts/lsp/local_module.mligo"
    ; definitions = None
    ; def_type = Def
    }
  ; { test_name = "stdlib type definition"
    ; file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:5 ~character:11
    ; file_with_definition = Path.from_relative "contracts/lsp/local_module.mligo"
    ; definitions = None
    ; def_type = Type_def
    }
  ; { test_name = "Registry package imported identifier"
    ; file_with_reference = "contracts/lsp/registry.jsligo"
    ; reference = Position.create ~line:8 ~character:20
    ; file_with_definition =
        Path.from_absolute
        @@ Option.value_exn
        @@ Lsp_test_helpers.Lib.resolve_lib_path
             ~project_root:"contracts/lsp"
             ~file:"contracts/lsp/registry.jsligo"
             ~lib_name:"bigarray"
             ~file_path:(Filename.concat "lib" "bigarray.mligo")
    ; definitions = Some [ interval 27 4 11 ]
    ; def_type = Def
    }
  ; { test_name = "Can find type t from module in signature"
    ; file_with_reference = "contracts/lsp/go_to_implementations/simple.mligo"
    ; reference = Position.create ~line:5 ~character:7
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/simple.mligo"
    ; definitions = Some [ interval 1 7 8 ]
    ; def_type = Def
    }
  ; { test_name = "Can find type t from signature in module"
    ; file_with_reference = "contracts/lsp/go_to_implementations/simple.mligo"
    ; reference = Position.create ~line:1 ~character:7
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/simple.mligo"
    ; definitions = Some [ interval 5 7 8 ]
    ; def_type = Impl
    }
  ; { test_name = "Can find inlined type t from module in signature"
    ; file_with_reference = "contracts/lsp/go_to_implementations/inline.mligo"
    ; reference = Position.create ~line:1 ~character:7
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/inline.mligo"
    ; definitions = Some [ interval 0 20 21 ]
    ; def_type = Def
    }
  ; { test_name = "Can find inline type t from signature in module"
    ; file_with_reference = "contracts/lsp/go_to_implementations/inline.mligo"
    ; reference = Position.create ~line:0 ~character:20
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/inline.mligo"
    ; definitions = Some [ interval 1 7 8 ]
    ; def_type = Impl
    }
  ; { test_name = "Can find two implementations from definition"
    ; file_with_reference = "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
    ; reference = Position.create ~line:2 ~character:8
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
    ; definitions = Some [ interval 7 8 9; interval 12 8 9 ]
    ; def_type = Impl
    }
  ; { test_name = "Can find the definition from an implementation"
    ; file_with_reference = "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
    ; reference = Position.create ~line:12 ~character:8
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
    ; definitions = Some [ interval 2 8 9 ]
    ; def_type = Def
    }
  ; { test_name = "Can find implementations across aliases and includes"
    ; file_with_reference = "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; reference = Position.create ~line:5 ~character:6
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; definitions = Some [ interval 25 8 9; interval 32 6 7 ]
    ; def_type = Impl
    }
  ; { test_name = "Can find definition across aliases and includes"
    ; file_with_reference = "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; reference = Position.create ~line:32 ~character:6
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; definitions = Some [ interval 5 6 7 ]
    ; def_type = Def
    }
  ; { test_name = "Can find definition across aliases and includes"
    ; file_with_reference = "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; reference = Position.create ~line:25 ~character:8
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; definitions = Some [ interval 5 6 7 ]
    ; def_type = Def
    }
  ; { test_name = "Multiple definitions"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
    ; reference = Position.create ~line:9 ~character:7
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
    ; definitions =
        Some [ interval 1 7 8; interval 5 7 8; interval 8 38 39; interval 8 70 71 ]
    ; def_type = Def
    }
  ; { test_name = "Find definition from top level"
    ; file_with_reference = "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; reference = Position.create ~line:12 ~character:13
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; definitions = Some [ interval 3 6 7 ]
    ; def_type = Def
    }
  ; { test_name = "Find declaration from top level"
    ; file_with_reference = "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; reference = Position.create ~line:12 ~character:13
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; definitions = Some [ interval 9 6 7 ]
    ; def_type = Decl
    }
  ; { test_name = "Find implementations of included signature"
    ; file_with_reference = "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; reference = Position.create ~line:4 ~character:12
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; definitions = Some [ interval 17 7 20; interval 29 7 21 ]
    ; def_type = Impl
    }
  ; { test_name = "Find implementations of signature alias"
    ; file_with_reference = "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; reference = Position.create ~line:29 ~character:24
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; definitions = Some [ interval 17 7 20; interval 29 7 21 ]
    ; def_type = Impl
    }
  ; { test_name = "Find type definition of inferred record type (CameLIGO)"
    ; file_with_reference = "contracts/lsp/type_definition_regression.mligo"
    ; reference = Position.create ~line:5 ~character:4
    ; file_with_definition =
        Path.from_relative "contracts/lsp/type_definition_regression.mligo"
    ; definitions = Some [ interval 0 5 6 ]
    ; def_type = Type_def
    }
  ; { test_name = "Find type definition of inferred record type (JsLIGO)"
    ; file_with_reference = "contracts/lsp/type_definition_regression.jsligo"
    ; reference = Position.create ~line:2 ~character:6
    ; file_with_definition =
        Path.from_relative "contracts/lsp/type_definition_regression.jsligo"
    ; definitions = Some [ interval 0 5 6 ]
    ; def_type = Type_def
    }
  ; { test_name = "Find implementations in other modules from inclusions"
    ; file_with_reference = "contracts/lsp/go_to_implementations/include.mligo"
    ; reference = Position.create ~line:1 ~character:7
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/include.mligo"
    ; definitions = Some [ interval 5 7 8; interval 9 7 8; interval 15 7 8 ]
    ; def_type = Impl
    }
  ; { test_name = "Find declaration in inclusion"
    ; file_with_reference = "contracts/lsp/go_to_implementations/include.mligo"
    ; reference = Position.create ~line:22 ~character:12
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/include.mligo"
    ; definitions = Some [ interval 15 7 8 ]
    ; def_type = Decl
    }
  ; { test_name = "Find implementations of signature within included module"
    ; file_with_reference = "contracts/lsp/go_to_implementations/include.mligo"
    ; reference = Position.create ~line:0 ~character:12
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/include.mligo"
    ; definitions =
        Some [ interval 4 7 9; interval 8 7 9; interval 12 7 9; interval 18 7 9 ]
    ; def_type = Impl
    }
  ; { test_name = "Go to definition of included variable in top-level inside module"
    ; file_with_reference = "contracts/lsp/go_to_implementations/top_level_include.mligo"
    ; reference = Position.create ~line:8 ~character:8
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/top_level_include.mligo"
    ; definitions = Some [ interval 1 6 7 ]
    ; def_type = Decl
    }
  ; { test_name = "Go to definition of record field in top-level"
    ; file_with_reference = "contracts/lsp/references_ctors_and_fields.mligo"
    ; reference = Position.create ~line:18 ~character:14
    ; file_with_definition =
        Path.from_relative "contracts/lsp/references_ctors_and_fields.mligo"
    ; definitions = Some [ interval 2 36 37 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of constructor in top-level"
    ; file_with_reference = "contracts/lsp/references_ctors_and_fields.mligo"
    ; reference = Position.create ~line:9 ~character:16
    ; file_with_definition =
        Path.from_relative "contracts/lsp/references_ctors_and_fields.mligo"
    ; definitions = Some [ interval 0 12 15 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of collided constructor (local scope)"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:4 ~character:3
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; definitions = Some [ interval 3 11 14 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of collided constructor in (global scope)"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:8 ~character:2
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; definitions = Some [ interval 0 9 12 ]
    ; def_type = Def
    }
  ; { test_name = "Go to type definition of collided constructor (local scope)"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:4 ~character:3
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; definitions = Some [ interval 3 7 8 ]
    ; def_type = Type_def
    }
  ; { test_name = "Go to type definition of collided constructor in (global scope)"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:8 ~character:2
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; definitions = Some [ interval 0 5 6 ]
    ; def_type = Type_def
    }
  ; { test_name = "Go to definition of constructor in match clause"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:12 ~character:4
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; definitions = Some [ interval 3 11 14 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of constructor defined in module"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:20 ~character:8
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; definitions = Some [ interval 16 13 16 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of poly record field inside constructor"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:24 ~character:14
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; definitions = Some [ interval 22 21 22 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of record field definied in signature"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:34 ~character:10
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; definitions = Some [ interval 27 13 14 ]
    ; def_type = Def
    }
  ; { test_name = "Go to declaration of record field definied in signature"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:34 ~character:10
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; definitions = Some [ interval 31 13 14 ]
    ; def_type = Decl
    }
  ; { test_name = "Go to type definition of record field in unnamed type"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:24 ~character:14
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; definitions = Some [ interval 22 19 39 ]
    ; def_type = Type_def
    }
  ; { test_name = "Go to definition of constructor (Core)"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
    ; reference = Position.create ~line:11 ~character:12
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
    ; definitions = Some [ interval 8 9 12 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of constructor defined in module (Core)"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
    ; reference = Position.create ~line:12 ~character:13
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
    ; definitions = Some [ interval 1 11 14 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of constructor defined in nested module (Core)"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
    ; reference = Position.create ~line:13 ~character:11
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
    ; definitions = Some [ interval 4 13 16 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of constructor defined in matchee (Core)"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
    ; reference = Position.create ~line:16 ~character:7
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
    ; definitions = Some [ interval 15 17 20 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of constructor defined in pattern"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:42 ~character:4
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; definitions = Some [ interval 40 11 12 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of constructor defined in return type"
    ; file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:42 ~character:11
    ; file_with_definition =
        Path.from_relative
          "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; definitions = Some [ interval 40 23 24 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of disc union type common field in switch"
    ; file_with_reference = "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; reference = Position.create ~line:3 ~character:12
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; definitions = Some [ interval 0 38 42 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of disc union type left field"
    ; file_with_reference = "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; reference = Position.create ~line:4 ~character:24
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; definitions = Some [ interval 0 24 25 ]
    ; def_type = Def
    }
  ; { test_name = "Go to definition of disc union type right field"
    ; file_with_reference = "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; reference = Position.create ~line:5 ~character:22
    ; file_with_definition =
        Path.from_relative "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; definitions = Some [ interval 0 52 53 ]
    ; def_type = Def
    }
  ]


let tests = "definition", List.map ~f:get_definition_test test_cases
