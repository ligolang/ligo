module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Lsp_helpers
open Requests.Handler

type def_type =
  | Decl
  | Def
  | Impl
  | Type_def

type definition_test =
  { file_with_reference : string
  ; reference : Position.t
  ; def_type : def_type
  }

let get_definition_test ({ file_with_reference; reference; def_type } : definition_test)
    : unit
  =
  let get_definition =
    Requests.(
      match def_type with
      | Decl -> on_req_declaration
      | Def -> on_req_definition
      | Impl -> on_req_implementation
      | Type_def -> on_req_type_definition)
  in
  let actual_definitions, _diagnostics =
    test_run_session
    @@ let@ uri = open_file @@ normalize_path file_with_reference in
       get_definition reference uri
  in
  let actual_definitions =
    Option.map actual_definitions ~f:(function
        | `Location locations ->
          `Location
            (List.map locations ~f:(fun loc -> { loc with uri = to_relative_uri loc.uri }))
        | `LocationLink links ->
          `LocationLink
            (List.map links ~f:(fun link ->
                 { link with targetUri = to_relative_uri link.targetUri })))
  in
  Format.printf "%a" (Fmt.Dump.option Locations.pp) actual_definitions


let%expect_test "Identifier" =
  get_definition_test
    { file_with_reference = "contracts/lsp/simple.mligo"
    ; reference = Position.create ~line:1 ~character:8
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 5, "line": 0 },
            "start": { "character": 4, "line": 0 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/simple.mligo"
        }
      ] |}]

let%expect_test "Imported identifier" =
  get_definition_test
    { file_with_reference = "contracts/build/B.mligo"
    ; reference = Position.create ~line:7 ~character:19
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 8, "line": 0 },
            "start": { "character": 4, "line": 0 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/build/A.mligo"
        }
      ] |}]

let%expect_test "Identifier (local module)" =
  get_definition_test
    { file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:6 ~character:18
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 5, "line": 2 },
            "start": { "character": 4, "line": 2 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/local_module.mligo"
        }
      ] |}]

let%expect_test "Type" =
  get_definition_test
    { file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:9 ~character:8
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 9, "line": 8 },
            "start": { "character": 5, "line": 8 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/local_module.mligo"
        }
      ] |}]

let%expect_test "Type (local module)" =
  get_definition_test
    { file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:8 ~character:14
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 8, "line": 1 },
            "start": { "character": 5, "line": 1 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/local_module.mligo"
        }
      ] |}]

let%expect_test "Local module" =
  get_definition_test
    { file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:6 ~character:8
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 8, "line": 0 },
            "start": { "character": 7, "line": 0 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/local_module.mligo"
        }
      ] |}]

let%expect_test "stdlib definition" =
  get_definition_test
    { file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:5 ~character:11
    ; def_type = Def
    };
  [%expect {| None |}]

let%expect_test "stdlib type definition" =
  get_definition_test
    { file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:5 ~character:11
    ; def_type = Type_def
    };
  [%expect {| None |}]

let%expect_test "Registry package imported identifier" =
  get_definition_test
    { file_with_reference = "contracts/lsp/registry.jsligo"
    ; reference = Position.create ~line:8 ~character:20
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 11, "line": 27 },
            "start": { "character": 4, "line": 27 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/lib/bigarray.mligo"
        }
      ] |}]

let%expect_test "Can find type t from module in signature" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/simple.mligo"
    ; reference = Position.create ~line:5 ~character:7
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 8, "line": 1 },
            "start": { "character": 7, "line": 1 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/simple.mligo"
        }
      ] |}]

let%expect_test "Can find type t from signature in module" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/simple.mligo"
    ; reference = Position.create ~line:1 ~character:7
    ; def_type = Impl
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 8, "line": 5 },
            "start": { "character": 7, "line": 5 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/simple.mligo"
        }
      ] |}]

let%expect_test "Can find inlined type t from module in signature" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/inline.mligo"
    ; reference = Position.create ~line:1 ~character:7
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 21, "line": 0 },
            "start": { "character": 20, "line": 0 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/inline.mligo"
        }
      ] |}]

let%expect_test "Can find inline type t from signature in module" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/inline.mligo"
    ; reference = Position.create ~line:0 ~character:20
    ; def_type = Impl
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 8, "line": 1 },
            "start": { "character": 7, "line": 1 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/inline.mligo"
        }
      ] |}]

let%expect_test "Can find two implementations from definition" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
    ; reference = Position.create ~line:2 ~character:8
    ; def_type = Impl
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 9, "line": 7 },
            "start": { "character": 8, "line": 7 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/two_namespaces.jsligo"
        },
        {
          "range": {
            "end": { "character": 9, "line": 12 },
            "start": { "character": 8, "line": 12 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/two_namespaces.jsligo"
        }
      ] |}]

let%expect_test "Can find the definition from an implementation" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
    ; reference = Position.create ~line:12 ~character:8
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 9, "line": 2 },
            "start": { "character": 8, "line": 2 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/two_namespaces.jsligo"
        }
      ] |}]

let%expect_test "Can find implementations across aliases and includes" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; reference = Position.create ~line:5 ~character:6
    ; def_type = Impl
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 9, "line": 25 },
            "start": { "character": 8, "line": 25 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/sig_alias.mligo"
        },
        {
          "range": {
            "end": { "character": 7, "line": 32 },
            "start": { "character": 6, "line": 32 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/sig_alias.mligo"
        }
      ] |}]

let%expect_test "Can find definition across aliases and includes" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; reference = Position.create ~line:32 ~character:6
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 7, "line": 5 },
            "start": { "character": 6, "line": 5 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/sig_alias.mligo"
        }
      ] |}]

let%expect_test "Can find definition across aliases and includes" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; reference = Position.create ~line:25 ~character:8
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 7, "line": 5 },
            "start": { "character": 6, "line": 5 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/sig_alias.mligo"
        }
      ] |}]

let%expect_test "Multiple definitions" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
    ; reference = Position.create ~line:9 ~character:7
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 8, "line": 1 },
            "start": { "character": 7, "line": 1 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
        },
        {
          "range": {
            "end": { "character": 8, "line": 5 },
            "start": { "character": 7, "line": 5 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
        },
        {
          "range": {
            "end": { "character": 39, "line": 8 },
            "start": { "character": 38, "line": 8 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
        },
        {
          "range": {
            "end": { "character": 71, "line": 8 },
            "start": { "character": 70, "line": 8 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
        }
      ] |}]

let%expect_test "Find definition from top level" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; reference = Position.create ~line:12 ~character:13
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 7, "line": 3 },
            "start": { "character": 6, "line": 3 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
        }
      ] |}]

let%expect_test "Find declaration from top level" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; reference = Position.create ~line:12 ~character:13
    ; def_type = Decl
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 7, "line": 9 },
            "start": { "character": 6, "line": 9 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
        }
      ] |}]

let%expect_test "Find implementations of included signature" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; reference = Position.create ~line:4 ~character:12
    ; def_type = Impl
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 20, "line": 17 },
            "start": { "character": 7, "line": 17 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/sig_alias.mligo"
        },
        {
          "range": {
            "end": { "character": 21, "line": 29 },
            "start": { "character": 7, "line": 29 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/sig_alias.mligo"
        }
      ] |}]

let%expect_test "Find implementations of signature alias" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/sig_alias.mligo"
    ; reference = Position.create ~line:29 ~character:24
    ; def_type = Impl
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 20, "line": 17 },
            "start": { "character": 7, "line": 17 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/sig_alias.mligo"
        },
        {
          "range": {
            "end": { "character": 21, "line": 29 },
            "start": { "character": 7, "line": 29 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/sig_alias.mligo"
        }
      ] |}]

let%expect_test "Find type definition of inferred record type (CameLIGO)" =
  get_definition_test
    { file_with_reference = "contracts/lsp/type_definition_regression.mligo"
    ; reference = Position.create ~line:5 ~character:4
    ; def_type = Type_def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 6, "line": 0 },
            "start": { "character": 5, "line": 0 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/type_definition_regression.mligo"
        }
      ] |}]

let%expect_test "Find type definition of inferred record type (JsLIGO)" =
  get_definition_test
    { file_with_reference = "contracts/lsp/type_definition_regression.jsligo"
    ; reference = Position.create ~line:2 ~character:6
    ; def_type = Type_def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 6, "line": 0 },
            "start": { "character": 5, "line": 0 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/type_definition_regression.jsligo"
        }
      ] |}]

let%expect_test "Find implementations in other modules from inclusions" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/include.mligo"
    ; reference = Position.create ~line:1 ~character:7
    ; def_type = Impl
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 8, "line": 5 },
            "start": { "character": 7, "line": 5 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
        },
        {
          "range": {
            "end": { "character": 8, "line": 9 },
            "start": { "character": 7, "line": 9 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
        },
        {
          "range": {
            "end": { "character": 8, "line": 15 },
            "start": { "character": 7, "line": 15 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
        }
      ] |}]

let%expect_test "Find declaration in inclusion" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/include.mligo"
    ; reference = Position.create ~line:22 ~character:12
    ; def_type = Decl
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 8, "line": 15 },
            "start": { "character": 7, "line": 15 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
        }
      ] |}]

let%expect_test "Find implementations of signature within included module" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/include.mligo"
    ; reference = Position.create ~line:0 ~character:12
    ; def_type = Impl
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 9, "line": 4 },
            "start": { "character": 7, "line": 4 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
        },
        {
          "range": {
            "end": { "character": 9, "line": 8 },
            "start": { "character": 7, "line": 8 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
        },
        {
          "range": {
            "end": { "character": 9, "line": 12 },
            "start": { "character": 7, "line": 12 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
        },
        {
          "range": {
            "end": { "character": 9, "line": 18 },
            "start": { "character": 7, "line": 18 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
        }
      ] |}]

let%expect_test "Go to definition of included variable in top-level inside module" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/top_level_include.mligo"
    ; reference = Position.create ~line:8 ~character:8
    ; def_type = Decl
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 7, "line": 1 },
            "start": { "character": 6, "line": 1 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/top_level_include.mligo"
        }
      ] |}]

let%expect_test "Go to definition of record field in top-level" =
  get_definition_test
    { file_with_reference = "contracts/lsp/references_ctors_and_fields.mligo"
    ; reference = Position.create ~line:18 ~character:14
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 37, "line": 2 },
            "start": { "character": 36, "line": 2 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
        }
      ] |}]

let%expect_test "Go to definition of constructor in top-level" =
  get_definition_test
    { file_with_reference = "contracts/lsp/references_ctors_and_fields.mligo"
    ; reference = Position.create ~line:9 ~character:16
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 15, "line": 0 },
            "start": { "character": 12, "line": 0 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
        }
      ] |}]

let%expect_test "Go to definition of collided constructor (local scope)" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:4 ~character:3
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 14, "line": 3 },
            "start": { "character": 11, "line": 3 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
        }
      ] |}]

let%expect_test "Go to definition of collided constructor in (global scope)" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:8 ~character:2
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 12, "line": 0 },
            "start": { "character": 9, "line": 0 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
        }
      ] |}]

let%expect_test "Go to type definition of collided constructor (local scope)" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:4 ~character:3
    ; def_type = Type_def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 8, "line": 3 },
            "start": { "character": 7, "line": 3 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
        }
      ] |}]

let%expect_test "Go to type definition of collided constructor in (global scope)" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:8 ~character:2
    ; def_type = Type_def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 6, "line": 0 },
            "start": { "character": 5, "line": 0 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
        }
      ] |}]

let%expect_test "Go to definition of constructor in match clause" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:12 ~character:4
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 14, "line": 3 },
            "start": { "character": 11, "line": 3 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
        }
      ] |}]

let%expect_test "Go to definition of constructor defined in module" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:20 ~character:8
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 16, "line": 16 },
            "start": { "character": 13, "line": 16 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
        }
      ] |}]

let%expect_test "Go to definition of poly record field inside constructor" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:24 ~character:14
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 22, "line": 22 },
            "start": { "character": 21, "line": 22 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
        }
      ] |}]

let%expect_test "Go to definition of record field definied in signature" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:34 ~character:10
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 14, "line": 27 },
            "start": { "character": 13, "line": 27 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
        }
      ] |}]

let%expect_test "Go to declaration of record field definied in signature" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:34 ~character:10
    ; def_type = Decl
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 14, "line": 31 },
            "start": { "character": 13, "line": 31 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
        }
      ] |}]

let%expect_test "Go to type definition of record field in unnamed type" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:24 ~character:14
    ; def_type = Type_def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 39, "line": 22 },
            "start": { "character": 19, "line": 22 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
        }
      ] |}]

let%expect_test "Go to definition of constructor (Core)" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
    ; reference = Position.create ~line:11 ~character:12
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 12, "line": 8 },
            "start": { "character": 9, "line": 8 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
        }
      ] |}]

let%expect_test "Go to definition of constructor defined in module (Core)" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
    ; reference = Position.create ~line:12 ~character:13
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 14, "line": 1 },
            "start": { "character": 11, "line": 1 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
        }
      ] |}]

let%expect_test "Go to definition of constructor defined in nested module (Core)" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
    ; reference = Position.create ~line:13 ~character:11
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 16, "line": 4 },
            "start": { "character": 13, "line": 4 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
        }
      ] |}]

let%expect_test "Go to definition of constructor defined in matchee (Core)" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
    ; reference = Position.create ~line:16 ~character:7
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 20, "line": 15 },
            "start": { "character": 17, "line": 15 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_core.mligo"
        }
      ] |}]

let%expect_test "Go to definition of constructor defined in pattern" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:42 ~character:4
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 12, "line": 40 },
            "start": { "character": 11, "line": 40 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
        }
      ] |}]

let%expect_test "Go to definition of constructor defined in return type" =
  get_definition_test
    { file_with_reference =
        "contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
    ; reference = Position.create ~line:42 ~character:11
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 24, "line": 40 },
            "start": { "character": 23, "line": 40 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ctors_and_fields_complex.mligo"
        }
      ] |}]

let%expect_test "Go to definition of disc union type common field in switch" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; reference = Position.create ~line:3 ~character:12
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 42, "line": 0 },
            "start": { "character": 38, "line": 0 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
        }
      ] |}]

let%expect_test "Go to definition of disc union type left field" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; reference = Position.create ~line:4 ~character:24
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 25, "line": 0 },
            "start": { "character": 24, "line": 0 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
        }
      ] |}]

let%expect_test "Go to definition of disc union type right field" =
  get_definition_test
    { file_with_reference = "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; reference = Position.create ~line:5 ~character:22
    ; def_type = Def
    };
  [%expect
    {|
    Some
      [
        {
          "range": {
            "end": { "character": 53, "line": 0 },
            "start": { "character": 52, "line": 0 }
          },
          "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
        }
      ] |}]
