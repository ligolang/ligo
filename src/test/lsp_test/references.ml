module Requests = Ligo_lsp.Server.Requests
open Lsp_test_helpers.Handlers
open Lsp_test_helpers.Common
open Lsp_helpers
open Requests.Handler

(* We use [Path.t] rather than [string] to support package files. *)
type references_test =
  { test_file : Path.t
  ; reference : Position.t
  }

let get_references_test ({ test_file; reference } : references_test) : unit =
  let actual_references, _diagnostics =
    test_run_session
    @@
    let open Handler.Let_syntax in
    let%bind uri = open_file test_file in
    let%bind normalize = ask_normalize in
    let%bind references = Requests.on_req_references reference uri in
    return
    @@ Option.map
         references
         ~f:
           (List.map ~f:(fun loc ->
                { loc with Location.uri = to_relative_uri ~normalize loc.Location.uri }))
  in
  match actual_references with
  | None ->
    failwith
    @@ Format.asprintf
         "Expected to find Some references for %a, but found None."
         Path.pp
         test_file
  | Some actual_references ->
    Format.printf "%a" (Fmt.Dump.list Location.pp) actual_references


let%expect_test "references in included file" =
  get_references_test
    { test_file = normalize_path "contracts/lsp/includer/includer.mligo"
    ; reference = Position.create ~line:1 ~character:8
    };
  [%expect
    {|
    ((stack ((Ident _#612))) "3=before tuple")
    ((stack ((Ident _#612))) "2=before tuple")
    ((stack (Value (Ident _#612))) "2=after tuple")
    ((stack (Value (Ident _#612))) "1=before tuple")
    ((stack (Value (Ident _#612)))
      "0=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#612)))
      "0=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#612))) "1=after tuple")
    ((stack (Value (Ident _#612))) "3=after tuple")
    [{
       "range": {
         "end": { "character": 5, "line": 0 },
         "start": { "character": 4, "line": 0 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/included.mligo"
     };
     {
       "range": {
         "end": { "character": 9, "line": 1 },
         "start": { "character": 8, "line": 1 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/includer/includer.mligo"
     }] |}]

let%expect_test "Reference of registry package in reverse dependency" =
  let package_path =
    Path.from_absolute
    @@ Option.value_exn
    @@ Lsp_test_helpers.Lib.resolve_lib_path
         ~project_root:(resolve "contracts/lsp")
         ~file:(resolve "contracts/lsp/registry.jsligo")
         ~lib_name:"bigarray"
         ~file_path:(Filename.concat "lib" "bigarray.mligo")
  in
  get_references_test
    { test_file = package_path; reference = Position.create ~line:27 ~character:4 };
  [%expect
    {|
    ((stack ((Ident _#1042))) "7=before tuple")
    ((stack ((Ident _#1042))) "6=before tuple")
    ((stack (Value (Ident _#1042))) "6=after tuple")
    ((stack (Value (Ident _#1042))) "5=before tuple")
    ((stack (Value (Ident _#1042)))
      "4=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1042)))
      "4=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1042))) "5=after tuple")
    ((stack (Value (Ident _#1042))) "7=after tuple")
    ((stack ((Ident _#1946))) "11=before tuple")
    ((stack ((Ident _#1946))) "10=before tuple")
    ((stack (Value (Ident _#1946))) "10=after tuple")
    ((stack (Value (Ident _#1946))) "9=before tuple")
    ((stack (Value (Ident _#1946)))
      "8=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1946)))
      "8=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1946))) "9=after tuple")
    ((stack (Value (Ident _#1946))) "11=after tuple")
    [{
       "range": {
         "end": { "character": 11, "line": 27 },
         "start": { "character": 4, "line": 27 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/lib/bigarray.mligo"
     };
     {
       "range": {
         "end": { "character": 28, "line": 49 },
         "start": { "character": 21, "line": 49 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/test/bigarray.test.mligo"
     };
     {
       "range": {
         "end": { "character": 28, "line": 50 },
         "start": { "character": 21, "line": 50 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/test/bigarray.test.mligo"
     };
     {
       "range": {
         "end": { "character": 28, "line": 51 },
         "start": { "character": 21, "line": 51 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/test/bigarray.test.mligo"
     };
     {
       "range": {
         "end": { "character": 23, "line": 53 },
         "start": { "character": 16, "line": 53 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/test/bigarray.test.mligo"
     };
     {
       "range": {
         "end": { "character": 23, "line": 62 },
         "start": { "character": 16, "line": 62 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/test/bigarray.test.mligo"
     };
     {
       "range": {
         "end": { "character": 23, "line": 71 },
         "start": { "character": 16, "line": 71 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/.ligo/source/i/ligo__s__bigarray__1.0.0__cf1c9d6c/test/bigarray.test.mligo"
     };
     {
       "range": {
         "end": { "character": 27, "line": 8 },
         "start": { "character": 20, "line": 8 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/registry.jsligo"
     };
     {
       "range": {
         "end": { "character": 33, "line": 14 },
         "start": { "character": 26, "line": 14 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/registry.jsligo"
     }] |}]

let%expect_test "references in a file with michelson injections" =
  get_references_test
    { test_file = normalize_path "contracts/lsp/references_michelson_inj.mligo"
    ; reference = Position.create ~line:4 ~character:8
    };
  [%expect
    {|
    ((stack ((Ident _#2334))) "15=before tuple")
    ((stack ((Ident _#2334))) "14=before tuple")
    ((stack (Value (Ident _#2334))) "14=after tuple")
    ((stack (Value (Ident _#2334))) "13=before tuple")
    ((stack (Value (Ident _#2334)))
      "12=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2334)))
      "12=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2334))) "13=after tuple")
    ((stack (Value (Ident _#2334))) "15=after tuple")
    [{
       "range": {
         "end": { "character": 5, "line": 0 },
         "start": { "character": 4, "line": 0 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_michelson_inj.mligo"
     };
     {
       "range": {
         "end": { "character": 9, "line": 4 },
         "start": { "character": 8, "line": 4 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_michelson_inj.mligo"
     };
     {
       "range": {
         "end": { "character": 12, "line": 4 },
         "start": { "character": 11, "line": 4 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_michelson_inj.mligo"
     }] |}]

let%expect_test "references of term with sig items and top level ref" =
  get_references_test
    { test_file =
        normalize_path "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; reference = Position.create ~line:9 ~character:6
    };
  [%expect
    {|
    ((stack ((Ident _#2723))) "19=before tuple")
    ((stack ((Ident _#2723))) "18=before tuple")
    ((stack (Value (Ident _#2723))) "18=after tuple")
    ((stack (Value (Ident _#2723))) "17=before tuple")
    ((stack (Value (Ident _#2723)))
      "16=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2723)))
      "16=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2723))) "17=after tuple")
    ((stack (Value (Ident _#2723))) "19=after tuple")
    [{
       "range": {
         "end": { "character": 7, "line": 3 },
         "start": { "character": 6, "line": 3 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
     };
     {
       "range": {
         "end": { "character": 7, "line": 9 },
         "start": { "character": 6, "line": 9 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
     };
     {
       "range": {
         "end": { "character": 14, "line": 12 },
         "start": { "character": 13, "line": 12 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
     }] |}]

let%expect_test "references of type with sig items and top level ref" =
  get_references_test
    { test_file =
        normalize_path "contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
    ; reference = Position.create ~line:9 ~character:10
    };
  [%expect
    {|
    ((stack ((Ident _#3112))) "23=before tuple")
    ((stack ((Ident _#3112))) "22=before tuple")
    ((stack (Value (Ident _#3112))) "22=after tuple")
    ((stack (Value (Ident _#3112))) "21=before tuple")
    ((stack (Value (Ident _#3112)))
      "20=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3112)))
      "20=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3112))) "21=after tuple")
    ((stack (Value (Ident _#3112))) "23=after tuple")
    [{
       "range": {
         "end": { "character": 8, "line": 1 },
         "start": { "character": 7, "line": 1 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
     };
     {
       "range": {
         "end": { "character": 11, "line": 3 },
         "start": { "character": 10, "line": 3 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
     };
     {
       "range": {
         "end": { "character": 8, "line": 7 },
         "start": { "character": 7, "line": 7 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
     };
     {
       "range": {
         "end": { "character": 11, "line": 9 },
         "start": { "character": 10, "line": 9 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/ref_from_top_level.mligo"
     }] |}]

let%expect_test "references from inline and standalone sigs and mods" =
  get_references_test
    { test_file =
        normalize_path "contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
    ; reference = Position.create ~line:8 ~character:70
    };
  [%expect
    {|
    ((stack ((Ident _#3501))) "27=before tuple")
    ((stack ((Ident _#3501))) "26=before tuple")
    ((stack (Value (Ident _#3501))) "26=after tuple")
    ((stack (Value (Ident _#3501))) "25=before tuple")
    ((stack (Value (Ident _#3501)))
      "24=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3501)))
      "24=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3501))) "25=after tuple")
    ((stack (Value (Ident _#3501))) "27=after tuple")
    [{
       "range": {
         "end": { "character": 8, "line": 1 },
         "start": { "character": 7, "line": 1 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
     };
     {
       "range": {
         "end": { "character": 8, "line": 5 },
         "start": { "character": 7, "line": 5 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
     };
     {
       "range": {
         "end": { "character": 39, "line": 8 },
         "start": { "character": 38, "line": 8 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
     };
     {
       "range": {
         "end": { "character": 71, "line": 8 },
         "start": { "character": 70, "line": 8 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
     };
     {
       "range": {
         "end": { "character": 8, "line": 9 },
         "start": { "character": 7, "line": 9 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/multiple_definitions.jsligo"
     }] |}]

let%expect_test "references from multiple impls" =
  get_references_test
    { test_file =
        normalize_path "contracts/lsp/go_to_implementations/two_namespaces.jsligo"
    ; reference = Position.create ~line:6 ~character:7
    };
  [%expect
    {|
    ((stack ((Ident _#3891))) "31=before tuple")
    ((stack ((Ident _#3891))) "30=before tuple")
    ((stack (Value (Ident _#3891))) "30=after tuple")
    ((stack (Value (Ident _#3891))) "29=before tuple")
    ((stack (Value (Ident _#3891)))
      "28=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3891)))
      "28=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3891))) "29=after tuple")
    ((stack (Value (Ident _#3891))) "31=after tuple")
    [{
       "range": {
         "end": { "character": 8, "line": 1 },
         "start": { "character": 7, "line": 1 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/two_namespaces.jsligo"
     };
     {
       "range": {
         "end": { "character": 13, "line": 2 },
         "start": { "character": 12, "line": 2 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/two_namespaces.jsligo"
     };
     {
       "range": {
         "end": { "character": 8, "line": 6 },
         "start": { "character": 7, "line": 6 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/two_namespaces.jsligo"
     };
     {
       "range": {
         "end": { "character": 8, "line": 11 },
         "start": { "character": 7, "line": 11 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/two_namespaces.jsligo"
     }] |}]

let%expect_test "signature and include" =
  get_references_test
    { test_file =
        normalize_path "contracts/lsp/go_to_implementations/signature_and_include.mligo"
    ; reference = Position.create ~line:12 ~character:11
    };
  [%expect
    {|
    ((stack ((Ident _#4280))) "35=before tuple")
    ((stack ((Ident _#4280))) "34=before tuple")
    ((stack (Value (Ident _#4280))) "34=after tuple")
    ((stack (Value (Ident _#4280))) "33=before tuple")
    ((stack (Value (Ident _#4280)))
      "32=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4280)))
      "32=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4280))) "33=after tuple")
    ((stack (Value (Ident _#4280))) "35=after tuple")
    [{
       "range": {
         "end": { "character": 7, "line": 1 },
         "start": { "character": 6, "line": 1 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/signature_and_include.mligo"
     };
     {
       "range": {
         "end": { "character": 11, "line": 7 },
         "start": { "character": 10, "line": 7 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/signature_and_include.mligo"
     };
     {
       "range": {
         "end": { "character": 11, "line": 12 },
         "start": { "character": 10, "line": 12 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/signature_and_include.mligo"
     }] |}]

let%expect_test "references from included type" =
  get_references_test
    { test_file = normalize_path "contracts/lsp/go_to_implementations/include.mligo"
    ; reference = Position.create ~line:22 ~character:12
    };
  [%expect
    {|
    ((stack ((Ident _#4668))) "39=before tuple")
    ((stack ((Ident _#4668))) "38=before tuple")
    ((stack (Value (Ident _#4668))) "38=after tuple")
    ((stack (Value (Ident _#4668))) "37=before tuple")
    ((stack (Value (Ident _#4668)))
      "36=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4668)))
      "36=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4668))) "37=after tuple")
    ((stack (Value (Ident _#4668))) "39=after tuple")
    [{
       "range": {
         "end": { "character": 8, "line": 1 },
         "start": { "character": 7, "line": 1 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
     };
     {
       "range": {
         "end": { "character": 8, "line": 5 },
         "start": { "character": 7, "line": 5 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
     };
     {
       "range": {
         "end": { "character": 8, "line": 9 },
         "start": { "character": 7, "line": 9 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
     };
     {
       "range": {
         "end": { "character": 8, "line": 15 },
         "start": { "character": 7, "line": 15 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
     };
     {
       "range": {
         "end": { "character": 13, "line": 22 },
         "start": { "character": 12, "line": 22 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
     }] |}]

let%expect_test "references of included module" =
  get_references_test
    { test_file = normalize_path "contracts/lsp/go_to_implementations/include.mligo"
    ; reference = Position.create ~line:19 ~character:10
    };
  [%expect
    {|
    ((stack ((Ident _#5056))) "43=before tuple")
    ((stack ((Ident _#5056))) "42=before tuple")
    ((stack (Value (Ident _#5056))) "42=after tuple")
    ((stack (Value (Ident _#5056))) "41=before tuple")
    ((stack (Value (Ident _#5056)))
      "40=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5056)))
      "40=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5056))) "41=after tuple")
    ((stack (Value (Ident _#5056))) "43=after tuple")
    [{
       "range": {
         "end": { "character": 9, "line": 12 },
         "start": { "character": 7, "line": 12 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
     };
     {
       "range": {
         "end": { "character": 12, "line": 19 },
         "start": { "character": 10, "line": 19 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/include.mligo"
     }] |}]

let%expect_test "references of shadowed identifier" =
  get_references_test
    { test_file = normalize_path "contracts/lsp/go_to_implementations/shadow.mligo"
    ; reference = Position.create ~line:13 ~character:11
    };
  [%expect
    {|
    ((stack ((Ident _#5444))) "47=before tuple")
    ((stack ((Ident _#5444))) "46=before tuple")
    ((stack (Value (Ident _#5444))) "46=after tuple")
    ((stack (Value (Ident _#5444))) "45=before tuple")
    ((stack (Value (Ident _#5444)))
      "44=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5444)))
      "44=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5444))) "45=after tuple")
    ((stack (Value (Ident _#5444))) "47=after tuple")
    [{
       "range": {
         "end": { "character": 8, "line": 1 },
         "start": { "character": 7, "line": 1 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/shadow.mligo"
     };
     {
       "range": {
         "end": { "character": 8, "line": 10 },
         "start": { "character": 7, "line": 10 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/shadow.mligo"
     };
     {
       "range": {
         "end": { "character": 12, "line": 13 },
         "start": { "character": 11, "line": 13 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/shadow.mligo"
     }] |}]

let%expect_test "references of record field" =
  get_references_test
    { test_file = normalize_path "contracts/lsp/references_ctors_and_fields.mligo"
    ; reference = Position.create ~line:19 ~character:28
    };
  [%expect
    {|
    ((stack ((Ident _#5847))) "51=before tuple")
    ((stack ((Ident _#5847))) "50=before tuple")
    ((stack (Value (Ident _#5847))) "50=after tuple")
    ((stack (Value (Ident _#5847))) "49=before tuple")
    ((stack (Value (Ident _#5847)))
      "48=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5847)))
      "48=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5847))) "49=after tuple")
    ((stack (Value (Ident _#5847))) "51=after tuple")
    [{
       "range": {
         "end": { "character": 21, "line": 2 },
         "start": { "character": 20, "line": 2 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 24, "line": 8 },
         "start": { "character": 23, "line": 8 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 25, "line": 17 },
         "start": { "character": 24, "line": 17 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 13, "line": 18 },
         "start": { "character": 12, "line": 18 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 29, "line": 19 },
         "start": { "character": 28, "line": 19 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 21, "line": 23 },
         "start": { "character": 20, "line": 23 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     }] |}]

let%expect_test "references of poly record field" =
  get_references_test
    { test_file = normalize_path "contracts/lsp/references_ctors_and_fields.mligo"
    ; reference = Position.create ~line:6 ~character:33
    };
  [%expect
    {|
    ((stack ((Ident _#6250))) "55=before tuple")
    ((stack ((Ident _#6250))) "54=before tuple")
    ((stack (Value (Ident _#6250))) "54=after tuple")
    ((stack (Value (Ident _#6250))) "53=before tuple")
    ((stack (Value (Ident _#6250)))
      "52=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6250)))
      "52=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6250))) "53=after tuple")
    ((stack (Value (Ident _#6250))) "55=after tuple")
    [{
       "range": {
         "end": { "character": 34, "line": 6 },
         "start": { "character": 33, "line": 6 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 41, "line": 20 },
         "start": { "character": 40, "line": 20 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 39, "line": 21 },
         "start": { "character": 38, "line": 21 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 14, "line": 22 },
         "start": { "character": 13, "line": 22 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     }] |}]

let%expect_test "references of constructor" =
  get_references_test
    { test_file = normalize_path "contracts/lsp/references_ctors_and_fields.mligo"
    ; reference = Position.create ~line:9 ~character:16
    };
  [%expect
    {|
    ((stack ((Ident _#6653))) "59=before tuple")
    ((stack ((Ident _#6653))) "58=before tuple")
    ((stack (Value (Ident _#6653))) "58=after tuple")
    ((stack (Value (Ident _#6653))) "57=before tuple")
    ((stack (Value (Ident _#6653)))
      "56=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6653)))
      "56=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6653))) "57=after tuple")
    ((stack (Value (Ident _#6653))) "59=after tuple")
    [{
       "range": {
         "end": { "character": 15, "line": 0 },
         "start": { "character": 12, "line": 0 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 9, "line": 8 },
         "start": { "character": 6, "line": 8 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 10, "line": 9 },
         "start": { "character": 7, "line": 9 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 19, "line": 9 },
         "start": { "character": 16, "line": 9 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 14, "line": 10 },
         "start": { "character": 11, "line": 10 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 23, "line": 10 },
         "start": { "character": 20, "line": 10 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 5, "line": 23 },
         "start": { "character": 2, "line": 23 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     }] |}]

let%expect_test "references of poly constructor" =
  get_references_test
    { test_file = normalize_path "contracts/lsp/references_ctors_and_fields.mligo"
    ; reference = Position.create ~line:11 ~character:31
    };
  [%expect
    {|
    ((stack ((Ident _#7056))) "63=before tuple")
    ((stack ((Ident _#7056))) "62=before tuple")
    ((stack (Value (Ident _#7056))) "62=after tuple")
    ((stack (Value (Ident _#7056))) "61=before tuple")
    ((stack (Value (Ident _#7056)))
      "60=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7056)))
      "60=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7056))) "61=after tuple")
    ((stack (Value (Ident _#7056))) "63=after tuple")
    [{
       "range": {
         "end": { "character": 25, "line": 4 },
         "start": { "character": 23, "line": 4 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 33, "line": 11 },
         "start": { "character": 31, "line": 11 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     };
     {
       "range": {
         "end": { "character": 8, "line": 14 },
         "start": { "character": 6, "line": 14 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/references_ctors_and_fields.mligo"
     }] |}]

let%expect_test "Reference in reverse dependency" =
  get_references_test
    { test_file = normalize_path "contracts/import_export/f.jsligo"
    ; reference = Position.create ~line:0 ~character:13
    };
  [%expect
    {|
    ((stack ((Ident _#7444))) "67=before tuple")
    ((stack ((Ident _#7444))) "66=before tuple")
    ((stack (Value (Ident _#7444))) "66=after tuple")
    ((stack (Value (Ident _#7444))) "65=before tuple")
    ((stack (Value (Ident _#7444)))
      "64=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7444)))
      "64=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7444))) "65=after tuple")
    ((stack (Value (Ident _#7444))) "67=after tuple")
    ((stack ((Ident _#7837))) "71=before tuple")
    ((stack ((Ident _#7837))) "70=before tuple")
    ((stack (Value (Ident _#7837))) "70=after tuple")
    ((stack (Value (Ident _#7837))) "69=before tuple")
    ((stack (Value (Ident _#7837)))
      "68=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7837)))
      "68=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7837))) "69=after tuple")
    ((stack (Value (Ident _#7837))) "71=after tuple")
    ((stack ((Ident _#8234))) "75=before tuple")
    ((stack ((Ident _#8234))) "74=before tuple")
    ((stack (Value (Ident _#8234))) "74=after tuple")
    ((stack (Value (Ident _#8234))) "73=before tuple")
    ((stack (Value (Ident _#8234)))
      "72=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8234)))
      "72=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8234))) "73=after tuple")
    ((stack (Value (Ident _#8234))) "75=after tuple")
    [{
       "range": {
         "end": { "character": 14, "line": 0 },
         "start": { "character": 13, "line": 0 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/import_export/f.jsligo"
     };
     {
       "range": {
         "end": { "character": 15, "line": 2 },
         "start": { "character": 14, "line": 2 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/import_export/h.jsligo"
     }] |}]

let%expect_test "Reference of signature item in module in reverse dependency" =
  get_references_test
    { test_file = normalize_path "contracts/lsp/go_to_implementations/interface.mligo"
    ; reference = Position.create ~line:2 ~character:6
    };
  [%expect
    {|
    ((stack ((Ident _#8622))) "79=before tuple")
    ((stack ((Ident _#8622))) "78=before tuple")
    ((stack (Value (Ident _#8622))) "78=after tuple")
    ((stack (Value (Ident _#8622))) "77=before tuple")
    ((stack (Value (Ident _#8622)))
      "76=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8622)))
      "76=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8622))) "77=after tuple")
    ((stack (Value (Ident _#8622))) "79=after tuple")
    ((stack ((Ident _#9014))) "83=before tuple")
    ((stack ((Ident _#9014))) "82=before tuple")
    ((stack (Value (Ident _#9014))) "82=after tuple")
    ((stack (Value (Ident _#9014))) "81=before tuple")
    ((stack (Value (Ident _#9014)))
      "80=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9014)))
      "80=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9014))) "81=after tuple")
    ((stack (Value (Ident _#9014))) "83=after tuple")
    [{
       "range": {
         "end": { "character": 7, "line": 4 },
         "start": { "character": 6, "line": 4 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/implementation.mligo"
     };
     {
       "range": {
         "end": { "character": 7, "line": 9 },
         "start": { "character": 6, "line": 9 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/implementation.mligo"
     };
     {
       "range": {
         "end": { "character": 7, "line": 2 },
         "start": { "character": 6, "line": 2 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/interface.mligo"
     }] |}]

let%expect_test "Reference of item in reverse dependency in other directory" =
  get_references_test
    { test_file = normalize_path "contracts/lsp/included.mligo"
    ; reference = Position.create ~line:0 ~character:4
    };
  [%expect
    {|
    ((stack ((Ident _#9402))) "87=before tuple")
    ((stack ((Ident _#9402))) "86=before tuple")
    ((stack (Value (Ident _#9402))) "86=after tuple")
    ((stack (Value (Ident _#9402))) "85=before tuple")
    ((stack (Value (Ident _#9402)))
      "84=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9402)))
      "84=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9402))) "85=after tuple")
    ((stack (Value (Ident _#9402))) "87=after tuple")
    ((stack ((Ident _#9792))) "91=before tuple")
    ((stack ((Ident _#9792))) "90=before tuple")
    ((stack (Value (Ident _#9792))) "90=after tuple")
    ((stack (Value (Ident _#9792))) "89=before tuple")
    ((stack (Value (Ident _#9792)))
      "88=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9792)))
      "88=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9792))) "89=after tuple")
    ((stack (Value (Ident _#9792))) "91=after tuple")
    [{
       "range": {
         "end": { "character": 5, "line": 0 },
         "start": { "character": 4, "line": 0 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/included.mligo"
     };
     {
       "range": {
         "end": { "character": 9, "line": 1 },
         "start": { "character": 8, "line": 1 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/includer/includer.mligo"
     }] |}]

let%expect_test "references of disc union type common field (left type)" =
  get_references_test
    { test_file =
        normalize_path "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; reference = Position.create ~line:0 ~character:11
    };
  [%expect
    {|
    ((stack ((Ident _#10206))) "95=before tuple")
    ((stack ((Ident _#10206))) "94=before tuple")
    ((stack (Value (Ident _#10206))) "94=after tuple")
    ((stack (Value (Ident _#10206))) "93=before tuple")
    ((stack (Value (Ident _#10206)))
      "92=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10206)))
      "92=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10206))) "93=after tuple")
    ((stack (Value (Ident _#10206))) "95=after tuple")
    [{
       "range": {
         "end": { "character": 15, "line": 0 },
         "start": { "character": 11, "line": 0 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
     };
     {
       "range": {
         "end": { "character": 16, "line": 3 },
         "start": { "character": 12, "line": 3 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
     }] |}]

let%expect_test "references of disc union type common field (right type)" =
  get_references_test
    { test_file =
        normalize_path "contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
    ; reference = Position.create ~line:0 ~character:38
    };
  [%expect
    {|
    ((stack ((Ident _#10620))) "99=before tuple")
    ((stack ((Ident _#10620))) "98=before tuple")
    ((stack (Value (Ident _#10620))) "98=after tuple")
    ((stack (Value (Ident _#10620))) "97=before tuple")
    ((stack (Value (Ident _#10620)))
      "96=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10620)))
      "96=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10620))) "97=after tuple")
    ((stack (Value (Ident _#10620))) "99=after tuple")
    [{
       "range": {
         "end": { "character": 42, "line": 0 },
         "start": { "character": 38, "line": 0 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
     };
     {
       "range": {
         "end": { "character": 16, "line": 3 },
         "start": { "character": 12, "line": 3 }
       },
       "uri": "file:///../../../../../default/src/test/contracts/lsp/go_to_implementations/disc_union_fields.jsligo"
     }] |}]
