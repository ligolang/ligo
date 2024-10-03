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
    @@
    let open Handler.Let_syntax in
    let%bind uri = open_file @@ normalize_path file_with_reference in
    let%bind definitions = get_definition reference uri in
    let%bind normalize = ask_normalize in
    return
    @@ Option.map definitions ~f:(function
           | `Location locations ->
             `Location
               (List.map locations ~f:(fun loc ->
                    { loc with uri = to_relative_uri ~normalize loc.uri }))
           | `LocationLink links ->
             `LocationLink
               (List.map links ~f:(fun link ->
                    { link with targetUri = to_relative_uri ~normalize link.targetUri })))
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
    ((stack ((Ident _#610))) "3=before tuple")
    ((stack ((Ident _#610))) "2=before tuple")
    ((stack (Value (Ident _#610))) "2=after tuple")
    ((stack (Value (Ident _#610))) "1=before tuple")
    ((stack (Value (Ident _#610)))
      "0=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#610)))
      "0=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#610))) "1=after tuple")
    ((stack (Value (Ident _#610))) "3=after tuple")
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
    ; reference = Position.create ~line:9 ~character:19
    ; def_type = Def
    };
  [%expect
    {|
    ((stack ())
     "11=before let in Mangled_module__s_home_s_eduardo_s_y_s_ligo_s__u_build_s_default_s_src_s_test_s_contracts_s_build_s_A_p_mligo.toto#1007")
    ((stack
      ((Ident
        Mangled_module__s_home_s_eduardo_s_y_s_ligo_s__u_build_s_default_s_src_s_test_s_contracts_s_build_s_A_p_mligo.toto#1007)))
     "10=before let in titi")
    ((stack
      ((Ident
        Mangled_module__s_home_s_eduardo_s_y_s_ligo_s__u_build_s_default_s_src_s_test_s_contracts_s_build_s_A_p_mligo.toto#1007)))
     "9=before Add")
    ((stack
      (Value
       (Ident
        Mangled_module__s_home_s_eduardo_s_y_s_ligo_s__u_build_s_default_s_src_s_test_s_contracts_s_build_s_A_p_mligo.toto#1007)))
     "8=before var Mangled_module__s_home_s_eduardo_s_y_s_ligo_s__u_build_s_default_s_src_s_test_s_contracts_s_build_s_A_p_mligo.toto#1007")
    ((stack
      (Value Value
       (Ident
        Mangled_module__s_home_s_eduardo_s_y_s_ligo_s__u_build_s_default_s_src_s_test_s_contracts_s_build_s_A_p_mligo.toto#1007)))
     "8=after var Mangled_module__s_home_s_eduardo_s_y_s_ligo_s__u_build_s_default_s_src_s_test_s_contracts_s_build_s_A_p_mligo.toto#1007")
    ((stack
      (Value
       (Ident
        Mangled_module__s_home_s_eduardo_s_y_s_ligo_s__u_build_s_default_s_src_s_test_s_contracts_s_build_s_A_p_mligo.toto#1007)))
     "9=after Add")
    ((stack ((Ident _#1022))) "7=before tuple")
    ((stack ((Ident _#1022))) "6=before tuple")
    ((stack (Value (Ident _#1022))) "6=after tuple")
    ((stack (Value (Ident _#1022))) "5=before tuple")
    ((stack (Value (Ident _#1022)))
      "4=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1022)))
      "4=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1022))) "5=after tuple")
    ((stack (Value (Ident _#1022))) "7=after tuple")
    ((stack
      (Value
       (Ident
        Mangled_module__s_home_s_eduardo_s_y_s_ligo_s__u_build_s_default_s_src_s_test_s_contracts_s_build_s_A_p_mligo.toto#1007)))
     "10=after let in titi")
    ((stack (Value))
     "11=after let in Mangled_module__s_home_s_eduardo_s_y_s_ligo_s__u_build_s_default_s_src_s_test_s_contracts_s_build_s_A_p_mligo.toto#1007")
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
    ((stack ((Ident _#1425))) "15=before tuple")
    ((stack ((Ident _#1425))) "14=before tuple")
    ((stack (Value (Ident _#1425))) "14=after tuple")
    ((stack (Value (Ident _#1425))) "13=before tuple")
    ((stack (Value (Ident _#1425)))
      "12=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1425)))
      "12=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1425))) "13=after tuple")
    ((stack (Value (Ident _#1425))) "15=after tuple")
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
    ((stack ((Ident _#1814))) "19=before tuple")
    ((stack ((Ident _#1814))) "18=before tuple")
    ((stack (Value (Ident _#1814))) "18=after tuple")
    ((stack (Value (Ident _#1814))) "17=before tuple")
    ((stack (Value (Ident _#1814)))
      "16=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1814)))
      "16=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#1814))) "17=after tuple")
    ((stack (Value (Ident _#1814))) "19=after tuple")
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
    ((stack ((Ident _#2203))) "23=before tuple")
    ((stack ((Ident _#2203))) "22=before tuple")
    ((stack (Value (Ident _#2203))) "22=after tuple")
    ((stack (Value (Ident _#2203))) "21=before tuple")
    ((stack (Value (Ident _#2203)))
      "20=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2203)))
      "20=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2203))) "21=after tuple")
    ((stack (Value (Ident _#2203))) "23=after tuple")
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
    ((stack ((Ident _#2592))) "27=before tuple")
    ((stack ((Ident _#2592))) "26=before tuple")
    ((stack (Value (Ident _#2592))) "26=after tuple")
    ((stack (Value (Ident _#2592))) "25=before tuple")
    ((stack (Value (Ident _#2592)))
      "24=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2592)))
      "24=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2592))) "25=after tuple")
    ((stack (Value (Ident _#2592))) "27=after tuple")
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
  [%expect {|
    ((stack ((Ident _#2981))) "31=before tuple")
    ((stack ((Ident _#2981))) "30=before tuple")
    ((stack (Value (Ident _#2981))) "30=after tuple")
    ((stack (Value (Ident _#2981))) "29=before tuple")
    ((stack (Value (Ident _#2981)))
      "28=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2981)))
      "28=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#2981))) "29=after tuple")
    ((stack (Value (Ident _#2981))) "31=after tuple")
    None |}]

let%expect_test "stdlib type definition" =
  get_definition_test
    { file_with_reference = "contracts/lsp/local_module.mligo"
    ; reference = Position.create ~line:5 ~character:11
    ; def_type = Type_def
    };
  [%expect {|
    ((stack ((Ident _#3370))) "35=before tuple")
    ((stack ((Ident _#3370))) "34=before tuple")
    ((stack (Value (Ident _#3370))) "34=after tuple")
    ((stack (Value (Ident _#3370))) "33=before tuple")
    ((stack (Value (Ident _#3370)))
      "32=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3370)))
      "32=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3370))) "33=after tuple")
    ((stack (Value (Ident _#3370))) "35=after tuple")
    None |}]

let%expect_test "Registry package imported identifier" =
  get_definition_test
    { file_with_reference = "contracts/lsp/registry.jsligo"
    ; reference = Position.create ~line:8 ~character:20
    ; def_type = Def
    };
  [%expect
    {|
    ((stack ((Ident _#3829))) "39=before tuple")
    ((stack ((Ident _#3829))) "38=before tuple")
    ((stack (Value (Ident _#3829))) "38=after tuple")
    ((stack (Value (Ident _#3829))) "37=before tuple")
    ((stack (Value (Ident _#3829)))
      "36=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3829)))
      "36=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#3829))) "37=after tuple")
    ((stack (Value (Ident _#3829))) "39=after tuple")
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
    ((stack ((Ident _#4217))) "43=before tuple")
    ((stack ((Ident _#4217))) "42=before tuple")
    ((stack (Value (Ident _#4217))) "42=after tuple")
    ((stack (Value (Ident _#4217))) "41=before tuple")
    ((stack (Value (Ident _#4217)))
      "40=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4217)))
      "40=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4217))) "41=after tuple")
    ((stack (Value (Ident _#4217))) "43=after tuple")
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
    ((stack ((Ident _#4605))) "47=before tuple")
    ((stack ((Ident _#4605))) "46=before tuple")
    ((stack (Value (Ident _#4605))) "46=after tuple")
    ((stack (Value (Ident _#4605))) "45=before tuple")
    ((stack (Value (Ident _#4605)))
      "44=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4605)))
      "44=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4605))) "45=after tuple")
    ((stack (Value (Ident _#4605))) "47=after tuple")
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
    ((stack ((Ident _#4993))) "51=before tuple")
    ((stack ((Ident _#4993))) "50=before tuple")
    ((stack (Value (Ident _#4993))) "50=after tuple")
    ((stack (Value (Ident _#4993))) "49=before tuple")
    ((stack (Value (Ident _#4993)))
      "48=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4993)))
      "48=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#4993))) "49=after tuple")
    ((stack (Value (Ident _#4993))) "51=after tuple")
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
    ((stack ((Ident _#5381))) "55=before tuple")
    ((stack ((Ident _#5381))) "54=before tuple")
    ((stack (Value (Ident _#5381))) "54=after tuple")
    ((stack (Value (Ident _#5381))) "53=before tuple")
    ((stack (Value (Ident _#5381)))
      "52=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5381)))
      "52=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5381))) "53=after tuple")
    ((stack (Value (Ident _#5381))) "55=after tuple")
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
    ((stack ((Ident _#5771))) "59=before tuple")
    ((stack ((Ident _#5771))) "58=before tuple")
    ((stack (Value (Ident _#5771))) "58=after tuple")
    ((stack (Value (Ident _#5771))) "57=before tuple")
    ((stack (Value (Ident _#5771)))
      "56=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5771)))
      "56=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#5771))) "57=after tuple")
    ((stack (Value (Ident _#5771))) "59=after tuple")
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
    ((stack ((Ident _#6161))) "63=before tuple")
    ((stack ((Ident _#6161))) "62=before tuple")
    ((stack (Value (Ident _#6161))) "62=after tuple")
    ((stack (Value (Ident _#6161))) "61=before tuple")
    ((stack (Value (Ident _#6161)))
      "60=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6161)))
      "60=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6161))) "61=after tuple")
    ((stack (Value (Ident _#6161))) "63=after tuple")
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
    ((stack ((Ident _#6553))) "67=before tuple")
    ((stack ((Ident _#6553))) "66=before tuple")
    ((stack (Value (Ident _#6553))) "66=after tuple")
    ((stack (Value (Ident _#6553))) "65=before tuple")
    ((stack (Value (Ident _#6553)))
      "64=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6553)))
      "64=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6553))) "65=after tuple")
    ((stack (Value (Ident _#6553))) "67=after tuple")
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
    ((stack ((Ident _#6945))) "71=before tuple")
    ((stack ((Ident _#6945))) "70=before tuple")
    ((stack (Value (Ident _#6945))) "70=after tuple")
    ((stack (Value (Ident _#6945))) "69=before tuple")
    ((stack (Value (Ident _#6945)))
      "68=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6945)))
      "68=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#6945))) "69=after tuple")
    ((stack (Value (Ident _#6945))) "71=after tuple")
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
    ((stack ((Ident _#7337))) "75=before tuple")
    ((stack ((Ident _#7337))) "74=before tuple")
    ((stack (Value (Ident _#7337))) "74=after tuple")
    ((stack (Value (Ident _#7337))) "73=before tuple")
    ((stack (Value (Ident _#7337)))
      "72=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7337)))
      "72=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7337))) "73=after tuple")
    ((stack (Value (Ident _#7337))) "75=after tuple")
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
    ((stack ((Ident _#7726))) "79=before tuple")
    ((stack ((Ident _#7726))) "78=before tuple")
    ((stack (Value (Ident _#7726))) "78=after tuple")
    ((stack (Value (Ident _#7726))) "77=before tuple")
    ((stack (Value (Ident _#7726)))
      "76=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7726)))
      "76=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#7726))) "77=after tuple")
    ((stack (Value (Ident _#7726))) "79=after tuple")
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
    ((stack ((Ident _#8115))) "83=before tuple")
    ((stack ((Ident _#8115))) "82=before tuple")
    ((stack (Value (Ident _#8115))) "82=after tuple")
    ((stack (Value (Ident _#8115))) "81=before tuple")
    ((stack (Value (Ident _#8115)))
      "80=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8115)))
      "80=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8115))) "81=after tuple")
    ((stack (Value (Ident _#8115))) "83=after tuple")
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
    ((stack ((Ident _#8504))) "87=before tuple")
    ((stack ((Ident _#8504))) "86=before tuple")
    ((stack (Value (Ident _#8504))) "86=after tuple")
    ((stack (Value (Ident _#8504))) "85=before tuple")
    ((stack (Value (Ident _#8504)))
      "84=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8504)))
      "84=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8504))) "85=after tuple")
    ((stack (Value (Ident _#8504))) "87=after tuple")
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
    ((stack ((Ident _#8896))) "91=before tuple")
    ((stack ((Ident _#8896))) "90=before tuple")
    ((stack (Value (Ident _#8896))) "90=after tuple")
    ((stack (Value (Ident _#8896))) "89=before tuple")
    ((stack (Value (Ident _#8896)))
      "88=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8896)))
      "88=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#8896))) "89=after tuple")
    ((stack (Value (Ident _#8896))) "91=after tuple")
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
    ((stack ((Ident _#9288))) "95=before tuple")
    ((stack ((Ident _#9288))) "94=before tuple")
    ((stack (Value (Ident _#9288))) "94=after tuple")
    ((stack (Value (Ident _#9288))) "93=before tuple")
    ((stack (Value (Ident _#9288)))
      "92=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9288)))
      "92=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9288))) "93=after tuple")
    ((stack (Value (Ident _#9288))) "95=after tuple")
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
    ((stack ((Ident _#9676))) "99=before tuple")
    ((stack ((Ident _#9676))) "98=before tuple")
    ((stack (Value (Ident _#9676))) "98=after tuple")
    ((stack (Value (Ident _#9676))) "97=before tuple")
    ((stack (Value (Ident _#9676)))
      "96=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9676)))
      "96=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#9676))) "97=after tuple")
    ((stack (Value (Ident _#9676))) "99=after tuple")
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
    ((stack ((Ident _#10064))) "103=before tuple")
    ((stack ((Ident _#10064))) "102=before tuple")
    ((stack (Value (Ident _#10064))) "102=after tuple")
    ((stack (Value (Ident _#10064))) "101=before tuple")
    ((stack (Value (Ident _#10064)))
      "100=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10064)))
      "100=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10064))) "101=after tuple")
    ((stack (Value (Ident _#10064))) "103=after tuple")
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
    ((stack ((Ident _#10452))) "107=before tuple")
    ((stack ((Ident _#10452))) "106=before tuple")
    ((stack (Value (Ident _#10452))) "106=after tuple")
    ((stack (Value (Ident _#10452))) "105=before tuple")
    ((stack (Value (Ident _#10452)))
      "104=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10452)))
      "104=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10452))) "105=after tuple")
    ((stack (Value (Ident _#10452))) "107=after tuple")
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
    ((stack ((Ident _#10840))) "111=before tuple")
    ((stack ((Ident _#10840))) "110=before tuple")
    ((stack (Value (Ident _#10840))) "110=after tuple")
    ((stack (Value (Ident _#10840))) "109=before tuple")
    ((stack (Value (Ident _#10840)))
      "108=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10840)))
      "108=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#10840))) "109=after tuple")
    ((stack (Value (Ident _#10840))) "111=after tuple")
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
    ((stack ((Ident _#11228))) "115=before tuple")
    ((stack ((Ident _#11228))) "114=before tuple")
    ((stack (Value (Ident _#11228))) "114=after tuple")
    ((stack (Value (Ident _#11228))) "113=before tuple")
    ((stack (Value (Ident _#11228)))
      "112=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#11228)))
      "112=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#11228))) "113=after tuple")
    ((stack (Value (Ident _#11228))) "115=after tuple")
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
    ((stack ((Ident _#11617))) "119=before tuple")
    ((stack ((Ident _#11617))) "118=before tuple")
    ((stack (Value (Ident _#11617))) "118=after tuple")
    ((stack (Value (Ident _#11617))) "117=before tuple")
    ((stack (Value (Ident _#11617)))
      "116=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#11617)))
      "116=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#11617))) "117=after tuple")
    ((stack (Value (Ident _#11617))) "119=after tuple")
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
    ((stack ((Ident _#12020))) "123=before tuple")
    ((stack ((Ident _#12020))) "122=before tuple")
    ((stack (Value (Ident _#12020))) "122=after tuple")
    ((stack (Value (Ident _#12020))) "121=before tuple")
    ((stack (Value (Ident _#12020)))
      "120=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#12020)))
      "120=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#12020))) "121=after tuple")
    ((stack (Value (Ident _#12020))) "123=after tuple")
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
    ((stack ((Ident _#12423))) "127=before tuple")
    ((stack ((Ident _#12423))) "126=before tuple")
    ((stack (Value (Ident _#12423))) "126=after tuple")
    ((stack (Value (Ident _#12423))) "125=before tuple")
    ((stack (Value (Ident _#12423)))
      "124=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#12423)))
      "124=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#12423))) "125=after tuple")
    ((stack (Value (Ident _#12423))) "127=after tuple")
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
    ((stack ((Ident _#12813))) "131=before tuple")
    ((stack ((Ident _#12813))) "130=before tuple")
    ((stack (Value (Ident _#12813))) "130=after tuple")
    ((stack (Value (Ident _#12813))) "129=before tuple")
    ((stack (Value (Ident _#12813)))
      "128=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#12813)))
      "128=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#12813))) "129=after tuple")
    ((stack (Value (Ident _#12813))) "131=after tuple")
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
    ((stack ((Ident _#13203))) "135=before tuple")
    ((stack ((Ident _#13203))) "134=before tuple")
    ((stack (Value (Ident _#13203))) "134=after tuple")
    ((stack (Value (Ident _#13203))) "133=before tuple")
    ((stack (Value (Ident _#13203)))
      "132=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#13203)))
      "132=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#13203))) "133=after tuple")
    ((stack (Value (Ident _#13203))) "135=after tuple")
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
    ((stack ((Ident _#13593))) "139=before tuple")
    ((stack ((Ident _#13593))) "138=before tuple")
    ((stack (Value (Ident _#13593))) "138=after tuple")
    ((stack (Value (Ident _#13593))) "137=before tuple")
    ((stack (Value (Ident _#13593)))
      "136=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#13593)))
      "136=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#13593))) "137=after tuple")
    ((stack (Value (Ident _#13593))) "139=after tuple")
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
    ((stack ((Ident _#13983))) "143=before tuple")
    ((stack ((Ident _#13983))) "142=before tuple")
    ((stack (Value (Ident _#13983))) "142=after tuple")
    ((stack (Value (Ident _#13983))) "141=before tuple")
    ((stack (Value (Ident _#13983)))
      "140=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#13983)))
      "140=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#13983))) "141=after tuple")
    ((stack (Value (Ident _#13983))) "143=after tuple")
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
    ((stack ((Ident _#14373))) "147=before tuple")
    ((stack ((Ident _#14373))) "146=before tuple")
    ((stack (Value (Ident _#14373))) "146=after tuple")
    ((stack (Value (Ident _#14373))) "145=before tuple")
    ((stack (Value (Ident _#14373)))
      "144=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#14373)))
      "144=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#14373))) "145=after tuple")
    ((stack (Value (Ident _#14373))) "147=after tuple")
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
    ((stack ((Ident _#14763))) "151=before tuple")
    ((stack ((Ident _#14763))) "150=before tuple")
    ((stack (Value (Ident _#14763))) "150=after tuple")
    ((stack (Value (Ident _#14763))) "149=before tuple")
    ((stack (Value (Ident _#14763)))
      "148=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#14763)))
      "148=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#14763))) "149=after tuple")
    ((stack (Value (Ident _#14763))) "151=after tuple")
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
    ((stack ((Ident _#15153))) "155=before tuple")
    ((stack ((Ident _#15153))) "154=before tuple")
    ((stack (Value (Ident _#15153))) "154=after tuple")
    ((stack (Value (Ident _#15153))) "153=before tuple")
    ((stack (Value (Ident _#15153)))
      "152=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#15153)))
      "152=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#15153))) "153=after tuple")
    ((stack (Value (Ident _#15153))) "155=after tuple")
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
    ((stack ((Ident _#15543))) "159=before tuple")
    ((stack ((Ident _#15543))) "158=before tuple")
    ((stack (Value (Ident _#15543))) "158=after tuple")
    ((stack (Value (Ident _#15543))) "157=before tuple")
    ((stack (Value (Ident _#15543)))
      "156=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#15543)))
      "156=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#15543))) "157=after tuple")
    ((stack (Value (Ident _#15543))) "159=after tuple")
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
    ((stack ((Ident _#15933))) "163=before tuple")
    ((stack ((Ident _#15933))) "162=before tuple")
    ((stack (Value (Ident _#15933))) "162=after tuple")
    ((stack (Value (Ident _#15933))) "161=before tuple")
    ((stack (Value (Ident _#15933)))
      "160=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#15933)))
      "160=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#15933))) "161=after tuple")
    ((stack (Value (Ident _#15933))) "163=after tuple")
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
    ((stack ((Ident _#16323))) "167=before tuple")
    ((stack ((Ident _#16323))) "166=before tuple")
    ((stack (Value (Ident _#16323))) "166=after tuple")
    ((stack (Value (Ident _#16323))) "165=before tuple")
    ((stack (Value (Ident _#16323)))
      "164=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#16323)))
      "164=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#16323))) "165=after tuple")
    ((stack (Value (Ident _#16323))) "167=after tuple")
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
    ((stack ((Ident _#18285))) "171=before tuple")
    ((stack ((Ident _#18285))) "170=before tuple")
    ((stack (Value (Ident _#18285))) "170=after tuple")
    ((stack (Value (Ident _#18285))) "169=before tuple")
    ((stack (Value (Ident _#18285)))
      "168=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#18285)))
      "168=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#18285))) "169=after tuple")
    ((stack (Value (Ident _#18285))) "171=after tuple")
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
    ((stack ((Ident _#18675))) "175=before tuple")
    ((stack ((Ident _#18675))) "174=before tuple")
    ((stack (Value (Ident _#18675))) "174=after tuple")
    ((stack (Value (Ident _#18675))) "173=before tuple")
    ((stack (Value (Ident _#18675)))
      "172=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#18675)))
      "172=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#18675))) "173=after tuple")
    ((stack (Value (Ident _#18675))) "175=after tuple")
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
    ((stack ((Ident _#19089))) "179=before tuple")
    ((stack ((Ident _#19089))) "178=before tuple")
    ((stack (Value (Ident _#19089))) "178=after tuple")
    ((stack (Value (Ident _#19089))) "177=before tuple")
    ((stack (Value (Ident _#19089)))
      "176=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#19089)))
      "176=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#19089))) "177=after tuple")
    ((stack (Value (Ident _#19089))) "179=after tuple")
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
    ((stack ((Ident _#19503))) "183=before tuple")
    ((stack ((Ident _#19503))) "182=before tuple")
    ((stack (Value (Ident _#19503))) "182=after tuple")
    ((stack (Value (Ident _#19503))) "181=before tuple")
    ((stack (Value (Ident _#19503)))
      "180=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#19503)))
      "180=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#19503))) "181=after tuple")
    ((stack (Value (Ident _#19503))) "183=after tuple")
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
    ((stack ((Ident _#19917))) "187=before tuple")
    ((stack ((Ident _#19917))) "186=before tuple")
    ((stack (Value (Ident _#19917))) "186=after tuple")
    ((stack (Value (Ident _#19917))) "185=before tuple")
    ((stack (Value (Ident _#19917)))
      "184=before (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#19917)))
      "184=after (Nil\
     \n ((desc Operation)\
     \n  (range\
     \n   ((start 0) (stop 0) (source (String ((name (generated)) (content \"\"))))))))")
    ((stack (Value Value (Ident _#19917))) "185=after tuple")
    ((stack (Value (Ident _#19917))) "187=after tuple")
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
