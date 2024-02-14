open Alcotest_extras
open Lsp_helpers
open Lsp_test_helpers.Handlers
module Requests = Ligo_lsp.Server.Requests
open Requests.Handler

type completion_test =
  { test_name : string
  ; file_name : string
  ; position : Position.t
  ; completions : CompletionItem.t list
  ; negative_labels : string list
  }

let completion_test
    (dialect : Syntax_types.t)
    ?(config : config option)
    { test_name; file_name; position; completions; negative_labels }
    : unit Alcotest.test_case
  =
  let pretty_syntax_type =
    match dialect with
    | CameLIGO -> "CameLIGO"
    | JsLIGO -> "JsLIGO"
  in
  Alcotest.test_case (Format.sprintf "%s: %s" pretty_syntax_type test_name) `Quick
  @@ fun () ->
  let actual_completions, _diagnostics =
    test_run_session ?config
    @@ let@ uri = open_file (Path.from_relative file_name) in
       Requests.on_req_completion position uri
  in
  let get_completion_list = function
    | `CompletionList items -> items
    | `List _ -> fail "Unexpected `List"
  in
  match actual_completions with
  | None -> fail "Expected some completion list, got None"
  | Some actual_completions ->
    let actual_completions = (get_completion_list actual_completions).items in
    should_be_contained_in
      CompletionItem.testable
      ~small:completions
      ~big:actual_completions;
    let actual_labels =
      List.map actual_completions ~f:(fun completion -> completion.label)
    in
    should_not_be_contained_in Alcotest.string ~small:negative_labels ~big:actual_labels


let test_cases_cameligo =
  [ { test_name = "Complete record fields"
    ; file_name = "contracts/lsp/completion_record.mligo"
    ; position = Position.create ~line:7 ~character:30
    ; completions =
        [ CompletionItem.create
            ~label:"numeric"
            ~kind:CompletionItemKind.Field
            ~detail:"int"
            ~sortText:"\x03"
            ()
        ; CompletionItem.create
            ~label:"stringy"
            ~kind:CompletionItemKind.Field
            ~detail:"string"
            ~sortText:"\x03"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete partially written nested record field"
    ; file_name = "contracts/lsp/completion_record.mligo"
    ; position = Position.create ~line:15 ~character:40
    ; completions =
        [ CompletionItem.create
            ~label:"stringy"
            ~kind:CompletionItemKind.Field
            ~detail:"string"
            ~sortText:"\x03"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete already written record field"
    ; file_name = "contracts/lsp/completion_record.mligo"
    ; position = Position.create ~line:15 ~character:30
    ; completions =
        [ CompletionItem.create
            ~label:"record_1"
            ~kind:CompletionItemKind.Field
            ~detail:"record_1"
            ~sortText:"\x03"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete module field"
    ; file_name = "contracts/lsp/completion_module.mligo"
    ; position = Position.create ~line:9 ~character:12
    ; completions =
        [ CompletionItem.create
            ~label:"module_field"
            ~kind:CompletionItemKind.Variable
            ~detail:"int"
            ~sortText:"\x05"
            ()
        ; CompletionItem.create
            ~label:"another_thing"
            ~kind:CompletionItemKind.Variable
            ~detail:"string"
            ~sortText:"\x05"
            ()
        ; CompletionItem.create
            ~label:"Bar"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete nested module field"
    ; file_name = "contracts/lsp/completion_module.mligo"
    ; position = Position.create ~line:10 ~character:16
    ; completions =
        [ CompletionItem.create
            ~label:"nested"
            ~kind:CompletionItemKind.Variable
            ~detail:"{\n left : int;\n right : int\n}"
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete nested record field within module"
    ; file_name = "contracts/lsp/completion_module.mligo"
    ; position = Position.create ~line:11 ~character:23
    ; completions =
        [ CompletionItem.create
            ~label:"left"
            ~kind:CompletionItemKind.Field
            ~detail:"int"
            ~sortText:"\x03"
            ()
        ; CompletionItem.create
            ~label:"right"
            ~kind:CompletionItemKind.Field
            ~detail:"int"
            ~sortText:"\x03"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete already written module field"
    ; file_name = "contracts/lsp/completion_module.mligo"
    ; position = Position.create ~line:11 ~character:12
    ; completions =
        [ CompletionItem.create
            ~label:"Bar"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete already written record field within module"
    ; file_name = "contracts/lsp/completion_module.mligo"
    ; position = Position.create ~line:11 ~character:19
    ; completions =
        [ CompletionItem.create
            ~label:"nested"
            ~kind:CompletionItemKind.Variable
            ~detail:"{\n left : int;\n right : int\n}"
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete simple suggestions"
    ; file_name = "contracts/lsp/completion_simple.mligo"
    ; position = Position.create ~line:4 ~character:17
    ; completions =
        [ CompletionItem.create
            ~label:"thing1"
            ~kind:CompletionItemKind.Variable
            ~detail:"int"
            ~sortText:"\x08"
            ()
        ; CompletionItem.create
            ~label:"some_string"
            ~kind:CompletionItemKind.Variable
            ~detail:"string"
            ~sortText:"\x08"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete type within module"
    ; file_name = "contracts/lsp/completion_type.mligo"
    ; position = Position.create ~line:5 ~character:30
    ; completions =
        [ CompletionItem.create
            ~label:"here_it_is"
            ~kind:CompletionItemKind.TypeParameter
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete variable within module alias"
    ; file_name = "contracts/lsp/completion_alias.mligo"
    ; position = Position.create ~line:10 ~character:17
    ; completions =
        [ CompletionItem.create
            ~label:"target"
            ~kind:CompletionItemKind.Variable
            ~detail:"int"
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete variable within already written module alias"
    ; file_name = "contracts/lsp/completion_alias.mligo"
    ; position = Position.create ~line:10 ~character:15
    ; completions =
        [ CompletionItem.create
            ~label:"B"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x05"
            ()
        ; CompletionItem.create
            ~label:"Y"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "From inner scope"
    ; file_name = "contracts/lsp/completion_inside_module.mligo"
    ; position = Position.create ~line:5 ~character:21
    ; completions =
        [ CompletionItem.create
            ~label:"outer"
            ~kind:CompletionItemKind.Variable
            ~detail:"(* Unresolved *)"
            ~sortText:"\x08"
            ()
        ; CompletionItem.create
            ~label:"inner"
            ~kind:CompletionItemKind.Variable
            ~detail:"(* Unresolved *)"
            ~sortText:"\x08"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "From outer scope"
    ; file_name = "contracts/lsp/completion_inside_module.mligo"
    ; position = Position.create ~line:8 ~character:19
    ; completions =
        [ CompletionItem.create
            ~label:"outer"
            ~kind:CompletionItemKind.Variable
            ~detail:"(* Unresolved *)"
            ~sortText:"\x08"
            ()
        ; CompletionItem.create
            ~label:"Inner"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x08"
            ()
        ]
    ; negative_labels = [ "Inner.inner"; "Inner.completion" ]
    }
  ; { test_name = "From top-level scope"
    ; file_name = "contracts/lsp/completion_inside_module.mligo"
    ; position = Position.create ~line:11 ~character:17
    ; completions =
        [ CompletionItem.create
            ~label:"Outer"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x08"
            ()
        ]
    ; negative_labels =
        [ "Outer.outer"
        ; "Outer.Inner.inner"
        ; "Outer.Inner.completion"
        ; "Outer.completion"
        ]
    }
  ; { test_name = "Module definitions don't contain scopes"
    ; file_name = "contracts/lsp/completion_similar_name.mligo"
    ; position = Position.create ~line:14 ~character:13
    ; completions =
        [ CompletionItem.create
            ~label:"add"
            ~kind:CompletionItemKind.Variable
            ~detail:"int -> int -> storage"
            ~sortText:"\x05"
            ()
        ; CompletionItem.create
            ~label:"sub"
            ~kind:CompletionItemKind.Variable
            ~detail:"int -> int -> storage"
            ~sortText:"\x05"
            ()
        ; CompletionItem.create
            ~label:"zero"
            ~kind:CompletionItemKind.Variable
            ~detail:"storage"
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = [ "a"; "b" ]
    }
  ; { test_name = "Complete from scope after a dot"
    ; file_name = "contracts/lsp/completion_similar_name.mligo"
    ; position = Position.create ~line:16 ~character:10
    ; completions =
        [ CompletionItem.create
            ~label:"Math"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x08"
            ()
        ; CompletionItem.create
            ~label:"Map"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x09"
            ()
        ]
    ; negative_labels = [ "Math.add"; "Math.sub"; "Math.zero" ]
    }
  ; { test_name = "Complete first module from written module path"
    ; file_name = "contracts/lsp/completion_module.mligo"
    ; position = Position.create ~line:9 ~character:9
    ; completions =
        [ CompletionItem.create
            ~label:"Foo"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x08"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Completion from shadowed module"
    ; file_name = "contracts/lsp/completion_shadowed_module_alias.mligo"
    ; position = Position.create ~line:6 ~character:10
    ; completions =
        [ CompletionItem.create
            ~label:"x"
            ~kind:CompletionItemKind.Variable
            ~detail:"int"
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete first module from written module path"
    ; file_name = "contracts/lsp/completion_missing_end.mligo"
    ; position = Position.create ~line:5 ~character:13
    ; completions =
        [ CompletionItem.create
            ~label:"ABA"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x08"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "File completions in import for outer file in the project"
    ; file_name = "contracts/lsp/completion_files/completion_files.mligo"
    ; position = Position.create ~line:0 ~character:9
    ; completions =
        [ CompletionItem.create
            ~label:"@ligo/math-lib/core/math.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ; CompletionItem.create
            ~label:"ligo-breathalyzer/lib/model.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ; CompletionItem.create
            ~label:"inner/inner.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ; CompletionItem.create
            ~label:"outer.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ]
    ; negative_labels = [ "completion_files.mligo"; ".hidden_dir/hidden.mligo" ]
    }
  ; { test_name = "File completions in include for outer file in the project"
    ; file_name = "contracts/lsp/completion_files/completion_files.mligo"
    ; position = Position.create ~line:1 ~character:10
    ; completions =
        [ CompletionItem.create
            ~label:"@ligo/math-lib/core/math.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ; CompletionItem.create
            ~label:"ligo-breathalyzer/lib/model.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ; CompletionItem.create
            ~label:"inner/inner.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ; CompletionItem.create
            ~label:"outer.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ]
    ; negative_labels = [ "completion_files.mligo"; ".hidden_dir/hidden.mligo" ]
    }
  ; { test_name = "File completions in import for inner file in the project"
    ; file_name = "contracts/lsp/completion_files/inner/inner.mligo"
    ; position = Position.create ~line:0 ~character:9
    ; completions =
        [ CompletionItem.create
            ~label:"@ligo/math-lib/core/math.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ; CompletionItem.create
            ~label:"ligo-breathalyzer/lib/model.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ; CompletionItem.create
            ~label:"../completion_files.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ; CompletionItem.create
            ~label:"../outer.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ]
    ; negative_labels = [ "inner.mligo"; ".hidden_dir/hidden.mligo" ]
    }
  ; { test_name = "File completions in include for inner file in the project"
    ; file_name = "contracts/lsp/completion_files/inner/inner.mligo"
    ; position = Position.create ~line:1 ~character:10
    ; completions =
        [ CompletionItem.create
            ~label:"@ligo/math-lib/core/math.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ; CompletionItem.create
            ~label:"ligo-breathalyzer/lib/model.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ; CompletionItem.create
            ~label:"../completion_files.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ; CompletionItem.create
            ~label:"../outer.mligo"
            ~kind:CompletionItemKind.File
            ~sortText:"\x00"
            ()
        ]
    ; negative_labels = [ "inner.mligo"; ".hidden_dir/hidden.mligo" ]
    }
  ; { test_name = "Scope completions for modules"
    ; file_name = "contracts/lsp/completion_imported_module.mligo"
    ; position = Position.create ~line:2 ~character:11
    ; completions =
        [ CompletionItem.create
            ~label:"A"
            ~kind:CompletionItemKind.Module
            ~sortText:"\b"
            ()
        ]
    ; negative_labels = [ "After"; "N"; "K" ]
    }
  ; { test_name = "Field completions for imported modules"
    ; file_name = "contracts/lsp/completion_imported_module.mligo"
    ; position = Position.create ~line:8 ~character:13
    ; completions =
        [ CompletionItem.create
            ~label:"M"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x06"
            ()
        ]
    ; negative_labels = [ "After"; "K" ]
    }
  ; { test_name = "Module scope in field completions"
    ; file_name = "contracts/lsp/completion_imported_module.mligo"
    ; position = Position.create ~line:9 ~character:15
    ; completions =
        [ CompletionItem.create
            ~label:"N"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x06"
            ()
        ]
    ; negative_labels = [ "t"; "y" ]
    }
  ; { test_name = "Constructor completions"
    ; file_name = "contracts/lsp/completion_ctors.mligo"
    ; position = Position.create ~line:3 ~character:10
    ; completions =
        [ CompletionItem.create
            ~label:"Foo"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ; CompletionItem.create
            ~label:"Bar"
            ~detail:"int"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ; CompletionItem.create
            ~label:"Excluding"
            ~detail:"address list"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\t"
            ()
        ]
    ; negative_labels = [ "Baz"; "Aaa" ]
    }
  ; { test_name = "Constructor completions with local defined one"
    ; file_name = "contracts/lsp/completion_ctors.mligo"
    ; position = Position.create ~line:5 ~character:10
    ; completions =
        [ CompletionItem.create
            ~label:"Foo"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ; CompletionItem.create
            ~label:"Bar"
            ~detail:"int"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ; CompletionItem.create
            ~label:"Baz"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ]
    ; negative_labels = [ "Aaa" ]
    }
  ; { test_name = "Constructor completions with type defined in module"
    ; file_name = "contracts/lsp/completion_ctors.mligo"
    ; position = Position.create ~line:12 ~character:8
    ; completions =
        [ CompletionItem.create
            ~label:"Foo"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ; CompletionItem.create
            ~label:"Bar"
            ~detail:"int"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ; CompletionItem.create
            ~label:"Aaa"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ]
    ; negative_labels = [ "Baz" ]
    }
  ; { test_name = "Field completions for stdlib module alias"
    ; file_name = "contracts/lsp/completion_stdlib_alias.mligo"
    ; position = Position.create ~line:2 ~character:15
    ; completions =
        [ CompletionItem.create
            ~label:"Typed_address"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x06"
            ()
        ; CompletionItem.create
            ~label:"originate"
            ~detail:"(* Unresolved *)"
            ~kind:CompletionItemKind.Variable
            ~sortText:"\x06"
            ()
        ]
    ; negative_labels = [ "typed_address" ]
    }
  ; { test_name = "Things from inner modules don't appear in field completions"
    ; file_name = "contracts/lsp/completion_module_defs.mligo"
    ; position = Position.create ~line:8 ~character:12
    ; completions =
        [ CompletionItem.create
            ~label:"B"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x05"
            ()
        ; CompletionItem.create
            ~label:"aaa"
            ~detail:"int"
            ~kind:CompletionItemKind.Variable
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = [ "bbb" ]
    }
  ]


let test_cases_jsligo =
  [ { test_name = "Complete record fields"
    ; file_name = "contracts/lsp/completion_record.jsligo"
    ; position = Position.create ~line:7 ~character:31
    ; completions =
        [ CompletionItem.create
            ~label:"numeric"
            ~kind:CompletionItemKind.Field
            ~detail:"int"
            ~sortText:"\x03"
            ()
        ; CompletionItem.create
            ~label:"stringy"
            ~kind:CompletionItemKind.Field
            ~detail:"string"
            ~sortText:"\x03"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete partially written nested record field"
    ; file_name = "contracts/lsp/completion_record.jsligo"
    ; position = Position.create ~line:15 ~character:43
    ; completions =
        [ CompletionItem.create
            ~label:"stringy"
            ~kind:CompletionItemKind.Field
            ~detail:"string"
            ~sortText:"\x03"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete already written record field"
    ; file_name = "contracts/lsp/completion_record.jsligo"
    ; position = Position.create ~line:15 ~character:33
    ; completions =
        [ CompletionItem.create
            ~label:"record_1"
            ~kind:CompletionItemKind.Field
            ~detail:"record_1"
            ~sortText:"\x03"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete module field"
    ; file_name = "contracts/lsp/completion_module.jsligo"
    ; position = Position.create ~line:9 ~character:14
    ; completions =
        [ CompletionItem.create
            ~label:"module_field"
            ~kind:CompletionItemKind.Variable
            ~detail:"int"
            ~sortText:"\x05"
            ()
        ; CompletionItem.create
            ~label:"another_thing"
            ~kind:CompletionItemKind.Variable
            ~detail:"string"
            ~sortText:"\x05"
            ()
        ; CompletionItem.create
            ~label:"Bar"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete nested module field"
    ; file_name = "contracts/lsp/completion_module.jsligo"
    ; position = Position.create ~line:10 ~character:18
    ; completions =
        [ CompletionItem.create
            ~label:"nested"
            ~kind:CompletionItemKind.Variable
            ~detail:"{ left: int; right: int }"
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete nested record field within module"
    ; file_name = "contracts/lsp/completion_module.jsligo"
    ; position = Position.create ~line:11 ~character:25
    ; completions =
        [ CompletionItem.create
            ~label:"left"
            ~kind:CompletionItemKind.Field
            ~detail:"int"
            ~sortText:"\x03"
            ()
        ; CompletionItem.create
            ~label:"right"
            ~kind:CompletionItemKind.Field
            ~detail:"int"
            ~sortText:"\x03"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete already written module field"
    ; file_name = "contracts/lsp/completion_module.jsligo"
    ; position = Position.create ~line:11 ~character:14
    ; completions =
        [ CompletionItem.create
            ~label:"Bar"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete already written record field within module"
    ; file_name = "contracts/lsp/completion_module.jsligo"
    ; position = Position.create ~line:11 ~character:21
    ; completions =
        [ CompletionItem.create
            ~label:"nested"
            ~kind:CompletionItemKind.Variable
            ~detail:"{ left: int; right: int }"
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete simple suggestions"
    ; file_name = "contracts/lsp/completion_simple.jsligo"
    ; position = Position.create ~line:4 ~character:19
    ; completions =
        [ CompletionItem.create
            ~label:"thing1"
            ~kind:CompletionItemKind.Variable
            ~detail:"int"
            ~sortText:"\x08"
            ()
        ; CompletionItem.create
            ~label:"some_string"
            ~kind:CompletionItemKind.Variable
            ~detail:"string"
            ~sortText:"\x08"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete type within module"
    ; file_name = "contracts/lsp/completion_type.jsligo"
    ; position = Position.create ~line:5 ~character:30
    ; completions =
        [ CompletionItem.create
            ~label:"here_it_is"
            ~kind:CompletionItemKind.TypeParameter
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete variable within module alias"
    ; file_name = "contracts/lsp/completion_alias.jsligo"
    ; position = Position.create ~line:10 ~character:19
    ; completions =
        [ CompletionItem.create
            ~label:"target"
            ~kind:CompletionItemKind.Variable
            ~detail:"int"
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Complete variable within already written module alias"
    ; file_name = "contracts/lsp/completion_alias.jsligo"
    ; position = Position.create ~line:10 ~character:17
    ; completions =
        [ CompletionItem.create
            ~label:"B"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x05"
            ()
        ; CompletionItem.create
            ~label:"Y"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "From inner scope"
    ; file_name = "contracts/lsp/completion_inside_module.jsligo"
    ; position = Position.create ~line:5 ~character:23
    ; completions =
        [ CompletionItem.create
            ~label:"outer"
            ~kind:CompletionItemKind.Variable
            ~detail:"/* Unresolved */"
            ~sortText:"\x08"
            ()
        ; CompletionItem.create
            ~label:"inner"
            ~kind:CompletionItemKind.Variable
            ~detail:"/* Unresolved */"
            ~sortText:"\x08"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "From outer scope"
    ; file_name = "contracts/lsp/completion_inside_module.jsligo"
    ; position = Position.create ~line:8 ~character:21
    ; completions =
        [ CompletionItem.create
            ~label:"outer"
            ~kind:CompletionItemKind.Variable
            ~detail:"/* Unresolved */"
            ~sortText:"\x08"
            ()
        ; CompletionItem.create
            ~label:"Inner"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x08"
            ()
        ]
    ; negative_labels = [ "Inner.inner"; "Inner.completion" ]
    }
  ; { test_name = "From top-level scope"
    ; file_name = "contracts/lsp/completion_inside_module.jsligo"
    ; position = Position.create ~line:11 ~character:19
    ; completions =
        [ CompletionItem.create
            ~label:"Outer"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x08"
            ()
        ]
    ; negative_labels =
        [ "Outer.outer"
        ; "Outer.Inner.inner"
        ; "Outer.Inner.completion"
        ; "Outer.completion"
        ]
    }
  ; { test_name = "Module definitions don't contain scopes"
    ; file_name = "contracts/lsp/completion_similar_name.jsligo"
    ; position = Position.create ~line:14 ~character:15
    ; completions =
        [ CompletionItem.create
            ~label:"add"
            ~kind:CompletionItemKind.Variable
            ~detail:"(a: int, b: int) => storage"
            ~sortText:"\x05"
            ()
        ; CompletionItem.create
            ~label:"sub"
            ~kind:CompletionItemKind.Variable
            ~detail:"(a: int, b: int) => storage"
            ~sortText:"\x05"
            ()
        ; CompletionItem.create
            ~label:"zero"
            ~kind:CompletionItemKind.Variable
            ~detail:"storage"
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = [ "a"; "b" ]
    }
  ; { test_name = "Complete from scope after a dot"
    ; file_name = "contracts/lsp/completion_similar_name.jsligo"
    ; position = Position.create ~line:16 ~character:12
    ; completions =
        [ CompletionItem.create
            ~label:"Math"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x08"
            ()
        ; CompletionItem.create
            ~label:"Map"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x09"
            ()
        ]
    ; negative_labels = [ "Math.add"; "Math.sub"; "Math.zero" ]
    }
  ; { test_name = "Complete first module from written module path"
    ; file_name = "contracts/lsp/completion_module.jsligo"
    ; position = Position.create ~line:9 ~character:11
    ; completions =
        [ CompletionItem.create
            ~label:"Foo"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x08"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Completion from shadowed module"
    ; file_name = "contracts/lsp/completion_shadowed_module_alias.jsligo"
    ; position = Position.create ~line:6 ~character:12
    ; completions =
        [ CompletionItem.create
            ~label:"x"
            ~kind:CompletionItemKind.Variable
            ~detail:"int"
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = []
    }
  ; { test_name = "Scope completions for modules"
    ; file_name = "contracts/lsp/completion_imported_module.jsligo"
    ; position = Position.create ~line:2 ~character:11
    ; completions =
        [ CompletionItem.create
            ~label:"A"
            ~kind:CompletionItemKind.Module
            ~sortText:"\b"
            ()
        ]
    ; negative_labels = [ "After"; "N"; "K" ]
    }
  ; { test_name = "Field completions for imported modules"
    ; file_name = "contracts/lsp/completion_imported_module.jsligo"
    ; position = Position.create ~line:8 ~character:13
    ; completions =
        [ CompletionItem.create
            ~label:"M"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x06"
            ()
        ]
    ; negative_labels = [ "After"; "K" ]
    }
  ; { test_name = "Module scope in field completions"
    ; file_name = "contracts/lsp/completion_imported_module.jsligo"
    ; position = Position.create ~line:9 ~character:15
    ; completions =
        [ CompletionItem.create
            ~label:"N"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x06"
            ()
        ]
    ; negative_labels = [ "t"; "y" ]
    }
  ; { test_name = "Constructor completions"
    ; file_name = "contracts/lsp/completion_ctors.jsligo"
    ; position = Position.create ~line:3 ~character:10
    ; completions =
        [ CompletionItem.create
            ~label:"Foo"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ; CompletionItem.create
            ~label:"Bar"
            ~detail:"int"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ; CompletionItem.create
            ~label:"Excluding"
            ~detail:"list<address>"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\t"
            ()
        ]
    ; negative_labels = [ "Baz"; "Aaa" ]
    }
  ; { test_name = "Constructor completions with local defined one"
    ; file_name = "contracts/lsp/completion_ctors.jsligo"
    ; position = Position.create ~line:5 ~character:10
    ; completions =
        [ CompletionItem.create
            ~label:"Foo"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ; CompletionItem.create
            ~label:"Bar"
            ~detail:"int"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ; CompletionItem.create
            ~label:"Baz"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ]
    ; negative_labels = [ "Aaa" ]
    }
  ; { test_name = "Constructor completions with type defined in namespace"
    ; file_name = "contracts/lsp/completion_ctors.jsligo"
    ; position = Position.create ~line:13 ~character:10
    ; completions =
        [ CompletionItem.create
            ~label:"Foo"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ; CompletionItem.create
            ~label:"Bar"
            ~detail:"int"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ; CompletionItem.create
            ~label:"Aaa"
            ~kind:CompletionItemKind.Constructor
            ~sortText:"\b"
            ()
        ]
    ; negative_labels = [ "Baz" ]
    }
  ; { test_name = "Field completions for stdlib namespace alias"
    ; file_name = "contracts/lsp/completion_stdlib_alias.jsligo"
    ; position = Position.create ~line:2 ~character:17
    ; completions =
        [ CompletionItem.create
            ~label:"Typed_address"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x06"
            ()
        ; CompletionItem.create
            ~label:"originate"
            ~detail:"/* Unresolved */"
            ~kind:CompletionItemKind.Variable
            ~sortText:"\x06"
            ()
        ]
    ; negative_labels = [ "typed_address" ]
    }
  ; { test_name = "Things from inner namespaces don't appear in field completions"
    ; file_name = "contracts/lsp/completion_module_defs.jsligo"
    ; position = Position.create ~line:8 ~character:14
    ; completions =
        [ CompletionItem.create
            ~label:"B"
            ~kind:CompletionItemKind.Module
            ~sortText:"\x05"
            ()
        ; CompletionItem.create
            ~label:"aaa"
            ~detail:"int"
            ~kind:CompletionItemKind.Variable
            ~sortText:"\x05"
            ()
        ]
    ; negative_labels = [ "bbb" ]
    }
  ]


let completion_implementations_test : unit Alcotest.test_case list =
  let file_name = "contracts/lsp/completion_inside_module.jsligo" in
  let position = Position.create ~line:8 ~character:20 in
  let cases =
    [ ( `With_scopes
      , { test_name = "Completion implementations: scopes"
        ; file_name
        ; position
        ; completions =
            [ CompletionItem.create
                ~label:"Inner"
                ~kind:CompletionItemKind.Module
                ~sortText:"\x08"
                ()
            ; CompletionItem.create
                ~label:"outer"
                ~kind:CompletionItemKind.Variable
                ~detail:"/* Unresolved */"
                ~sortText:"\x08"
                ()
            ]
        ; negative_labels = [ "Outer"; "inner"; "completion" ]
        } )
    ; ( `All_definitions
      , { test_name = "Completion implementations: all definitions"
        ; file_name
        ; position
        ; completions =
            [ CompletionItem.create
                ~label:"Inner"
                ~kind:CompletionItemKind.Module
                ~sortText:"\x08"
                ()
            ; CompletionItem.create
                ~label:"Outer"
                ~kind:CompletionItemKind.Module
                ~sortText:"\x08"
                ()
            ; CompletionItem.create
                ~label:"inner"
                ~kind:CompletionItemKind.Variable
                ~detail:"/* Unresolved */"
                ~sortText:"\x08"
                ()
            ; CompletionItem.create
                ~label:"outer"
                ~kind:CompletionItemKind.Variable
                ~detail:"/* Unresolved */"
                ~sortText:"\x08"
                ()
            ]
        ; negative_labels = []
        } )
    ; ( `Only_keywords_and_fields
      , { test_name = "Completion implementations: only keywords"
        ; file_name
        ; position
        ; completions =
            [ CompletionItem.create
                ~label:"let"
                ~kind:CompletionItemKind.Keyword
                ~sortText:"\x0c"
                ()
            ]
        ; negative_labels = [ "inner"; "outer"; "Inner"; "Outer" ]
        } )
    ]
  in
  List.map
    ~f:(fun (completion_implementation, test) ->
      let config = { default_test_config with completion_implementation } in
      completion_test ~config CameLIGO test)
    cases


let tests =
  ( "completion"
  , List.map ~f:(completion_test CameLIGO) test_cases_cameligo
    @ List.map ~f:(completion_test JsLIGO) test_cases_jsligo
    @ completion_implementations_test )
