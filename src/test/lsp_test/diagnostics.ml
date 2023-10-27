module Requests = Ligo_lsp.Server.Requests
open Alcotest_extras
open Handlers
open Lsp_helpers
open Range.Construct

type diagnostics_test =
  { test_name : string
  ; file_path : string
  ; diagnostics : Requests.simple_diagnostic list
  ; max_number_of_problems : int option
  }

let get_diagnostics_test
    ({ test_name; file_path; diagnostics; max_number_of_problems } : diagnostics_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let file_path_normalized = Path.from_relative file_path in
  let config =
    Option.map max_number_of_problems ~f:(fun max_number_of_problems ->
        { default_test_config with max_number_of_problems })
  in
  let _uri, actual_diagnostics =
    test_run_session ?config @@ open_file file_path_normalized
  in
  (* [on_doc] sends an empty list in case there are no diags, so we also need to
     address this corner case in our test expectations. *)
  let expected_diagnostics =
    let diags_by_file =
      Requests.partition_simple_diagnostics
        file_path_normalized
        max_number_of_problems
        diagnostics
    in
    let uri = DocumentUri.of_path file_path_normalized in
    if List.Assoc.mem diags_by_file ~equal:DocumentUri.equal uri
    then diags_by_file
    else (uri, []) :: diags_by_file
  in
  should_match_list
    ~msg:(Format.asprintf "Diagnostics mismatch for %s:" file_path)
    Alcotest.(pair Path.testable (unordered_list Diagnostic.testable))
    ~actual:(Requests.Handler.Path_hashtbl.to_alist actual_diagnostics)
    ~expected:(List.map ~f:(Tuple2.map_fst ~f:DocumentUri.to_path) expected_diagnostics)


let test_cases =
  [ { test_name = "Type errors"
    ; file_path = "contracts/negative/error_typer_1.mligo"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Error
          ; message = "Invalid type(s)\nCannot unify \"int\" with \"string\"."
          ; location =
              { range = interval 2 19 27
              ; path = Path.from_relative "contracts/negative/error_typer_1.mligo"
              }
          }
        ; { severity = DiagnosticSeverity.Error
          ; message = "Variable \"foo\" not found. "
          ; location =
              { range = interval 5 31 34
              ; path = Path.from_relative "contracts/negative/error_typer_1.mligo"
              }
          }
        ]
    ; max_number_of_problems = None
    }
  ; { test_name = "Syntax error"
    ; file_path = "contracts/lsp/syntax_error.mligo"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Error
          ; message =
              "Ill-formed contract.\n\
               At this point, if the current declaration is complete, one of the\n\
               following is expected:\n\
              \  * another declaration;\n\
              \  * the end of the file.\n"
          ; location =
              { range = interval 0 10 11
              ; path = Path.from_relative "contracts/lsp/syntax_error.mligo"
              }
          }
        ]
    ; max_number_of_problems = None
    }
  ; { test_name = "Warnings"
    ; file_path = "contracts/lsp/warnings.jsligo"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Warning
          ; message =
              "\n\
               Warning: unused variable \"x\".\n\
               Hint: replace it by \"_x\" to prevent this warning.\n"
          ; location =
              { range = interval 2 10 11
              ; path = Path.from_relative "contracts/lsp/warnings.jsligo"
              }
          }
        ; { severity = DiagnosticSeverity.Warning
          ; message = "Toplevel let declaration is silently changed to const declaration."
          ; location =
              { range = interval 0 7 17
              ; path = Path.from_relative "contracts/lsp/warnings.jsligo"
              }
          }
        ]
    ; max_number_of_problems = None
    }
  ; { test_name = "Syntax and type errors"
    ; file_path = "contracts/lsp/syntax_plus_type_errors.jsligo"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Error
          ; message = "Variable \"ghost_ident\" not found. "
          ; location =
              { range = point 4 18
              ; path = Path.from_relative "contracts/lsp/syntax_plus_type_errors.jsligo"
              }
          }
        ; { severity = DiagnosticSeverity.Error
          ; message = "Variable \"ghost_ident\" not found. "
          ; location =
              { range = point 4 14
              ; path = Path.from_relative "contracts/lsp/syntax_plus_type_errors.jsligo"
              }
          }
        ; { severity = DiagnosticSeverity.Error
          ; message = "Invalid type(s).\nExpected \"string\", but got: \"int\"."
          ; location =
              { range = interval 2 19 21
              ; path = Path.from_relative "contracts/lsp/syntax_plus_type_errors.jsligo"
              }
          }
        ; { severity = DiagnosticSeverity.Error
          ; message =
              "Ill-formed expression.\nAt this point, an expression is expected.\n"
          ; location =
              { range = interval 4 15 18
              ; path = Path.from_relative "contracts/lsp/syntax_plus_type_errors.jsligo"
              }
          }
        ; { severity = DiagnosticSeverity.Error
          ; message =
              "Ill-formed value declaration.\n\
               At this point, a pattern is expected, e.g. a variable.\n"
          ; location =
              { range = point 4 18
              ; path = Path.from_relative "contracts/lsp/syntax_plus_type_errors.jsligo"
              }
          }
        ; { severity = DiagnosticSeverity.Warning
          ; message = "Toplevel let declaration is silently changed to const declaration."
          ; location =
              { range = interval 4 15 18
              ; path = Path.from_relative "contracts/lsp/syntax_plus_type_errors.jsligo"
              }
          }
        ]
    ; max_number_of_problems = None
    }
  ; { test_name = "All OK"
    ; file_path = "contracts/lsp/simple.mligo"
    ; diagnostics = []
    ; max_number_of_problems = None
    }
  ; { test_name = "Limit from 11 to 2 diagnostics in session"
    ; file_path = "contracts/warning_sum_types.mligo"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Warning
          ; message =
              "Warning: The type of \"TopTop(42)\" is ambiguous: Inferred type is \
               \"ttop2\" but could be of type \"ttop\".\n\
               Hint: You might want to add a type annotation. \n"
          ; location =
              { range = interval 85 14 23
              ; path = Path.from_relative "contracts/warning_sum_types.mligo"
              }
          }
        ; { severity = DiagnosticSeverity.Warning
          ; message =
              "Warning: The type of \"TopA(42)\" is ambiguous: Inferred type is \"ttop\" \
               but could be of type \"ta\".\n\
               Hint: You might want to add a type annotation. \n"
          ; location =
              { range = interval 87 14 21
              ; path = Path.from_relative "contracts/warning_sum_types.mligo"
              }
          }
        ]
    ; max_number_of_problems = Some 2
    }
  ; { test_name = "Polymorphic Type error"
    ; file_path = "contracts/lsp/poly_type_error.mligo"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Error
          ; message =
              "Invalid type(s)\n\
               Cannot unify \"int\" with \"( ^a * ^b ) -> ^a\".\n\
               Hint: \"^b\", \"^a\" represent placeholder type(s).\n"
          ; location =
              { range = interval 0 21 22
              ; path = Path.from_relative "contracts/lsp/poly_type_error.mligo"
              }
          }
        ]
    ; max_number_of_problems = None
    }
  ; { test_name = "No diagnostics for imported package."
    ; file_path = "contracts/lsp/registry.jsligo"
    ; diagnostics = []
    ; max_number_of_problems = None
    }
  ; { test_name = "Shows diagnostics from another file."
    ; file_path = "contracts/lsp/import_warnings.jsligo"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Warning
          ; message =
              "\n\
               Warning: unused variable \"x\".\n\
               Hint: replace it by \"_x\" to prevent this warning.\n"
          ; location =
              { range = interval 2 10 11
              ; path = Path.from_relative "contracts/lsp/warnings.jsligo"
              }
          }
        ; { severity = DiagnosticSeverity.Warning
          ; message = "Toplevel let declaration is silently changed to const declaration."
          ; location =
              { range = interval 0 7 17
              ; path = Path.from_relative "contracts/lsp/warnings.jsligo"
              }
          }
        ]
    ; max_number_of_problems = None
    }
  ; { test_name = "Shows TZIP-16 checks with a top-level storage."
    ; file_path = "contracts/lsp/test_metadata.mligo"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Warning
          ; message =
              "Warning: If the following metadata is meant to be TZIP-16 compliant,\n\
               then it should be a 'big_map' from 'string' to 'bytes'.\n\
               Hint: The corresponding type should be :\n\
              \  (string, bytes) big_map\n\
               You can disable this warning with the '--no-metadata-check' flag.\n"
          ; location =
              { range = interval 2 15 19
              ; path = Path.from_relative "contracts/lsp/test_metadata.mligo"
              }
          }
        ]
    ; max_number_of_problems = None
    }
  ; { test_name = "Shows a duplicate entrypoint error."
    ; file_path = "contracts/lsp/entrypoints_repeated.mligo"
    ; diagnostics =
        [ { message = "Duplicate entry-point ep_int"
          ; location =
              { range = interval 1 4 10
              ; path = Path.from_relative "contracts/lsp/entrypoints_repeated.mligo"
              }
          ; severity = DiagnosticSeverity.Error
          }
        ]
    ; max_number_of_problems = None
    }
  ; { test_name = "Shows an error when two toplevel entrypoints have different storage."
    ; file_path = "contracts/lsp/entrypoints_different_storage.mligo"
    ; diagnostics =
        [ { message =
              "Storage types do not match for different entrypoints:\n\
               - ep_int : int\n\
               - ep_string : string"
          ; location =
              { range = interval 1 4 10
              ; path =
                  Path.from_relative "contracts/lsp/entrypoints_different_storage.mligo"
              }
          ; severity = DiagnosticSeverity.Error
          }
        ]
    ; max_number_of_problems = None
    }
  ; { test_name =
        "Shows views-related errors and storage warnings."
        (* TODO #2086 add tests for more complicated errors *)
    ; file_path = "contracts/lsp/entrypoints_views.mligo"
    ; diagnostics =
        [ { message =
              "Warning: If the following metadata is meant to be TZIP-16 compliant,\n\
               then it should be a 'big_map' from 'string' to 'bytes'.\n\
               Hint: The corresponding type should be :\n\
              \  (string, bytes) big_map\n\
               You can disable this warning with the '--no-metadata-check' flag.\n"
          ; location =
              { range = interval 4 16 20
              ; path = Path.from_relative "contracts/lsp/entrypoints_views.mligo"
              }
          ; severity = DiagnosticSeverity.Warning
          }
        ; { message = "The view \"bad_view_not_func\" is not a function."
          ; location =
              { range = interval 21 4 21
              ; path = Path.from_relative "contracts/lsp/entrypoints_views.mligo"
              }
          ; severity = DiagnosticSeverity.Error
          }
        ]
    ; max_number_of_problems = None
    }
  ; { test_name = "Shows entrypoint-related errors in many modules simultaneously."
    ; file_path = "contracts/lsp/entrypoints_modules.mligo"
    ; diagnostics =
        [ { message = "Invalid type for view \"Bad_4\".\nA view must be a function."
          ; location =
              { range = interval 51 6 7
              ; path = Path.from_relative "contracts/lsp/entrypoints_modules.mligo"
              }
          ; severity = DiagnosticSeverity.Error
          }
        ; { message = "Not an entrypoint: unit -> ( list (operation) * string )"
          ; location =
              { range = interval 41 6 12
              ; path = Path.from_relative "contracts/lsp/entrypoints_modules.mligo"
              }
          ; severity = DiagnosticSeverity.Error
          }
        ; { message = "Duplicate entry-point ep_string"
          ; location =
              { range = interval 32 6 15
              ; path = Path.from_relative "contracts/lsp/entrypoints_modules.mligo"
              }
          ; severity = DiagnosticSeverity.Error
          }
        ; { message =
              "Storage types do not match for different entrypoints:\n\
               - ep_string : string\n\
               - ep_int : int"
          ; location =
              { range = interval 23 6 15
              ; path = Path.from_relative "contracts/lsp/entrypoints_modules.mligo"
              }
          ; severity = DiagnosticSeverity.Error
          }
        ]
    ; max_number_of_problems = None
    }
  ]


let tests = "diagnostics", List.map ~f:get_diagnostics_test test_cases
