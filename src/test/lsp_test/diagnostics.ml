module Scopes = Ligo_interface

module Ligo_interface = Ligo_interface.Make (struct
  module Info = Ligo_api.Info
  module Print = Ligo_api.Print
end)

open Lsp.Types

module Requests = Ligo_lsp.Server.Requests.Make (struct
  module Info = Ligo_api.Info
  module Print = Ligo_api.Print
end)

module Diagnostics = Requests.Diagnostics
open Common
open Handlers

type diagnostics_test =
  { test_name : string
  ; file_path : string
  ; diagnostics : Diagnostics.simple_diagnostic list
  }

let pp_diagnostic = pp_with_yojson Lsp.Types.Diagnostic.yojson_of_t
let eq_diagnostic = Caml.( = )

let testable_diagnostic : Diagnostic.t Alcotest.testable =
  Alcotest.testable pp_diagnostic eq_diagnostic


let get_diagnostics_test ({ test_name; file_path; diagnostics } : diagnostics_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let _uri, actual_diagnostics = test_run_session @@ open_file (to_absolute file_path) in
  should_match_list
    ~msg:(Format.asprintf "Diagnostics mismatch for %s:" file_path)
    testable_diagnostic
    ~actual:actual_diagnostics
    ~expected:(List.map ~f:Diagnostics.from_simple_diagnostic diagnostics)


let test_cases =
  [ { test_name = "Type errors"
    ; file_path = "contracts/negative/error_typer_1.mligo"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Error
          ; message = "Invalid type(s)\nCannot unify int with string."
          ; range = Some (Utils.interval 2 19 27)
          }
        ; { severity = DiagnosticSeverity.Error
          ; message = "Variable \"foo\" not found. "
          ; range = Some (Utils.interval 5 31 34)
          }
        ]
    }
  ; { test_name = "Syntax error"
    ; file_path = "contracts/lsp/syntax_error.mligo"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Error
          ; message =
              "Ill-formed contract.\n\
               At this point, if the declaration is complete, one of the following is\n\
               expected:\n\
              \  * another declaration;\n\
              \  * the end of the file.\n"
          ; range = Some (Utils.interval 0 10 11)
          }
        ]
    }
  ; { test_name = "Warnings"
    ; file_path = "contracts/lsp/warnings.jsligo"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Warning
          ; message =
              "Toplevel let declaration are silently change to const declaration.@"
          ; range = Some (Utils.interval 0 0 10)
          }
        ; { severity = DiagnosticSeverity.Warning
          ; message =
              "\n\
               Warning: unused variable \"x\".\n\
               Hint: replace it by \"_x\" to prevent this warning.\n"
          ; range = Some (Utils.interval 2 10 11)
          }
        ]
    }
  ; { test_name = "Syntax and type errors"
    ; file_path = "contracts/lsp/syntax_plus_type_errors.jsligo"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Error
          ; message =
              "Ill-formed expression.\nAt this point, an expression is expected.\n"
          ; range = Some (Utils.interval 4 15 18)
          }
        ; { severity = DiagnosticSeverity.Error
          ; message = "Invalid type(s).\nExpected \"string\", but got: \"int\"."
          ; range = Some (Utils.interval 2 19 21)
          }
        ; { severity = DiagnosticSeverity.Error
          ; message =
              "Variable \"_#170\" not found. "
              (* FIXME 1689 - we shoul not report that things added by
                 error recovery do not exist, also the number here can
                 be changed after any changes in LIGO, maybe we want to
                 rewrite that test so it would not require promotion too often*)
          ; range = Some (Utils.point 4 13)
          }
        ]
    }
  ; { test_name = "All OK"; file_path = "contracts/lsp/simple.mligo"; diagnostics = [] }
  ]


let tests = "diagnostics", List.map ~f:get_diagnostics_test test_cases
