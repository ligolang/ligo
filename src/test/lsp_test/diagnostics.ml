module Scopes = Ligo_interface

module Ligo_interface = Ligo_interface.Make (struct
  module Info = Ligo_api.Info
  module Print = Ligo_api.Print
end)

open Lsp.Types
module Diagnostics = Ligo_api.Lsp_server.Server.Requests.Diagnostics
open Common

(* TODO: Allow reading from a file. *)
type diagnostics_test =
  { test_name : string
  ; dialect : Syntax_types.t
  ; source : string
  ; diagnostics : Diagnostics.simple_diagnostic list
  }

let pp_simple_diagnostic
    ppf
    ({ severity; message; range } : Diagnostics.simple_diagnostic)
  =
  let range_str =
    Option.value_map range ~default:"<no location>" ~f:Utils.range_to_string
  in
  Fmt.pf ppf "{%S; %S; %S}" (diagnostic_severity_to_string severity) message range_str


let eq_simple_diagnostic = Caml.( = )

let simple_diagnostic : Diagnostics.simple_diagnostic Alcotest.testable =
  Alcotest.testable pp_simple_diagnostic eq_simple_diagnostic


let get_diagnostics_test ({ test_name; dialect; source; diagnostics } : diagnostics_test)
    : unit Alcotest.test_case
  =
  Alcotest.test_case test_name `Quick
  @@ fun () ->
  let file =
    match dialect with
    | CameLIGO -> "test.mligo"
    | JsLIGO -> "test.jsligo"
  in
  let uri = DocumentUri.of_path file in
  let get_scope_info = Scopes.unfold_get_scope @@ Ligo_interface.get_scope uri source in
  let expected_diagnostics = List.sort ~compare:Caml.compare diagnostics in
  let actual_diagnostics =
    List.sort ~compare:Caml.compare @@ Diagnostics.get_diagnostics get_scope_info
  in
  Alcotest.(check (list simple_diagnostic))
    "Diagnostics match"
    expected_diagnostics
    actual_diagnostics


let test_cases =
  [ { test_name = "Type error"
    ; dialect = CameLIGO
    ; source = "let x : int = \"string\""
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Error
          ; message = "Invalid type(s).\nExpected \"int\", but got: \"string\"."
          ; range = Some (Utils.interval 0 14 22)
          }
        ]
    }
  ; { test_name = "Syntax error"
    ; dialect = JsLIGO
    ; source = "type number => int"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Error
          ; message =
              "Ill-formed type declaration.\n\
               At this point, one of the following is expected:\n\
              \  * the assignment symbol '=' followed by a type expression;\n\
              \  * type parameters between chevrons '<' and '>', if defining a\n\
              \    parametric type.\n"
          ; range = Some (Utils.interval 0 12 14)
          }
        ]
    }
  ; { test_name = "Warning"
    ; dialect = JsLIGO
    ; source = "let x = 0"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Warning
          ; message =
              "Toplevel let declaration are silently change to const declaration.@"
          ; range = Some (Utils.interval 0 0 9)
          }
        ]
    }
  ; { test_name = "All OK"
    ; dialect = JsLIGO
    ; source = "const x : int = 0"
    ; diagnostics = []
    }
  ; { test_name = "Multiple diagnostics"
    ; dialect = CameLIGO
    ; source = "let x = end\nlet y : string = 0"
    ; diagnostics =
        [ { severity = DiagnosticSeverity.Error
          ; message =
              "Ill-formed value declaration.\nAt this point, an expression is expected.\n"
          ; range = Some (Utils.interval 0 8 11)
          }
        ; { severity = DiagnosticSeverity.Error
          ; message =
              "Ill-formed local value declaration.\n\
               At this point, if the expression of the left-hand side is complete,\n\
               the keyword 'in' is expected, followed by an expression.\n"
          ; range = Some (Utils.point 1 18)
          }
        ; { severity = DiagnosticSeverity.Error
          ; message = "Invalid type(s).\nExpected \"string\", but got: \"int\"."
          ; range = Some (Utils.interval 1 17 18)
          }
        ]
    }
  ]


let tests = "diagnostics", List.map ~f:get_diagnostics_test test_cases
