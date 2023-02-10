open Lsp.Types

(* Range *)

let pp_range ppf x = Fmt.pf ppf "%S" (Utils.range_to_string x)
let eq_range = Caml.( = )
let range : Range.t Alcotest.testable = Alcotest.testable pp_range eq_range

let diagnostic_severity_to_string = function
  | DiagnosticSeverity.Error -> "error"
  | DiagnosticSeverity.Warning -> "warning"
  | DiagnosticSeverity.Information -> "information"
  | DiagnosticSeverity.Hint -> "hint"


(* DiagnosticSeverity *)

let pp_diagnostic_severity ppf x = Fmt.pf ppf "%S" (diagnostic_severity_to_string x)
let eq_diagnostic_severity = Caml.( = )

let diagnostic_severity : DiagnosticSeverity.t Alcotest.testable =
  Alcotest.testable pp_diagnostic_severity eq_diagnostic_severity
