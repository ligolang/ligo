open Lsp_helpers

type simple_diagnostic =
  { severity : DiagnosticSeverity.t
  ; message : string
  ; range : Range.t option
  }

let from_simple_diagnostic : simple_diagnostic -> Diagnostic.t =
 fun { range; message; severity } ->
  let range = Option.value ~default:Range.dummy range in
  Diagnostic.create ?severity:(Some severity) ~message ~range ()


let to_simple_diagnostic : Diagnostic.t -> simple_diagnostic =
 fun diag ->
  { severity = Option.value ~default:DiagnosticSeverity.Error diag.severity
  ; message = diag.message
  ; range = Some diag.range
  }


(** Extract all errors and warnings for the given scopes and collect them in a list. *)
let get_diagnostics : Ligo_interface.get_scope_info -> simple_diagnostic list =
 fun { errors; warnings; _ } ->
  let extract_error_information : Main_errors.all -> simple_diagnostic list =
   fun errs ->
    let errs = Main_errors.Formatter.error_json errs in
    List.map
      ~f:(fun ({ content = { message; location; _ }; _ } : Simple_utils.Error.t) ->
        let range = Option.bind location ~f:Range.of_loc in
        { range; message; severity = DiagnosticSeverity.Error })
      errs
  in
  let extract_warning_information : Main_warnings.all -> simple_diagnostic =
   fun warn ->
    let ({ content = { message; location; _ }; _ } : Simple_utils.Warning.t) =
      Main_warnings.to_warning warn
    in
    { range = Range.of_loc location; message; severity = DiagnosticSeverity.Warning }
  in
  List.concat_map ~f:extract_error_information errors
  @ List.map ~f:extract_warning_information warnings
