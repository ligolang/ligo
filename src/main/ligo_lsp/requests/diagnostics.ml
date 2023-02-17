open Lsp.Types
module Option = Caml.Option

type simple_diagnostic =
  { severity : DiagnosticSeverity.t
  ; message : string
  ; range : Range.t option
  }

let from_simple_diagnostic : simple_diagnostic -> Diagnostic.t =
 fun { range; message; severity } ->
  let range = Option.value ~default:Utils.dummy_range range in
  Diagnostic.create ?severity:(Some severity) ~message ~range ()


(** Extract all errors and warnings for the given scopes and collect them in a list. *)
let get_diagnostics : Ligo_interface.get_scope_info -> simple_diagnostic list =
 fun { errors; warnings; _ } ->
  let extract_error_information : Main_errors.all -> simple_diagnostic list =
   fun errs ->
    let errs = Main_errors.Formatter.error_json errs in
    List.map
      ~f:(fun ({ content = { message; location; _ }; _ } : Simple_utils.Error.t) ->
        let range = Option.bind location Utils.location_to_range in
        { range; message; severity = DiagnosticSeverity.Error })
      errs
  in
  let extract_warning_information : Main_warnings.all -> simple_diagnostic =
   fun warn ->
    let ({ content = { message; location; _ }; _ } : Simple_utils.Warning.t) =
      Main_warnings.to_warning warn
    in
    { range = Utils.location_to_range location
    ; message
    ; severity = DiagnosticSeverity.Warning
    }
  in
  List.concat_map ~f:extract_error_information errors
  @ List.map ~f:extract_warning_information warnings
