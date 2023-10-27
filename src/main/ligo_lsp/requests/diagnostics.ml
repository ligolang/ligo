open Lsp_helpers

type simple_diagnostic =
  { severity : DiagnosticSeverity.t
  ; message : string
  ; location : Def.Loc_in_file.t
  }

let from_simple_diagnostic : simple_diagnostic -> Diagnostic.t =
 fun { severity; message; location } ->
  Diagnostic.create ?severity:(Some severity) ~message ~range:location.range ()


let partition_simple_diagnostics
    (current_path : Path.t)
    (max_number_of_problems : int option)
    (diags : simple_diagnostic list)
    : (DocumentUri.t * Diagnostic.t list) list
  =
  let take_problems =
    match max_number_of_problems with
    | None -> Fn.id
    | Some max_number_of_problems -> Fn.flip List.take max_number_of_problems
  in
  diags
  (* We want to limit the quantity of diagnostics, but we should prioritize the
     ones from the current file to be shown. We first sort by the path,
     prioritizing the ones that match the current file, then take the provided
     maximum number of diagnostics. *)
  |> List.sort ~compare:(fun x y ->
         let is_x_current_path = Path.equal x.location.path current_path in
         let is_y_current_path = Path.equal y.location.path current_path in
         if is_x_current_path && is_y_current_path
         then 0
         else if is_x_current_path
         then -1
         else if is_y_current_path
         then 1
         else Path.compare x.location.path y.location.path)
  |> take_problems
  |> List.group ~break:(fun x y -> not (Path.equal x.location.path y.location.path))
  |> List.map ~f:(fun diags ->
         ( DocumentUri.of_path (List.hd_exn diags).location.path
         , List.map ~f:from_simple_diagnostic diags ))


(** Extract all errors and warnings for the given scopes and collect them in a list. *)
let get_diagnostics : Ligo_interface.defs_and_diagnostics -> simple_diagnostic list =
 fun { errors; warnings; definitions = _ } ->
  let open Option.Let_syntax in
  let mk_diag region message severity =
    let range = Range.of_region region in
    let location = Def.Loc_in_file.{ path = Path.from_absolute region#file; range } in
    Some { message; severity; location }
  in
  let extract_error_information : Main_errors.all -> simple_diagnostic list =
   fun errs ->
    let errs = Main_errors.Formatter.error_json errs in
    List.filter_map
      ~f:(fun ({ content = { message; location; _ }; _ } : Simple_utils.Error.t) ->
        match%bind location with
        | File region -> mk_diag region message DiagnosticSeverity.Error
        | Virtual _ -> None)
      errs
  in
  let extract_warning_information : Main_warnings.all -> simple_diagnostic option =
   fun warn ->
    let ({ content = { message; location; _ }; _ } : Simple_utils.Warning.t) =
      Main_warnings.to_warning warn
    in
    match location with
    | File region -> mk_diag region message DiagnosticSeverity.Warning
    | Virtual _ -> None
  in
  List.concat_map ~f:extract_error_information errors
  @ List.filter_map ~f:extract_warning_information warnings
