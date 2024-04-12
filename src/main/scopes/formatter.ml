open Simple_utils.Display

(** Formats definitions and scopes, for use by [ligo info get-scope] in the CLI.

    @param display_format How to format this. [Human_readable] is unsupported.
    @param no_colour Ignored. The [no_colour] option is provided to all [_ppformat]
    functions by default, but not needed by all of them, including this function. *)
let scope_ppformat
    ~display_format
    ~no_colour
    f
    ({ definitions = { definitions }; program = _; inlined_scopes; lambda_types = _ } :
      Types.t)
  =
  let () = ignore no_colour in
  match display_format with
  | Human_readable ->
    Format.fprintf
      f
      "there is to human-readable pretty printer for you, use --format json"
  | Dev ->
    Format.fprintf
      f
      "@[<v>%a@ %a@]"
      PP.scopes
      (force inlined_scopes)
      PP.definitions
      definitions


(** Result of scopes for formatting by the CLI. *)
type get_scope_output =
  { errors : Main_errors.all list (** Collected errors by scopes. *)
  ; warns : Main_warnings.all list (** Collected warnings by scopes. *)
  ; info : Types.t option
        (** Result of running [Scopes.run]. If a fatal error occurred, returns [None]. *)
  }

let error_format = Main_errors.Formatter.error_format
let warn_format = Main_warnings.format

(** Formats [get_scope_output], displaying errors, warnings, and [get_scope_output], in
    this order. Errors and warnings are sorted and deduped before getting formatted. *)
let pp_get_scope_output : get_scope_output pp =
 fun ~display_format ~no_colour f { errors; warns; info } ->
  let () =
    match info with
    | Some info -> scope_ppformat ~display_format ~no_colour f info
    | None -> ()
  in
  let () = if not @@ List.is_empty errors then Format.fprintf f "@[<v>Errors: @,@]" in
  let () =
    errors
    |> List.dedup_and_sort ~compare:Caml.compare
    |> List.iter ~f:(fun err ->
           error_format.pp ~display_format ~no_colour f err;
           Format.fprintf f "@,")
  in
  let () = if not @@ List.is_empty warns then Format.fprintf f "@[<v>Warnings: @,@]" in
  warns
  |> List.dedup_and_sort ~compare:Caml.compare
  |> List.iter ~f:(fun warn ->
         warn_format.pp ~display_format ~no_colour f warn;
         Format.fprintf f "@,")


(** Format a list of errors to JSON. *)
let to_errors list =
  let value = list |> List.concat_map ~f:Main_errors.Formatter.error_json in
  `List (List.map ~f:Simple_utils.Error.to_yojson value)


(** Format a list of warnings to JSON. *)
let to_warnings list =
  let warnings = list |> List.map ~f:Main_warnings.to_json in
  `List warnings


(** Formats [get_scope_output] to JSON, including errors, warnings, definitions, and
    scopes. *)
let get_scope_output_to_json : get_scope_output -> json =
 fun { errors; warns; info } ->
  let info_json =
    match info with
    | Some
        { definitions = { definitions }; program = _; inlined_scopes; lambda_types = _ }
      ->
      [ "definitions", PP.defs_json definitions
      ; "scopes", PP.scopes_json (force inlined_scopes)
      ]
    | None -> []
  in
  let content = [ "errors", to_errors errors; "warnings", to_warnings warns ] in
  `Assoc (content @ info_json)


(** [Dev] and [Json] formatters for [get_scope_output]. *)
let get_scope_format : get_scope_output format =
  { pp = pp_get_scope_output; to_json = get_scope_output_to_json }
