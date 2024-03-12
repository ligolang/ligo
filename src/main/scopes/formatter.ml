open Simple_utils.Display

let scope_ppformat
    ~display_format
    ~no_colour
    f
    ({ definitions = { definitions }
     ; program = _
     ; subst
     ; inlined_scopes
     ; lambda_types = _
     } :
      Types.t)
  =
  (* The [no_colour] option is provided to all [_ppformat] functions by default,
     but not needed by all of them. Remove the [ignore] if you need it. *)
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


type get_scope_output =
  { errors : Main_errors.all list
  ; warns : Main_warnings.all list
  ; info : Types.t option
  }

let format_with_subst formatter subst =
  { formatter with
    pp =
      (fun ~display_format ~no_colour ppf e ->
        (* HACK: We want to run a substitution, but it happens at string level, so we
           format it to string, run the substitution, and then output it to the formatter.
         *)
        let msg = Format.asprintf "%a" (formatter.pp ~display_format ~no_colour) e in
        Format.fprintf ppf "%s"
        @@ Option.value_map ~default:Fn.id ~f:Types.Subst.replace_string subst msg)
  }


let error_format = format_with_subst Main_errors.Formatter.error_format
let warn_format = format_with_subst Main_warnings.format

let pp_get_scope_output : get_scope_output pp =
 fun ~display_format ~no_colour f { errors; warns; info } ->
  let () =
    match info with
    | Some info -> scope_ppformat ~display_format ~no_colour f info
    | None -> ()
  in
  let () = if not @@ List.is_empty errors then Format.fprintf f "@[<v>Errors: @,@]" in
  let subst_opt = Option.map ~f:(fun info -> info.subst) info in
  let () =
    errors
    |> List.dedup_and_sort ~compare:Caml.compare
    |> List.iter ~f:(fun err ->
           (error_format subst_opt).pp ~display_format ~no_colour f err;
           Format.fprintf f "@,")
  in
  let () = if not @@ List.is_empty warns then Format.fprintf f "@[<v>Warnings: @,@]" in
  warns
  |> List.dedup_and_sort ~compare:Caml.compare
  |> List.iter ~f:(fun warn ->
         (warn_format subst_opt).pp ~display_format ~no_colour f warn;
         Format.fprintf f "@,")


let rec map_error_message ~f error =
  { error with
    Simple_utils.Error.content =
      { error.Simple_utils.Error.content with
        message = f error.content.message
      ; children = Option.map ~f:(map_error_message ~f) error.content.children
      }
  }


let to_errors subst list =
  let value =
    list
    |> List.concat_map ~f:Main_errors.Formatter.error_json
    |> List.map
         ~f:
           (map_error_message
              ~f:(Option.value_map ~default:Fn.id ~f:Types.Subst.replace_string subst))
    |> List.dedup_and_sort ~compare:Simple_utils.Error.compare
  in
  `List (List.map ~f:Simple_utils.Error.to_yojson value)


let to_warnings subst list =
  (* TODO: apply subst like in [to_errors] *)
  ignore subst;
  let warnings =
    list |> List.map ~f:Main_warnings.to_json |> List.dedup_and_sort ~compare:Caml.compare
  in
  `List warnings


let get_scope_output_to_json : get_scope_output -> json =
 fun { errors; warns; info } ->
  let info_json =
    match info with
    | Some
        { definitions = { definitions }
        ; program = _
        ; inlined_scopes
        ; subst = _
        ; lambda_types = _
        } ->
      [ "definitions", PP.defs_json definitions
      ; "scopes", PP.scopes_json (force inlined_scopes)
      ]
    | None -> []
  in
  let subst_opt = Option.map ~f:(fun info -> info.subst) info in
  let content =
    [ "errors", to_errors subst_opt errors; "warnings", to_warnings subst_opt warns ]
  in
  `Assoc (content @ info_json)


let get_scope_format : get_scope_output format =
  { pp = pp_get_scope_output; to_json = get_scope_output_to_json }
