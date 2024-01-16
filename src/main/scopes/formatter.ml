open Simple_utils.Display

let scope_ppformat ~display_format ~no_colour f (d, s) =
  (* The [no_colour] option is provided to all [_ppformat] functions by default,
     but not needed by all of them. Remove the [ignore] if you need it. *)
  let () = ignore no_colour in
  match display_format with
  | Human_readable ->
    Format.fprintf
      f
      "there is to human-readable pretty printer for you, use --format json"
  | Dev -> Format.fprintf f "@[<v>%a@ %a@]" PP.scopes s PP.definitions d


type get_scope_output =
  { errors : Main_errors.all list
  ; warns : Main_warnings.all list
  ; info : (Types.def list * Types.inlined_scopes) option
  }

let error_format = Main_errors.Formatter.error_format
let warn_format = Main_warnings.format

let pp_get_scope_output : get_scope_output pp =
 fun ~display_format ~no_colour f { errors; warns; info } ->
  let () =
    match info with
    | Some info -> scope_ppformat ~display_format ~no_colour f info
    | None -> ()
  in
  let () = if not @@ List.is_empty errors then Format.fprintf f "@[<v>Errors: @,@]" in
  let () =
    List.iter errors ~f:(fun err ->
        error_format.pp ~display_format ~no_colour f err;
        Format.fprintf f "@,")
  in
  let () = if not @@ List.is_empty warns then Format.fprintf f "@[<v>Warnings: @,@]" in
  List.iter warns ~f:(fun warn ->
      warn_format.pp ~display_format ~no_colour f warn;
      Format.fprintf f "@,")


let to_errors list =
  let value = List.map list ~f:Main_errors.Formatter.error_json |> List.concat in
  `List (List.map ~f:Simple_utils.Error.to_yojson value)


let to_warnings list =
  let warnings = List.map list ~f:Main_warnings.to_json in
  `List warnings


let get_scope_output_to_json : get_scope_output -> json =
 fun { errors; warns; info } ->
  let info_json =
    match info with
    | Some (d, s) -> [ "definitions", PP.defs_json d; "scopes", PP.scopes_json s ]
    | None -> []
  in
  let content = [ "errors", to_errors errors; "warnings", to_warnings warns ] in
  `Assoc (content @ info_json)


let get_scope_format : get_scope_output format =
  { pp = pp_get_scope_output; to_json = get_scope_output_to_json }
