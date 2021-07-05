open Simple_utils.Display
open Simple_utils

module Trace = Simple_utils.Trace

let warn_str (display_format:ex_display_format) (a: 'a list) : string =
  let (Ex_display_format t) = display_format in
  match t with
  | Human_readable | Dev as t ->
     Format.asprintf "%a\n" (Simple_utils.PP_helpers.list_sep (Main_errors.Formatter.error_format.pp ~display_format:t) (Simple_utils.PP_helpers.tag "")) a
  | Json -> let json = List.map ~f:Main_errors.Formatter.error_format.to_json a in
            let s = Yojson.Safe.pretty_to_string @@ `List json in
            Format.asprintf "%s\n" s

let toplevel : ?werror:bool -> display_format:ex_display_format -> displayable -> ('value, _) Trace.result -> _ =
  fun ?(werror=false) ~display_format disp value ->
    let (Ex_display_format t) = display_format in
    let as_str : string =
      match t with
      | Human_readable -> convert ~display_format:t disp ;
      | Dev -> convert ~display_format:t disp ;
      | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp
    in
    let warns = Trace.warnings value in
    let warns_str = warn_str display_format warns in
    if not (List.is_empty warns) && werror then
        Error (warns_str,warns_str)
    else
    match Trace.to_stdlib_result value with
    | Ok _ -> Ok (as_str,warns_str)
    | Error _ -> Error (as_str,warns_str)

let format_result : ?werror:bool -> display_format:ex_display_format -> 'value format -> ('value, Main_errors.all) Trace.result -> _ =
  fun ?(werror=false) ~display_format value_format value ->
    let format = bind_format value_format Main_errors.Formatter.error_format in
    toplevel ~werror ~display_format (Displayable {value ; format}) value
