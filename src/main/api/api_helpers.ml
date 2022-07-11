open Simple_utils.Display
open Simple_utils

module Trace = Simple_utils.Trace

let toplevel : ?warning_as_error:bool -> display_format:ex_display_format -> displayable -> ('value * 'w, _) result -> _ =
  fun ?(warning_as_error=false) ~display_format disp value ->
    let (Ex_display_format t) = display_format in
    let as_str : string =
      match t with
      | Human_readable -> convert ~display_format:t disp ;
      | Dev -> convert ~display_format:t disp ;
      | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp
    in
    let warns = match value with Ok (_,w) -> w | Error (_,w) -> w in
    let warns = List.map warns ~f:(fun value ->
      match t with
        ( Human_readable | Dev) as s -> convert ~display_format:s (Displayable {value;format=Main_warnings.format})
        | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t (Displayable {value;format=Main_warnings.format})) in
    let warns_str = String.concat ~sep:"\n" warns in
    if not (List.is_empty warns) && warning_as_error then
        Error (warns_str,warns_str)
    else
    match value with
    | Ok _ -> Ok (as_str,warns_str)
    | Error _ -> Error (as_str,warns_str)

let format_result : ?warning_as_error:bool -> display_format:ex_display_format -> 'value format -> (raise:(Main_errors.all,Main_warnings.all) Trace.raise -> 'value) -> _ =
  fun ?(warning_as_error=false) ~display_format value_format value ->
    let format = bind_format value_format Main_errors.Formatter.error_format in
    let res =
        Trace.to_stdlib_result @@ value
    in
    let value = match res with Ok(v,_w) -> Ok(v) | Error (e,_w) -> Error(e) in
    toplevel ~warning_as_error ~display_format (Displayable {value ; format}) res
