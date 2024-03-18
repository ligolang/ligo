open Simple_utils.Display
open Simple_utils
module Trace = Simple_utils.Trace

let toplevel
    :  ?warning_as_error:bool -> display_format:ex_display_format -> no_colour:bool
    -> displayable -> ('value * _ * 'w, _) result -> _
  =
 fun ?(warning_as_error = false) ~display_format ~no_colour disp value ->
  let (Ex_display_format t) = display_format in
  let as_str : string =
    match t with
    | Human_readable -> convert ~display_format:t ~no_colour disp
    | Dev -> convert ~display_format:t ~no_colour disp
    | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t ~no_colour disp
  in
  let warns =
    match value with
    | Ok (_, _, w) -> w
    | Error (_, w) -> w
  in
  let warns =
    List.map warns ~f:(fun value ->
        match t with
        | (Human_readable | Dev) as s ->
          convert
            ~display_format:s
            ~no_colour
            (Displayable { value; format = Main_warnings.format })
        | Json ->
          Yojson.Safe.pretty_to_string
          @@ convert
               ~display_format:t
               ~no_colour
               (Displayable { value; format = Main_warnings.format }))
  in
  let warns_str = String.concat ~sep:"\n" warns in
  if (not (List.is_empty warns)) && warning_as_error
  then Error (warns_str ^ as_str, warns_str)
  else (
    match value with
    | Ok _ -> Ok (as_str, warns_str)
    | Error _ -> Error (as_str, warns_str))


let format_result
    :  ?warning_as_error:bool -> display_format:ex_display_format -> no_colour:bool
    -> 'value format -> (raise:(Main_errors.all, Main_warnings.all) Trace.raise -> 'value)
    -> _
  =
 fun ?(warning_as_error = false) ~display_format ~no_colour value_format value ->
  let format = bind_format value_format Main_errors.Formatter.error_format in
  let res = Trace.to_stdlib_result ~fast_fail:Fast_fail value in
  let value =
    match res with
    | Ok (v, (), _w) -> Ok v
    | Error (e, _w) -> Error e
  in
  toplevel
    ~warning_as_error
    ~display_format
    ~no_colour
    (Displayable { value; format })
    res
