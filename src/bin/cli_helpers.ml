open Cmdliner
open Main.Display

let returned_value : (_,_) result -> unit -> unit Term.ret = 
  fun v () -> match v with
  | Ok _ -> `Ok ()
  | Error _ -> `Error (false, "error")

let toplevel : display_format:ex_display_format -> displayable -> (unit -> unit Term.ret) -> unit Term.ret =
  fun ~display_format disp return ->
  let (Ex_display_format t) = display_format in
  let as_str : string =
    match t with
    | Human_readable -> convert ~display_format:t disp ;
    | Dev -> convert ~display_format:t disp ;
    | Json -> Yojson.Basic.to_string @@ convert ~display_format:t disp in
  Format.printf "%s\n" as_str ;
  return ()

let return_result : display_format:ex_display_format -> 'value format -> ('value, Main_errors.Types.all) result -> unit Term.ret =
  fun ~display_format value_format value ->
    let format = Display.bind_format value_format Main.Formatter.error_format in
    toplevel ~display_format (Display.Displayable {value ; format}) (returned_value value)