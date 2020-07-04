open Cmdliner
open Main.Display

let return_good v = `Ok v
let return_bad v = `Error (false, Format.asprintf "@[<hv>error@ %s@]" v)

let toplevel : ?output_file:string option -> display_format:ex_display_format -> displayable -> ('value, Main_errors.Types.all) result -> unit Term.ret =
  fun ?(output_file=None) ~display_format disp value ->
    let (Ex_display_format t) = display_format in
    let as_str : string =
      match t with
      | Human_readable -> convert ~display_format:t disp ;
      | Dev -> convert ~display_format:t disp ;
      | Json -> Yojson.to_string @@ convert ~display_format:t disp
    in
    match value with
    | Ok _ ->
      let fmt = match output_file with
        | Some file_path -> Format.formatter_of_out_channel @@ open_out file_path
        | None -> Format.std_formatter
      in
      return_good @@ Format.fprintf fmt "%s\n" as_str
    | Error _ -> return_bad as_str

let return_result : ?output_file:string option -> display_format:ex_display_format -> 'value format -> ('value, Main_errors.Types.all) result -> unit Term.ret =
  fun ?(output_file=None) ~display_format value_format value ->
    let format = bind_format value_format Main.Formatter.error_format in
    toplevel ~output_file ~display_format (Displayable {value ; format}) value