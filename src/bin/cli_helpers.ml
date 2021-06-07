open Cmdliner
open Display

let return_good v = `Ok v
let return_bad v = (
  if v.[String.length v - 1] = '\n' then
    Format.eprintf "%s" v
  else
    Format.eprintf "%s\n" v;
  Format.pp_print_flush Format.err_formatter ();
  `Error (false, "")
  )

let warn_str (display_format:ex_display_format) a : string =
  let (Ex_display_format t) = display_format in
  match t with
  | Human_readable | Dev as t ->
     Format.asprintf "%a\n" (Simple_utils.PP_helpers.list_sep (Main_errors.Formatter.error_format.pp ~display_format:t) (Simple_utils.PP_helpers.tag "")) a
  | Json -> let _json = a |> List.map ~f:Main_errors.Formatter.error_format.to_json in
            let s = Yojson.Safe.pretty_to_string @@ `List _json in
            Format.asprintf "%s\n" s

let toplevel : ?werror:bool -> ?warn:bool -> ?output_file:string option -> display_format:ex_display_format -> displayable -> ('value, _) Trace.result -> unit Term.ret =
  fun ?(werror=false) ?(warn=false) ?(output_file=None) ~display_format disp value ->
    let (Ex_display_format t) = display_format in
    let as_str : string =
      match t with
      | Human_readable -> convert ~display_format:t disp ;
      | Dev -> convert ~display_format:t disp ;
      | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp
    in
    let warns = Trace.warnings value in
    let warns_str = warn_str display_format warns in
    let return_with_warn f =
      if not (List.is_empty warns) && werror then
        return_bad warns_str
      else begin
          if not (List.is_empty warns) && warn then
            begin
              Format.eprintf "%s" warns_str;
              Format.pp_print_flush Format.err_formatter ()
            end;
          f ()
        end in
    match Trace.to_stdlib_result value with
    | Ok _ ->
      let fmt : Format.formatter = match output_file with
        | Some file_path -> Format.formatter_of_out_channel @@ open_out file_path
        | None -> Format.std_formatter in
      return_with_warn (fun () -> return_good @@ (Format.fprintf fmt "%s\n" as_str;
                                                  Format.pp_print_flush fmt ()))
    | Error _ ->
       return_with_warn (fun () -> return_bad as_str)

let return_result : ?werror:bool -> ?warn:bool -> ?output_file:string option -> display_format:ex_display_format -> 'value format -> ('value, Main_errors.all) Trace.result -> unit Term.ret =
  fun ?(werror=false) ?(warn=false) ?(output_file=None) ~display_format value_format value ->
    let format = bind_format value_format Main_errors.Formatter.error_format in
    toplevel ~werror ~warn ~output_file ~display_format (Displayable {value ; format}) value
