open Cmdliner
open Main.Display

let error_suggest: string = "\n
If you're not sure how to fix this error, you can do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/introduction
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'"

let return_good v = `Ok v
let return_bad v brief =
  if brief then `Error (false, Format.asprintf "@[<hv>error@ %s@]" v)
  else `Error (false, Format.asprintf "@[<hv>error@ %s@ %s@]" v error_suggest)

let toplevel : ?output_file:string option -> brief:bool -> display_format:ex_display_format -> displayable -> ('value, Main_errors.Types.all) result -> unit Term.ret =
  fun ?(output_file=None) ~brief ~display_format disp value ->
    let (Ex_display_format t) = display_format in
    let as_str : string =
      match t with
      | Human_readable -> convert ~display_format:t disp ;
      | Dev -> convert ~display_format:t disp ;
      | Json -> Yojson.Safe.to_string @@ convert ~display_format:t disp
    in
    match value with
    | Ok _ ->
      let fmt : Format.formatter = match output_file with
        | Some file_path -> Format.formatter_of_out_channel @@ open_out file_path
        | None -> Format.std_formatter
      in
      return_good @@ (Format.fprintf fmt "%s\n" as_str ; Format.pp_print_flush fmt ())
    | Error _ -> return_bad as_str brief

let return_result : ?output_file:string option -> brief:bool -> display_format:ex_display_format -> 'value format -> ('value, Main_errors.Types.all) result -> unit Term.ret =
  fun ?(output_file=None) ~brief ~display_format value_format value ->
    let format = bind_format value_format Main.Formatter.error_format in
    toplevel ~output_file ~brief ~display_format (Displayable {value ; format}) value
