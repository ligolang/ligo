open Cmdliner
open Trace
open Main.Display

let toplevel ~(display_format : display_format) (x : string result) : unit Term.ret =
  match x with
  | Ok _ -> Format.printf "%a%!" (formatted_string_result_pp display_format) x;
            `Ok ()
  | Error _ ->
     `Error (false, Format.asprintf "%a%!" (formatted_string_result_pp display_format) x)
