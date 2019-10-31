open Trace
open Main.Display

let toplevel ~(display_format : display_format) (x : string result) =
  match x with
  | Ok _ -> Format.printf "%a%!" (formatted_string_result_pp display_format) x
  | Error _ ->
     Format.eprintf "%a%!" (formatted_string_result_pp display_format) x ;
     exit 1
