open Trace
open Main.Display

let toplevel ~(display_format : string) (x : string result) =
  let display_format =
    try display_format_of_string display_format
    with _ -> (
        Format.printf "bad display format %s, try looking at DISPLAY_FORMAT in the man (--help)." display_format ;
        failwith "Display format"
      )
  in
  match x with
  | Ok _ -> Format.printf "%a\n%!" (formatted_string_result_pp display_format) x
  | Error _ ->
     Format.eprintf "%a\n%!" (formatted_string_result_pp display_format) x ;
     exit 1
