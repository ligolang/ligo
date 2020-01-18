open Cmdliner
open Trace
open Main.Display

let error_suggest: string = "\n If you're not sure how to fix this error, you can
 do one of the following:

* Visit our documentation: https://ligolang.org/docs/intro/what-and-why/
* Ask a question on our Discord: https://discord.gg/9rhYaEt
* Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
* Check the changelog by running 'ligo changelog'\n"

let toplevel ~(display_format : display_format) (x : string result) : unit Term.ret =
  match x with
  | Ok _ -> Format.printf "%a%!" (formatted_string_result_pp display_format) x;
            `Ok ()
  | Error _ ->
    begin
    match display_format with 
    | `Human_readable -> print_string error_suggest ;
    | _ -> () 
    end ;       
    `Error (false, Format.asprintf "%a%!" (formatted_string_result_pp display_format) x)
