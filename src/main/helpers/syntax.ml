module Trace = Simple_utils.Trace
open Trace
open Main_errors
open Syntax_types

let file_extension_to_variant ~raise sf : t option =
  ignore raise;
  match sf with
  | ".ligo" | ".pligo" -> Some PascaLIGO
  | ".mligo" -> Some CameLIGO
  | ".jsligo" -> Some JsLIGO
  | _ -> None


let of_ext_opt = function
  | None -> None
  | Some "ligo" | Some "pligo" -> Some PascaLIGO
  | Some "mligo" -> Some CameLIGO
  | Some "jsligo" -> Some JsLIGO
  | Some _ -> None


let of_string_opt ~raise (Syntax_name syntax) source =
  match syntax, source with
  | "auto", Some sf ->
    let ext = Caml.Filename.extension sf in
    trace_option
      ~raise
      (main_invalid_extension ext)
      (file_extension_to_variant ~raise ext)
  | ("pascaligo" | "PascaLIGO"), _ -> PascaLIGO
  | ("cameligo" | "CameLIGO"), _ -> CameLIGO
  | ("jsligo" | "JsLIGO"), _ -> JsLIGO
  | _ -> raise.error (main_invalid_syntax_name syntax)


let to_string = function
  | PascaLIGO -> "pascaligo"
  | CameLIGO -> "cameligo"
  | JsLIGO -> "jsligo"


let to_ext = function
  | PascaLIGO -> ".ligo"
  | CameLIGO -> ".mligo"
  | JsLIGO -> ".jsligo"
