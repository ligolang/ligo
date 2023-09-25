module Trace = Simple_utils.Trace
open Trace
open Main_errors
open Syntax_types

let file_name_to_variant ~raise sf : t =
  let ext = Caml.Filename.extension sf in
  match ext with
  | ".mligo" -> CameLIGO
  | ".jsligo" -> JsLIGO
  | ".ligo" | ".pligo" -> raise.error (main_deprecated_pascaligo_filename sf)
  | _ -> raise.error (main_invalid_extension sf)


(* For some reason, likely a bug in Js_of_ocaml, pattern matching fails
   when matched against ("mligo" | ".mligo") and needs to be explicitly written as
   | "mligo" -> ...
   | ".mligo" -> ...
 *)
let of_ext_opt = function
  | None -> None
  | Some "mligo" -> Some CameLIGO
  | Some ".mligo" -> Some CameLIGO
  | Some "jsligo" -> Some JsLIGO
  | Some ".jsligo" -> Some JsLIGO
  | Some _ -> None


let of_string_opt ~raise (Syntax_name syntax) source =
  match syntax, source with
  | "auto", Some sf -> file_name_to_variant ~raise sf
  | ("cameligo" | "CameLIGO"), _ -> CameLIGO
  | ("jsligo" | "JsLIGO"), _ -> JsLIGO
  | ("pascaligo" | "PascaLIGO"), _ -> raise.error (main_deprecated_pascaligo_syntax ())
  | _ -> raise.error (main_invalid_syntax_name syntax)


let to_string = function
  | CameLIGO -> "cameligo"
  | JsLIGO -> "jsligo"


let to_ext = function
  | CameLIGO -> ".mligo"
  | JsLIGO -> ".jsligo"
