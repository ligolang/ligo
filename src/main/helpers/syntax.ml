module Trace = Simple_utils.Trace
open Trace
open Main_errors
open Syntax_types

let file_name_to_variant ~raise ~support_pascaligo sf : t =
  let ext = Caml.Filename.extension sf in
  match ext with
  | ".mligo" -> CameLIGO
  | ".jsligo" -> JsLIGO
  | (".ligo" | ".pligo") when support_pascaligo -> PascaLIGO
  | (".ligo" | ".pligo") when not support_pascaligo ->
    raise.error (main_deprecated_pascaligo_filename sf)
  | _ -> raise.error (main_invalid_extension sf)


let of_ext_opt ~support_pascaligo = function
  | None -> None
  | Some ("mligo" | ".mligo") -> Some CameLIGO
  | Some ("jsligo" | ".jsligo") -> Some JsLIGO
  | Some ("ligo" | ".ligo" | "pligo" | ".pligo") when support_pascaligo -> Some PascaLIGO
  | Some _ -> None


let of_string_opt ~raise ~support_pascaligo (Syntax_name syntax) source =
  match syntax, source with
  | "auto", Some sf -> file_name_to_variant ~support_pascaligo ~raise sf
  | ("cameligo" | "CameLIGO"), _ -> CameLIGO
  | ("jsligo" | "JsLIGO"), _ -> JsLIGO
  | ("pascaligo" | "PascaLIGO"), _ when support_pascaligo -> PascaLIGO
  | ("pascaligo" | "PascaLIGO"), _ when not support_pascaligo ->
    raise.error (main_deprecated_pascaligo_syntax ())
  | _ -> raise.error (main_invalid_syntax_name syntax)


let to_string = function
  | CameLIGO -> "cameligo"
  | JsLIGO -> "jsligo"
  | PascaLIGO -> "pascaligo"


let to_ext = function
  | CameLIGO -> ".mligo"
  | JsLIGO -> ".jsligo"
  | PascaLIGO -> ".ligo"
