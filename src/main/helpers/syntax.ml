module Trace = Simple_utils.Trace
open Trace
open Main_errors
open Syntax_types

let file_name_to_variant ~raise sf : t =
  match Filename.split_extension sf with
  | _, Some "mligo" -> CameLIGO
  | _, Some "jsligo" -> JsLIGO
  | _ -> raise.error (main_invalid_extension sf)


let of_ext_opt = function
  | Some "mligo" -> Some CameLIGO
  | Some "jsligo" -> Some JsLIGO
  | _ -> None


let of_string_opt ~raise (Syntax_name syntax) source =
  match syntax, source with
  | "auto", Some sf -> file_name_to_variant ~raise sf
  | ("cameligo" | "CameLIGO"), _ -> CameLIGO
  | ("jsligo" | "JsLIGO"), _ -> JsLIGO
  | _ -> raise.error (main_invalid_syntax_name syntax)


let to_string = function
  | CameLIGO -> "cameligo"
  | JsLIGO -> "jsligo"


let to_ext = function
  | CameLIGO -> ".mligo"
  | JsLIGO -> ".jsligo"


let is_cameligo = Fn.flip Filename.check_suffix ".mligo"
let is_jsligo = Fn.flip Filename.check_suffix ".jsligo"
let is_ligo file_path = is_cameligo file_path || is_jsligo file_path
let cameligo_glob = "**/*.mligo"
let jsligo_glob = "**/*.jsligo"
