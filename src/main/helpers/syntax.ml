module Trace = Simple_utils.Trace

open Trace
open Main_errors
open Syntax_types

let file_extension_to_variant ~raise sf : t option =
  match sf with
  | ".ligo" | ".pligo" -> Some PascaLIGO
  | ".mligo"           -> Some CameLIGO
  | ".religo"          ->
    raise.warning `Deprecated_reasonligo ;
    Some ReasonLIGO
  | ".jsligo"          -> Some JsLIGO
  | _                  -> None

let of_string_opt ~raise (Syntax_name syntax) source =
  match syntax, source with
    "auto", Some sf ->
      let ext = Caml.Filename.extension sf in
      trace_option ~raise (main_invalid_extension ext)
        (file_extension_to_variant ~raise ext)
  | ("pascaligo"  | "PascaLIGO"),  _ -> PascaLIGO
  | ("cameligo"   | "CameLIGO"),   _ -> CameLIGO
  | ("reasonligo" | "ReasonLIGO"), _ ->
    raise.warning `Deprecated_reasonligo ;
    ReasonLIGO
  | ("jsligo"     | "JsLIGO"),     _ -> JsLIGO
  | _                                -> raise.error (main_invalid_syntax_name syntax)

let to_string = function
    PascaLIGO  -> "pascaligo"
  | CameLIGO   -> "cameligo"
  | ReasonLIGO -> "reasonligo"
  | JsLIGO     -> "jsligo"
