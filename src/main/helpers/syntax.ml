module Trace = Simple_utils.Trace

open Trace
open Main_errors
open Syntax_types

let dialect_to_variant ~raise dialect =
  match dialect with
  | None -> None
  | Some (Dialect_name dialect) ->
     match dialect with
     | "terse"   -> (Some Terse)
     | "verbose" -> (Some Verbose)
     | _         -> raise.error (main_invalid_dialect_name dialect)

let file_extension_to_variant ~raise ?dialect sf : t option =
  match sf with
  | ".ligo" | ".pligo" ->
    let dialect = dialect_to_variant ~raise dialect in
    Some (PascaLIGO dialect)
  | ".mligo"           -> Some CameLIGO
  | ".religo"          -> Some ReasonLIGO
  | ".jsligo"          -> Some JsLIGO
  | _                  -> None

let of_string_opt ~raise ?dialect (Syntax_name syntax) source =
  match syntax, source with
    "auto", Some sf ->
      let ext = Caml.Filename.extension sf in
      trace_option ~raise (main_invalid_extension ext)
        (file_extension_to_variant ~raise ?dialect ext)
  | ("pascaligo"  | "PascaLIGO"),  _ -> let dialect = dialect_to_variant ~raise dialect in
                                        PascaLIGO dialect
  | ("cameligo"   | "CameLIGO"),   _ -> CameLIGO
  | ("reasonligo" | "ReasonLIGO"), _ -> ReasonLIGO
  | ("jsligo"     | "JsLIGO"),     _ -> JsLIGO
  | _                                -> raise.error (main_invalid_syntax_name syntax)

let to_string = function
    PascaLIGO _ -> "pascaligo"
  | CameLIGO    -> "cameligo"
  | ReasonLIGO  -> "reasonligo"
  | JsLIGO      -> "jsligo"
