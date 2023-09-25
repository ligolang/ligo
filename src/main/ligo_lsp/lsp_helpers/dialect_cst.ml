(** We want to write some high-level functions that can take toplevel declarations
    / cst / etc for all dialects,
    this wrapper allow to simplify types of such functions and create some combinators *)
type ('cameligo, 'jsligo) dialect =
  | CameLIGO of 'cameligo
  | JsLIGO of 'jsligo

(** Nice form for functions that take [(...) dialect] *)
type ('cameligo, 'jsligo, 'result) from_dialect =
  { cameligo : 'cameligo -> 'result
  ; jsligo : 'jsligo -> 'result
  }

let from_dialect : ('a, 'b, 'result) from_dialect -> ('a, 'b) dialect -> 'result =
 fun f -> function
  | CameLIGO x -> f.cameligo x
  | JsLIGO x -> f.jsligo x


type t = (Parsing.Cameligo.CST.t, Parsing.Jsligo.CST.t) dialect
type parsing_raise = (Parsing.Errors.t, Main_warnings.all) Simple_utils.Trace.raise

exception Fatal_cst_error of string

module Config = Preprocessing_cameligo.Config
module PreprocParams = Preprocessor.CLI.MakeDefault (Config)
module LexerParams = LexerLib.CLI.MakeDefault (PreprocParams)
module Parameters = ParserLib.CLI.MakeDefault (LexerParams)
module Options = Parameters.Options

let get_cst ~(strict : bool) ~(file : Path.t) (syntax : Syntax_types.t) (code : string)
    : (t, string) result
  =
  let buffer = Caml.Buffer.of_seq (Caml.String.to_seq code) in
  (* Warnings and errors will be reported to the user via diagnostics, so we
     ignore them here unless the strict mode is enabled. *)
  let raise : parsing_raise =
    { error =
        (fun err -> raise @@ Fatal_cst_error (Helpers_pretty.parsing_error_to_string err))
    ; warning = (fun _ -> ())
    ; log_error =
        (fun err ->
          if strict
          then raise @@ Fatal_cst_error (Helpers_pretty.parsing_error_to_string err))
    ; fast_fail = false
    }
  in
  (* FIXME [#1657]: Once we have a project system, set the correct [project_root]. *)
  let project_root = Path.to_string @@ Path.dirname file in
  (* since the Path.t is currently used only by LSP, we need to convert it to string here *)
  let file = Path.to_string file in
  let preprocess = false in
  try
    let open Parsing in
    match syntax with
    | CameLIGO ->
      let module Parse = Cameligo.Make (Options) in
      Ok (CameLIGO (Parse.parse_file ~preprocess ~project_root ~raise buffer file))
    | JsLIGO ->
      let module Parse = Jsligo.Make (Options) in
      Ok (JsLIGO (Parse.parse_file ~preprocess ~project_root ~raise buffer file))
  with
  | Fatal_cst_error err -> Error err
