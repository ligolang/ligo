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


let to_syntax_type : ('cameligo, 'jsligo) dialect -> Syntax_types.t = function
  | CameLIGO _ -> CameLIGO
  | JsLIGO _ -> JsLIGO


type t = (Parsing.Cameligo.CST.t, Parsing.Jsligo.CST.t) dialect
type parsing_raise = (Parsing.Errors.t, Main_warnings.all) Simple_utils.Trace.raise

exception Fatal_cst_error of string

let get_cst_exn
    ?(preprocess = false)
    ?(project_root : string option)
    ~preprocess_define
    ~(strict : bool)
    ~(file : string)
    (syntax : Syntax_types.t)
    (c_unit : Buffer.t)
    : t
  =
  let parsing_error_to_string (err : Parsing.Errors.t) : string =
    let ({ content = { message; _ }; _ } : Simple_utils.Error.t) =
      Parsing.Errors.error_json err
    in
    message
  in
  (* Warnings and errors will be reported to the user via diagnostics, so we ignore them
     here unless the strict mode is enabled. *)
  let raise : parsing_raise =
    { error = (fun err -> raise @@ Fatal_cst_error (parsing_error_to_string err))
    ; warning = (fun _ -> ())
    ; log_error =
        (fun err -> if strict then raise @@ Fatal_cst_error (parsing_error_to_string err))
    ; fast_fail = false
    }
  in
  match syntax with
  | CameLIGO ->
    (* Make Config with CameLIGO options. *)
    let module Config = Preprocessing_cameligo.Config in
    let module PreprocParams = Preprocessor.CLI.MakeDefault (Config) in
    let module LexerParams = LexerLib.CLI.MakeDefault (PreprocParams) in
    let module Parameters = ParserLib.CLI.MakeDefault (LexerParams) in
    let module Parse = Parsing.Cameligo.Make (Parameters.Options) in
    CameLIGO
      (Parse.parse_file ~preprocess ~preprocess_define ?project_root ~raise c_unit file)
  | JsLIGO ->
    (* Make Config with JsLIGO options. *)
    let module Config = Preprocessing_jsligo.Config in
    let module PreprocParams = Preprocessor.CLI.MakeDefault (Config) in
    let module LexerParams = LexerLib.CLI.MakeDefault (PreprocParams) in
    let module Parameters = ParserLib.CLI.MakeDefault (LexerParams) in
    let module Parse = Parsing.Jsligo.Make (Parameters.Options) in
    JsLIGO
      (Parse.parse_file ~preprocess ~preprocess_define ?project_root ~raise c_unit file)


let get_cst
    ?(preprocess = false)
    ~(preprocess_define : string list)
    ?(project_root : string option)
    ~(strict : bool)
    ~(file : string)
    (syntax : Syntax_types.t)
    (c_unit : Buffer.t)
    : (t, string) result
  =
  try
    Ok
      (get_cst_exn
         ~preprocess
         ~preprocess_define
         ?project_root
         ~strict
         ~file
         syntax
         c_unit)
  with
  | Fatal_cst_error err -> Error err
