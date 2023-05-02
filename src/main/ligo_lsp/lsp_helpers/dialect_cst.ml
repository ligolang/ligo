type t =
  | CameLIGO_cst of Parsing.Cameligo.CST.t
  | JsLIGO_cst of Parsing.Jsligo.CST.t
  | PascaLIGO_cst of Parsing.Pascaligo.CST.t

type parsing_raise = (Parsing.Errors.t, Main_warnings.all) Simple_utils.Trace.raise

exception Fatal_cst_error of string

let parsing_error_to_string (err : Parsing.Errors.t) : string =
  let ({ content = { message; _ }; _ } : Simple_utils.Error.t) =
    Parsing.Errors.error_json err
  in
  message


let get_cst ~(strict : bool) (syntax : Syntax_types.t) (code : string)
    : (t, string) result
  =
  let buffer = Caml.Buffer.of_seq (Caml.String.to_seq code) in
  (* Warnings and errors will be reported to the user via diagnostics, so we
     ignore them here unless the strict mode is enabled. *)
  let raise : parsing_raise =
    { error = (fun err -> raise @@ Fatal_cst_error (parsing_error_to_string err))
    ; warning = (fun _ -> ())
    ; log_error =
        (fun err -> if strict then raise @@ Fatal_cst_error (parsing_error_to_string err))
    ; fast_fail = false
    }
  in
  try
    match syntax with
    | CameLIGO ->
      Ok (CameLIGO_cst (Parsing.Cameligo.parse_string ~preprocess:false ~raise buffer))
    | JsLIGO ->
      Ok (JsLIGO_cst (Parsing.Jsligo.parse_string ~preprocess:false ~raise buffer))
    | PascaLIGO ->
      Ok (PascaLIGO_cst (Parsing.Pascaligo.parse_string ~preprocess:false ~raise buffer))
  with
  | Fatal_cst_error err -> Error err
