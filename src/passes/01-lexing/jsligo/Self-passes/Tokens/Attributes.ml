(* Collecting attributes from comments *)

(* Vendor dependencies *)

module Std           = Simple_utils.Std
module Region        = Simple_utils.Region
module Lexbuf        = Simple_utils.Lexbuf
module Config        = Preprocessing_jsligo.Config
module PreprocParams = Preprocessor.CLI.Make (Config)
module Parameters    = LexerLib.CLI.Make (PreprocParams)
module Options       = Parameters.Options
module Lexer         = Lexing_shared.Lexer.Make (Options) (Token)

let scan_comment scan comment =
  let lexbuf = Lexing.from_string comment#payload in
  let ()     = Lexbuf.reset_file comment#region#file lexbuf in
  let line   = comment#region#start#line in
  let ()     = Lexbuf.reset_line line lexbuf
  in scan lexbuf

let collect_attributes tokens =
  let open! Token in
  let rec inner acc = function
    LineCom c :: tokens -> (
      match scan_comment (Lexer.line_comment_attr acc) c with
        Ok acc    -> inner acc tokens
      | Error msg -> Error (acc, msg))

  | BlockCom c :: tokens -> (
      match scan_comment (Lexer.block_comment_attr acc) c with
        Ok acc    -> inner acc tokens
      | Error msg -> Error (acc, msg))

  | token :: tokens -> inner (token :: acc) tokens
  | [] -> Ok (List.rev acc)
  in inner [] tokens

(* Exported *)

type message = string Region.reg

let filter :
      ?print_passes:Std.t ->
      add_warning:(Main_warnings.all -> unit) ->
      Token.t list ->
      (Token.t list, Token.t list * message) result =
  fun ?print_passes ~add_warning:_ tokens -> (* No warning registered *)
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
             "Running JsLIGO token self-pass: \
              Extraction of attributes from comments.")
    | None -> ()
  in collect_attributes tokens
