open Trace

module Parser = Parser_pascaligo.Parser
module AST = Parser_pascaligo.AST
module ParserLog = Parser_pascaligo.ParserLog
module LexToken = Parser_pascaligo.LexToken
module Lexer = Lexer.Make(LexToken)
module SyntaxError = Parser_pascaligo.SyntaxError

module Errors = struct

  let lexer_error (e: Lexer.error AST.reg) =
    let title () = "lexer error" in
    let message () = Lexer.error_to_string e.value in
    let data = [
      ("parser_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ e.region
      )
    ] in
    error ~data title message

  let reserved_name Region.{value; region} =
    let title () = Printf.sprintf "reserved name \"%s\"" value in
    let message () = "" in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ region)
    ] in
    error ~data title message

  let duplicate_parameter Region.{value; region} =
    let title () = Printf.sprintf "duplicate parameter \"%s\"" value in
    let message () = "" in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ region)
    ] in
    error ~data title message

  let duplicate_variant Region.{value; region} =
    let title () = Printf.sprintf "duplicate variant \"%s\" in this\
                                   type declaration" value in
    let message () = "" in
    let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ region)
    ] in
    error ~data title message

  let parser_error source (start: Lexing.position) (end_: Lexing.position) lexbuf =
    let title () = "parser error" in
    let file = if source = "" then
        ""
      else
        Format.sprintf "In file \"%s|%s\"" start.pos_fname source
    in
    let str = Format.sprintf
              "Parse error at \"%s\" from (%d, %d) to (%d, %d). %s\n"
              (Lexing.lexeme lexbuf)
              start.pos_lnum (start.pos_cnum - start.pos_bol)
              end_.pos_lnum (end_.pos_cnum - end_.pos_bol)
              file
    in
    let message () = str in
    let loc = Region.make
      ~start:(Pos.from_byte start)
      ~stop:(Pos.from_byte end_)
    in
    let data = [
      ("parser_loc",
        fun () -> Format.asprintf "%a" Location.pp_lift @@ loc
      )
    ] in
    error ~data title message

  let unrecognized_error source (start: Lexing.position) (end_: Lexing.position) lexbuf =
    let title () = "unrecognized error" in
    let file = if source = "" then
        ""
      else
        Format.sprintf "In file \"%s|%s\"" start.pos_fname source
    in
    let str = Format.sprintf
              "Parse error at \"%s\" from (%d, %d) to (%d, %d). %s\n"
              (Lexing.lexeme lexbuf)
              start.pos_lnum (start.pos_cnum - start.pos_bol)
              end_.pos_lnum (end_.pos_cnum - end_.pos_bol)
              file
    in
    let message () = str in
    let loc = Region.make
      ~start:(Pos.from_byte start)
      ~stop:(Pos.from_byte end_)
    in
    let data = [
      ("unrecognized_loc",
        fun () -> Format.asprintf "%a" Location.pp_lift @@ loc
      )
    ] in
    error ~data title message

end

open Errors

type 'a parser = (Lexing.lexbuf -> LexToken.token) -> Lexing.lexbuf -> 'a

let parse (parser: 'a parser) source lexbuf =
  let Lexer.{read ; close ; _} = Lexer.open_token_stream None in
  let result =
    try
      ok (parser read lexbuf)
    with
      SyntaxError.Error (Duplicate_parameter name) ->
        fail @@ (duplicate_parameter name)
    | SyntaxError.Error (Duplicate_variant name) ->
        fail @@ (duplicate_variant name)
    | SyntaxError.Error (Reserved_name name) ->
        fail @@ (reserved_name name)
    | Parser.Error ->
        let start = Lexing.lexeme_start_p lexbuf in
        let end_ = Lexing.lexeme_end_p lexbuf in
        fail @@ (parser_error source start end_ lexbuf)
    | Lexer.Error e ->
        fail @@ (lexer_error e)
    | _ ->
        let _ = Printexc.print_backtrace Pervasives.stdout in
        let start = Lexing.lexeme_start_p lexbuf in
        let end_ = Lexing.lexeme_end_p lexbuf in
        fail @@ (unrecognized_error source start end_ lexbuf)
  in
  close ();
  result

let parse_file (source: string) : AST.t result =
  let pp_input =
    let prefix = Filename.(source |> basename |> remove_extension)
    and suffix = ".pp.ligo"
    in prefix ^ suffix in

  let cpp_cmd = Printf.sprintf "cpp -traditional-cpp %s > %s"
                               source pp_input in
  let%bind () = sys_command cpp_cmd in

  let%bind channel =
    generic_try (simple_error "error opening file") @@
    (fun () -> open_in pp_input) in
  let lexbuf = Lexing.from_channel channel in
  parse (Parser.contract) source lexbuf

let parse_string (s:string) : AST.t result =
  let lexbuf = Lexing.from_string s in
  parse (Parser.contract) "" lexbuf

let parse_expression (s:string) : AST.expr result =
  let lexbuf = Lexing.from_string s in
  parse (Parser.interactive_expr) "" lexbuf
