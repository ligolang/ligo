open Trace

module Parser = Parser_reasonligo.Parser
module AST = Parser_cameligo.AST
module ParserLog = Parser_cameligo.ParserLog
module LexToken = Parser_reasonligo.LexToken
module Lexer = Lexer.Make(LexToken)
module SyntaxError = Parser_reasonligo.SyntaxError

module Errors = struct

  let wrong_function_arguments expr =
    let title () = "wrong function arguments" in
    let message () = "" in
    let expression_loc = AST.expr_to_region expr in
    let data = [
      ("expression_loc",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ expression_loc)
    ] in
    error ~data title message

  let parser_error start end_ = 
    let title () = "parser error" in
    let message () = "" in    
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
  
  let unrecognized_error start end_ = 
    let title () = "unrecognized error" in
    let message () = "" in
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

let parse (parser: 'a parser) lexbuf = 
  let Lexer.{read ; close ; _} = Lexer.open_token_stream None in
  let result = 
    try
      ok (parser read lexbuf)
    with 
      | SyntaxError.Error (WrongFunctionArguments e) -> 
        fail @@ (wrong_function_arguments e)
      | Parser.Error ->
        let start = Lexing.lexeme_start_p lexbuf in
        let end_ = Lexing.lexeme_end_p lexbuf in
        fail @@ (parser_error start end_)
      | _ ->
        let start = Lexing.lexeme_start_p lexbuf in
        let end_ = Lexing.lexeme_end_p lexbuf in
        fail @@ (unrecognized_error start end_)          
  in
  close ();
  result

let parse_file (source: string) : AST.t result =  
  let pp_input =
    let prefix = Filename.(source |> basename |> remove_extension)
    and suffix = ".pp.religo"
    in prefix ^ suffix in

  let cpp_cmd = Printf.sprintf "cpp -traditional-cpp %s > %s"
                               source pp_input in
  let%bind () = sys_command cpp_cmd in

  let%bind channel =
    generic_try (simple_error "error opening file") @@
    (fun () -> open_in pp_input) in
  let lexbuf = Lexing.from_channel channel in
  parse (Parser.contract) lexbuf

let parse_string (s:string) : AST.t result =
  let lexbuf = Lexing.from_string s in
  parse (Parser.contract) lexbuf

let parse_expression (s:string) : AST.expr result =
  let lexbuf = Lexing.from_string s in  
  parse (Parser.interactive_expr) lexbuf
