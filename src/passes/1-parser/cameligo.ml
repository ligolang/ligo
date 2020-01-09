open Trace

module Parser = Parser_cameligo.Parser
module AST = Parser_cameligo.AST
module ParserLog = Parser_cameligo.ParserLog
module LexToken = Parser_cameligo.LexToken
module Lexer = Lexer.Make(LexToken)

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
    let loc = if start.pos_cnum = -1 then
      Region.make
        ~start: Pos.min
        ~stop:(Pos.from_byte end_)    
    else
      Region.make
        ~start:(Pos.from_byte start)
        ~stop:(Pos.from_byte end_)
    in 
    let data =
      [
        ("parser_loc",
          fun () -> Format.asprintf "%a" Location.pp_lift @@ loc
        )
      ]    
    in
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
    and suffix = ".pp.mligo"
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