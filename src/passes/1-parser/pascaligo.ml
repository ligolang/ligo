open Trace

(*module Parser = Parser_pascaligo.Parser*)
(*module ParserLog = Parser_pascaligo.ParserLog*)
module AST = Parser_pascaligo.AST
module ParErr = Parser_pascaligo.ParErr
module LexToken = Parser_pascaligo.LexToken
module Lexer = Lexer.Make(LexToken)
module Scoping = Parser_pascaligo.Scoping
module SSet = Utils.String.Set

(* Mock options. TODO: Plug in cmdliner. *)

let pre_options =
  EvalOpt.make
    ~libs:[]
    ~verbose:SSet.empty
    ~offsets:true
    ~mode:`Point
    ~cmd:EvalOpt.Quiet
    ~mono:true (* Monolithic API of Menhir for now *)
(*  ~input:None *)
(*  ~expr:true  *)

module Parser =
  struct
    type ast  = AST.t
    type expr = AST.expr
    include Parser_pascaligo.Parser
  end

module ParserLog =
  struct
    type ast  = AST.t
    type expr = AST.expr
    include Parser_pascaligo.ParserLog
  end

module PreUnit = ParserUnit.Make (Lexer)(AST)(Parser)(ParErr)(ParserLog)
module Front   = ParserAPI.Make (Lexer)(Parser)(ParErr)

let issue_error point =
  let error = Front.format_error ~offsets:true (* TODO: CLI *)
                                 `Point (* TODO: CLI *) point
  in Stdlib.Error error

module Errors =
  struct
    let reserved_name Region.{value; region} =
      let title () = Printf.sprintf "reserved name \"%s\"" value in
      let message () = "" in
      let data = [
          ("location",
           fun () -> Format.asprintf "%a" Location.pp_lift @@ region)]
      in error ~data title message

    let non_linear_pattern Region.{value; region} =
      let title () =
        Printf.sprintf "repeated variable \"%s\" in this pattern" value in
      let message () = "" in
      let data = [
          ("location",
           fun () -> Format.asprintf "%a" Location.pp_lift @@ region)]
      in error ~data title message

    let duplicate_parameter Region.{value; region} =
      let title () =
        Printf.sprintf "duplicate parameter \"%s\"" value in
      let message () = "" in
      let data = [
          ("location",
           fun () -> Format.asprintf "%a" Location.pp_lift @@ region)]
      in error ~data title message

    let duplicate_variant Region.{value; region} =
      let title () =
        Printf.sprintf "duplicate variant \"%s\" in this\
                        type declaration" value in
      let message () = "" in
      let data = [
          ("location",
           fun () -> Format.asprintf "%a" Location.pp_lift @@ region)]
      in error ~data title message

    let unrecognized_error source (start: Lexing.position)
                           (stop: Lexing.position) lexbuf =
      let title () = "unrecognized error" in
      let file =
        if source = "" then ""
        else
          Format.sprintf "In file \"%s|%s\"" start.pos_fname source in
      let message () =
        Format.sprintf
                  "Parse error at \"%s\" from (%d, %d) to (%d, %d). %s\n"
                  (Lexing.lexeme lexbuf)
                  start.pos_lnum (start.pos_cnum - start.pos_bol)
                  stop.pos_lnum (stop.pos_cnum - stop.pos_bol)
                  file in
      let loc = Region.make ~start:(Pos.from_byte start)
                            ~stop:(Pos.from_byte stop) in
      let data = [
        ("unrecognized_loc",
         fun () -> Format.asprintf "%a" Location.pp_lift @@ loc)]
      in error ~data title message

    let detached_attributes (attrs: AST.attributes) =
      let title () = "detached attributes" in
      let message () = "" in
      let data = [
      ("location",
       fun () -> Format.asprintf "%a" Location.pp_lift @@ attrs.region)]
      in error ~data title message

    let parser_error source (start: Lexing.position)
                     (stop: Lexing.position) lexbuf =
      let title () = "parser error" in
      let file =
        if source = "" then ""
        else
          Format.sprintf "In file \"%s|%s\"" start.pos_fname source in
      let message () =
        Format.sprintf
          "Parse error at \"%s\" from (%d, %d) to (%d, %d). %s\n"
          (Lexing.lexeme lexbuf)
          start.pos_lnum (start.pos_cnum - start.pos_bol)
          stop.pos_lnum (stop.pos_cnum - stop.pos_bol)
          file in
      let loc =
        if start.pos_cnum = -1 then
          Region.make ~start: Pos.min ~stop:(Pos.from_byte stop)
        else
          Region.make ~start:(Pos.from_byte start)
                      ~stop:(Pos.from_byte stop) in
      let data =
        [("parser_loc",
          fun () -> Format.asprintf "%a" Location.pp_lift @@ loc)] in
      error ~data title message

    let lexer_error (e: Lexer.error AST.reg) =
      let title () = "lexer error" in
      let message () = Lexer.error_to_string e.value in
      let data = [
          ("parser_loc",
           fun () -> Format.asprintf "%a" Location.pp_lift @@ e.region)]
      in error ~data title message
end

open Errors

type 'a parser = (Lexing.lexbuf -> LexToken.token) -> Lexing.lexbuf -> 'a

let parse (parser: 'a parser) source lexbuf =
  let Lexer.{read; close; _} = Lexer.open_token_stream None in
  let result =
    try ok (parser read lexbuf) with
      Lexer.Error e ->
        fail @@ lexer_error e
    | Parser.Error ->
        let start = Lexing.lexeme_start_p lexbuf in
        let stop = Lexing.lexeme_end_p lexbuf in
        fail @@ parser_error source start stop lexbuf
    | Scoping.Error (Scoping.Non_linear_pattern var) ->
        fail @@ non_linear_pattern var
    | Scoping.Error (Duplicate_parameter name) ->
        fail @@ duplicate_parameter name
    | Scoping.Error (Duplicate_variant name) ->
        fail @@ duplicate_variant name
    | Scoping.Error (Reserved_name name) ->
        fail @@ reserved_name name
    | Scoping.Error (Detached_attributes attrs) ->
        fail @@ detached_attributes attrs
    | Parser.Error ->
        let start = Lexing.lexeme_start_p lexbuf in
        let end_ = Lexing.lexeme_end_p lexbuf in
        fail @@ (parser_error source start end_ lexbuf)
    | Lexer.Error e ->
        fail @@ lexer_error e
    | _ ->
        let () = Printexc.print_backtrace Pervasives.stdout in
        let start = Lexing.lexeme_start_p lexbuf in
        let stop = Lexing.lexeme_end_p lexbuf in
        fail @@ unrecognized_error source start stop lexbuf
  in close (); result

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

let parse_file' (source: string) : AST.t result =
  let module IO =
    struct
      let ext = "ligo"
      let options = pre_options ~input:(Some source) ~expr:false
    end in
  let module Unit = PreUnit(IO) in
  match Unit.parse Unit.parse_contract with
    Ok ast -> ok ast
  | Error error -> failwith "TODO" (* fail @@ parser_or_lexer_error error *)

let parse_string (s:string) : AST.t result =
  let lexbuf = Lexing.from_string s in
  parse (Parser.contract) "" lexbuf

let parse_expression (s:string) : AST.expr result =
  let lexbuf = Lexing.from_string s in
  parse (Parser.interactive_expr) "" lexbuf
