(* Functor to build a LIGO parser *)

module Region  = Simple_utils.Region
module Preproc = Preprocessor.Preproc
module SSet    = Set.Make (String)

module type SubIO =
  sig
    type options = <
      libs    : string list;
      verbose : SSet.t;
      offsets : bool;
      block   : EvalOpt.block_comment option;
      line    : EvalOpt.line_comment option;
      ext     : string;
      mode    : [`Byte | `Point];
      cmd     : EvalOpt.command;
      mono    : bool
    >

    val options : options
    val make : input:string option -> expr:bool -> EvalOpt.options
  end

module type Printer =
  sig
    type state
    type ast
    type expr

    val mk_state :
      offsets:bool -> mode:[`Point|`Byte] -> buffer:Buffer.t -> state

    val pp_ast       : state -> ast -> unit
    val pp_expr      : state -> expr -> unit
    val print_tokens : state -> ast -> unit
    val print_expr   : state -> expr -> unit
  end

module Make (Lexer: Lexer.S)
            (AST: sig type t type expr end)
            (Parser: ParserAPI.PARSER
                     with type ast   = AST.t
                      and type expr  = AST.expr
                      and type token = Lexer.token)
            (ParErr: sig val message : int -> string end)
            (ParserLog: Printer with type ast  = AST.t
                                and type expr = AST.expr)
            (SubIO: SubIO) =
  struct
    open Printf
    module SSet = Set.Make (String)

    (* Log of the lexer *)

    module Log = LexerLog.Make (Lexer)

    let log =
      Log.output_token ~offsets:SubIO.options#offsets
                       SubIO.options#mode SubIO.options#cmd stdout

    (* Error handling (reexported from [ParserAPI]) *)

    type message = string
    type valid   = Parser.token
    type invalid = Parser.token
    type error   = message * valid option * invalid

    (* Instantiating the parser *)

    module API_IO =
      struct
        let options = (SubIO.options :> ParserAPI.options)
      end

    module Front = ParserAPI.Make (API_IO)(Lexer)(Parser)(ParErr)

    let format_error = Front.format_error

    let short_error ?(offsets=true) mode msg (reg: Region.t) =
      sprintf "Parse error %s:\n%s" (reg#to_string ~offsets mode) msg

    (* Parsing an expression *)

    let parse_expr lexer_inst :
      (AST.expr, message Region.reg) Stdlib.result =
      let output = Buffer.create 131 in
      let state  =
        ParserLog.mk_state ~offsets:SubIO.options#offsets
                           ~mode:SubIO.options#mode
                           ~buffer:output in
      let close () = lexer_inst.LexerLib.close () in
      let expr =
        try
          if SubIO.options#mono then
            let tokeniser = lexer_inst.LexerLib.read ~log
            and lexbuf = lexer_inst.LexerLib.buffer
            in Front.mono_expr tokeniser lexbuf
          else
            Front.incr_expr lexer_inst
        with exn -> close (); raise exn in
      let () =
        if SSet.mem "ast-tokens" SubIO.options#verbose then
          begin
            Buffer.clear output;
            ParserLog.print_expr state expr;
            Buffer.output_buffer stdout output
          end in
      let () =
        if SSet.mem "ast" SubIO.options#verbose then
          begin
            Buffer.clear output;
            ParserLog.pp_expr state expr;
            Buffer.output_buffer stdout output
          end
      in flush_all (); close (); Ok expr

    (* Parsing a contract *)

    let parse_contract lexer_inst :
      (AST.t, message Region.reg) Stdlib.result =
      let output = Buffer.create 131 in
      let state  =
        ParserLog.mk_state ~offsets:SubIO.options#offsets
                           ~mode:SubIO.options#mode
                           ~buffer:output in
      let close () = lexer_inst.LexerLib.close () in
      let ast =
        try
          if SubIO.options#mono then
            let tokeniser = lexer_inst.LexerLib.read ~log
            and lexbuf = lexer_inst.LexerLib.buffer
            in Front.mono_contract tokeniser lexbuf
          else
            Front.incr_contract lexer_inst
        with exn -> close (); raise exn in
      let () =
        if SSet.mem "ast-tokens" SubIO.options#verbose then
          begin
            Buffer.clear output;
            ParserLog.print_tokens state ast;
            Buffer.output_buffer stdout output
          end in
      let () =
        if SSet.mem "ast" SubIO.options#verbose then
          begin
            Buffer.clear output;
            ParserLog.pp_ast state ast;
            Buffer.output_buffer stdout output
          end
      in flush_all (); close (); Ok ast

    (* Wrapper for the parsers above *)

    let apply lexer_inst parser =
      (* Calling the parser and filtering errors *)

      match parser lexer_inst with
        Stdlib.Error _ as error -> error
      | Stdlib.Ok _ as node -> node

      (* Lexing errors *)

      | exception Lexer.Error err ->
          let file =
            lexer_inst.LexerLib.buffer.Lexing.lex_curr_p.Lexing.pos_fname in
          let error = Lexer.format_error
                        ~offsets:SubIO.options#offsets
                        SubIO.options#mode err ~file:(file <> "")
          in Stdlib.Error error

      | exception Lexer.Token.Error err ->
          let file =
            lexer_inst.LexerLib.buffer.Lexing.lex_curr_p.Lexing.pos_fname in
          let error = Lexer.Token.format_error
                        ~offsets:SubIO.options#offsets
                        SubIO.options#mode err ~file:(file <> "")
          in Stdlib.Error error

      (* Incremental API of Menhir *)

      | exception Front.Point point ->
          let error =
            Front.format_error ~offsets:SubIO.options#offsets
                               SubIO.options#mode point
          in Stdlib.Error error

      (* Monolithic API of Menhir *)

      | exception Parser.Error ->
          let invalid, valid_opt =
            match lexer_inst.LexerLib.get_win () with
              LexerLib.Nil ->
                  assert false (* Safe: There is always at least EOF. *)
            | LexerLib.One invalid -> invalid, None
            | LexerLib.Two (invalid, valid) -> invalid, Some valid in
            let point = "", valid_opt, invalid in
            let error =
              Front.format_error ~offsets:SubIO.options#offsets
                                 SubIO.options#mode point
            in Stdlib.Error error

       (* I/O errors *)

       | exception Sys_error error ->
           flush_all (); Stdlib.Error (Region.wrap_ghost error)

    (* Preprocessing the input source *)

    let preproc options lexbuf =
      Preproc.lex (options :> Preprocessor.EvalOpt.options) lexbuf

    (* Parsing a contract *)

    let gen_parser options input parser =
      match LexerLib.lexbuf_from_input input with
        Stdlib.Error (LexerLib.File_opening msg) ->
          Stdlib.Error (Region.wrap_ghost msg)
      | Ok (lexbuf, close) ->
         (* Preprocessing the input source *)
         let file = Lexing.(lexbuf.lex_curr_p.pos_fname) in
         match preproc options lexbuf with
            Stdlib.Error (pp_buffer, err) ->
              if SSet.mem "preproc" options#verbose then
                Printf.printf "%s\n%!" (Buffer.contents pp_buffer);
              let formatted =
                Preproc.format ~offsets:options#offsets
                               ~file:(file <> "")
                               err
              in close (); Stdlib.Error formatted
          | Stdlib.Ok buffer ->
             (* Lexing and parsing the preprocessed input source *)

             let () = close () in
             let input' = LexerLib.String (Buffer.contents buffer) in
             match LexerLib.open_token_stream
                     ~scan:Lexer.scan
                     ~token_to_region:Lexer.Token.to_region
                     ~style:Lexer.Token.check_right_context
                     ?line:options#line
                     ?block:options#block
                     input'
             with
               Ok instance ->
                 let open Lexing in
                 instance.LexerLib.buffer.lex_curr_p <-
                   {instance.LexerLib.buffer.lex_curr_p with pos_fname=file};
                 apply instance parser
             | Stdlib.Error (LexerLib.File_opening msg) ->
                 Stdlib.Error (Region.wrap_ghost msg)

    (* Parsing a contract in a file *)

    let contract_in_file (source : string) =
      let options = SubIO.make ~input:(Some source) ~expr:false
      in gen_parser options (LexerLib.File source) parse_contract

    (* Parsing a contract in a string *)

    let contract_in_string (source : string) =
      let options = SubIO.make ~input:None ~expr:false in
      gen_parser options (LexerLib.String source) parse_contract

    (* Parsing a contract in stdin *)

    let contract_in_stdin () =
      let options = SubIO.make ~input:None ~expr:false in
      gen_parser options (LexerLib.Channel stdin) parse_contract

    (* Parsing an expression in a string *)

    let expr_in_string (source : string) =
      let options = SubIO.make ~input:None ~expr:true in
      gen_parser options (LexerLib.String source) parse_expr

    (* Parsing an expression in stdin *)

    let expr_in_stdin () =
      let options = SubIO.make ~input:None ~expr:true in
      gen_parser options (LexerLib.Channel stdin) parse_expr

    (* Preprocess only *)

    let preprocess (source : string) =
      let options = SubIO.make ~input:(Some source) ~expr:false in
      try
        let cin     = open_in source in
        let lexbuf  = Lexing.from_channel cin in
        let () =
          let open Lexing in
          lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname=source}
        and options = (options :> Preprocessor.EvalOpt.options) in
        match Preprocessor.Preproc.lex options lexbuf with
          Stdlib.Ok _ as ok  -> ok
        | Error (_, err) ->
            let formatted =
              Preproc.format ~offsets:options#offsets
                             ~file:true
                             err
            in close_in cin; Stdlib.Error formatted
      with Sys_error error ->
             flush_all (); Stdlib.Error (Region.wrap_ghost error)

  end
