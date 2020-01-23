(* Functor to build a standalone LIGO parser *)

module Region = Simple_utils.Region

module type IO =
  sig
    val ext : string              (* LIGO file extension *)
    val options : EvalOpt.options (* CLI options *)
  end

module type Pretty =
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
            (ParserLog: Pretty with type ast  = AST.t
                                and type expr = AST.expr)
            (IO: IO) =
  struct
    open Printf
    module SSet = Utils.String.Set

    (* Log of the lexer *)

    module Log = LexerLog.Make (Lexer)

    let log =
      Log.output_token ~offsets:IO.options#offsets
                       IO.options#mode IO.options#cmd stdout

    (* Error handling (reexported from [ParserAPI]) *)

    type message = string
    type valid   = Parser.token
    type invalid = Parser.token
    type error   = message * valid option * invalid

    (* Instantiating the parser *)

    module Front = ParserAPI.Make (Lexer)(Parser)(ParErr)

    let format_error = Front.format_error

    let short_error ?(offsets=true) mode msg (reg: Region.t) =
      sprintf "Parse error %s:\n%s" (reg#to_string ~offsets mode) msg

    (* Parsing an expression *)

    let parse_expr lexer_inst :
      (AST.expr, message Region.reg) Stdlib.result =
      let output = Buffer.create 131 in
      let state  =
        ParserLog.mk_state ~offsets:IO.options#offsets
                           ~mode:IO.options#mode
                           ~buffer:output in
      let close_all () =
        lexer_inst.Lexer.close (); close_out stdout in
      let expr =
        try
          if IO.options#mono then
            let tokeniser = lexer_inst.Lexer.read ~log
            and lexbuf = lexer_inst.Lexer.buffer
            in Front.mono_expr tokeniser lexbuf
          else
            Front.incr_expr lexer_inst
        with exn -> close_all (); raise exn in
      let () =
        if SSet.mem "ast-tokens" IO.options#verbose then
          begin
            Buffer.clear output;
            ParserLog.print_expr state expr;
            Buffer.output_buffer stdout output
          end in
      let () =
        if SSet.mem "ast" IO.options#verbose then
          begin
            Buffer.clear output;
            ParserLog.pp_expr state expr;
            Buffer.output_buffer stdout output
          end
      in close_all (); Ok expr

    (* Parsing a contract *)

    let parse_contract lexer_inst :
      (AST.t, message Region.reg) Stdlib.result =
      let output = Buffer.create 131 in
      let state  =
        ParserLog.mk_state ~offsets:IO.options#offsets
                           ~mode:IO.options#mode
                           ~buffer:output in
      let close_all () =
        lexer_inst.Lexer.close (); close_out stdout in
      let ast =
        try
          if IO.options#mono then
            let tokeniser = lexer_inst.Lexer.read ~log
            and lexbuf = lexer_inst.Lexer.buffer
            in Front.mono_contract tokeniser lexbuf
          else
            Front.incr_contract lexer_inst
        with exn -> close_all (); raise exn in
      let () =
        if SSet.mem "ast-tokens" IO.options#verbose then
          begin
            Buffer.clear output;
            ParserLog.print_tokens state ast;
            Buffer.output_buffer stdout output
          end in
      let () =
        if SSet.mem "ast" IO.options#verbose then
          begin
            Buffer.clear output;
            ParserLog.pp_ast state ast;
            Buffer.output_buffer stdout output
          end
      in close_all (); Ok ast

    (* Wrapper for the parsers above *)

    type 'a parser = Lexer.instance -> ('a, message Region.reg) result

    let apply lexer_inst parser =
      (* Calling the parser and filtering errors *)

      match parser lexer_inst with
        Stdlib.Error _ as error -> error
      | Stdlib.Ok _ as node -> node

      (* Lexing errors *)

      | exception Lexer.Error err ->
          let file =
            match IO.options#input with
              None | Some "-" -> false
            |          Some _ -> true in
          let error =
            Lexer.format_error ~offsets:IO.options#offsets
                               IO.options#mode err ~file
          in Stdlib.Error error

      (* Incremental API of Menhir *)

      | exception Front.Point point ->
          let error =
            Front.format_error ~offsets:IO.options#offsets
                               IO.options#mode point
          in Stdlib.Error error

      (* Monolithic API of Menhir *)

      | exception Parser.Error ->
          let invalid, valid_opt =
            match lexer_inst.Lexer.get_win () with
              Lexer.Nil ->
                  assert false (* Safe: There is always at least EOF. *)
              | Lexer.One invalid -> invalid, None
              | Lexer.Two (invalid, valid) -> invalid, Some valid in
            let point = "", valid_opt, invalid in
            let error =
              Front.format_error ~offsets:IO.options#offsets
                                 IO.options#mode point
            in Stdlib.Error error

       (* I/O errors *)

       | exception Sys_error error ->
           Stdlib.Error (Region.wrap_ghost error)

  end
