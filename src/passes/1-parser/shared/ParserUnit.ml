(* Functor to build a standalone LIGO parser *)

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

    (* Error printing and exception tracing *)

    let () = Printexc.record_backtrace true

    let external_ text =
      Utils.highlight (sprintf "External error: %s" text); exit 1

    (* Extracting the input file *)

    let file =
      match IO.options#input with
        None | Some "-" -> false
      |          Some _ -> true

    (* Preprocessing the input source and opening the input channels *)

    (* Path for CPP inclusions (#include) *)

    let lib_path =
      match IO.options#libs with
          [] -> ""
      | libs -> let mk_I dir path = sprintf " -I %s%s" dir path
               in List.fold_right mk_I libs ""

    let prefix =
      match IO.options#input with
        None | Some "-" -> "temp"
      | Some file -> Filename.(file |> basename |> remove_extension)

    let suffix = ".pp" ^ IO.ext

    module SSet = Utils.String.Set

    let pp_input =
      if SSet.mem "cpp" IO.options#verbose
      then prefix ^ suffix
      else let pp_input, pp_out =
             Filename.open_temp_file prefix suffix
           in close_out pp_out; pp_input

    let cpp_cmd =
      match IO.options#input with
        None | Some "-" ->
          sprintf "cpp -traditional-cpp%s - > %s"
                  lib_path pp_input
      | Some file ->
          sprintf "cpp -traditional-cpp%s %s > %s"
                  lib_path file pp_input

    let () =
      if SSet.mem "cpp" IO.options#verbose
      then eprintf "%s\n%!" cpp_cmd;
      if Sys.command cpp_cmd <> 0 then
        external_ (sprintf "the command \"%s\" failed." cpp_cmd)

    (* Instanciating the lexer *)

    module ParserFront = ParserAPI.Make (Lexer)(Parser)(ParErr)

    let format_error = ParserFront.format_error
    let short_error  = ParserFront.short_error

    let lexer_inst = Lexer.open_token_stream (Some pp_input)
    let Lexer.{read; buffer; get_win; close; _} = lexer_inst

    and cout = stdout

    let close_all () = close (); close_out cout

    (* Tokeniser *)

    module Log = LexerLog.Make (Lexer)

    let log = Log.output_token ~offsets:IO.options#offsets
                               IO.options#mode IO.options#cmd cout

    let tokeniser = read ~log

    (* Main *)

    let output = Buffer.create 131
    let state = ParserLog.mk_state
                  ~offsets:IO.options#offsets
                  ~mode:IO.options#mode
                  ~buffer:output

    (* Parsing an expression *)

    let parse_expr () : AST.expr =
      let expr =
        if IO.options#mono then
          ParserFront.mono_expr tokeniser buffer
        else
          ParserFront.incr_expr lexer_inst in
      let () =
        if SSet.mem "ast-tokens" IO.options#verbose
        then begin
               Buffer.clear output;
               ParserLog.print_expr state expr;
               Buffer.output_buffer stdout output
             end in
      let () =
        if SSet.mem "ast" IO.options#verbose
        then begin
               Buffer.clear output;
               ParserLog.pp_expr state expr;
               Buffer.output_buffer stdout output
             end
      in expr (* Or more CLI options handled before *)

    (* Parsing a contract *)

    let parse_contract () : AST.t =
      let ast =
        if IO.options#mono then
          ParserFront.mono_contract tokeniser buffer
        else
          ParserFront.incr_contract lexer_inst in
      let () =
        if SSet.mem "ast" IO.options#verbose
        then begin
               Buffer.clear output;
               ParserLog.pp_ast state ast;
               Buffer.output_buffer stdout output
             end in
      let () =
        if SSet.mem "ast-tokens" IO.options#verbose
        then begin
               Buffer.clear output;
               ParserLog.print_tokens state ast;
               Buffer.output_buffer stdout output
             end
      in ast (* Or more CLI options handled before. *)

    let parse (parser: unit -> 'a) : ('a,string) Stdlib.result =
      try
        let node = parser () in (close_all (); Ok node)
      with
        (* Lexing errors *)

        Lexer.Error err ->
          let error =
            Lexer.format_error ~offsets:IO.options#offsets
                               IO.options#mode err ~file
          in close_all (); Stdlib.Error error

      (* Incremental API of Menhir *)

      | ParserFront.Point point ->
          let error =
            ParserFront.format_error ~offsets:IO.options#offsets
                                     IO.options#mode point
          in close_all (); Stdlib.Error error
      (* Monolithic API of Menhir *)

      | Parser.Error ->
          let invalid, valid_opt =
           match get_win () with
             Lexer.Nil ->
               assert false (* Safe: There is always at least EOF. *)
           | Lexer.One invalid -> invalid, None
           | Lexer.Two (invalid, valid) -> invalid, Some valid in
          let point = "", valid_opt, invalid in
          let error =
            ParserFront.format_error ~offsets:IO.options#offsets
                                     IO.options#mode point
          in close_all (); Stdlib.Error error

      (* I/O errors *)

      | Sys_error error -> Stdlib.Error error

  end
