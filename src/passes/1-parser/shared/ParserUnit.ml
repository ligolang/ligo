(* Functor to build a standalone LIGO parser *)

module type S =
  sig
    val ext : string              (* LIGO file extension *)
    val options : EvalOpt.options (* CLI options *)
  end

module type Pretty =
  sig
    type state
    type ast
    val pp_ast :
      state -> ast -> unit
    val mk_state :
      offsets:bool -> mode:[`Point|`Byte] -> buffer:Buffer.t -> state
    val print_tokens : state -> ast -> unit
  end

module Make (IO: S)
            (Lexer: Lexer.S)
            (AST: sig type t type expr end)
            (Parser: ParserAPI.PARSER
                     with type ast   = AST.t
                      and type expr  = AST.expr
                      and type token = Lexer.token)
            (ParErr: sig val message : int -> string end)
            (ParserLog: Pretty with type ast = AST.t) =
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
      | Some file ->  Filename.(file |> basename |> remove_extension)

    let suffix = ".pp" ^ IO.ext

    let pp_input =
      if Utils.String.Set.mem "cpp" IO.options#verbose
      then prefix ^ suffix
      else let pp_input, pp_out = Filename.open_temp_file prefix suffix
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
      if Utils.String.Set.mem "cpp" IO.options#verbose
      then eprintf "%s\n%!" cpp_cmd;
      if Sys.command cpp_cmd <> 0 then
        external_ (sprintf "the command \"%s\" failed." cpp_cmd)

    (* Instanciating the lexer *)

    module ParserFront = ParserAPI.Make (Lexer) (Parser) (ParErr)

    let lexer_inst = Lexer.open_token_stream (Some pp_input)
    let Lexer.{read; buffer; get_win; get_pos; get_last; close} = lexer_inst

    and cout = stdout

    let close_all () = close (); close_out cout

    (* Tokeniser *)

    module Log = LexerLog.Make (Lexer)

    let log = Log.output_token ~offsets:IO.options#offsets
                               IO.options#mode IO.options#cmd cout

    let tokeniser = read ~log

    (* Main *)

    let () =
      try
        let ast =
          if   IO.options#mono
          then ParserFront.mono_contract tokeniser buffer
          else ParserFront.incr_contract lexer_inst in
        if Utils.String.Set.mem "ast" IO.options#verbose
        then let buffer = Buffer.create 131 in
             let state = ParserLog.mk_state
                           ~offsets:IO.options#offsets
                           ~mode:IO.options#mode
                           ~buffer in
             begin
               ParserLog.pp_ast state ast;
               Buffer.output_buffer stdout buffer
             end
        else if Utils.String.Set.mem "ast-tokens" IO.options#verbose
        then let buffer = Buffer.create 131 in
             let state = ParserLog.mk_state
                           ~offsets:IO.options#offsets
                           ~mode:IO.options#mode
                           ~buffer in
             begin
               ParserLog.print_tokens state ast;
               Buffer.output_buffer stdout buffer
             end
      with
        (* Lexing errors *)
        Lexer.Error err ->
        close_all ();
        let msg =
          Lexer.format_error ~offsets:IO.options#offsets
                             IO.options#mode err ~file
        in prerr_string msg

      (* Incremental API of Menhir *)
      | ParserFront.Point point ->
         let () = close_all () in
         let error =
           ParserFront.format_error ~offsets:IO.options#offsets
                                    IO.options#mode point
         in eprintf "\027[31m%s\027[0m%!" error

      (* Monolithic API of Menhir *)
      | Parser.Error ->
         let () = close_all () in
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
         in eprintf "\027[31m%s\027[0m%!" error

      (* I/O errors *)
      | Sys_error msg -> Utils.highlight msg

  end
