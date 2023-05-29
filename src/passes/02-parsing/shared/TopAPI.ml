(* This module is a wrapper for running the LIGO parsers as standalone
   pieces of software. *)

(* Vendor dependencies *)

module Region          = Simple_utils.Region
module Utils           = Simple_utils.Utils
module Std             = Simple_utils.Std
module Lexbuf          = Simple_utils.Lexbuf
module Unit            = LexerLib.Unit
module type LEXER      = ParserLib.LowAPI.LEXER
module type PARSER     = ParserLib.LowAPI.PARSER
module type PARAMETERS = ParserLib.CLI.PARAMETERS

(* Internal dependencies *)

module Token       = Lexing_shared.Token
module LexerAPI    = Lexing_shared.TopAPI
module type PASSES = Lexing_shared.Pipeline.PASSES

(* Utilities *)

let sprintf = Printf.sprintf

(* The functor *)

module type PRINTER =
  sig
    type tree
    type state

    val mk_state :
      ?buffer:Buffer.t -> offsets:bool -> [`Point | `Byte] -> state

    type ('src, 'dst) printer = state -> 'src -> 'dst

    val to_buffer : (tree, Buffer.t) printer
    val to_string : (tree, string) printer
  end

module type PRETTY =
  sig
    type state
    val default_state : state

    type tree
    val print : state -> tree -> PPrint.document
  end

module type WARNING =
  sig
    val add : Main_warnings.all -> unit
  end

module Make
         (Lexer       : LexerAPI.S)
         (Parameters  : PARAMETERS)
         (ParErr      : sig val message : int -> string end)
         (Warning     : WARNING)
         (UnitPasses  : PASSES with type item = Lexer.Token.t Unit.t)
         (TokenPasses : PASSES with type item = Lexer.Token.t)
         (CST         : sig type t end)
         (Parser      : PARSER with type token = Lexer.Token.t
                                and type tree = CST.t)
         (Print       : PRINTER with type tree = CST.t)
         (Pretty      : PRETTY with type tree = CST.t) =
  struct
    (* CLI *)

    module Options = Parameters.Options

    (* Checking for errors and valid exits *)

    type cli_status =
      Ok
    | Info  of string
    | Error of string

    let cli_error = Std.redden

    let check_cli () : cli_status =
      match Parameters.Status.status with
        `SyntaxError  msg
      | `WrongFileExt msg
      | `FileNotFound msg -> Error (cli_error msg)
      | `Help         buf
      | `CLI          buf -> Info (Buffer.contents buf)
      | `Version      ver -> Info (ver ^ "\n")
      | `Done             -> Ok
      | `Conflict (o1,o2) ->
           let msg = sprintf "Choose either %s or %s." o1 o2
           in Error (cli_error msg)
      | `DependsOn (o1, o2) ->
           let msg = sprintf "Option %s requires option %s" o1 o2
           in Error (cli_error msg)

    (* INSTANTIATING THE LEXER *)

    (* The specialised lexer for [ParserLib.LowAPI.Make] *)

    module Lexer4LowAPI =
      struct
        include Lexer

        let scan_token ~no_colour lexbuf =
          match scan_token ~no_colour lexbuf with
            Ok _ as ok -> ok
          | Error {message; _} -> Error message
      end

    (* INSTANTIATING THE PARSER *)

    module Debug =
      struct
        let mode           = `Point
        let trace_recovery = Options.trace_recovery
      end

    module MainParser =
      ParserLib.LowAPI.Make (Lexer4LowAPI) (Parser) (Debug)

    (* PARSING ERRORS *)

    type message = string Region.reg

    type single_error = {
      preprocessed : Buffer.t option;
      used_tokens  : Lexer.Token.t list;
      message      : message
    }

    type error =
      Single   of single_error
    | Multiple of message Utils.nseq

    (* Committing tokens to [std] *)

    let commit tokens (std : Std.t) : unit =
      let f () token =
        Lexer.Token.to_string ~offsets:Options.offsets `Point token
        |> Std.(add_line std.out)
      in List.fold_left ~f ~init:() tokens; Std.(add_nl std.out)

    (* Exit printing *)

    let finalise tree (std : Std.t) : unit =
      if Options.pretty then
        (* Printing the syntax tree to source code *)
        let doc = Pretty.(print default_state) tree in
        let width =
          match Terminal_size.get_columns () with
            None -> 60
          | Some c -> c in
        let buffer = Buffer.create 2000 in
        let () =
          PPrint.ToBuffer.pretty 1.0 width buffer doc in
        Std.(add_line std.out @@ Buffer.contents buffer)
      else
        if Options.cst then
          (* Printing the syntax tree as ASCII *)
          let buffer = Buffer.create 2000 in
          let state  = Print.mk_state
                         ~buffer
                         ~offsets:Options.offsets
                         Options.mode in
          let string = Print.to_string state tree
          in Std.(add_line std.out string)

    (* Formatting multiple errors *)

    let format_errors ~no_colour std errors : unit =
      let print Region.{value; region} =
        let contents = MainParser.format_error ~no_colour ~file:true value region
        in Std.(add_line std.err contents.Region.value)
      in List.iter ~f:print @@ List.rev errors
    (* Converting results from ParserLib and handling CLI options *)

    let mk_single ?(preprocessed : Buffer.t option) (std : Std.t)
        : (Parser.tree, MainParser.pass_error) result ->
          (Parser.tree * message list, error) result =
      function
        Error (Parsing {used_tokens; message}) ->
          let () = if Options.used_tokens then commit used_tokens std in
          let () = Std.(add_line std.err message.Region.value) in
          Error (Single {preprocessed; used_tokens; message})

      | Error (Lexing {used_tokens; message}) ->
          let () = if Options.used_tokens then commit used_tokens std in
          Error (Single {preprocessed; used_tokens; message})

      | Error (System {used_tokens; message}) ->
          let () = Std.(add_line std.err message.Region.value) in
          Error (Single {preprocessed; used_tokens; message})

      | Ok tree -> (* No syntax error *)
          finalise tree std; Ok (tree, [])

    let mk_multiple ~no_colour (std : Std.t)
        : (Parser.tree * message list, message Utils.nseq) result ->
          (Parser.tree * message list, error) result =
      function
        Ok (tree, []) as ok -> finalise tree std; ok

      | Ok (tree, (first_msg::others as msg)) ->
          (* Syntax errors. We drop the repaired syntax tree [_]. *)
          finalise tree std;
          format_errors ~no_colour std msg;
          Error (Multiple (first_msg, others))

      | Error (first_msg, others as msg) -> (* Non-syntactical errors *)
          format_errors ~no_colour std (first_msg::others);
          Error (Multiple msg)

    (* The type of the parser (which scans its tokens into a tree of
       type [parser.tree]) *)

    type parser =
      Lexbuf.input -> Std.t * (Parser.tree * message list, error) result

    (* Putting preprocessor, lexer and parser together *)

    let parse : no_colour:bool -> parser =
      fun ~no_colour ->
      fun (input : Lexbuf.input) ->
          let from_stdin () =
            let std   = Std.empty
            and stdin = In_channel.stdin in
            let result =
              if Options.mono then
                MainParser.mono_from_channel ~no_colour stdin |> mk_single std
              else
                if Options.recovery then
                  MainParser.recov_from_channel ~no_colour (module ParErr) stdin
                  |> mk_multiple ~no_colour std
                else
                  MainParser.incr_from_channel ~no_colour (module ParErr) stdin
                  |> mk_single std
            in std, result in

          let from_file file =
            let std = Std.empty in
            let result =
              if Options.mono then
                MainParser.mono_from_file ~no_colour file |> mk_single std
              else
                if Options.recovery then
                  MainParser.recov_from_file ~no_colour (module ParErr) file
                  |> mk_multiple ~no_colour std
                else
                  MainParser.incr_from_file ~no_colour (module ParErr) file
                  |> mk_single std
            in std, result in

          let file_path = Lexbuf.file_from_input input in

          if String.(file_path = "")
          then from_stdin ()
          else from_file file_path
  end
