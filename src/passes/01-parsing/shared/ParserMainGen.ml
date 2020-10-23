(* This module is a wrapper for running the LIGO parsers as standalone
   pieces of software. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* The functor *)

module type PARSER = ParserLib.API.PARSER

module type PRINTER =
  sig
    type tree
    type state

    val mk_state :
      offsets:bool -> mode:[`Point|`Byte] -> buffer:Buffer.t -> state

    val print_tokens : state -> tree -> unit
    val pp_cst       : state -> tree -> unit
  end

module type PRETTY =
  sig
    type tree
    val print : tree -> PPrint.document
  end

module Make (Comments : Comments.S)
            (File     : File.S)
            (Token    : Token.S)
            (CST      : sig type t end)
            (Parser   : PARSER with type token = Token.t
                                and type tree = CST.t)
            (ParErr   : sig val message : int -> string end)
            (Printer  : PRINTER with type tree = CST.t)
            (Pretty   : PRETTY with type tree = CST.t)
            (CLI      : ParserLib.CLI.S) =
  struct
    (* Instantiating the lexer *)

    module Lexer_CLI = CLI.Lexer_CLI

    module MainLexer =
      LexerMainGen.Make (Comments) (File) (Token)
                        (Lexer_CLI : LexerLib.CLI.S)
    (* Other CLIs *)

    module Preproc_CLI = Lexer_CLI.Preproc_CLI

    (* All exits *)

    let print_in_red msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

    let red_exit msg = print_in_red msg; exit 1

    let cli_error msg =
      red_exit (Printf.sprintf "Command-line error: %s\n" msg)

    let print_and_quit msg = print_string msg; flush stdout; exit 0

    (* Checking for errors and valid exits *)

    let check_cli () =
      MainLexer.check_cli ();
      match CLI.status with
        `SyntaxError  msg
      | `FileNotFound msg -> cli_error msg
      | `Help         buf
      | `CLI          buf -> print_and_quit (Buffer.contents buf)
      | `Version      ver -> print_and_quit (ver ^ "\n")
      | `Conflict (o1,o2) ->
           cli_error (Printf.sprintf "Choose either %s or %s." o1 o2)
      | `Done ->
           match Preproc_CLI.extension with
             Some ext when ext <> File.extension ->
               let msg =
                 Printf.sprintf "Expected extension %s." File.extension
               in cli_error msg
      | _ -> ()

    (* Main *)

    module MainParser = ParserLib.API.Make (MainLexer) (Parser)

    let wrap : (Parser.tree, MainParser.message) result -> unit =
      function
        Stdlib.Ok tree ->
          if CLI.pretty then
            let doc = Pretty.print tree in
            let width =
              match Terminal_size.get_columns () with
                None -> 60
              | Some c -> c in
            begin
              PPrint.ToChannel.pretty 1.0 width stdout doc;
              print_newline ()
            end
          else
            let buffer = Buffer.create 231 in
            let state  = Printer.mk_state
                           ~offsets:Preproc_CLI.offsets
                           ~mode:Lexer_CLI.mode
                           ~buffer in
            if CLI.cst then
              begin
                Printer.pp_cst state tree;
                Printf.printf "%s%!" (Buffer.contents buffer)
              end
            else
              if CLI.cst_tokens then
                begin
                  Printer.print_tokens state tree;
                  Printf.printf "%s%!" (Buffer.contents buffer);
                end
              else ();
            flush_all ()
      | Error msg -> (flush_all (); print_in_red msg.Region.value)

    module Preproc = Preprocessor.PreprocMainGen.Make (Preproc_CLI)

    let config =
      object
        method offsets = Preproc_CLI.offsets
        method mode    = Lexer_CLI.mode
      end

    let parse () =
      if Lexer_CLI.preproc then
        match Preproc.preprocess () with
          Stdlib.Error _ -> ()
        | Stdlib.Ok (buffer, _deps) ->
            if Preproc_CLI.show_pp then
              Printf.printf "%s%!" (Buffer.contents buffer)
            else ();
            let string = Buffer.contents buffer in
            let lexbuf = Lexing.from_string string in
            let open MainParser in
            if CLI.mono then
              mono_from_lexbuf lexbuf |> wrap
            else
              incr_from_lexbuf (module ParErr) lexbuf |> wrap
      else
        let open MainParser in
        match Preproc_CLI.input with
          None ->
            if CLI.mono then
              mono_from_channel stdin |> wrap
            else
              incr_from_channel (module ParErr) stdin |> wrap
        | Some file_path ->
            if CLI.mono then
              mono_from_file file_path |> wrap
            else
              incr_from_file (module ParErr) file_path |> wrap
  end
