(* This module is a wrapper for running the LIGO parsers as standalone
   pieces of software. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module PreprocMainGen = Preprocessor.PreprocMainGen

(* Internal dependencies *)

module type FILE        = Preprocessing_shared.File.S
module type COMMENTS    = Preprocessing_shared.Comments.S
module type MODULES     = Preprocessing_shared.Modules.S
module type TOKEN       = Lexing_shared.Token.S
module type SELF_TOKENS = Lexing_shared.Self_tokens.S
module type PARSER      = ParserLib.API.PARSER

module LexerMainGen = Lexing_shared.LexerMainGen
module Tree         = Cst_shared.Tree

(* The functor *)

(* Error & Warning handling *)
module type Raiser = sig
  val add_warning : Main_warnings.all -> unit
end
module type PRINTER =
  sig
    type tree

    val to_buffer : Tree.state -> tree -> Buffer.t
  end

module type PRETTY =
  sig
    type tree
    val print : tree -> PPrint.document
  end

type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
>

module Make
         (File        : FILE)
         (Comments    : COMMENTS)
         (Modules     : MODULES)
         (Token       : TOKEN)
         (ParErr      : sig val message : int -> string end)
         (Self_tokens : SELF_TOKENS with type token = Token.t)
         (CST         : sig type t end)
         (Parser      : PARSER with type token = Token.t
                                and type tree = CST.t)
         (Print       : PRINTER with type tree = CST.t)
         (Pretty      : PRETTY with type tree = CST.t)
         (CLI         : ParserLib.CLI.S)
         (Raiser      : Raiser) =

  struct
    (* Instantiating the lexer *)

    module Lexer_CLI = CLI.Lexer_CLI

    module MainLexer =
      LexerMainGen.Make (File)
                        (Token)
                        (Lexer_CLI : LexerLib.CLI.S)
                        (Self_tokens)
                        (Raiser)
    (* Other CLIs *)

    module Preprocessor_CLI = Lexer_CLI.Preprocessor_CLI

    (* All exits *)

    let print_in_red msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

    let red_exit msg = print_in_red msg; exit 1

    let cli_error msg =
      red_exit (Printf.sprintf "Command-line error: %s\n" msg)

    let print_and_quit msg =
      print_string msg; Out_channel.flush stdout; exit 0

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
      | `DependsOnOtherOption (o1, o2) ->
           cli_error (Printf.sprintf "Option %s requires option %s" o1 o2)
      | `Done ->
           match Preprocessor_CLI.extension with
             Some ext when String.(<>) ext File.extension ->
               let msg =
                 Printf.sprintf "Expected extension %s." File.extension
               in cli_error msg
           | _ -> ()

    (* Main *)

    module Recovery =
      struct
        let mode                   = CLI.Lexer_CLI.mode
        let error_recovery_tracing = CLI.trace_recovery
        let tracing_output         = CLI.trace_recovery_output
      end

    module MainParser = ParserLib.API.Make (MainLexer) (Parser) (Recovery)

    let show_error_message : MainParser.message -> unit =
      function Region.{value; region} ->
        let reg =
          region#to_string ~file:true
                           ~offsets:Preprocessor_CLI.offsets
                           Lexer_CLI.mode in
        let msg = Printf.sprintf "Parse error %s:\n%s" reg value
        in (Out_channel.flush stdout; print_in_red msg)

    let show_tree (tree : Parser.tree) : unit =
      if CLI.pretty then
        let doc = Pretty.print tree in
        let width =
          match Terminal_size.get_columns () with
            None -> 60
          | Some c -> c in
        begin
            PPrint.ToChannel.pretty 1.0 width stdout doc;
            Out_channel.newline stdout
        end
      else
        let buffer = Buffer.create 231 in
        let state  = Tree.mk_state
                       ~buffer
                       ~offsets:Preprocessor_CLI.offsets
                       Lexer_CLI.mode in
        if CLI.cst then
          let buffer = Print.to_buffer state tree
          in Printf.printf "%s%!" (Buffer.contents buffer)
        else ();
        Out_channel.flush stdout;
        Out_channel.flush stderr

    let wrap =
      function
        Stdlib.Ok tree -> show_tree tree
      | Stdlib.Error message -> show_error_message message; exit 1

    let wrap_recovery result =
      let tree, messages = MainParser.extract_recovery_results (result) in
      List.iter
          ~f:(fun msg -> show_error_message msg; Printf.eprintf "\n")
          (List.rev messages);
      Option.iter ~f:show_tree tree;
      if List.length messages > 0 then
        exit 1

    module Preproc = PreprocMainGen.Make (Preprocessor_CLI)

    let parse () =
      if Lexer_CLI.preprocess then
        match Preproc.preprocess () with
          Stdlib.Error _ -> ()
        | Stdlib.Ok (buffer, _deps) ->
            if Preprocessor_CLI.show_pp then
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
        let stdin_writer () =
          if CLI.mono then
            mono_from_channel In_channel.stdin |> wrap
          else
            if not CLI.recovery then
              incr_from_channel (module ParErr) In_channel.stdin |> wrap
            else
              recov_from_channel (module ParErr) In_channel.stdin
              |> wrap_recovery in
        let file_writer file_path =
          if CLI.mono then
              mono_from_file file_path |> wrap
          else if not CLI.recovery then
              incr_from_file (module ParErr) file_path |> wrap
          else
              recov_from_file (module ParErr) file_path |> wrap_recovery in
        match Preprocessor_CLI.input with
          None           -> stdin_writer ()
        | Some file_path -> file_writer file_path
  end
