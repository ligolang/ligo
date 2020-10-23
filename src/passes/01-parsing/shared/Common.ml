(* CONFIGURATION *)

type file_path = string

module type FILE =
  sig
    val input     : file_path option
    val extension : string (* No option here *)
    val dirs      : file_path list
  end

module Config (File : FILE) (Comments : Comments.S) =
  struct
    (* Stubs for the libraries CLIs *)

    module Preproc_CLI : Preprocessor.CLI.S =
      struct
        include Comments

        let input     = File.input
        let extension = Some File.extension
        let dirs      = File.dirs
        let show_pp   = false
        let offsets   = true

        type status = [
          `Done
        | `Version      of string
        | `Help         of Buffer.t
        | `CLI          of Buffer.t
        | `SyntaxError  of string
        | `FileNotFound of string
        ]

        let status = `Done
      end

    module Lexer_CLI : LexerLib.CLI.S =
      struct
        module Preproc_CLI = Preproc_CLI

        let preproc = true
        let mode    = `Point
        let command = None

        type status = [
          Preproc_CLI.status
        | `Conflict of string * string (* Two conflicting options *)
        ]

        let status = `Done
      end

    (* Configurations for the parsers based on the
       librairies CLIs. *)

    let parser =
      object
        method offsets = Preproc_CLI.offsets
        method mode    = Lexer_CLI.mode
      end
  end

(* PARSING *)

module type PRETTY =
  sig
    type cst
    type expr
    type type_expr
    type pattern

    val print           : cst       -> PPrint.document
    val print_expr      : expr      -> PPrint.document
    val print_type_expr : type_expr -> PPrint.document
    val print_pattern   : pattern   -> PPrint.document
  end

module type CST =
  sig
    type t
    type expr
    type type_expr
    type pattern
  end

module type PAR_ERR =
  sig
    val message : int -> string
  end

type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
>

module MakeParser
         (File     : File.S)
         (Comments : Comments.S)
         (Token    : Token.S)
         (Scoping  : sig exception Error of string * Token.t window end)
         (ParErr   : PAR_ERR)
         (Parser   : ParserLib.API.PARSER with type token = Token.t) =
  struct
    (* PARSING *)

    (* Parsing from a file *)

    let parse_file dirs buffer file_path =
      let module File =
        struct
          let input     = Some file_path
          let extension = File.extension
          let dirs      = dirs
        end in
      let module Config = Config (File) (Comments) in
      let module MainLexer =
        LexerMainGen.Make (Comments) (File) (Token) (Config.Lexer_CLI) in
      let module MainParser =
        ParserLib.API.Make (MainLexer) (Parser) in
      let tree =
        let string = Buffer.contents buffer in
        if Config.Preproc_CLI.show_pp then
          Printf.printf "%s\n%!" string;
        let lexbuf = Lexing.from_string string in
        let     () = LexerLib.Core.reset ~file:file_path lexbuf in
        let parser = MainParser.incr_from_lexbuf in
        try Ok (fun () -> parser (module ParErr: PAR_ERR) lexbuf) with
          Scoping.Error (value, window) ->
            let token  = window#current_token in
            let region = Token.to_region token
            in Stdlib.Error ({value;region} : _ Simple_utils.Region.reg)
      in MainLexer.clear (); tree

    (* Parsing from a string to merge*)

    let parse_string dirs buffer =
      let module File =
        struct
          let input     = None
          let extension = File.extension
          let dirs      = dirs
        end in
      let module Config = Config (File) (Comments) in
      let module MainLexer =
        LexerMainGen.Make (Comments) (File) (Token) (Config.Lexer_CLI) in
      let module MainParser =
        ParserLib.API.Make (MainLexer) (Parser) in
      let tree =
        let string = Buffer.contents buffer in
        if Config.Preproc_CLI.show_pp then
          Printf.printf "%s\n%!" string;
        let lexbuf = Lexing.from_string string in
        let parser = MainParser.incr_from_lexbuf in
        try Ok (fun () -> parser (module ParErr: PAR_ERR) lexbuf) with
          Scoping.Error (value, window) ->
            let token  = window#current_token in
            let region = Token.to_region token
            in Stdlib.Error ({value;region} : _ Simple_utils.Region.reg)
      in MainLexer.clear (); tree
  end

(* PRETTY-PRINTING *)

module MakePretty (CST    : CST)
                  (Pretty : PRETTY
                            with type cst       = CST.t
                            and  type expr      = CST.expr
                            and  type type_expr = CST.type_expr
                            and  type pattern   = CST.pattern) =
  struct
    (* Pretty-print a contract from its CST *)

    let set () =
      let buffer = Buffer.create 131
      and width  =
        match Terminal_size.get_columns () with
          None -> 60
        | Some c -> c
      in width, buffer

    let print_cst cst =
      let width, buffer = set () in
      let doc = Pretty.print cst in
      let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
      in buffer

    (* Pretty-print an expression from its CST *)

    let print_expr expr =
      let width, buffer = set () in
      let doc = Pretty.print_expr expr in
      let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
      in buffer

    (* Pretty-print a pattern from its CST *)

    let print_pattern pattern =
      let width, buffer = set () in
      let doc = Pretty.print_pattern pattern in
      let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
      in buffer

    (* Pretty-print a type expression from its CST *)

    let print_type_expr type_expr =
      let width, buffer = set () in
       let doc = Pretty.print_type_expr type_expr in
      let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
      in buffer
end
