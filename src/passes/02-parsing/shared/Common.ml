(* Parser and pretty-printer factory *)

(* Vendors dependencies *)

module Region = Simple_utils.Region
module Trace  = Simple_utils.Trace

(* Internal dependencies *)

module type FILE        = Preprocessing_shared.Common.FILE
module type COMMENTS    = Preprocessing_shared.Comments.S
module type MODULES     = Preprocessing_shared.Modules.S
module type TOKEN       = Lexing_shared.Token.S
module type SELF_TOKENS = Lexing_shared.Self_tokens.S
module type PARSER      = ParserLib.API.PARSER

module LexerMainGen = Lexing_shared.LexerMainGen

(* CONFIGURATION *)

module CLI (File : FILE) (Comments : COMMENTS) (Modules : MODULES) =
  struct
    (* Stubs for the libraries CLIs *)

    module Preprocessor_CLI : Preprocessor.CLI.S =
      struct
        include Comments
        include Modules

        let input        = File.input
        let extension    = Some File.extension
        let dirs         = File.dirs
        let project_root = File.project_root
        let show_pp      = false
        let offsets      = true

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
        module Preprocessor_CLI = Preprocessor_CLI

        let preprocess = false
        let mode       = `Point
        let command    = None

        type status = [
          Preprocessor_CLI.status
        | `Conflict of string * string (* Two conflicting options *)
        ]

        let status = `Done
      end

    module ParserConfig : ParserLib.API.CONFIG =
      struct
        let mode = Lexer_CLI.mode

        (* Disable all debug options for the parser *)

        let error_recovery_tracing = false
        let tracing_output         = None
      end
  end

(* PRETTY-PRINTING *)

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

(* PARSING *)

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
         (File        : Preprocessing_shared.File.S)
         (Comments    : COMMENTS)
         (Modules     : MODULES)
         (Token       : TOKEN)
         (ParErr      : PAR_ERR)
         (Self_tokens : SELF_TOKENS with type token = Token.t)
         (CST         : sig type tree end)
         (Parser      : PARSER with type token = Token.t
                                and type tree = CST.tree) =
  struct
    type file_path = string list

    (* Lifting [Stdlib.result] to [Trace]. *)

    let lift ~(raise:(Errors.t, Main_warnings.all) Trace.raise) = function
      Ok tree -> tree
    | Error msg -> raise.error @@ `Parsing msg

    let lift_recov ~(raise:(Errors.t, Main_warnings.all) Trace.raise)
      = function
          Ok (tree, errors)     -> List.iter (List.rev errors)
                                       ~f:(fun e -> raise.log_error @@ `Parsing e);
                                   tree
        | Error (error, errors) -> List.iter (List.rev errors)
                                       ~f:(fun e -> raise.log_error @@ `Parsing e);
                                   raise.error @@ `Parsing error

    (* We always parse a string buffer of type [Buffer.t], but the
       interpretation of its contents depends on the functions
       below. In [parse_file buffer file_path], the argument [buffer]
       is interpreted as the contents of the file located at
       [file_path]. In [parse_string buffer], the argument [buffer] is
       interpreted as the contents of a string given on the CLI. *)

    (* Parsing a file *)

    let from_file ~raise buffer file_path : CST.tree =
      let module File =
        struct
          let input        = Some file_path
          let extension    = File.extension
          let dirs         = []
          let project_root = None
        end in
      let module CLI = CLI (File) (Comments) (Modules) in
      let module Raiser = struct let add_warning = raise.Trace.warning end in
      let module MainLexer =
        LexerMainGen.Make
          (File) (Token) (CLI.Lexer_CLI) (Self_tokens) (Raiser) in
      let module MainParser =
        ParserLib.API.Make (MainLexer) (Parser) (CLI.ParserConfig) in
      let string = Buffer.contents buffer in
      if CLI.Preprocessor_CLI.show_pp then
          Printf.printf "%s\n%!" string;
      let lexbuf = Lexing.from_string string in
      let     () = LexerLib.Core.reset ~file:file_path lexbuf in
      let     () = MainLexer.clear () in
      lift_recov ~raise
      @@ MainParser.recov_from_lexbuf (module ParErr: PAR_ERR) lexbuf

    let parse_file = from_file

    (* Parsing a string *)

    let from_string ~raise buffer : CST.tree =
      let module File =
        struct
          let input        = None
          let extension    = File.extension
          let dirs         = []
          let project_root = None
        end in
      let module CLI = CLI (File) (Comments) (Modules) in
      let module Warning = struct let add_warning = raise.Trace.warning end in
      let module MainLexer =
        LexerMainGen.Make
          (File) (Token) (CLI.Lexer_CLI) (Self_tokens) (Warning) in
      let module MainParser =
        ParserLib.API.Make (MainLexer) (Parser) (CLI.ParserConfig) in
      let string = Buffer.contents buffer in
      if CLI.Preprocessor_CLI.show_pp then
          Printf.printf "%s\n%!" string;
      let lexbuf = Lexing.from_string string in
      let     () = MainLexer.clear () in
      lift_recov ~raise
      @@ MainParser.recov_from_lexbuf (module ParErr: PAR_ERR) lexbuf

    let parse_string = from_string
  end

(* Signature of parsers generated by Menhir, plus module [CST]. *)

module type LIGO_PARSER =
  sig
    (* Results *)

    module CST :
      sig
        type t
        type expr
      end

    (* The type of tokens. *)

    type token

    (* This exception is raised by the monolithic API functions. *)

    exception Error

    val interactive_expr :
      (Lexing.lexbuf -> token) -> Lexing.lexbuf -> CST.expr

    val contract :
      (Lexing.lexbuf -> token) -> Lexing.lexbuf -> CST.t

    (* The monolithic API. *)

    module MenhirInterpreter : MenhirLib.IncrementalEngine.EVERYTHING
           with type token = token

    (* The entry point(s) to the incremental API. *)

    module Incremental :
      sig
        val interactive_expr :
          Lexing.position -> CST.expr MenhirInterpreter.checkpoint

        val contract :
          Lexing.position -> CST.t MenhirInterpreter.checkpoint
      end

    (* The recovery API. *)

    module Recovery :
      sig
        include Merlin_recovery.RECOVERY_GENERATED
                with module I := MenhirInterpreter

        val default_value : Region.t -> 'a MenhirInterpreter.symbol -> 'a
      end
  end

(* Making parsers for CSTs and expressions *)

module MakeTwoParsers
         (File        : Preprocessing_shared.File.S)
         (Comments    : COMMENTS)
         (Modules     : MODULES)
         (Token       : TOKEN)
         (ParErr      : PAR_ERR)
         (Self_tokens : SELF_TOKENS with type token = Token.t)
         (CST         : sig type t type expr end)
         (Parser      : LIGO_PARSER with type token = Token.t
                                     and module CST = CST) =
  struct
    type file_path = string
    type 'a parser = raise:(Errors.t, Main_warnings.all) Trace.raise -> Buffer.t -> 'a
    module Errors = Errors

    (* Results *)

    type cst    = CST.t
    type expr   = CST.expr
    type buffer = Buffer.t

    module Partial =
      MakeParser (File) (Comments) (Modules) (Token) (ParErr) (Self_tokens)

    (* Parsing contracts *)

    module ContractCST =
      struct
        type tree = CST.t
      end

    module ContractParser_Menhir =
      struct
        include Parser
        type tree = ContractCST.tree

        let main = contract

        module Incremental =
          struct
            let main = Incremental.contract
          end
      end

    module ContractParser =
      Partial (ContractCST) (ContractParser_Menhir)

    let from_file  = ContractParser.parse_file
    let parse_file = from_file

    let from_string  = ContractParser.parse_string
    let parse_string = from_string

    (* Parsing expressions *)

    module ExprCST =
      struct
        type tree = CST.expr
      end

    module ExprParser_Menhir =
      struct
        include Parser
        type tree = ExprCST.tree

        let main = interactive_expr

        module Incremental =
          struct
            let main = Incremental.interactive_expr
          end
      end

    module ExprParser = Partial (ExprCST) (ExprParser_Menhir)

    let expression       = ExprParser.parse_string
    let parse_expression = expression
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

    let pretty_print cst =
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

    let pretty_print_expression = print_expr

    (* Pretty-print a pattern from its CST *)

    let print_pattern pattern =
      let width, buffer = set () in
      let doc = Pretty.print_pattern pattern in
      let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
      in buffer

    let pretty_print_pattern = print_pattern

    (* Pretty-print a type expression from its CST *)

    let print_type_expr type_expr =
      let width, buffer = set () in
      let doc = Pretty.print_type_expr type_expr in
      let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
      in buffer

    let pretty_print_type_expr = print_type_expr
  end
