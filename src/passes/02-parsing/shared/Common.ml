[@@@warning "-32"]
(* Parser and pretty-printer factory *)

(* Vendors dependencies *)

module Region      = Simple_utils.Region
module Trace       = Simple_utils.Trace
module Lexbuf      = Simple_utils.Lexbuf
module Unit        = LexerLib.Unit
module Config      = Preprocessor.Config
module PreprocAPI  = Preprocessor.TopAPI
module Options     = ParserLib.Options
module type PARSER = ParserLib.LowAPI.PARSER

(* Internal dependencies *)

module Token    = Lexing_shared.Token
module Pipeline = Lexing_shared.Pipeline
module LexerAPI = Lexing_shared.TopAPI

(* Local dependencies *)



(* PRETTY-PRINTING *)

module type PRETTY =
  sig
    type state
    val default_state : state

    type cst
    type expr
    type type_expr
    type pattern
    type signature_expr

    val print           : state -> cst       -> PPrint.document
    val print_expr      : state -> expr      -> PPrint.document
    val print_type_expr : state -> type_expr -> PPrint.document
    val print_pattern   : state -> pattern   -> PPrint.document
    val print_signature_expr : state -> signature_expr -> PPrint.document
  end

(* PARSING *)

module type CST =
  sig
    type t
    type expr
    type type_expr
    type pattern
    type signature_expr
  end

module type PAR_ERR =
  sig
    val message : int -> string
  end

module MakeParser
         (Config      : Config.S)
         (Token       : Token.S)
         (ParErr      : PAR_ERR)
         (UnitPasses  : Pipeline.PASSES with type item = Token.t Unit.t)
         (TokenPasses : Pipeline.PASSES with type item = Token.t)
         (CST         : sig type tree end)
         (Parser      : PARSER with type token = Token.t
                                and type tree = CST.tree) =
  struct
    (* Utilities *)

    type file_path = string

    type raise = (Errors.t, Main_warnings.all) Trace.raise

    type 'a parser =
      ?jsligo:string option option ->
      ?preprocess:bool ->
      ?project_root:file_path ->
      raise:raise ->
      Buffer.t ->
      'a

    (* Lifting [Stdlib.result] to [Trace.raise] and logging errors. *)

    let log_errors ~(raise:raise) errors =
      List.iter (List.rev errors)
                ~f:(fun e -> raise.Trace.log_error @@ `Parsing e)

    let lift ~(raise:raise) = function
      Ok (tree, errors) ->
        log_errors ~raise errors; tree
    | Error (error, errors) ->
        log_errors ~raise errors;
        raise.Trace.error @@ `Parsing error (* Only the first error *)

    (* We always parse a string buffer of type [Buffer.t], but the
       interpretation of its contents depends on the functions
       below. In [parse_file buffer file_path], the argument [buffer]
       is interpreted as the contents of the file located at
       [file_path]. In [parse_string buffer], the argument [buffer] is
       interpreted as the contents of a string given on the CLI. *)

    (* Generic parser *)

    module type ParserLexer = sig
      module Parser : ParserLib.LowAPI.S
        with type token = Token.t and type tree = CST.tree
      module Lexer : Lexing_shared.TopAPI.S
      module DefaultPreprocParams : Preprocessor.CLI.PARAMETERS
    end

    module type ParserLexerOptions = sig
      val jsligo : file_path option option
      val preprocess : bool
      val project_root : file_path option
      val raise : raise
      val file_path : file_path option
    end

    module ParserLexerGenerator
      (Options : ParserLexerOptions) : ParserLexer = struct
      (* Instantiating the general lexer of LexerLib *)
      open Options
      let preprocess_opt   = preprocess
      let project_root_opt = project_root
      let jsligo_opt       = jsligo
        module Warning =
          struct
            let add = raise.Trace.warning
          end
        module DefaultPreprocParams =
          Preprocessor.CLI.MakeDefault (Config)
        module PreprocParams =
          struct
            module Config = Config
            module Status = DefaultPreprocParams.Status
            module Options: Preprocessor.Options.S =
              struct
                include DefaultPreprocParams.Options

                let input = file_path
                let project_root = project_root_opt
              end
          end

        module Preproc = PreprocAPI.Make (PreprocParams)

        module LexerParams =
          struct
            include LexerLib.CLI.MakeDefault (PreprocParams)

            module Options =
              struct
                include Options

                let preprocess = preprocess_opt
                let jsligo = jsligo_opt
              end
          end

        module Lexer =
          LexerAPI.Make
            (Preproc) (LexerParams) (Token)
            (UnitPasses) (TokenPasses) (Warning)

        (* Adapting the lexer of the LexerLib to the one expected by the
          [ParserLib.LowAPI.Make] *)

        module MainLexer =
          struct
            include Lexer

            let scan_token ~no_colour lexbuf =
              match scan_token ~no_colour lexbuf with
                Ok _ as ok -> ok
              | Error {message; _} -> Error message
          end

        (* Instantiating the parser of LexerLib *)

        module NoDebug =
          struct
            let mode           = `Point
            let trace_recovery = None
          end

        module Parser =
          ParserLib.LowAPI.Make (MainLexer) (Parser) (NoDebug)

        end

    let gen_parser ?(jsligo=None) ?(preprocess=true) ?project_root ~raise
                ?file_path buffer : CST.tree =
      let module Options : ParserLexerOptions = struct
        let jsligo = jsligo
        let preprocess = preprocess
        let project_root = project_root
        let raise = raise
        let file_path = file_path
      end in
      let module ParserLexer = ParserLexerGenerator (Options) in

      (* Running the parser in error recovery mode *)

      let tree =
        let string    = Buffer.contents buffer in
        let lexbuf    = Lexing.from_string string in
        let no_colour = ParserLexer.DefaultPreprocParams.Options.no_colour in
        let        () = Lexbuf.reset ?file:file_path lexbuf in
        let        () = ParserLexer.Lexer.clear () in
        ParserLexer.Parser.recov_from_lexbuf ~no_colour (module ParErr) lexbuf

      in lift ~raise tree

    (* Parsing a file *)

    let from_file ?jsligo ?preprocess ?project_root ~raise buffer file_path
      : CST.tree =
      gen_parser ?jsligo ?preprocess ?project_root ~raise ~file_path buffer

    let parse_file = from_file

    (* Parsing a string *)

    let from_string ?jsligo ?preprocess ?project_root ~raise buffer : CST.tree =
      gen_parser ?jsligo ?preprocess ?project_root ~raise buffer

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
         (Config      : Config.S)
         (Token       : Token.S)
         (ParErr      : PAR_ERR)
         (UnitPasses  : Pipeline.PASSES with type item = Token.t Unit.t)
         (TokenPasses : Pipeline.PASSES with type item = Token.t)
         (CST         : sig type t type expr end)
         (Parser      : LIGO_PARSER with type token = Token.t
                                     and module CST = CST) =
  struct
    type file_path = string

    type raise = (Errors.t, Main_warnings.all) Trace.raise

    type 'a parser =
      ?jsligo:string option option ->
      ?preprocess:bool ->
      ?project_root:file_path ->
      raise:raise ->
      Buffer.t ->
      'a

    module Errors = Errors

    (* Partially instantiating a parser *)

    module Partial =
      MakeParser (Config) (Token) (ParErr) (UnitPasses) (TokenPasses)

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

    module ContractParser = Partial (ContractCST) (ContractParser_Menhir)

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
                            and  type signature_expr = CST.signature_expr
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

    let pretty_print env cst =
      let width, buffer = set () in
      let doc = Pretty.print env cst in
      let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
      in buffer

    (* Pretty-print an expression from its CST *)

    let print_expr env expr =
      let width, buffer = set () in
      let doc = Pretty.print_expr env expr in
      let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
      in buffer

    let pretty_print_expression = print_expr

    (* Pretty-print a pattern from its CST *)

    let print_pattern ?cols env pattern =
      let width, buffer = set () in
      let doc = Pretty.print_pattern env pattern in
      let width = match cols with Some cols -> cols | None -> width in
      let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
      in buffer

    let pretty_print_pattern ?cols = print_pattern ?cols

    (* Pretty-print a type expression from its CST *)

    let print_type_expr env type_expr =
      let width, buffer = set () in
      let doc = Pretty.print_type_expr env type_expr in
      let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
      in buffer

    let pretty_print_type_expr = print_type_expr


    let print_signature_expr env signature_expr =
      let width, buffer = set () in
      let doc = Pretty.print_signature_expr env signature_expr in
      let () = PPrint.ToBuffer.pretty 1.0 width buffer doc
      in buffer

    let pretty_print_signature_expr = print_signature_expr
  end
