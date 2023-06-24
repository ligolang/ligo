(* This file provides an interface to the CameLIGO parser and
   pretty-printer. *)

(* Vendor dependencies *)

module Trace   = Simple_utils.Trace
module Options = LexerLib.Options

(* Internal dependencies *)

module CST    = Cst_jsligo.CST
module Errors = Parsing_shared.Errors
module Pretty = Parsing_jsligo.Pretty

(* Making the pretty-printers from CST nodes *)

include Parsing_shared.Common.MakePretty (CST) (Pretty)

(* The functor *)

module Make (Options : Options.S) =
  struct
    (* Dependencies *)

    module Config  = Preprocessing_jsligo.Config
    module Token   = Lexing_jsligo.Token
    module ParErr  = Parsing_jsligo.ParErr
    module Parser  = Parsing_jsligo.Parser
    module Tree    = Cst_shared.Tree
    module UnitPasses  = Lx_js_self_units.Self.Make (Options)
    module TokenPasses = Lx_js_self_tokens.Self.Make (Options)

    (* Making the parsers *)

    module JsligoParser =
      struct
        module CST = CST
        include Parser

        module Recovery = Parsing_jsligo.RecoverParser
      end

    include Parsing_shared.Common.MakeTwoParsers
        (Config) (Token) (ParErr)
        (UnitPasses) (TokenPasses) (CST) (JsligoParser)

    (* Making the pretty-printers from source *)

    type raise = (Errors.t, Main_warnings.all) Trace.raise

    let pretty_print_file
        env ?jsligo ?preprocess ?project_root ~raise buffer file_path =
      parse_file
        ?jsligo ?preprocess ?project_root ~raise buffer file_path
      |> pretty_print env

    let pretty_print_cst ?jsligo ?preprocess ?project_root
                         ~raise buffer file_path =
      let module PreprocParams =
        Preprocessor.CLI.MakeDefault (Config) in
      let module LexerParams =
        LexerLib.CLI.MakeDefault (PreprocParams) in
      let module Options = LexerParams.Options in
      let tree   =
        parse_file ?jsligo ?preprocess ?project_root
                   ~raise buffer file_path in
      let buffer = Buffer.create 59 in
      let state  = Tree.mk_state
                     ~buffer
                     ~offsets:Options.offsets
                     Options.mode
      in Cst_jsligo.Print.to_buffer state tree
  end
