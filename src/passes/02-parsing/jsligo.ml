(* This file provides an interface to the CameLIGO parser and
   pretty-printer. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module Config      = Preprocessing_jsligo.Config
module Token       = Lexing_jsligo.Token
module UnitPasses  = Lx_jsl_self_units.Self
module TokenPasses = Lx_js_self_tokens.Self
module ParErr      = Parsing_jsligo.ParErr
module Parser      = Parsing_jsligo.Parser
module Pretty      = Parsing_jsligo.Pretty
module CST         = Cst_jsligo.CST
module Tree        = Cst_shared.Tree

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

(* Making the pretty-printers *)

include Parsing_shared.Common.MakePretty (CST) (Pretty)

type raise = (Errors.t, Main_warnings.all) Trace.raise

let pretty_print_file ?preprocess ~raise buffer file_path =
  parse_file ?preprocess ~raise buffer file_path |> pretty_print

let pretty_print_cst ?preprocess ~raise buffer file_path =
  let module PreprocParams =
    Preprocessor.CLI.MakeDefault (Config) in
  let module LexerParams =
    LexerLib.CLI.MakeDefault (PreprocParams) in
  let module Options = LexerParams.Options in
  let tree   = parse_file ?preprocess ~raise buffer file_path in
  let buffer = Buffer.create 59 in
  let state  = Tree.mk_state
                 ~buffer
                 ~offsets:Options.offsets
                 Options.mode
  in Cst_jsligo.Print.to_buffer state tree
