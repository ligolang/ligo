(* This file provides an interface to the ReasonLIGO parser and
   pretty-printer. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module Config      = Preprocessing_reasonligo.Config
module Token       = Lexing_reasonligo.Token
module UnitPasses  = Lx_rsn_self_units.Self
module TokenPasses = Lx_rsn_self_tokens.Self
module ParErr      = Parsing_reasonligo.ParErr
module Parser      = Parsing_reasonligo.Parser
module Pretty      = Parsing_reasonligo.Pretty
module CST         = Cst_reasonligo.CST
module Tree        = Cst_shared.Tree

(* Making the parsers *)

module ReasonligoParser =
  struct
    module CST = CST
    include Parser

    module Recovery = Parsing_reasonligo.RecoverParser
  end

include Parsing_shared.Common.MakeTwoParsers
          (Config) (Token) (ParErr)
          (UnitPasses) (TokenPasses) (CST) (ReasonligoParser)

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
  in Cst_reasonligo.Print.to_buffer state tree
