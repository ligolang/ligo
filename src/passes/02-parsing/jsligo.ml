(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module File        = Preprocessing_jsligo.File
module Comments    = Preprocessing_jsligo.Comments
module Modules     = Preprocessing_jsligo.Modules
module Token       = Lexing_jsligo.Token
module Self_tokens = Lexing_jsligo.Self_tokens
module ParErr      = Parsing_jsligo.ParErr
module Parser      = Parsing_jsligo.Parser
module Pretty      = Parsing_jsligo.Pretty
module Common      = Parsing_shared.Common
module CST         = Cst_jsligo.CST
module Tree        = Cst_shared.Tree

(* Making the parsers *)

module JsligoParser =
  struct
    module CST = CST
    include Parser

    module Recovery = Parsing_jsligo.RecoverParser
  end

include Common.MakeTwoParsers
          (File) (Comments) (Modules) (Token) (ParErr) (Self_tokens)
          (CST) (JsligoParser)

(* Making the pretty-printers *)

include Common.MakePretty (CST) (Pretty)

let pretty_print_file ~raise buffer file_path =
  ContractParser.parse_file ~raise buffer file_path |> pretty_print

let pretty_print_cst ~raise buffer file_path =
  let cst = ContractParser.parse_file ~raise buffer file_path in
  let buffer = Buffer.create 59 in
  let state =
    Tree.mk_state ~buffer
                  ~offsets:true
                  `Point
  in Cst_jsligo.Print.to_buffer state cst
