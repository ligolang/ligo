(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module File        = Preprocessing_cameligo.File
module Comments    = Preprocessing_cameligo.Comments
module Modules     = Preprocessing_cameligo.Modules
module Token       = Lexing_cameligo.Token
module Self_tokens = Lexing_cameligo.Self_tokens
module ParErr      = Parsing_cameligo.ParErr
module Parser      = Parsing_cameligo.Parser
module Pretty      = Parsing_cameligo.Pretty
module Common      = Parsing_shared.Common
module CST         = Cst_cameligo.CST
module Tree        = Cst_shared.Tree

(* Making the parsers *)

module CameligoParser =
  struct
    module CST = CST
    include Parser

    module Recovery = Parsing_cameligo.RecoverParser
  end

include Common.MakeTwoParsers
          (File) (Comments) (Modules) (Token) (ParErr) (Self_tokens)
          (CST) (CameligoParser)

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
  in Cst_cameligo.Print.to_buffer state cst
