(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module File        = Preprocessing_reasonligo.File
module Comments    = Preprocessing_reasonligo.Comments
module Modules     = Preprocessing_reasonligo.Modules
module Token       = Lexing_reasonligo.Token
module Self_tokens = Lexing_reasonligo.Self_tokens
module ParErr      = Parsing_reasonligo.ParErr
module Parser      = Parsing_reasonligo.Parser
module Common      = Parsing_shared.Common
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

include Common.MakeTwoParsers
          (File) (Comments) (Modules) (Token) (ParErr) (Self_tokens)
          (CST) (ReasonligoParser)

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
  in Cst_reasonligo.Print.to_buffer state cst
