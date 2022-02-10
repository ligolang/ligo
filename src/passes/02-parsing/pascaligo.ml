(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module File        = Preprocessing_pascaligo.File
module Comments    = Preprocessing_pascaligo.Comments
module Token       = Lexing_pascaligo.Token
module Self_tokens = Lexing_pascaligo.Self_tokens
module ParErr      = Parsing_pascaligo.ParErr
module Parser      = Parsing_pascaligo.Parser
module Pretty      = Parsing_pascaligo.Pretty
module Common      = Parsing_shared.Common
module CST         = Cst_pascaligo.CST
module Tree        = Cst_shared.Tree

(* Making the parsers *)

module PascaligoParser =
  struct
    module CST = CST
    include Parser

    module Recovery = Parsing_pascaligo.RecoverParser
  end

include Common.MakeTwoParsers
          (File) (Comments) (Token) (ParErr) (Self_tokens)
          (CST) (PascaligoParser)

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
  in Cst_pascaligo.Print.to_buffer state cst
