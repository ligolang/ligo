(* This file provides an interface to the PascaLIGO parser and
   pretty-printer. *)

(* Vendor dependencies *)

module Trace   = Simple_utils.Trace
module Options = LexerLib.Options

(* Internal dependencies *)

module CST    = Cst_pascaligo.CST
module Errors = Parsing_shared.Errors
module Pretty = Parsing_pascaligo.Pretty

(* Making the pretty-printers from CST nodes *)


(* Dead code *)
(* include Parsing_shared.Common.MakePretty (CST) (Pretty) *)
let pretty_print a b = assert false

(* The functor *)

module Make (Options : Options.S) =
  struct
    (* Dependencies *)

    module Config  = Preprocessing_pascaligo.Config
    module Token   = Lexing_pascaligo.Token
    module ParErr  = Parsing_pascaligo.ParErr
    module Parser  = Parsing_pascaligo.Parser
    module Tree    = Cst_shared.Tree
    module UnitPasses  = Lx_psc_self_units.Self.Make (Options)
    module TokenPasses = Lx_psc_self_tokens.Self.Make (Options)

    (* Making the parsers *)

    module PascaligoParser =
      struct
        module CST = CST
        include Parser

        module Recovery = Parsing_pascaligo.RecoverParser
      end

    include Parsing_shared.Common.MakeTwoParsers
        (Config) (Token) (ParErr)
        (UnitPasses) (TokenPasses) (CST) (PascaligoParser)

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
      let module ParserParams =
        ParserLib.CLI.MakeDefault (LexerParams) in
      let module Options =
        struct
          include LexerParams.Options
          let layout  = true
          let regions = true
        end in
      let tree   = parse_file ?jsligo ?preprocess ?project_root
                              ~raise buffer file_path in
      let buffer = Buffer.create 59 in
      let open Options in
      let state  = Tree.mk_state
                     ~buffer
                     ~layout
                     ~regions
                     ~offsets
                     mode
      in Cst_pascaligo.Print.to_buffer state tree
end
