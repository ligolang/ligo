open Api_helpers
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options
module Config = Preprocessing_cameligo.Config
module PreprocParams = Preprocessor.CLI.MakeDefault (Config)
module LexerParams = LexerLib.CLI.MakeDefault (PreprocParams)
module Parameters = ParserLib.CLI.MakeDefault (LexerParams)
module Options = Parameters.Options

(* XXX Transpilation is not implemented *)
let contract source_file to_syntax from_syntax output_file =
  ( Parsing.Formatter.ppx_format
  , fun ~raise ->
      let from_syntax =
        Syntax.of_string_opt ~raise (Syntax_name from_syntax) (Some source_file)
      in
      let to_syntax =
        match to_syntax, output_file with
        | "auto", None ->
          raise.error @@ Main_errors.main_transpilation_unspecified_dest_syntax
        | _ -> Syntax.of_string_opt ~raise (Syntax_name to_syntax) output_file
      in
      let buffer =
        let open Trace in
        if Syntax_types.equal from_syntax to_syntax
        then
          raise.error
          @@ Main_errors.main_transpilation_same_source_and_dest_syntax
               (Syntax.to_string from_syntax)
        else
          raise.error
          @@ Main_errors.main_transpilation_unsupported_syntaxes
               (Syntax.to_string from_syntax)
               (Syntax.to_string to_syntax)
      in
      buffer, [] )
