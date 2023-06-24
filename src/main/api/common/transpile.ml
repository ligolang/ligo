open Api_helpers
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options
module Config = Preprocessing_cameligo.Config
module PreprocParams = Preprocessor.CLI.MakeDefault (Config)
module LexerParams = LexerLib.CLI.MakeDefault (PreprocParams)
module Parameters = ParserLib.CLI.MakeDefault (LexerParams)
module Options = Parameters.Options

let contract source_file to_syntax from_syntax output_file =
  ( Parsing.Formatter.ppx_format
  , fun ~raise ->
      let from_syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:true
          (Syntax_name from_syntax)
          (Some source_file)
      in
      let to_syntax =
        match to_syntax, output_file with
        | "auto", None ->
          raise.error @@ Main_errors.main_transpilation_unspecified_dest_syntax
        | _ ->
          Syntax.of_string_opt
            ~raise
            ~support_pascaligo:true
            (Syntax_name to_syntax)
            output_file
      in
      let c_unit = Compile.Utils.buffer_from_path source_file in
      let buffer =
        let open Trace in
        let open Main_errors in
        match from_syntax, to_syntax with
        | PascaLIGO, JsLIGO ->
          let module Options = struct
            include Options

            let jsligo = Some output_file
          end
          in
          let module Parse = Parsing.Pascaligo.Make (Options) in
          let old_cst =
            trace ~raise parser_tracer
            @@ Parse.parse_file ~preprocess:false c_unit source_file
          in
          let new_cst = Parsing_pascaligo.JsLIGO.of_cst old_cst in
          Parsing.Jsligo.pretty_print Parsing.Jsligo.Pretty.default_state new_cst
        | _ ->
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
