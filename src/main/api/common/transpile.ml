open Api_helpers
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options

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
      let options =
        Compiler_options.make ~raw_options:(Raw_options.make ()) ~syntax:from_syntax ()
      in
      let meta = Compile.Of_source.extract_meta from_syntax in
      let c_unit, _ =
        Compile.Of_source.preprocess_file
          ~raise
          ~options:options.frontend
          ~meta
          source_file
      in
      let buffer =
        let open Trace in
        let open Main_errors in
        match from_syntax, to_syntax with
        | PascaLIGO, JsLIGO ->
          let old_cst =
            trace ~raise parser_tracer @@ Parsing.Pascaligo.parse_file c_unit source_file
          in
          let new_cst = Parsing_pascaligo.JsLIGO.of_cst old_cst in
          Parsing.Jsligo.pretty_print Parsing.Jsligo.Pretty.default_environment new_cst
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
