open Api_helpers
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options

let contract source_file new_syntax syntax display_format no_colour () =
  format_result ~display_format ~no_colour Parsing.Formatter.ppx_format
  @@ fun ~raise ->
  let syntax =
    Syntax.of_string_opt
      ~raise
      ~support_pascaligo:true
      (Syntax_name syntax)
      (Some source_file)
  in
  let new_syntax =
    Syntax.of_string_opt ~raise ~support_pascaligo:true (Syntax_name new_syntax) None
  in
  let options = Compiler_options.make ~raw_options:(Raw_options.make ()) ~syntax () in
  let meta = Compile.Of_source.extract_meta syntax in
  let c_unit, _ =
    Compile.Of_source.preprocess_file ~raise ~options:options.frontend ~meta source_file
  in
  let buffer =
    let open Trace in
    let open Main_errors in
    match syntax, new_syntax with
    | PascaLIGO, JsLIGO ->
      let old_cst =
        trace ~raise parser_tracer @@ Parsing.Pascaligo.parse_file c_unit source_file
      in
      let new_cst = Parsing_pascaligo.JsLIGO.of_cst old_cst in
      Parsing.Jsligo.pretty_print new_cst
    | _ ->
      if Syntax_types.equal syntax new_syntax
      then
        raise.error
        @@ Main_errors.main_transpilation_same_source_and_dest_syntax
             (Syntax.to_string syntax)
      else
        raise.error
        @@ Main_errors.main_transpilation_unsupported_syntaxes
             (Syntax.to_string syntax)
             (Syntax.to_string new_syntax)
  in
  buffer
