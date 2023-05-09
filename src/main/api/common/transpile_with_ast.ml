module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Trace = Simple_utils.Trace
module Raw_options = Compiler_options.Raw_options

let contract source_file new_syntax syntax libraries =
  ( Parsing.Formatter.ppx_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:true
          (Syntax_name syntax)
          (Some source_file)
      in
      let options =
        Compiler_options.make ~raw_options:(Raw_options.make ~libraries ()) ~syntax ()
      in
      let meta = Compile.Of_source.extract_meta syntax in
      let c_unit, _ =
        Compile.Of_source.preprocess_file
          ~raise
          ~options:options.frontend
          ~meta
          source_file
      in
      let core = Compile.Utils.to_core ~raise ~options ~meta c_unit source_file in
      let new_syntax =
        Syntax.of_string_opt ~raise ~support_pascaligo:true (Syntax_name new_syntax) None
      in
      let unified =
        Trace.trace ~raise Main_errors.nanopasses_tracer
        @@ Decompile.Of_core.decompile ~syntax:new_syntax core
      in
      Decompile.Of_unified.decompile unified new_syntax, [] )


let expression expression new_syntax syntax libraries =
  ( Parsing.Formatter.ppx_format
  , fun ~raise ->
      (* Compiling chain *)
      let syntax =
        Syntax.of_string_opt ~raise ~support_pascaligo:true (Syntax_name syntax) None
      in
      let options =
        Compiler_options.make ~raw_options:(Raw_options.make ~libraries ()) ~syntax ()
      in
      let meta = Compile.Of_source.make_meta syntax in
      let c_unit_expr, _ =
        Compile.Of_source.preprocess_string
          ~raise
          ~options:options.frontend
          ~meta
          expression
      in
      let unified = Compile.Of_c_unit.compile_expression ~raise ~meta c_unit_expr in
      let core = Compile.Of_unified.compile_expression ~options ~raise unified in
      (* Decompiling chain *)
      let n_syntax =
        Syntax.of_string_opt ~raise ~support_pascaligo:true (Syntax_name new_syntax) None
      in
      let imperative =
        Trace.trace ~raise Main_errors.nanopasses_tracer
        @@ Decompile.Of_core.decompile_expression ~syntax:n_syntax core
      in
      let buffer = Decompile.Of_unified.decompile_expression imperative n_syntax in
      buffer, [] )
