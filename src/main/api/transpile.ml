open Api_helpers
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers

let contract source_file new_syntax syntax new_dialect display_format () =
    format_result ~display_format (Parsing.Formatter.ppx_format) @@
      fun ~raise ->
      let syntax     = Syntax.of_string_opt ~raise (Syntax_name syntax) (Some source_file) in
      let options    = Compiler_options.make ~raw_options:Compiler_options.default_raw_options ~syntax () in
      let meta       = Compile.Of_source.extract_meta syntax in
      let c_unit,_   = Compile.Utils.to_c_unit ~raise ~options:options.frontend ~meta source_file in
      let core       = Compile.Utils.to_core ~raise ~options ~meta c_unit source_file in
      let sugar      = Decompile.Of_core.decompile core in
      let imperative = Decompile.Of_sugar.decompile sugar in
      let dialect    = Syntax_types.Dialect_name new_dialect in
      let buffer     =
        Decompile.Of_imperative.decompile ~raise ~dialect imperative (Syntax_name new_syntax) in
      buffer

let expression expression new_syntax syntax new_dialect display_format () =
    format_result ~display_format (Parsing.Formatter.ppx_format) @@
      fun ~raise ->
      (* Compiling chain *)
      let syntax        = Syntax.of_string_opt ~raise (Syntax_name syntax) None in
      let options       = Compiler_options.make ~raw_options:Compiler_options.default_raw_options ~syntax () in
      let meta          = Compile.Of_source.make_meta syntax in
      let c_unit_expr,_ = Compile.Of_source.compile_string ~raise ~options:options.frontend ~meta expression in
      let imperative    = Compile.Of_c_unit.compile_expression ~raise ~meta c_unit_expr in
      let sugar         = Compile.Of_imperative.compile_expression ~raise imperative in
      let core          = Compile.Of_sugar.compile_expression ~raise sugar in
      (* Decompiling chain *)
      let dialect       = Syntax_types.Dialect_name new_dialect in
      let n_syntax      = Syntax.of_string_opt ~raise ~dialect (Syntax_name new_syntax) None in
      let sugar         = Decompile.Of_core.decompile_expression core in
      let imperative    = Decompile.Of_sugar.decompile_expression sugar in
      let buffer        = Decompile.Of_imperative.decompile_expression imperative n_syntax in
      buffer
