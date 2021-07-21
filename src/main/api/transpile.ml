open Api_helpers
open Simple_utils.Trace
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers

let contract source_file new_syntax syntax new_dialect display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Parsing.Formatter.ppx_format) get_warnings @@
      let options         = Compiler_options.make () in
      let* meta       = Compile.Of_source.extract_meta syntax source_file in
      let* c_unit,_   = Compile.Utils.to_c_unit ~options ~meta source_file in
      let* core       = Compile.Utils.to_core ~add_warning ~options ~meta c_unit source_file in
      let* sugar      = Decompile.Of_core.decompile core in
      let* imperative = Decompile.Of_sugar.decompile sugar in
      let dialect         = Decompile.Helpers.Dialect_name new_dialect in
      let* buffer     =
        Decompile.Of_imperative.decompile ~dialect imperative (Syntax_name new_syntax) in
      ok @@ buffer

let expression expression new_syntax syntax new_dialect display_format =
    format_result ~display_format (Parsing.Formatter.ppx_format) (fun _ -> []) @@
      (* Compiling chain *)
      let options            = Compiler_options.make () in
      let* meta          = Compile.Of_source.make_meta syntax None in
      let* c_unit_expr,_ = Compile.Of_source.compile_string ~options ~meta expression in
      let* imperative    = Compile.Of_c_unit.compile_expression ~meta c_unit_expr in
      let* sugar         = Compile.Of_imperative.compile_expression imperative in
      let* core          = Compile.Of_sugar.compile_expression sugar in
      (* Decompiling chain *)
      let      dialect       = Decompile.Helpers.Dialect_name new_dialect in
      let* n_syntax      = Decompile.Helpers.syntax_to_variant ~dialect (Syntax_name new_syntax) None in
      let* sugar         = Decompile.Of_core.decompile_expression core in
      let* imperative    = Decompile.Of_sugar.decompile_expression sugar in
      let* buffer        = Decompile.Of_imperative.decompile_expression imperative n_syntax in
      ok @@ buffer
