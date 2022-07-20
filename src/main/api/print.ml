open Api_helpers
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers

let pretty_print (raw_options : Compiler_options.raw) source_file display_format () =
    let warning_as_error = raw_options.warning_as_error in
    format_result ~warning_as_error ~display_format (Parsing.Formatter.ppx_format) @@
    fun ~raise ->
    let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
    let options = Compiler_options.make ~raw_options ~syntax () in
    let meta = Compile.Of_source.extract_meta syntax in
    Compile.Utils.pretty_print ~raise ~options:options.frontend ~meta source_file

let dependency_graph (raw_options : Compiler_options.raw) source_file display_format () =
    format_result ~display_format (BuildSystem.Formatter.graph_format) @@
      fun ~raise ->
      let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options = Compiler_options.make ~raw_options ~syntax () in
      let g,_ = Build.dependency_graph ~raise ~options Env source_file in
      (g,source_file)

let preprocess (raw_options : Compiler_options.raw) source_file display_format () =
    format_result ~display_format Parsing.Formatter.ppx_format @@
    fun ~raise ->
    fst @@
    let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
    let options = Compiler_options.make ~raw_options ~syntax () in
    let meta = Compile.Of_source.extract_meta syntax in
    Compile.Of_source.compile ~raise ~options:options.frontend ~meta source_file

let cst (raw_options : Compiler_options.raw) source_file display_format () =
    format_result ~display_format (Parsing.Formatter.ppx_format) @@
      fun ~raise ->
      let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options = Compiler_options.make ~raw_options ~syntax () in
      let meta = Compile.Of_source.extract_meta syntax in
      Compile.Utils.pretty_print_cst ~raise ~options:options.frontend ~meta source_file

let ast (raw_options : Compiler_options.raw) source_file display_format () =
    format_result ~display_format (Ast_imperative.Formatter.module_format) @@
      fun ~raise ->
      let syntax   = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options  = Compiler_options.make ~raw_options ~syntax () in
      let meta     = Compile.Of_source.extract_meta syntax in
      let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options:options.frontend ~meta source_file in
      Compile.Utils.to_imperative ~raise ~options ~meta c_unit source_file

let ast_sugar (raw_options : Compiler_options.raw) source_file display_format () =
    format_result ~display_format (Ast_sugar.Formatter.module_format) @@
      fun ~raise ->
      let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options = Compiler_options.make ~raw_options ~syntax () in
      let Compiler_options.{ self_pass ; _ } = options.tools in
      let meta     = Compile.Of_source.extract_meta syntax in
      let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options:options.frontend ~meta source_file in
      let sugar = Compile.Utils.to_sugar ~raise ~options ~meta c_unit source_file in
      if self_pass then
        Self_ast_sugar.all_module sugar
      else
        sugar

let ast_core (raw_options : Compiler_options.raw) source_file display_format () =
    format_result ~display_format (Ast_core.Formatter.module_format) @@
    fun ~raise ->
      let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options = Compiler_options.make ~raw_options ~syntax () in
      let Compiler_options.{ self_pass ; _ } = options.tools in
      let meta     = Compile.Of_source.extract_meta syntax in
      let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options:options.frontend ~meta source_file in
      let core = Compile.Utils.to_core ~raise ~options ~meta c_unit source_file in
      if self_pass then
        (*NOTE: this run self_ast_core a second time*)
        Self_ast_core.all_module ~init:core
      else
        core

let ast_typed (raw_options : Compiler_options.raw) source_file display_format () =
    format_result ~display_format (Ast_typed.Formatter.program_format) @@
    fun ~raise ->
      let options = (* TODO: options should be computed outside of the API *)
        let syntax           = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
        let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let Compiler_options.{ self_pass ; _ } = options.tools in
      let typed = Build.type_contract ~raise ~options source_file in
      if self_pass then
        Trace.trace ~raise Main_errors.self_ast_typed_tracer
          @@ Self_ast_typed.all_module ~warn_unused_rec:options.middle_end.warn_unused_rec typed
      else
        typed

let ast_aggregated (raw_options : Compiler_options.raw) source_file display_format () =
    format_result ~display_format (Ast_aggregated.Formatter.expression_format) @@
    fun ~raise ->
      let options = (* TODO: options should be computed outside of the API *)
        let syntax           = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
        let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let Compiler_options.{ self_pass ; _ } = options.tools in
      let typed = Build.merge_and_type_libraries ~raise ~options source_file in
      let aggregated = Compile.Of_typed.compile_program ~raise typed in
      let aggregated = Aggregation.compile_expression_in_context (Ast_typed.e_a_unit ()) aggregated in
      if self_pass then
        Trace.trace ~raise Main_errors.self_ast_aggregated_tracer @@ Self_ast_aggregated.all_expression ~options:options.middle_end aggregated
      else
        aggregated

let mini_c (raw_options : Compiler_options.raw) source_file display_format optimize () =
    format_result ~display_format (Mini_c.Formatter.program_format) @@
    fun ~raise ->
      let options = (* TODO: options should be computed outside of the API *)
        let syntax           = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
        let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let typed = Build.merge_and_type_libraries ~raise ~options source_file in
      let aggregated = Compile.Of_typed.compile_program ~raise typed in
      match optimize with
        | None ->
          let expr = Compile.Of_typed.compile_expression_in_context ~raise ~options:options.middle_end (Ast_typed.e_a_unit ()) aggregated in
          let mini_c = Compile.Of_aggregated.compile_expression ~raise expr in
          Mini_c.Formatter.Raw mini_c
        | Some entry_point ->
          let expr = Compile.Of_typed.apply_to_entrypoint ~raise ~options:options.middle_end typed entry_point in
          let mini_c = Compile.Of_aggregated.compile_expression ~raise expr in
          let _,o = Compile.Of_mini_c.optimize_for_contract ~raise options mini_c in
          Mini_c.Formatter.Optimized o.body
