open Api_helpers
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers

let pretty_print ?werror source_file syntax display_format () =
    format_result ?werror ~display_format (Parsing.Formatter.ppx_format) (fun _ -> []) @@
    fun ~raise ->
    let options = Compiler_options.make () in
    let meta = Compile.Of_source.extract_meta ~raise syntax source_file in
    Compile.Utils.pretty_print ~raise ~options ~meta source_file

let dependency_graph source_file syntax display_format project_root () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (BuildSystem.Formatter.graph_format) get_warnings @@
      fun ~raise ->
      let options = Compiler_options.make ?project_root () in
      let g,_ = Build.dependency_graph ~raise ~add_warning ~options syntax Env source_file in
      (g,source_file)

let preprocess source_file syntax libs display_format project_root () =
    format_result ~display_format Parsing.Formatter.ppx_format (fun _ -> []) @@
    fun ~raise ->
    fst @@
    let options   = Compiler_options.make ~libs ?project_root () in
    let meta = Compile.Of_source.extract_meta ~raise syntax source_file in
    Compile.Of_source.compile ~raise ~options ~meta source_file

let cst source_file syntax display_format () =
    format_result ~display_format (Parsing.Formatter.ppx_format) (fun _ -> []) @@
      fun ~raise ->
      let options = Compiler_options.make () in
      let meta = Compile.Of_source.extract_meta ~raise syntax source_file in
      Compile.Utils.pretty_print_cst ~raise ~options ~meta source_file

let ast source_file syntax display_format () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_imperative.Formatter.module_format) get_warnings @@
      fun ~raise ->
      let options       = Compiler_options.make () in
      let meta     = Compile.Of_source.extract_meta ~raise syntax source_file in
      let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options ~meta source_file in
      Compile.Utils.to_imperative ~raise ~add_warning ~options ~meta c_unit source_file

let ast_sugar source_file syntax display_format self_pass () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_sugar.Formatter.module_format) get_warnings @@
      fun ~raise ->
      let options = Compiler_options.make () in
      let meta     = Compile.Of_source.extract_meta ~raise syntax source_file in
      let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options ~meta source_file in
      let sugar = Compile.Utils.to_sugar ~raise ~add_warning ~options ~meta c_unit source_file in
      if self_pass then
        Self_ast_sugar.all_module sugar
      else
        sugar

let ast_core source_file syntax display_format self_pass project_root () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_core.Formatter.module_format) get_warnings @@
    fun ~raise ->
      let options = Compiler_options.make ?project_root () in
      let meta     = Compile.Of_source.extract_meta ~raise syntax source_file in
      let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options ~meta source_file in
      let core = Compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit source_file in
      if self_pass then
        Self_ast_core.all_module ~raise ~init:core
      else
        core

let ast_typed source_file syntax protocol_version display_format self_pass project_root () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_typed.Formatter.program_format) get_warnings @@
    fun ~raise ->
      let options = (* TODO: options should be computed outside of the API *)
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~protocol_version ~test:true ?project_root ()
      in
      let typed = Build.type_contract ~raise ~add_warning ~options syntax Env source_file in
      if self_pass then
        Trace.trace ~raise Main_errors.self_ast_typed_tracer @@ Self_ast_typed.all_module ~add_warning typed
      else
        typed

let ast_aggregated source_file syntax protocol_version display_format self_pass project_root () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_aggregated.Formatter.expression_format) get_warnings @@
    fun ~raise ->
      let options = (* TODO: options should be computed outside of the API *)
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~protocol_version ?project_root ()
      in
      let typed = Build.build_context ~raise ~add_warning ~options syntax source_file in
      let aggregated = Compile.Of_typed.compile_program ~raise typed in
      let aggregated = Aggregation.compile_expression_in_context (Ast_typed.e_a_unit ()) aggregated in
      if self_pass then
        Trace.trace ~raise Main_errors.self_ast_aggregated_tracer @@ Self_ast_aggregated.all_expression aggregated
      else
        aggregated

let ast_combined  source_file syntax protocol_version display_format project_root () =
  Trace.warning_with @@ fun add_warning get_warnings ->
  format_result ~display_format Ast_typed.Formatter.program_format get_warnings @@
  fun ~raise ->
    let options = (* TODO: options should be computed outside of the API *)
      let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
      Compiler_options.make ~protocol_version ?project_root ()
    in
    let typed = Build.build_context ~raise ~add_warning ~options syntax source_file in
    typed

let mini_c source_file syntax protocol_version display_format optimize project_root () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Mini_c.Formatter.program_format) get_warnings @@
    fun ~raise ->
      let options = (* TODO: options should be computed outside of the API *)
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~protocol_version ?project_root ()
      in
      let typed = Build.build_context ~raise ~add_warning ~options syntax source_file in
      let aggregated = Compile.Of_typed.compile_program ~raise typed in
      match optimize with
        | None ->
          let expr = Compile.Of_typed.compile_expression_in_context ~raise (Ast_typed.e_a_unit ()) aggregated in
          let mini_c = Compile.Of_aggregated.compile_expression ~raise expr in
          Mini_c.Formatter.Raw mini_c
        | Some entry_point ->
          let expr = Compile.Of_typed.apply_to_entrypoint ~raise typed entry_point in
          let o = Compile.Of_aggregated.compile_expression ~raise expr in
          Mini_c.Formatter.Optimized o
