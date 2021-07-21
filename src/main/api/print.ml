open Api_helpers
open Simple_utils.Trace
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers

let pretty_print ?werror source_file syntax display_format =
    format_result ?werror ~display_format (Parsing.Formatter.ppx_format) (fun _ -> []) @@
    let options = Compiler_options.make () in
    let* meta = Compile.Of_source.extract_meta syntax source_file in
    Compile.Utils.pretty_print ~options ~meta source_file

let dependency_graph source_file syntax display_format =
    format_result ~display_format (Build.Formatter.graph_format) (fun _ -> []) @@
      let options = Compiler_options.make () in
      let* g,_ = Build.dependency_graph ~options syntax Env source_file in
      ok @@ (g,source_file)

let preprocess source_file syntax display_format =
    format_result ~display_format Parsing.Formatter.ppx_format (fun _ -> []) @@
    Trace.map ~f:fst @@
    let options   = Compiler_options.make () in
    let* meta = Compile.Of_source.extract_meta syntax source_file in
    Compile.Of_source.compile ~options ~meta source_file

let cst source_file syntax display_format =
    format_result ~display_format (Parsing.Formatter.ppx_format) (fun _ -> []) @@
      let options = Compiler_options.make () in
      let* meta = Compile.Of_source.extract_meta syntax source_file in
      Compile.Utils.pretty_print_cst ~options ~meta source_file

let ast source_file syntax display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_imperative.Formatter.module_format) get_warnings @@
      let options       = Compiler_options.make () in
      let* meta     = Compile.Of_source.extract_meta syntax source_file in
      let* c_unit,_ = Compile.Utils.to_c_unit ~options ~meta source_file in
      Compile.Utils.to_imperative ~add_warning ~options ~meta c_unit source_file

let ast_sugar source_file syntax display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_sugar.Formatter.module_format) get_warnings @@
      let options = Compiler_options.make () in
      let* meta     = Compile.Of_source.extract_meta syntax source_file in
      let* c_unit,_ = Compile.Utils.to_c_unit ~options ~meta source_file in
      Compile.Utils.to_sugar ~add_warning ~options ~meta c_unit source_file

let ast_core source_file syntax infer protocol_version display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_core.Formatter.module_format) get_warnings @@
    if infer then
      (* Do the same thing as for print_ast_typed, but only infer the main module
         (it still needs to infer+typecheck the dependencies) *)
        let* options =
          let* init_env = Helpers.get_initial_env protocol_version in
          ok @@ Compiler_options.make ~infer ~init_env ()
        in
        let* _,inferred_core,_,_ = Build.infer_contract ~add_warning ~options syntax Env source_file in
        ok @@ inferred_core
    else
      (* Print the ast as-is without inferring and typechecking dependencies *)
        let options = Compiler_options.make ~infer () in
        let* meta     = Compile.Of_source.extract_meta syntax source_file in
        let* c_unit,_ = Compile.Utils.to_c_unit ~options ~meta source_file in
        Compile.Utils.to_core ~add_warning ~options ~meta c_unit source_file

let ast_typed source_file syntax infer protocol_version display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_typed.Formatter.module_format_fully_typed) get_warnings @@
      let* options =
        let* init_env = Helpers.get_initial_env protocol_version in
        ok @@ Compiler_options.make ~infer ~init_env ()
      in
      let* typed,_ = Build.type_contract ~add_warning ~options syntax Env source_file in
      ok @@ typed

let ast_combined  source_file syntax infer protocol_version display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_typed.Formatter.module_format_fully_typed) get_warnings @@
      let* options =
        let* init_env = Helpers.get_initial_env protocol_version in
        let* protocol_version = Helpers.protocol_to_variant protocol_version in
        ok @@ Compiler_options.make ~infer ~init_env ~protocol_version ()
      in
      let* typed,_ = Build.combined_contract ~add_warning ~options syntax Env source_file in
      ok @@ typed

let mini_c source_file syntax infer protocol_version display_format optimize =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Mini_c.Formatter.program_format) get_warnings @@
      let* options =
        let* init_env   = Helpers.get_initial_env protocol_version in
        let* protocol_version = Helpers.protocol_to_variant protocol_version in
        ok @@ Compiler_options.make ~infer ~init_env ~protocol_version ()
      in
      let* mini_c,_ = Build.build_mini_c ~add_warning ~options syntax Env source_file in
      match optimize with
        | None -> ok @@ Mini_c.Formatter.Raw mini_c
        | Some entry_point ->
          let* o = Compile.Of_mini_c.aggregate_contract mini_c entry_point in
          ok @@ Mini_c.Formatter.Optimized o
