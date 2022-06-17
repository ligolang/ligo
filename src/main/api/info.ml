open Api_helpers
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers

let measure_contract (raw_options : Compiler_options.raw) source_file display_format () =
    let warning_as_error = raw_options.warning_as_error in
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~warning_as_error ~display_format Formatter.contract_size_format get_warnings @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
      let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options = Compiler_options.make ~protocol_version ~raw_options ~syntax () in
      let Compiler_options.{ entry_point ; _ } = options.frontend in
      let Compiler_options.{ views ; _ } = options.backend in
      let michelson,e =  Build.build_contract ~raise ~add_warning ~options entry_point source_file in
      let views = Build.build_views ~raise ~add_warning ~options entry_point (views,e) source_file in
      let contract = Compile.Of_michelson.build_contract ~raise ~add_warning ~enable_typed_opt:options.backend.enable_typed_opt ~protocol_version michelson views in
      Compile.Of_michelson.measure ~raise contract

let list_declarations (raw_options : Compiler_options.raw) source_file display_format () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format Formatter.declarations_format get_warnings @@
      fun ~raise ->
      let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options  = Compiler_options.make ~raw_options ~syntax () in
      let meta     = Compile.Of_source.extract_meta syntax in
      let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options:options.frontend ~meta source_file in
      let core_prg = Compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit source_file in
      let declarations  = Compile.Of_core.list_declarations core_prg in
      (source_file, declarations)

let get_scope (raw_options : Compiler_options.raw) source_file display_format () =
    Trace.warning_with @@ fun add_warning get_warnings ->
    Scopes.Api_helper.format_result ~display_format get_warnings @@
      fun ~raise ->
      let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
      let options = Compiler_options.make ~raw_options ~syntax ~protocol_version () in
      let Compiler_options.{ with_types ; _ } = options.tools in
      let core_prg = Build.infer_contract ~raise ~add_warning ~options source_file in
      Scopes.scopes ~add_warning ~options:options.middle_end ~with_types core_prg
