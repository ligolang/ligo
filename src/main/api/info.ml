open Api_helpers
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options

let measure_contract (raw_options : Raw_options.t) source_file display_format () =
    let warning_as_error = raw_options.warning_as_error in
    format_result ~warning_as_error ~display_format Formatter.contract_size_format @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
      let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options = Compiler_options.make ~protocol_version ~raw_options ~syntax () in
      let Compiler_options.{ entry_point ; _ } = options.frontend in
      let Compiler_options.{ views ; _ } = options.backend in
      let michelson =  Build.build_contract ~raise ~options entry_point source_file in
      let views = Build.build_views ~raise ~options entry_point views source_file in
      let contract = Compile.Of_michelson.build_contract ~raise ~enable_typed_opt:options.backend.enable_typed_opt ~protocol_version michelson views in
      Compile.Of_michelson.measure ~raise contract

let list_declarations (raw_options : Raw_options.t) source_file display_format () =
    format_result ~display_format Formatter.declarations_format @@
      fun ~raise ->
      let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options  = Compiler_options.make ~raw_options ~syntax () in
      let prg = Build.type_contract ~raise ~options source_file in
      let declarations  = Compile.Of_typed.list_declarations prg in
      (source_file, declarations)

let get_scope (raw_options : Raw_options.t) source_file display_format () =
    Scopes.Api_helper.format_result ~display_format @@
      fun ~raise ->
      let syntax = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
      let options = Compiler_options.make ~raw_options ~syntax ~protocol_version () in
      let Compiler_options.{ with_types ; _ } = options.tools in
      let core_prg = Build.infer_contract ~raise ~options source_file in
      Scopes.scopes ~options:options.middle_end ~with_types core_prg
