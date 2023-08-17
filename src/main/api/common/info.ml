module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options
module Formatter = Ligo_formatter

let measure_contract (raw_options : Raw_options.t) source_file =
  ( Formatter.contract_size_format
  , fun ~raise ->
      let protocol_version =
        Helpers.protocol_to_variant ~raise raw_options.protocol_version
      in
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:raw_options.deprecated
          (Syntax_name raw_options.syntax)
          (Some source_file)
      in
      let options = Compiler_options.make ~protocol_version ~raw_options ~syntax () in
      let Compiler_options.{ entry_point; module_; _ } = options.frontend in
      let Build.{ entrypoint; views } =
        Build.build_contract
          ~raise
          ~options
          entry_point
          module_
          (Build.Source_input.From_file source_file)
      in
      let michelson = entrypoint.value in
      let views = List.map ~f:(fun { name; value } -> name, value) views in
      let contract =
        Compile.Of_michelson.build_contract
          ~raise
          ~enable_typed_opt:options.backend.enable_typed_opt
          ~protocol_version
          michelson
          views
      in
      Compile.Of_michelson.measure ~raise contract, [] )


let list_declarations (raw_options : Raw_options.t) source_file =
  ( Formatter.declarations_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:raw_options.deprecated
          (Syntax_name raw_options.syntax)
          (Some source_file)
      in
      let options = Compiler_options.make ~raw_options ~syntax () in
      let prg =
        Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
      in
      let prg =
        Simple_utils.Trace.trace ~raise Main_errors.self_ast_typed_tracer
        @@ Self_ast_typed.all_program prg
      in
      let declarations =
        Compile.Of_typed.list_declarations
          ~skip_generated:raw_options.skip_generated
          raw_options.only_ep
          prg
      in
      (source_file, declarations), [] )


let resolve_config (raw_options : Raw_options.t) source_file =
  ( Resolve_config.config_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt
          ~support_pascaligo:raw_options.deprecated
          ~raise
          (Syntax_name raw_options.syntax)
          (Some source_file)
      in
      let options =
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make
          ~raw_options
          ~syntax
          ~protocol_version
          ~has_env_comments:false
          ()
      in
      let config = Resolve_config.resolve_config ~raise ~options syntax source_file in
      config, [] )
