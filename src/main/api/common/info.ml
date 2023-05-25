module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options
module Formatter = Ligo_formatter

let measure_contract (raw_options : Raw_options.t) entry_point source_file =
  ( Formatter.contract_size_format
  , fun ~raise ->
      let open Lwt.Let_syntax in
      let protocol_version =
        Helpers.protocol_to_variant ~raise raw_options.protocol_version
      in
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
      in
      Deprecation.entry_cli ~raise syntax entry_point;
      let options = Compiler_options.make ~protocol_version ~raw_options ~syntax () in
      let Compiler_options.{ module_; _ } = options.frontend in
      let%bind Build.{ entrypoint; views } =
        Build.build_contract
          ~raise
          ~options
          module_
          (Build.Source_input.From_file source_file)
      in
      let michelson = entrypoint.value in
      let views = List.map ~f:(fun { name; value } -> name, value) views in
      let%bind contract =
        Compile.Of_michelson.build_contract
          ~raise
          ~enable_typed_opt:options.backend.enable_typed_opt
          ~protocol_version
          michelson
          views
      in
      let%map measure = Compile.Of_michelson.measure ~raise contract in
      measure, [] )


let dump_cst (raw_options : Raw_options.t) (source_files : string list) =
  ( Dump_cst.output_format
  , fun ~raise ->
      ( List.map source_files ~f:(fun source_file ->
            let syntax =
              Syntax.of_string_opt
                ~raise
                (Syntax_name raw_options.syntax)
                (Some source_file)
            in
            let protocol_version =
              Helpers.protocol_to_variant ~raise raw_options.protocol_version
            in
            let options = Compiler_options.make ~raw_options ~protocol_version () in
            let meta = Compile.Of_source.extract_meta syntax in
            let c_unit, _ =
              Compile.Of_source.preprocess_file
                ~raise
                ~options:options.frontend
                ~meta
                source_file
            in
            let cst =
              Simple_utils.Trace.trace ~raise (fun e -> `Parser_tracer e)
              @@ fun ~raise ->
              Dialect_cst.get_cst_exn
                ~preprocess:true
                ~strict:false
                ~file:source_file
                syntax
                c_unit
            in
            cst)
      , [] ) )


let list_declarations (raw_options : Raw_options.t) source_file =
  ( Formatter.declarations_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
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
      let open Lwt.Let_syntax in
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
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
      let%map config = Resolve_config.resolve_config ~raise ~options syntax source_file in
      config, [] )
