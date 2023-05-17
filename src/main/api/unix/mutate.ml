(* open Simple_utils.Trace *)
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options

(* let generator_to_variant ~raise s =
  if String.equal s "list"
  then `Generator_list
  else if String.equal s "random"
  then `Generator_random
  else raise.error @@ Main_errors.main_invalid_generator_name s *)

let mutate_cst (raw_options : Raw_options.t) source_file seed =
  ( Parsing.Formatter.ppx_format
  , fun ~raise ->

    (* let generator = generator_to_variant ~raise raw_options.generator in
      let get_module =
        match generator with
        | `Generator_list -> (module Fuzz.Lst : Fuzz.Monad)
        | `Generator_random -> (module Fuzz.Rnd : Fuzz.Monad)
      in
      let module Gen : Fuzz.Monad = (val get_module : Fuzz.Monad) in
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
      let options = Compiler_options.make ~raw_options ~syntax ~protocol_version () in
      let meta = Compile.Of_source.extract_meta syntax in
      let c_unit, _ =
        Compile.Of_source.preprocess_file
          ~raise
          ~options:options.frontend
          ~meta
          source_file
      in
      match meta with
      | { syntax = CameLIGO } ->
        let module Fuzzer = Fuzz.Cameligo.Mutator (Gen) in
        let raw =
          trace ~raise Main_errors.parser_tracer
          @@ Parsing.Cameligo.parse_file c_unit source_file
        in
        let _, mutated_prg = Fuzzer.mutate_module_ ?n:seed raw in
        let buffer =
          Parsing.Cameligo.pretty_print
            Parsing.Cameligo.Pretty.default_environment
            mutated_prg
        in
        buffer, []
      | { syntax = JsLIGO } ->
        let module Fuzzer = Fuzz.Jsligo.Mutator (Gen) in
        let raw =
          trace ~raise Main_errors.parser_tracer
          @@ Parsing.Jsligo.parse_file c_unit source_file
        in
        let _, mutated_prg = Fuzzer.mutate_module_ ?n:seed raw in
        let buffer =
          Parsing.Jsligo.pretty_print
            Parsing.Jsligo.Pretty.default_environment
            mutated_prg
        in
        buffer, []
      | { syntax = PascaLIGO } ->
        let module Fuzzer = Fuzz.Pascaligo.Mutator (Gen) in
        let raw =
          trace ~raise Main_errors.parser_tracer
          @@ Parsing.Pascaligo.parse_file c_unit source_file
        in
        let _, mutated_prg = Fuzzer.mutate_module_ ?n:seed raw in
        let buffer =
          Parsing.Pascaligo.pretty_print
            Parsing.Pascaligo.Pretty.default_environment
            mutated_prg
        in *)
      ignore (raw_options, source_file, seed, raise);
      let buffer = Buffer.create 50 in
      Buffer.add_string buffer "Fuzzing disabled";
      buffer, [] )
