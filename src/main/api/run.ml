open Api_helpers
open Simple_utils
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson
module Raw_options = Compiler_options.Raw_options
open Ligo_prim

let test (raw_options : Raw_options.t) source_file display_format () =
    format_result ~display_format (Ligo_interpreter.Formatter.tests_format) @@
      fun ~raise ->
      let raw_options = { raw_options with protocol_version = Environment.Protocols.(variant_to_string in_use) } in
      let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
      let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options = Compiler_options.make ~protocol_version ~syntax ~raw_options () in
      let Compiler_options.{ steps ; _ } = options.test_framework in
      let typed   = Build.qualified_typed ~raise ~options Env source_file in
      Interpreter.eval_test ~raise ~steps ~options typed

let dry_run (raw_options : Raw_options.t) source_file parameter storage amount balance sender source now display_format () =
    let warning_as_error = raw_options.warning_as_error in
    format_result ~warning_as_error ~display_format (Decompile.Formatter.expression_format) @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
      let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options = Compiler_options.make ~protocol_version ~syntax ~raw_options () in
      let Compiler_options.{ entry_point ; _ } = options.frontend in
      let entry_point = Value_var.of_input_var entry_point in
      let typed_prg = Build.qualified_typed ~raise ~options Env source_file in
      let aggregated_prg = Compile.Of_typed.apply_to_entrypoint_contract ~raise ~options:options.middle_end typed_prg entry_point in
      let mini_c_prg = Compile.Of_aggregated.compile_expression ~raise aggregated_prg in
      let compile_exp = Compile.Of_mini_c.compile_contract ~raise ~options mini_c_prg in
      let parameter_ty =
        (* fails if the given entry point is not a valid contract *)
        let _contract : Mini_c.meta Tezos_utils.Michelson.michelson = Compile.Of_michelson.build_contract ~raise ~enable_typed_opt:options.backend.enable_typed_opt ~protocol_version compile_exp [] in
        Option.map ~f:fst @@ Self_michelson.fetch_contract_ty_inputs compile_exp.expr_ty
      in
      let compiled_input    = Compile.Utils.compile_contract_input ~raise ~options parameter storage syntax typed_prg in
      let args_michelson    = Run.evaluate_expression ~raise compiled_input.expr compiled_input.expr_ty in
      let options           = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty } in
      let runres  = Run.run_contract ~raise ~options compile_exp.expr compile_exp.expr_ty args_michelson in
      Decompile.Of_michelson.decompile_value_from_contract_execution ~raise aggregated_prg.type_expression runres

let interpret (raw_options : Raw_options.t) expression init_file amount balance sender source now display_format () =
    format_result ~display_format (Decompile.Formatter.expression_format) @@
      fun ~raise ->
      let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) init_file in
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let (mini_c_exp, aggregated_exp) = Build.build_expression ~raise ~options syntax expression init_file in
      let compiled_exp = Compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp in
      let options           = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
      let runres  = Run.run_expression ~raise ~options compiled_exp.expr compiled_exp.expr_ty in
      Decompile.Of_michelson.decompile_expression ~raise aggregated_exp.type_expression runres

let evaluate_call (raw_options : Raw_options.t) source_file parameter amount balance sender source now display_format () =
    let warning_as_error = raw_options.warning_as_error in
    format_result ~warning_as_error ~display_format (Decompile.Formatter.expression_format) @@
      fun ~raise ->
      let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let Compiler_options.{ entry_point ; _ } = options.frontend in
      let init_prog = Build.qualified_typed ~raise Env ~options source_file in
      let meta             = Compile.Of_source.extract_meta syntax in
      let c_unit_param,_   = Compile.Of_source.preprocess_string ~raise ~options:options.frontend ~meta parameter in
      let imperative_param = Compile.Of_c_unit.compile_expression ~raise ~meta c_unit_param in
      let core_param       = Compile.Of_imperative.compile_expression ~raise imperative_param in
      let app              = Compile.Of_core.apply entry_point core_param in
      let typed_app        = Compile.Of_core.compile_expression ~raise ~options ~init_prog app in
      let app_aggregated   = Compile.Of_typed.compile_expression_in_context ~raise ~options:options.middle_end init_prog typed_app in
      let app_mini_c       = Compile.Of_aggregated.compile_expression ~raise app_aggregated in
      let michelson        = Compile.Of_mini_c.compile_expression ~raise ~options app_mini_c in
      let options          = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None} in
      let runres           = Run.run_expression ~raise ~options michelson.expr michelson.expr_ty in
      Decompile.Of_michelson.decompile_expression ~raise app_aggregated.type_expression runres

let evaluate_expr (raw_options : Raw_options.t) source_file amount balance sender source now display_format () =
    let warning_as_error = raw_options.warning_as_error in
    format_result ~warning_as_error ~display_format Decompile.Formatter.expression_format @@
      fun ~raise ->
        let syntax  = Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file) in
        let options =
          let protocol_version = Helpers.protocol_to_variant ~raise raw_options.protocol_version in
          Compiler_options.make ~protocol_version ~raw_options ~syntax ()
        in
        let Compiler_options.{ entry_point ; _ } = options.frontend in
        let (mini_c_exp, typed_exp) = Build.build_expression ~raise ~options syntax entry_point (Some source_file) in
        let compiled_exp = Compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp in
        let options           = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
        let runres  = Run.run_expression ~raise ~options compiled_exp.expr compiled_exp.expr_ty in
        Decompile.Of_michelson.decompile_expression ~raise typed_exp.type_expression runres
