open Simple_utils
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson
module Raw_options = Compiler_options.Raw_options

let test (raw_options : Raw_options.t) source_file =
  ( Ligo_interpreter.Formatter.tests_format
  , fun ~raise ->
      let raw_options =
        { raw_options with
          protocol_version = Environment.Protocols.(variant_to_string in_use)
        }
      in
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
      let options = Compiler_options.make ~protocol_version ~syntax ~raw_options () in
      let Compiler_options.{ steps; _ } = options.test_framework in
      let typed =
        Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
      in
      Interpreter.eval_test ~raise ~steps ~options typed, [] )


let test_expression (raw_options : Raw_options.t) expr source_file =
  ( Ligo_interpreter.Formatter.tests_format
  , fun ~raise ->
      let raw_options =
        { raw_options with
          protocol_version = Environment.Protocols.(variant_to_string in_use)
        }
      in
      let protocol_version =
        Helpers.protocol_to_variant ~raise raw_options.protocol_version
      in
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:raw_options.deprecated
          (Syntax_name raw_options.syntax)
          source_file
      in
      let options = Compiler_options.make ~protocol_version ~syntax ~raw_options () in
      let Compiler_options.{ steps; _ } = options.test_framework in
      let module Stdlib = Build.Stdlib in
      let module Source_input = BuildSystem.Source_input in
      let init_prg =
        let f : Source_input.file_name -> Ast_typed.program =
         fun filename ->
          Build.qualified_typed ~raise ~options (Build.Source_input.From_file filename)
        in
        let default = Stdlib.select_lib_typed syntax (Stdlib.get ~options) in
        Option.value_map source_file ~f ~default
      in
      let typed =
        Ligo_compile.Utils.type_expression ~raise ~options syntax expr init_prg
      in
      let b, v = Interpreter.eval_expression ~raise ~steps ~options init_prg typed in
      (b, [ "eval", v ]), [] )


let dry_run
    (raw_options : Raw_options.t)
    source_file
    parameter
    storage
    amount
    balance
    sender
    source
    now
  =
  ( Decompile.Formatter.expression_format
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
      let options = Compiler_options.make ~protocol_version ~syntax ~raw_options () in
      let Compiler_options.{ entry_point; module_; _ } = options.frontend in
      let module_path = Build.parse_module_path ~loc:Location.dummy module_ in
      let typed_prg =
        Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
      in
      let contract_info, typed_contract =
        Trace.trace ~raise Main_errors.self_ast_typed_tracer
        @@ Ligo_compile.Of_core.specific_passes
             (Ligo_compile.Of_core.Contract { entrypoints = entry_point; module_path })
             typed_prg
      in
      let entry_point, _contract_type = contract_info in
      let aggregated_prg =
        Compile.Of_typed.apply_to_entrypoint_contract
          ~raise
          ~options:options.middle_end
          typed_contract
          entry_point
          module_path
      in
      let expanded_prg = Compile.Of_aggregated.compile_expression ~raise aggregated_prg in
      let mini_c_prg = Compile.Of_expanded.compile_expression ~raise expanded_prg in
      let compile_exp = Compile.Of_mini_c.compile_contract ~raise ~options mini_c_prg in
      let parameter_ty =
        (* fails if the given entry point is not a valid contract *)
        let _contract : Mini_c.meta Tezos_utils.Michelson.michelson =
          Compile.Of_michelson.build_contract
            ~raise
            ~enable_typed_opt:options.backend.enable_typed_opt
            ~protocol_version
            compile_exp
            []
        in
        Option.map ~f:fst @@ Self_michelson.fetch_contract_ty_inputs compile_exp.expr_ty
      in
      let compiled_input =
        Compile.Utils.compile_contract_input
          ~raise
          ~options
          parameter
          storage
          syntax
          typed_prg
      in
      let args_michelson =
        Run.evaluate_expression ~raise compiled_input.expr compiled_input.expr_ty
      in
      let options =
        Run.make_dry_run_options
          ~raise
          { now; amount; balance; sender; source; parameter_ty }
      in
      let runres =
        Run.run_contract
          ~raise
          ~options
          compile_exp.expr
          compile_exp.expr_ty
          args_michelson
      in
      ( Decompile.Of_michelson.decompile_value_from_contract_execution
          ~raise
          aggregated_prg.type_expression
          runres
      , [] ) )


let interpret
    (raw_options : Raw_options.t)
    expression
    init_file
    amount
    balance
    sender
    source
    now
  =
  ( Decompile.Formatter.expression_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:raw_options.deprecated
          (Syntax_name raw_options.syntax)
          init_file
      in
      let options =
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let Build.{ expression; ast_type } =
        Build.build_expression ~raise ~options syntax expression init_file
      in
      let options =
        Run.make_dry_run_options
          ~raise
          { now; amount; balance; sender; source; parameter_ty = None }
      in
      let runres =
        Run.run_expression ~raise ~options expression.expr expression.expr_ty
      in
      Decompile.Of_michelson.decompile_expression ~raise ast_type runres, [] )


let evaluate_call
    (raw_options : Raw_options.t)
    source_file
    function_name
    parameter
    amount
    balance
    sender
    source
    now
  =
  ( Decompile.Formatter.expression_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:raw_options.deprecated
          (Syntax_name raw_options.syntax)
          (Some source_file)
      in
      let options =
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let init_prog =
        Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
      in
      let entry_point =
        Ligo_prim.Value_var.of_input_var ~loc:Location.dummy function_name
      in
      let meta = Compile.Of_source.extract_meta syntax in
      let c_unit_param, _ =
        Compile.Of_source.preprocess_string
          ~raise
          ~options:options.frontend
          ~meta
          parameter
      in
      let imperative_param =
        Compile.Of_c_unit.compile_expression ~raise ~meta c_unit_param
      in
      let core_param =
        Compile.Of_unified.compile_expression ~raise ~options imperative_param
      in
      let app = Compile.Of_core.apply entry_point core_param in
      let typed_app = Compile.Of_core.compile_expression ~raise ~options ~init_prog app in
      let app_aggregated =
        Compile.Of_typed.compile_expression_in_context
          ~raise
          ~options:options.middle_end
          init_prog
          typed_app
      in
      let app_expanded = Compile.Of_aggregated.compile_expression ~raise app_aggregated in
      let app_mini_c = Compile.Of_expanded.compile_expression ~raise app_expanded in
      let michelson = Compile.Of_mini_c.compile_expression ~raise ~options app_mini_c in
      let options =
        Run.make_dry_run_options
          ~raise
          { now; amount; balance; sender; source; parameter_ty = None }
      in
      let runres = Run.run_expression ~raise ~options michelson.expr michelson.expr_ty in
      ( Decompile.Of_michelson.decompile_expression
          ~raise
          app_aggregated.type_expression
          runres
      , [] ) )


let evaluate_expr
    (raw_options : Raw_options.t)
    source_file
    amount
    balance
    sender
    source
    now
  =
  ( Decompile.Formatter.expression_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt
          ~raise
          ~support_pascaligo:raw_options.deprecated
          (Syntax_name raw_options.syntax)
          (Some source_file)
      in
      let options =
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let Compiler_options.{ entry_point; _ } = options.frontend in
      let entry_point = List.hd_exn entry_point in
      let Build.{ expression; ast_type } =
        Build.build_expression ~raise ~options syntax entry_point (Some source_file)
      in
      let options =
        Run.make_dry_run_options
          ~raise
          { now; amount; balance; sender; source; parameter_ty = None }
      in
      let runres =
        Run.run_expression ~raise ~options expression.expr expression.expr_ty
      in
      Decompile.Of_michelson.decompile_expression ~raise ast_type runres, [] )
