open Simple_utils
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson
module Raw_options = Compiler_options.Raw_options

let test (raw_options : Raw_options.t) code_input =
  ( Ligo_interpreter.Formatter.tests_format
  , fun ~raise ->
      let open Lwt.Let_syntax in
      let syntax =
        Syntax.of_string_opt
          ~raise
          (Syntax_name raw_options.syntax)
          Build.Source_input.(
            match code_input with
            | HTTP uri -> Some (Http_uri.get_filename uri)
            | From_file file_name -> Some file_name
            | Raw { id; _ } -> Some id
            | Raw_input_lsp { file; _ } -> Some file)
      in
      let options = Compiler_options.make ~syntax ~raw_options () in
      let Compiler_options.{ steps; _ } = options.test_framework in
      let typed = Build.qualified_typed ~raise ~options code_input in
      let%map result = Interpreter.eval_test ~raise ~steps ~options typed in
      result, [] )


let test_expression (raw_options : Raw_options.t) expr source_file =
  ( Ligo_interpreter.Formatter.tests_format
  , fun ~raise ->
      let open Lwt.Let_syntax in
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) source_file
      in
      let options = Compiler_options.make ~syntax ~raw_options () in
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
        Ligo_compile.Utils.type_expression ~raise ~options syntax expr init_prg.pr_sig
      in
      let%map b, v = Interpreter.eval_expression ~raise ~steps ~options init_prg typed in
      (b, [ "eval", v ]), [] )


let dry_run
    (raw_options : Raw_options.t)
    entry_point
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
      let open Lwt.Let_syntax in
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
      in
      Deprecation.entry_cli ~raise syntax entry_point;
      let options = Compiler_options.make ~syntax ~raw_options () in
      let Compiler_options.{ module_; _ } = options.frontend in
      let module_path = Build.parse_module_path ~loc:Location.dummy module_ in
      let typed_prg =
        Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
      in
      let typed_contract =
        Trace.trace ~raise Main_errors.self_ast_typed_tracer
        @@ Self_ast_typed.all_program typed_prg
      in
      let aggregated_prg =
        let _sig, contract_sig =
          Trace.trace_option
            ~raise
            (`Self_ast_aggregated_tracer
              (Self_ast_aggregated.Errors.corner_case
                 "Could not recover types from contract"))
            Ast_typed.Misc.(
              get_contract_signature (to_extended_signature typed_prg) module_path)
        in
        Compile.Of_typed.apply_to_entrypoint_with_contract_type
          ~raise
          ~options:options.middle_end
          typed_contract
          module_path
          contract_sig
      in
      let expanded_prg = Compile.Of_aggregated.compile_expression ~raise aggregated_prg in
      let mini_c_prg = Compile.Of_expanded.compile_expression ~raise expanded_prg in
      let%bind compile_exp =
        Compile.Of_mini_c.compile_contract ~raise ~options mini_c_prg
      in
      let%bind parameter_ty =
        (* fails if the given entry point is not a valid contract *)
        let%map _contract : Mini_c.meta Tezos_utils.Michelson.michelson Lwt.t =
          Compile.Of_michelson.build_contract
            ~raise
            ~enable_typed_opt:options.backend.enable_typed_opt
            compile_exp
            []
        in
        Option.map ~f:fst @@ Self_michelson.fetch_contract_ty_inputs compile_exp.expr_ty
      in
      let%bind compiled_input =
        Compile.Utils.compile_contract_input
          ~raise
          ~options
          parameter
          storage
          syntax
          typed_contract
      in
      let%bind args_michelson =
        Run.evaluate_expression ~raise compiled_input.expr compiled_input.expr_ty
      in
      let%bind options =
        Run.make_dry_run_options
          ~raise
          { now; amount; balance; sender; source; parameter_ty }
      in
      let%map runres =
        Run.run_contract
          ~raise
          ~options
          compile_exp.expr
          compile_exp.expr_ty
          args_michelson
      in
      ( Decompile.Of_michelson.decompile_value_from_contract_execution
          ~raise
          (Compile.Of_aggregated.compile_type_expression
             ~raise
             aggregated_prg.type_expression)
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
      let open Lwt.Let_syntax in
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) init_file
      in
      let options = Compiler_options.make ~raw_options ~syntax () in
      let%bind Build.{ expression; ast_type } =
        Build.build_expression ~raise ~options syntax expression init_file
      in
      let%bind options =
        Run.make_dry_run_options
          ~raise
          { now; amount; balance; sender; source; parameter_ty = None }
      in
      let%map runres =
        Run.run_expression ~raise ~options expression.expr expression.expr_ty
      in
      let ast_type = Compile.Of_aggregated.compile_type_expression ~raise ast_type in
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
      let open Lwt.Let_syntax in
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
      in
      let options = Compiler_options.make ~raw_options ~syntax () in
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
        Compile.Of_c_unit.compile_expression
          ~raise
          ~meta
          ~preprocess_define:options.frontend.preprocess_define
          c_unit_param
      in
      let core_param =
        Compile.Of_unified.compile_expression ~raise ~options imperative_param
      in
      let app = Compile.Of_core.apply entry_point core_param in
      let typed_app =
        Compile.Of_core.compile_expression
          ~raise
          ~options
          ~context:(Ast_typed.to_signature init_prog.pr_module)
          app
      in
      let app_aggregated =
        Compile.Of_typed.compile_expression_in_context
          ~raise
          ~options:options.middle_end
          None
          init_prog
          typed_app
      in
      let app_expanded = Compile.Of_aggregated.compile_expression ~raise app_aggregated in
      let app_mini_c = Compile.Of_expanded.compile_expression ~raise app_expanded in
      let%bind michelson =
        Compile.Of_mini_c.compile_expression ~raise ~options app_mini_c
      in
      let%bind options =
        Run.make_dry_run_options
          ~raise
          { now; amount; balance; sender; source; parameter_ty = None }
      in
      let%map runres =
        Run.run_expression ~raise ~options michelson.expr michelson.expr_ty
      in
      ( Decompile.Of_michelson.decompile_expression
          ~raise
          (Compile.Of_aggregated.compile_type_expression
             ~raise
             app_aggregated.type_expression)
          runres
      , [] ) )


let evaluate_expr
    (raw_options : Raw_options.t)
    source_file
    exp
    amount
    balance
    sender
    source
    now
  =
  ( Decompile.Formatter.expression_format
  , fun ~raise ->
      let open Lwt.Let_syntax in
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
      in
      let options = Compiler_options.make ~raw_options ~syntax () in
      let%bind Build.{ expression; ast_type } =
        Build.build_expression ~raise ~options syntax exp (Some source_file)
      in
      let%bind options =
        Run.make_dry_run_options
          ~raise
          { now; amount; balance; sender; source; parameter_ty = None }
      in
      let%map runres =
        Run.run_expression ~raise ~options expression.expr expression.expr_ty
      in
      let ast_type = Compile.Of_aggregated.compile_type_expression ~raise ast_type in
      Decompile.Of_michelson.decompile_expression ~raise ast_type runres, [] )
