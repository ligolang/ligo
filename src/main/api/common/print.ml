module Location = Simple_utils.Location
open Api_helpers
open Simple_utils.Function
module Compile = Ligo_compile
module Helpers = Ligo_compile.Helpers
module Raw_options = Compiler_options.Raw_options

let loc = Location.dummy

let pretty_print (raw_options : Raw_options.t) source_file =
  ( Parsing.Formatter.ppx_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
      in
      let options = Compiler_options.make ~raw_options ~syntax () in
      let meta = Compile.Of_source.extract_meta syntax in
      ( Compile.Utils.pretty_print
          ~preprocess:false
          ~raise
          ~options:options.frontend
          ~meta
          source_file
      , [] ) )


let dependency_graph (raw_options : Raw_options.t) source_file =
  ( BuildSystem.Formatter.graph_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
      in
      let options = Compiler_options.make ~raw_options ~syntax () in
      let g, _ =
        Build.dependency_graph
          ~raise
          ~options
          (BuildSystem.Source_input.From_file source_file)
      in
      (g, source_file), [] )


let preprocess (raw_options : Raw_options.t) source_file =
  ( Parsing.Formatter.ppx_format
  , fun ~raise ->
      let fst =
        fst
        @@
        let syntax =
          Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
        in
        let options = Compiler_options.make ~raw_options ~syntax () in
        let meta = Compile.Of_source.extract_meta syntax in
        Compile.Of_source.preprocess_file
          ~raise
          ~options:options.frontend
          ~meta
          source_file
      in
      fst, [] )


let cst (raw_options : Raw_options.t) source_file =
  ( Parsing.Formatter.ppx_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
      in
      let options = Compiler_options.make ~raw_options ~syntax () in
      let meta = Compile.Of_source.extract_meta syntax in
      ( Compile.Utils.pretty_print_cst
          ~preprocess:false
          ~raise
          ~options:options.frontend
          ~meta
          source_file
      , [] ) )


let ast_unified (raw_options : Raw_options.t) show_loc hide_sort stop_before source_file =
  ( Ast_unified.Formatter.program_format ~show_loc ~hide_sort
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
      in
      let options = Compiler_options.make ~raw_options ~syntax () in
      let meta = Compile.Of_source.extract_meta syntax in
      let c_unit, _ =
        Compile.Of_source.preprocess_file
          ~raise
          ~options:options.frontend
          ~meta
          source_file
      in
      let unified = Compile.Utils.to_unified ~raise ~meta c_unit source_file in
      match stop_before with
      | None -> unified, []
      | Some _ ->
        Compile.Of_unified.compile_until ~raise ~options ?stop_before unified, [] )


let ast_core (raw_options : Raw_options.t) source_file =
  ( Ast_core.Formatter.program_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
      in
      let options = Compiler_options.make ~raw_options ~syntax () in
      let meta = Compile.Of_source.extract_meta syntax in
      let c_unit, _ =
        Compile.Of_source.preprocess_file
          ~raise
          ~options:options.frontend
          ~meta
          source_file
      in
      let core = Compile.Utils.to_core ~raise ~options ~meta c_unit source_file in
      core, [] )


let ast_typed (raw_options : Raw_options.t) source_file =
  ( Ast_typed.Formatter.program_format
  , fun ~raise ->
      let options =
        (* TODO: options should be computed outside of the API *)
        let syntax =
          Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
        in
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let Compiler_options.{ self_pass; _ } = options.tools in
      let typed =
        Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
      in
      (* Here, I would like to write this, but it become slow ...
         let typed = Build.unqualified_typed ~raise ~options Env source_file in
      *)
      if self_pass
      then (
        let prg =
          Trace.trace
            ~raise
            Main_errors.self_ast_typed_tracer
            (Self_ast_typed.all_program typed)
        in
        prg, [])
      else typed, [] )


let ast_aggregated (raw_options : Raw_options.t) source_file =
  ( Ast_aggregated.Formatter.expression_format
  , fun ~raise ->
      let options =
        (* TODO: options should be computed outside of the API *)
        let syntax =
          Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
        in
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let Compiler_options.{ self_pass; _ } = options.tools in
      let typed =
        Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
      in
      ( Compile.Of_typed.compile_expression_in_context
          ~raise
          ~options:options.middle_end
          ~self_pass
          None
          typed
          (Ast_typed.e_a_unit ~loc ())
      , [] ) )


let signature (raw_options : Raw_options.t) source_file =
  ( Parsing.Formatter.ppx_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
      in
      let options =
        (* TODO: options should be computed outside of the API *)
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let sig_ =
        let Compiler_options.{ module_; _ } = options.frontend in
        let module_path = Build.parse_module_path ~loc module_ in
        let typed =
          Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
        in
        Option.value_exn ~message:"could not find module"
        @@ Ast_typed.Misc.get_path_signature typed.pr_sig module_path
      in
      let core_sig = Checking.untype_signature ~use_orig_var:true sig_ in
      let unified_sig_expr =
        Trace.trace ~raise Main_errors.nanopasses_tracer
        @@ Nanopasses.decompile_sig_expr ~syntax
        @@ Location.wrap ~loc:Location.generated (Ast_core.S_sig core_sig)
      in
      let to_syntax =
        match syntax with
        | Syntax_types.CameLIGO ->
          Parsing.Cameligo.(pretty_print_signature_expr Pretty.default_state)
          <@ Unification.Cameligo.decompile_sig_expr
        | Syntax_types.JsLIGO ->
          Parsing.Jsligo.(pretty_print_signature_expr Pretty.default_state)
          <@ Unification.Jsligo.decompile_sig_expr
      in
      to_syntax unified_sig_expr, [] )


let ast_expanded (raw_options : Raw_options.t) source_file =
  ( Ast_expanded.Formatter.expression_format
  , fun ~raise ->
      let options =
        (* TODO: options should be computed outside of the API *)
        let syntax =
          Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
        in
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let Compiler_options.{ self_pass; _ } = options.tools in
      let typed =
        Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
      in
      let aggregated =
        Compile.Of_typed.compile_expression_in_context
          ~raise
          ~options:options.middle_end
          ~self_pass
          None
          typed
          (Ast_typed.e_a_unit ~loc ())
      in
      Compile.Of_aggregated.compile_expression ~raise aggregated, [] )


let mini_c (raw_options : Raw_options.t) source_file optimize =
  ( Mini_c.Formatter.program_format
  , fun ~raise ->
      let options =
        (* TODO: options should be computed outside of the API *)
        let syntax =
          Syntax.of_string_opt ~raise (Syntax_name raw_options.syntax) (Some source_file)
        in
        let protocol_version =
          Helpers.protocol_to_variant ~raise raw_options.protocol_version
        in
        Compiler_options.make ~protocol_version ~raw_options ~syntax ()
      in
      let typed =
        Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
      in
      match optimize with
      | None ->
        let expr =
          Compile.Of_typed.compile_expression_in_context
            ~raise
            ~options:options.middle_end
            None
            typed
            (Ast_typed.e_a_unit ~loc ())
        in
        let expanded = Compile.Of_aggregated.compile_expression ~raise expr in
        let mini_c = Compile.Of_expanded.compile_expression ~raise expanded in
        Mini_c.Formatter.Raw mini_c, []
      | Some entry_point ->
        let expr =
          Compile.Of_typed.apply_to_var
            ~raise
            ~options:options.middle_end
            typed
            entry_point
        in
        let expanded = Compile.Of_aggregated.compile_expression ~raise expr in
        let mini_c = Compile.Of_expanded.compile_expression ~raise expanded in
        let _, o = Compile.Of_mini_c.optimize_for_contract ~raise options mini_c in
        Mini_c.Formatter.Optimized o.body, [] )
