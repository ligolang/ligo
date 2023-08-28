open Simple_utils
module Helpers = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson
module Raw_options = Compiler_options.Raw_options
module Formatter = Ligo_formatter

let loc = Location.dummy

let no_comment node =
  Tezos_micheline.Micheline.(
    inject_locations (fun _ -> Mini_c.dummy_meta) (strip_locations node))


let has_env_comments michelson_comments =
  Option.is_some
    (List.find michelson_comments ~f:(function
        | `Location | `Env -> true
        | _ -> false))


let read_file_constants ~raise file_constants =
  match file_constants with
  | None -> []
  | Some fn ->
    (try
       let buf = In_channel.read_all fn in
       let json = Yojson.Basic.from_string buf in
       json |> Yojson.Basic.Util.to_list |> List.map ~f:Yojson.Basic.Util.to_string
     with
    | Sys_error _ -> raise.Trace.error (`Main_cannot_open_global_constants fn)
    | Yojson.Json_error s ->
      raise.Trace.error (`Main_cannot_parse_global_constants (fn, s)))


module Path = struct
  type t = string
end

type source =
  | Text of string * Syntax_types.t
  | File of Path.t

let contract
    (raw_options : Raw_options.t)
    entry_point
    source
    michelson_code_format
    michelson_comments
    views
  =
  ( Formatter.Michelson_formatter.michelson_format michelson_code_format michelson_comments
  , fun ~raise ->
      let syntax =
        match source with
        | Text (_source_code, syntax) -> syntax
        | File source_file ->
          Syntax.of_string_opt
            ~raise
            ~support_pascaligo:raw_options.deprecated
            (Syntax_name raw_options.syntax)
            (Some source_file)
      in
      Deprecation.view_cli ~raise syntax views;
      Deprecation.entry_cli ~raise syntax entry_point;
      let protocol_version =
        Helpers.protocol_to_variant ~raise raw_options.protocol_version
      in
      let options =
        let has_env_comments = has_env_comments michelson_comments in
        Compiler_options.make ~raw_options ~syntax ~protocol_version ~has_env_comments ()
      in
      let Compiler_options.
            { disable_michelson_typechecking = disable_typecheck
            ; constants
            ; file_constants
            ; _
            }
        =
        options.backend
      in
      let Compiler_options.{ module_; _ } = options.frontend in
      let source_filename =
        match source with
        | Text (_source_code, _syntax) -> ""
        | File source_file -> source_file
      in
      let source =
        match source with
        | File filename -> BuildSystem.Source_input.From_file filename
        | Text (source_code, syntax) ->
          BuildSystem.Source_input.(
            Raw { id = "source_of_text" ^ Syntax.to_ext syntax; code = source_code })
      in
      let Build.{ entrypoint; views } =
        Build.build_contract ~raise ~options module_ source
      in
      let code = entrypoint.value in
      let views = List.map ~f:(fun { name; value } -> name, value) views in
      let file_constants = read_file_constants ~raise file_constants in
      let constants = constants @ file_constants in
      let compiled_contract =
        Ligo_compile.Of_michelson.build_contract
          ~raise
          ~enable_typed_opt:options.backend.enable_typed_opt
          ~protocol_version:options.middle_end.protocol_version
          ~has_env_comments:options.backend.has_env_comments
          ~disable_typecheck
          ~constants
          code
          views
      in
      let compiled_contract_size =
        Ligo_compile.Of_michelson.measure ~raise compiled_contract
      in
      let contract_discriminant = Int.to_string (String.hash source_filename) in
      let analytics_input_compilation_size =
        Analytics.
          { group =
              Gauge_compilation_size
                { contract_discriminant
                ; syntax = Syntax.to_string syntax
                ; protocol = Environment.Protocols.variant_to_string protocol_version
                }
          ; metric_value = Int.to_float compiled_contract_size
          }
      in
      compiled_contract, [ analytics_input_compilation_size ] )


let expression (raw_options : Raw_options.t) expression init_file michelson_format =
  ( Formatter.Michelson_formatter.michelson_format michelson_format []
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt
          ~support_pascaligo:raw_options.deprecated
          ~raise
          (Syntax_name raw_options.syntax)
          init_file
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
      let Compiler_options.{ without_run; function_body; _ } = options.backend in
      let Compiler_options.{ constants; file_constants; _ } = options.backend in
      let file_constants = read_file_constants ~raise file_constants in
      let constants = constants @ file_constants in
      let Build.{ expression; _ } =
        Build.build_expression ~raise ~options syntax expression init_file
      in
      let mich =
        no_comment
        @@
        if without_run || function_body
        then Run.clean_expression expression.expr
        else (
          let options =
            Run.make_dry_run_options
              ~raise
              ~constants
              { now = None
              ; amount = "0"
              ; balance = "0"
              ; sender = None
              ; source = None
              ; parameter_ty = None
              }
          in
          Run.evaluate_expression ~raise ~options expression.expr expression.expr_ty)
      in
      mich, [] )


let constant (raw_options : Raw_options.t) constants init_file =
  ( Formatter.Michelson_formatter.michelson_constant_format
  , fun ~raise ->
      let syntax =
        Syntax.of_string_opt
          ~support_pascaligo:raw_options.deprecated
          ~raise
          (Syntax_name raw_options.syntax)
          init_file
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
      let Compiler_options.{ without_run; _ } = options.backend in
      let Build.{ expression; _ } =
        Build.build_expression ~raise ~options syntax constants init_file
      in
      let hash, value =
        if without_run
        then Run.clean_constant ~raise expression.expr
        else Run.evaluate_constant ~raise expression.expr expression.expr_ty
      in
      (hash, value), [] )


let typed_contract_and_expression
    ~raise
    ~(options : Compiler_options.t)
    ~syntax
    ~source_file
    ?entrypoint_ctor
    ~expression
    check_type
  =
  let Compiler_options.{ constants; file_constants; _ } = options.backend in
  let Compiler_options.{ module_; _ } = options.frontend in
  let file_constants = read_file_constants ~raise file_constants in
  let constants = constants @ file_constants in
  let module_path = Build.parse_module_path ~loc module_ in
  let typed_prg =
    Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
  in
  let app_typed_prg =
    Trace.trace ~raise Main_errors.self_ast_typed_tracer
    @@ Self_ast_typed.all_program typed_prg
  in
  let _, ctrct_sig =
    let sig_ = Ast_typed.to_extended_signature typed_prg in
    Trace.trace_option
      ~raise
      (Main_errors.self_ast_typed_tracer @@ `Self_ast_typed_not_a_contract module_)
      (Ast_typed.get_contract_signature sig_ module_path)
  in
  let ctrct_sig =
    { ctrct_sig with
      parameter =
        (* to handle single entrypoints:
        parameter type for single entry-point contracts such as
        `[@entry] let main (p:p) (s:s) = ...`
        are now compiled to `| Main of p`
        This representation do not yet persist up until the michelson representation
        due to "optimisations" :  `| Main of p` compiles to `p`
        When using `compile parameter /path/to/file` without using the -e CLI option,
        we assume the given expression is of type `p` and not `| Main of p`   
        *)
        (match
           Option.map (Ast_typed.get_t_sum ctrct_sig.parameter) ~f:Ast_typed.Row.to_alist
         with
        | Some [ (_, ty) ] -> ty
        | _ -> ctrct_sig.parameter)
    }
  in
  let app_typed_sig = Ast_typed.to_signature app_typed_prg.pr_module in
  let annotation =
    match check_type with
    | Runned_result.Check_storage -> Checking.untype_type_expression ctrct_sig.storage
    | Check_parameter -> Checking.untype_type_expression ctrct_sig.parameter
  in
  let typed_expr =
    Trace.try_with
      (fun ~raise ~catch:_ ->
        Ligo_compile.Utils.type_expression
          ~raise
          ~options
          ?wrap_variant:entrypoint_ctor
          ~annotation
          syntax
          expression
          app_typed_sig)
      (fun ~catch:_ _ ->
        let typed_param =
          Ligo_compile.Utils.type_expression
            ~raise
            ~options
            ~annotation
            ?wrap_variant:entrypoint_ctor
            syntax
            expression
            app_typed_sig
        in
        let () =
          Ligo_compile.Of_typed.assert_equal_contract_type
            ~raise
            check_type
            ctrct_sig
            typed_param
        in
        typed_param)
  in
  typed_expr, app_typed_prg, constants, ctrct_sig, module_path


let parameter
    (raw_options : Raw_options.t)
    (parameter_entrypoint_opt : string option)
    source_file
    expression
    amount
    balance
    sender
    source
    now
    michelson_format
  =
  ( Formatter.Michelson_formatter.michelson_format michelson_format []
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
      let options =
        Compiler_options.make
          ~raw_options
          ~syntax
          ~protocol_version
          ~has_env_comments:false
          ()
      in
      let typed_param, app_typed_prg, constants, contract_type, module_path =
        typed_contract_and_expression
          ~raise
          ~options
          ~syntax
          ~source_file
          ~expression
          ?entrypoint_ctor:parameter_entrypoint_opt
          Check_parameter
      in
      let (_ : Mini_c.meta Run.Michelson.michelson) =
        let aggregated_contract =
          Ligo_compile.Of_typed.apply_to_entrypoint_with_contract_type
            ~raise
            ~options:options.middle_end
            app_typed_prg
            module_path
            contract_type
        in
        let expanded =
          Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_contract
        in
        let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
        let michelson = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
        (* fails if the given entry point is not a valid contract *)
        Ligo_compile.Of_michelson.build_contract
          ~raise
          ~enable_typed_opt:options.backend.enable_typed_opt
          ~protocol_version
          ~constants
          michelson
          []
      in
      let aggregated_param =
        Ligo_compile.Of_typed.compile_expression_in_context
          ~raise
          ~options:options.middle_end
          None
          app_typed_prg
          typed_param
      in
      let expanded_param =
        Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_param
      in
      let mini_c_param =
        Ligo_compile.Of_expanded.compile_expression ~raise expanded_param
      in
      let compiled_param =
        Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_param
      in
      let options =
        Run.make_dry_run_options
          ~raise
          ~constants
          { now; amount; balance; sender; source; parameter_ty = None }
      in
      ( no_comment
          (Run.evaluate_expression
             ~raise
             ~options
             compiled_param.expr
             compiled_param.expr_ty)
      , [] ) )


let storage
    (raw_options : Raw_options.t)
    entry_point
    source_file
    expression
    amount
    balance
    sender
    source
    now
    michelson_format
  =
  ( Formatter.Michelson_formatter.michelson_format michelson_format []
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
      Deprecation.entry_cli ~raise syntax entry_point;
      let options =
        Compiler_options.make
          ~raw_options
          ~syntax
          ~protocol_version
          ~has_env_comments:false
          ()
      in
      let typed_store, app_typed_prg, constants, contract_type, module_path =
        typed_contract_and_expression
          ~raise
          ~options
          ~syntax
          ~source_file
          ~expression
          Check_storage
      in
      let (_ : Mini_c.meta Run.Michelson.michelson) =
        let aggregated_contract =
          Ligo_compile.Of_typed.apply_to_entrypoint_with_contract_type
            ~raise
            ~options:options.middle_end
            app_typed_prg
            module_path
            contract_type
        in
        let expanded =
          Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_contract
        in
        let mini_c = Ligo_compile.Of_expanded.compile_expression ~raise expanded in
        let michelson = Ligo_compile.Of_mini_c.compile_contract ~raise ~options mini_c in
        (* fails if the given entry point is not a valid contract *)
        Ligo_compile.Of_michelson.build_contract
          ~raise
          ~enable_typed_opt:options.backend.enable_typed_opt
          ~protocol_version
          ~constants
          michelson
          []
      in
      let aggregated_param =
        Ligo_compile.Of_typed.compile_expression_in_context
          ~raise
          ~options:options.middle_end
          None
          app_typed_prg
          typed_store
      in
      let expanded_param =
        Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_param
      in
      let mini_c_param =
        Ligo_compile.Of_expanded.compile_expression ~raise expanded_param
      in
      let compiled_param =
        Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_param
      in
      let michelson_value =
        let options =
          Run.make_dry_run_options
            ~raise
            ~constants
            { now; amount; balance; sender; source; parameter_ty = None }
        in
        Run.evaluate_expression ~raise ~options compiled_param.expr compiled_param.expr_ty
      in
      let () =
        Run.Checks.storage
          ~raise
          ~options
          ~loc:typed_store.location
          ~type_:compiled_param.expr_ty
          michelson_value
      in
      no_comment michelson_value, [] )
