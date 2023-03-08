open Api_helpers
open Simple_utils
module Helpers = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson
open Ligo_prim
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
    source
    display_format
    michelson_code_format
    michelson_comments
    ()
  =
  let warning_as_error = raw_options.warning_as_error in
  format_result
    ~warning_as_error
    ~display_format
    ~no_colour:raw_options.no_colour
    (Formatter.Michelson_formatter.michelson_format
       michelson_code_format
       michelson_comments)
  @@ fun ~raise ->
  let options =
    let protocol_version =
      Helpers.protocol_to_variant ~raise raw_options.protocol_version
    in
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
    let has_env_comments = has_env_comments michelson_comments in
    Compiler_options.make ~raw_options ~syntax ~protocol_version ~has_env_comments ()
  in
  let Compiler_options.
        { disable_michelson_typechecking = disable_typecheck
        ; views
        ; constants
        ; file_constants
        ; _
        }
    =
    options.backend
  in
  let Compiler_options.{ entry_point; module_; _ } = options.frontend in
  let source =
    match source with
    | File filename -> BuildSystem.Source_input.From_file filename
    | Text (source_code, syntax) ->
      BuildSystem.Source_input.(
        Raw { id = "source_of_text" ^ Syntax.to_ext syntax; code = source_code })
  in
  let Build.{ entrypoint; views } =
    Build.build_contract ~raise ~options entry_point module_ views source
  in
  let code = entrypoint.value in
  let views = List.map ~f:(fun { name; value } -> name, value) views in
  let file_constants = read_file_constants ~raise file_constants in
  let constants = constants @ file_constants in
  Ligo_compile.Of_michelson.build_contract
    ~raise
    ~enable_typed_opt:options.backend.enable_typed_opt
    ~protocol_version:options.middle_end.protocol_version
    ~has_env_comments:options.backend.has_env_comments
    ~disable_typecheck
    ~constants
    code
    views


let expression
    (raw_options : Raw_options.t)
    expression
    init_file
    display_format
    no_colour
    michelson_format
    ()
  =
  let warning_as_error = raw_options.warning_as_error in
  format_result
    ~warning_as_error
    ~display_format
    ~no_colour
    (Formatter.Michelson_formatter.michelson_format michelson_format [])
  @@ fun ~raise ->
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
  let Compiler_options.{ constants; file_constants; _ } = options.backend in
  let file_constants = read_file_constants ~raise file_constants in
  let constants = constants @ file_constants in
  let Build.{ expression; _ } =
    Build.build_expression ~raise ~options syntax expression init_file
  in
  no_comment
  @@
  if without_run
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


let constant (raw_options : Raw_options.t) constants init_file display_format no_colour ()
  =
  let warning_as_error = raw_options.warning_as_error in
  format_result
    ~warning_as_error
    ~display_format
    ~no_colour
    Formatter.Michelson_formatter.michelson_constant_format
  @@ fun ~raise ->
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
  hash, value


let parameter
    (raw_options : Raw_options.t)
    source_file
    expression
    amount
    balance
    sender
    source
    now
    display_format
    no_colour
    michelson_format
    ()
  =
  let warning_as_error = raw_options.warning_as_error in
  format_result
    ~warning_as_error
    ~display_format
    ~no_colour
    (Formatter.Michelson_formatter.michelson_format michelson_format [])
  @@ fun ~raise ->
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
  let Compiler_options.{ constants; file_constants; _ } = options.backend in
  let Compiler_options.{ entry_point = _; module_; _ } = options.frontend in
  let file_constants = read_file_constants ~raise file_constants in
  let constants = constants @ file_constants in
  (* let entry_point = List.map ~f:(Value_var.of_input_var ~loc) entry_point in *)
  let module_path = Build.parse_module_path ~loc module_ in
  let app_typed_prg =
    Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
  in
  let entry_point, app_typed_prg, contract_type =
    Trace.trace ~raise Main_errors.self_ast_typed_tracer
    @@ fun ~raise ->
    let f module_ =
      let main_name, module_ = Self_ast_typed.make_entry_point_program ~raise module_ in
      module_, main_name
    in
    let prg, main_name =
      Self_ast_typed.Helpers.update_module module_path f app_typed_prg
    in
    let prg, main_name, contract_type =
      Self_ast_typed.Helpers.fetch_contract_type ~raise main_name module_path prg
    in
    main_name, prg, contract_type
  in
  let Self_ast_typed.Helpers.{ parameter = parameter_ty; storage = _ } = contract_type in
  let parameter_ty = Checking.untype_type_expression parameter_ty in
  let typed_param =
    Trace.try_with
      (fun ~raise ~catch:_ ->
        Ligo_compile.Utils.type_expression
          ~raise
          ~options
          ~annotation:parameter_ty
          syntax
          expression
          app_typed_prg)
      (fun ~catch:_ _ ->
        let typed_param =
          Ligo_compile.Utils.type_expression
            ~raise
            ~options
            syntax
            expression
            app_typed_prg
        in
        let () =
          Ligo_compile.Of_typed.assert_equal_contract_type
            ~raise
            Check_parameter
            entry_point
            app_typed_prg
            typed_param
        in
        typed_param)
  in
  let typed_param, typed_prg =
    Self_ast_typed.remove_unused_expression typed_param app_typed_prg
  in
  let (_ : Mini_c.meta Run.Michelson.michelson) =
    let aggregated_contract =
      Ligo_compile.Of_typed.apply_to_entrypoint_with_contract_type
        ~raise
        ~options:options.middle_end
        app_typed_prg
        entry_point
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
      typed_prg
      typed_param
  in
  let expanded_param =
    Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_param
  in
  let mini_c_param = Ligo_compile.Of_expanded.compile_expression ~raise expanded_param in
  let compiled_param =
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_param
  in
  let module_ = Self_ast_typed.Helpers.get_module module_path app_typed_prg in
  let () =
    Ligo_compile.Of_typed.assert_equal_contract_type
      ~raise
      Check_parameter
      entry_point
      module_
      typed_param
  in
  let options =
    Run.make_dry_run_options
      ~raise
      ~constants
      { now; amount; balance; sender; source; parameter_ty = None }
  in
  no_comment
    (Run.evaluate_expression ~raise ~options compiled_param.expr compiled_param.expr_ty)


let storage
    (raw_options : Raw_options.t)
    source_file
    expression
    amount
    balance
    sender
    source
    now
    display_format
    no_colour
    michelson_format
    ()
  =
  let warning_as_error = raw_options.warning_as_error in
  format_result
    ~warning_as_error
    ~display_format
    ~no_colour
    (Formatter.Michelson_formatter.michelson_format michelson_format [])
  @@ fun ~raise ->
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
  let Compiler_options.{ entry_point; module_; _ } = options.frontend in
  let Compiler_options.{ constants; file_constants; _ } = options.backend in
  let file_constants = read_file_constants ~raise file_constants in
  let constants = constants @ file_constants in
  let _entry_point = List.map ~f:(Value_var.of_input_var ~loc) entry_point in
  let module_path = Build.parse_module_path ~loc module_ in
  let app_typed_prg =
    Build.qualified_typed ~raise ~options (Build.Source_input.From_file source_file)
  in
  let entry_point, app_typed_prg, contract_type =
    Trace.trace ~raise Main_errors.self_ast_typed_tracer
    @@ fun ~raise ->
    let f module_ =
      let main_name, module_ = Self_ast_typed.make_entry_point_program ~raise module_ in
      module_, main_name
    in
    let prg, main_name =
      Self_ast_typed.Helpers.update_module module_path f app_typed_prg
    in
    let prg, main_name, contract_type =
      Self_ast_typed.Helpers.fetch_contract_type ~raise main_name module_path prg
    in
    main_name, prg, contract_type
  in
  let Self_ast_typed.Helpers.{ parameter = _; storage = storage_ty } = contract_type in
  let storage_ty = Checking.untype_type_expression storage_ty in
  let typed_param =
    Trace.try_with
      (fun ~raise ~catch:_ ->
        Ligo_compile.Utils.type_expression
          ~raise
          ~options
          ~annotation:storage_ty
          syntax
          expression
          app_typed_prg)
      (fun ~catch:_ _ ->
        let typed_param =
          Ligo_compile.Utils.type_expression
            ~raise
            ~options
            syntax
            expression
            app_typed_prg
        in
        let () =
          Ligo_compile.Of_typed.assert_equal_contract_type
            ~raise
            Check_storage
            entry_point
            app_typed_prg
            typed_param
        in
        typed_param)
  in
  let typed_param, typed_prg =
    Self_ast_typed.remove_unused_expression typed_param app_typed_prg
  in
  let (_ : Mini_c.meta Run.Michelson.michelson) =
    let aggregated_contract =
      Ligo_compile.Of_typed.apply_to_entrypoint_with_contract_type
        ~raise
        ~options:options.middle_end
        app_typed_prg
        entry_point
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
      typed_prg
      typed_param
  in
  let expanded_param =
    Ligo_compile.Of_aggregated.compile_expression ~raise aggregated_param
  in
  let mini_c_param = Ligo_compile.Of_expanded.compile_expression ~raise expanded_param in
  let compiled_param =
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c_param
  in
  let module_ = Self_ast_typed.Helpers.get_module module_path app_typed_prg in
  let () =
    Ligo_compile.Of_typed.assert_equal_contract_type
      ~raise
      Check_storage
      entry_point
      module_
      typed_param
  in
  let options =
    Run.make_dry_run_options
      ~raise
      ~constants
      { now; amount; balance; sender; source; parameter_ty = None }
  in
  no_comment
    (Run.evaluate_expression ~raise ~options compiled_param.expr compiled_param.expr_ty)
