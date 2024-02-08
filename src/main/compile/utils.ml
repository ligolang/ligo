module Trace = Simple_utils.Trace

let to_unified
    ~(raise : (Main_errors.all, Main_warnings.all) Trace.raise)
    ~meta
    (c_unit : Buffer.t)
    file_path
  =
  Of_c_unit.compile ~raise ~meta c_unit file_path


let to_core ~raise ~options ~meta (c_unit : Buffer.t) file_path =
  let unified = to_unified ~raise ~meta c_unit file_path in
  Of_unified.compile ~raise ~options unified


let core_expression_string ~raise ~options syntax expression =
  let meta = Of_source.make_meta_from_syntax syntax in
  let c_unit_exp, _ = Of_source.compile_string_without_preproc expression in
  let unified = Of_c_unit.compile_expression ~raise ~meta c_unit_exp in
  Of_unified.compile_expression ~raise ~options unified


let type_expression_string ~raise ~options syntax expression init_prog =
  let core_exp = core_expression_string ~raise ~options syntax expression in
  Of_core.compile_expression ~raise ~options ~context:init_prog core_exp


let core_program_string ~raise ~options syntax expression =
  let meta = Of_source.make_meta_from_syntax syntax in
  let options = Compiler_options.set_syntax options (Some syntax) in
  let c_unit, _ =
    Of_source.preprocess_string
      ~raise
      ~options:Compiler_options.(options.frontend)
      ~meta
      expression
  in
  let unified = Of_c_unit.compile_string ~raise ~meta c_unit in
  Of_unified.compile ~raise ~options unified


let type_program_string ~raise ~options ?context syntax expression =
  let core = core_program_string ~raise ~options syntax expression in
  let typed = Of_core.typecheck ~raise ~options ?context core in
  typed, core


let type_expression ~raise ~options ?annotation ?wrap_variant syntax expression init_sig =
  let meta = Of_source.make_meta syntax in
  (* TODO: should be computed outside *)
  let c_unit_exp, _ =
    Of_source.preprocess_string
      ~raise
      ~options:options.Compiler_options.frontend
      ~meta
      expression
  in
  let unified = Of_c_unit.compile_expression ~raise ~meta c_unit_exp in
  let core_exp = Of_unified.compile_expression ~raise ~options unified in
  let core_exp =
    match wrap_variant with
    | None -> core_exp
    | Some ctor ->
      Ast_core.e_constructor
        ~loc:core_exp.location
        (Ligo_prim.Label.of_string (String.capitalize ctor))
        core_exp
  in
  let core_exp =
    match annotation with
    | None -> core_exp
    | Some ann -> Ast_core.e_ascription ~loc:core_exp.location core_exp ann
  in
  let typed_exp = Of_core.compile_expression ~raise ~options ~context:init_sig core_exp in
  typed_exp


let type_ty_expression
    ~raise
    ~options
    ?annotation
    ?wrap_variant
    syntax
    expression
    init_sig
  =
  let meta = Of_source.make_meta syntax in
  let c_unit_exp, _ =
    Of_source.preprocess_string
      ~raise
      ~options:options.Compiler_options.frontend
      ~meta
      expression
  in
  c_unit_exp
  |> Of_c_unit.compile_type_expression ~raise ~meta
  |> Of_unified.compile_type_expression ~raise ~options
  |> Of_core.compile_type_expression ~raise ~options ~context:init_sig
  |> Of_typed.compile_type_expression ~raise ~options
  |> Of_expanded.compile_type ~raise
  |> Of_mini_c.compile_type


let compile_contract_input
    ~raise
    ~options
    parameter
    storage
    syntax
    (init_prog : Ast_typed.program)
  =
  let meta = Of_source.extract_meta syntax in
  let (parameter, _), (storage, _) =
    Of_source.compile_contract_input ~raise ~options ~meta parameter storage
  in
  let unified = Of_c_unit.compile_contract_input ~raise ~meta parameter storage in
  let core = Of_unified.compile_expression ~raise ~options unified in
  let typed =
    let context = Ast_typed.to_signature init_prog.pr_module in
    Of_core.compile_expression ~raise ~options ~context core
  in
  let aggregated =
    Of_typed.compile_expression_in_context
      ~raise
      ~options:options.middle_end
      ~self_program:false
      None
      init_prog
      typed
  in
  let expanded = Of_aggregated.compile_expression ~raise aggregated in
  let mini_c = Of_expanded.compile_expression ~raise expanded in
  let compiled = Of_mini_c.compile_expression ~raise ~options mini_c in
  compiled


(* Pretty-printing source and CST from file *)

let buffer_from_path file_path =
  let in_channel = In_channel.create file_path in
  let chan_length = In_channel.length in_channel in
  let int_length =
    match Int64.to_int chan_length with
    | Some int -> int
    | None -> Caml.Sys.max_string_length
  in
  let buffer = Buffer.create int_length in
  let () = Caml.Buffer.add_channel buffer in_channel int_length in
  buffer


let pretty_print ?(preprocess = false) ~raise ~options ~meta file_path =
  let buffer =
    if preprocess
    then fst @@ Of_source.preprocess_file ~raise ~options ~meta file_path
    else buffer_from_path file_path
  in
  Of_c_unit.pretty_print ~preprocess ~raise ~meta buffer file_path


let pretty_print_cst ?(preprocess = true) ~raise ~options ~meta file_path =
  ignore preprocess;
  let buffer =
    if preprocess
    then fst @@ Of_source.preprocess_file ~raise ~options ~meta file_path
    else buffer_from_path file_path
  in
  Of_c_unit.pretty_print_cst ~raise ~meta buffer file_path
