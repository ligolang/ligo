let to_imperative ~raise ~options ~meta (c_unit: Buffer.t) file_path =
  let () = ignore options in
  let imperative = Of_c_unit.compile ~raise ~meta c_unit file_path in
  imperative

let to_sugar ~raise ~options ~meta (c_unit: Buffer.t) file_path =
  let imperative = to_imperative ~raise ~options ~meta c_unit file_path in
  let sugar      = Of_imperative.compile ~raise imperative in
  sugar

let to_core ~raise ~options ~meta (c_unit: Buffer.t) file_path =
  let sugar  = to_sugar ~raise ~options ~meta c_unit file_path in
  let core   = Of_sugar.compile sugar in
  core

let type_file ~raise ~(options : Compiler_options.t) f stx form : Ast_typed.program =
  let meta          = Of_source.extract_meta stx in
  let c_unit,_      = Of_source.preprocess_file ~raise ~options:options.frontend ~meta f in
  let core          = to_core ~raise ~options ~meta c_unit f in
  let typed         = Of_core.typecheck ~raise ~options form core in
  typed

let compile_file ~raise ~options f stx ep =
  let typed    = type_file ~raise ~options f stx @@ Contract ep in
  let aggregated = Of_typed.apply_to_entrypoint_contract ~raise ~options:options.middle_end typed ep in
  let mini_c     = Of_aggregated.compile_expression ~raise aggregated in
  let michelson  = Of_mini_c.compile_contract ~raise ~options mini_c in
  let contract   = Of_michelson.build_contract ~raise michelson in
  contract

let core_expression_string ~raise syntax expression =
  let meta              =  Of_source.make_meta_from_syntax syntax in
  let c_unit_exp, _     = Of_source.compile_string_without_preproc expression in
  let imperative_exp    = Of_c_unit.compile_expression ~raise ~meta c_unit_exp in
  let sugar_exp         = Of_imperative.compile_expression ~raise imperative_exp in
  Of_sugar.compile_expression ~raise sugar_exp

let type_expression_string ~raise ~options syntax expression init_prog =
  let core_exp          = core_expression_string ~raise syntax expression in
  Of_core.compile_expression ~raise ~options ~init_prog core_exp

let core_program_string ~raise ~options syntax expression =
  let meta          = Of_source.make_meta_from_syntax syntax in
  let c_unit, _     = Of_source.preprocess_string ~raise ~options:(Compiler_options.(options.frontend)) ~meta expression in
  let imperative    = Of_c_unit.compile_string ~raise ~meta c_unit in
  let sugar         = Of_imperative.compile ~raise imperative in
  let core          = Of_sugar.compile sugar in
  core

let type_program_string ~raise ~options syntax expression =
  let core          = core_program_string ~raise ~options syntax expression in
  let typed         = Of_core.typecheck ~raise ~options Env core in
  typed,core

let type_expression ~raise ~options syntax expression init_prog =
  let meta              = Of_source.make_meta syntax in (* TODO: should be computed outside *)
  let c_unit_exp, _     = Of_source.preprocess_string ~raise ~options:options.Compiler_options.frontend ~meta expression in
  let imperative_exp    = Of_c_unit.compile_expression ~raise ~meta c_unit_exp in
  let sugar_exp         = Of_imperative.compile_expression ~raise imperative_exp in
  let core_exp          = Of_sugar.compile_expression ~raise sugar_exp in
  let typed_exp         = Of_core.compile_expression ~raise ~options ~init_prog core_exp in
  typed_exp

let compile_contract_input ~raise ~options parameter storage syntax init_prog =
  let meta       = Of_source.extract_meta syntax in
  let (parameter,_),(storage,_) = Of_source.compile_contract_input ~raise ~options ~meta parameter storage in
  let imperative = Of_c_unit.compile_contract_input ~raise ~meta parameter storage in
  let sugar      = Of_imperative.compile_expression ~raise imperative in
  let core       = Of_sugar.compile_expression ~raise sugar in
  let typed      = Of_core.compile_expression ~raise ~options ~init_prog core in
  let aggregated = Of_typed.compile_expression_in_context ~raise ~options:options.middle_end init_prog typed in
  let mini_c     = Of_aggregated.compile_expression ~raise aggregated in
  let compiled   = Of_mini_c.compile_expression ~raise ~options mini_c in
  compiled

(* Pretty-printing source and CST from file *)

let buffer_from_path file_path =
  let in_channel  = In_channel.create file_path in
  let chan_length = In_channel.length in_channel in
  let int_length  = match Int64.to_int chan_length with
                      Some int -> int
                    | None -> Caml.Sys.max_string_length in
  let buffer      = Buffer.create int_length in
  let ()          = Caml.Buffer.add_channel buffer in_channel int_length
  in buffer

let pretty_print ?(preprocess=true) ~raise ~options ~meta file_path =
  let buffer =
    if preprocess then
      fst @@ Of_source.preprocess_file ~raise ~options ~meta file_path
    else buffer_from_path file_path
  in Of_c_unit.pretty_print ~raise ~meta buffer file_path

let pretty_print_cst ?(preprocess=true) ~raise ~options ~meta file_path =
  let buffer =
    if preprocess then
      fst @@ Of_source.preprocess_file ~raise ~options ~meta file_path
    else buffer_from_path file_path
  in Of_c_unit.pretty_print_cst ~raise ~meta buffer file_path
