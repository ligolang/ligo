open Trace

let to_c_unit ~options ~meta file_path =
  let* c_unit  = Of_source.compile ~options ~meta file_path in
  ok @@ c_unit

let to_imperative ~options ~meta (c_unit: Buffer.t) file_path =
  let () = ignore options in
  let* imperative = Of_c_unit.compile ~meta c_unit file_path in
  ok @@ imperative

let to_sugar ~options ~meta (c_unit: Buffer.t) file_path =
  let* imperative = to_imperative ~options ~meta c_unit file_path in
  let* sugar      = Of_imperative.compile imperative in
  ok @@ sugar

let to_core ~options ~meta (c_unit: Buffer.t) file_path =
  let* sugar  = to_sugar ~options ~meta c_unit file_path in
  let* core   = Of_sugar.compile sugar in
  ok @@ core

let type_file ~options f stx form : (Ast_typed.module_fully_typed * Ast_typed.environment, _) result =
  let* meta          = Of_source.extract_meta stx f in
  let* c_unit,_      = Of_source.compile ~options ~meta f in
  let* core          = to_core ~options ~meta c_unit f in
  let* inferred      = Of_core.infer ~options core in
  let* typed,e       = Of_core.typecheck ~options form inferred in
  ok @@ (typed,e)

let to_mini_c ~options f stx env =
  let* typed,_  = type_file ~options f stx env in
  let* mini_c     = Of_typed.compile typed in
  ok @@ mini_c

let compile_file ~options f stx ep =
  let* typed,_    = type_file ~options f stx @@ Contract ep in
  let* mini_c     = Of_typed.compile typed in
  let* michelson  = Of_mini_c.aggregate_and_compile_contract ~options mini_c ep in
  let* contract   = Of_michelson.build_contract michelson in
  ok @@ contract

let type_expression_string ~options syntax expression env =
  let {infer ; _} : Compiler_options.t = options in
  let* meta              = Of_source.make_meta_from_syntax syntax in
  let* c_unit_exp, _     = Of_source.compile_string_without_preproc expression in
  let* imperative_exp    = Of_c_unit.compile_expression ~meta c_unit_exp in
  let* sugar_exp         = Of_imperative.compile_expression imperative_exp in
  let* core_exp          = Of_sugar.compile_expression sugar_exp in
  let* typed_exp,e       = Of_core.compile_expression ~infer ~env core_exp in
  ok @@ (typed_exp,e)

let type_contract_string ~options syntax expression env =
  let* meta          = Of_source.make_meta_from_syntax syntax in
  let* c_unit, _     = Of_source.compile_string_without_preproc expression in
  let* imperative    = Of_c_unit.compile_string ~meta c_unit in
  let* sugar         = Of_imperative.compile imperative in
  let* core          = Of_sugar.compile sugar in
  let* inferred      = Of_core.infer ~options:{options with init_env = env} core in
  let* typed,e       = Of_core.typecheck ~options:{options with init_env = env} Env inferred in
  ok @@ (typed,core,e)

let type_expression ~options source_file syntax expression env =
  let* meta              = Of_source.make_meta syntax source_file in
  let* c_unit_exp, _     = Of_source.compile_string ~options ~meta expression in
  let* imperative_exp    = Of_c_unit.compile_expression ~meta c_unit_exp in
  let* sugar_exp         = Of_imperative.compile_expression imperative_exp in
  let* core_exp          = Of_sugar.compile_expression sugar_exp in
  let* typed_exp,e       = Of_core.compile_expression ~env core_exp in
  ok @@ (typed_exp,e)

let expression_to_mini_c ~options source_file syntax expression env =
  let* (typed_exp,_)  = type_expression ~options source_file syntax expression env in
  let* mini_c_exp     = Of_typed.compile_expression typed_exp in
  ok @@ mini_c_exp

let compile_expression ~options source_file syntax expression env =
  let* mini_c_exp = expression_to_mini_c ~options source_file syntax expression env in
  let* compiled   = Of_mini_c.compile_expression ~options mini_c_exp in
  ok @@ compiled

let compile_and_aggregate_expression ~options source_file syntax expression env mini_c_prg =
  let* mini_c_exp = expression_to_mini_c ~options source_file syntax expression env in
  let* compiled   = Of_mini_c.aggregate_and_compile_expression ~options mini_c_prg mini_c_exp in
  ok @@ compiled

let compile_storage ~options storage input source_file syntax env mini_c_prg =
  let* meta       = Of_source.extract_meta syntax source_file in
  let* (storage,_),(input,_) = Of_source.compile_contract_input ~options ~meta storage input in
  let* imperative = Of_c_unit.compile_contract_input ~meta storage input in
  let* sugar      = Of_imperative.compile_expression imperative in
  let* core       = Of_sugar.compile_expression sugar in
  let* typed,_    = Of_core.compile_expression ~env core in
  let* mini_c     = Of_typed.compile_expression typed in
  let* compiled   = Of_mini_c.aggregate_and_compile_expression ~options mini_c_prg mini_c in
  ok @@ compiled

let pretty_print ~options ~meta file_path =
  let* c_unit,_ = to_c_unit ~options ~meta file_path in
  Of_c_unit.pretty_print ~meta c_unit file_path

let pretty_print_cst ~options ~meta file_path =
  let* c_unit,_ = to_c_unit ~options ~meta file_path in
  Of_c_unit.pretty_print_cst ~meta c_unit file_path
