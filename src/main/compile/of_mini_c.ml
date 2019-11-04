open Trace
open Mini_c
open Tezos_utils

let compile_expression_as_function : expression -> _ result = fun e ->
  let (input , output) = t_unit , e.type_value in
  let%bind body = Compiler.Program.translate_expression e Compiler.Environment.empty in
  let body = Self_michelson.optimize body in
  let body = Michelson.(seq [ i_drop ; body ]) in
  let%bind (input , output) = bind_map_pair Compiler.Type.Ty.type_ (input , output) in
  let open! Compiler.Program in
  ok { input ; output ; body }

let compile_function = fun e ->
  let%bind (input , output) = get_t_function e.type_value in
  let%bind body = get_function e in
  let%bind body = Compiler.Program.translate_function_body body [] input in
  let body = Self_michelson.optimize body in
  let%bind (input , output) = bind_map_pair Compiler.Type.Ty.type_ (input , output) in
  let open! Compiler.Program in
  ok { input ; output ; body }

let compile_expression_as_function_entry = fun program name ->
  let%bind aggregated = aggregate_entry program name true in
  let%bind aggregated = Self_mini_c.all_expression aggregated in
  compile_function aggregated

let compile_function_entry = fun program name ->
  let%bind aggregated = aggregate_entry program name false in
  let%bind aggregated = Self_mini_c.all_expression aggregated in
  compile_function aggregated

let compile_contract_entry = fun program name ->
  let%bind aggregated = aggregate_entry program name false in
  let%bind aggregated = Self_mini_c.all_expression aggregated in
  let%bind compiled = compile_function aggregated in
  let%bind (param_ty , storage_ty) =
    let%bind fun_ty = get_t_function aggregated.type_value in
    Mini_c.get_t_pair (fst fun_ty)
  in
  let%bind param_michelson = Compiler.Type.type_ param_ty in
  let%bind storage_michelson = Compiler.Type.type_ storage_ty in
  let contract = Michelson.contract param_michelson storage_michelson compiled.body in
  ok contract


let uncompile_value : Proto_alpha_utils.Memory_proto_alpha.X.ex_typed_value -> value result = fun x ->
  Compiler.Uncompiler.translate_value x
