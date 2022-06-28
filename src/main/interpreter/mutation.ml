open Simple_utils.Trace
open Errors
module LT = Ligo_interpreter.Types
module LC = Ligo_interpreter.Combinators

let mutate_some_value : raise:interpreter_error raise -> Location.t -> Z.t -> LT.value -> Ast_aggregated.type_expression -> (Ast_aggregated.expression * LT.mutation) option =
  fun ~raise loc z v v_type ->
    let n = Z.to_int z in
    let expr = Michelson_backend.val_to_ast ~raise ~loc v v_type in
    let module Fuzzer = Fuzz.Ast_aggregated.Mutator in
    Fuzzer.some_mutate_expression ~n expr

let mutate_all_value : raise:interpreter_error raise -> Location.t -> LT.value -> Ast_aggregated.type_expression -> (Ast_aggregated.expression * LT.mutation) list =
  fun ~raise loc v v_type ->
    let expr = Michelson_backend.val_to_ast ~raise ~loc v v_type in
    let module Fuzzer = Fuzz.Ast_aggregated.Mutator in
    Fuzzer.all_mutate_expression expr

let rec value_gen : raise:interpreter_error raise -> ?small:bool -> ?addresses:LT.mcontract list -> Ast_aggregated.type_expression -> LT.value QCheck.Gen.t =
  fun ~raise ?(small = true) ?addresses type_expr ->
  let open Ast_aggregated in
  let open LC in
  let addresses = [Michelson_to_value.contract_of_string ~raise "tz1fakefakefakefakefakefakefakcphLA5"] @ match addresses with
    | None -> [] | Some xs -> xs in
  if is_t_unit type_expr then
    QCheck.Gen.(unit >>= fun _ -> return (v_unit ()))
  else if is_t_string type_expr then
    QCheck.Gen.(string >>= fun s -> return (v_string s))
  else if is_t_address type_expr then
    QCheck.Gen.(oneofl addresses >>= fun addr -> return (v_address addr))
  else if is_t_int type_expr then
    QCheck.Gen.((if small then small_int else int) >>= fun n ->
                return (v_int (Z.of_int n)))
  else if is_t_nat type_expr then
    QCheck.Gen.((if small then small_nat else int) >>= fun n ->
                return (v_nat (Z.of_int n)))
  else if is_t_list type_expr then
    match get_t_list type_expr with
    | Some type_value_r ->
       QCheck.Gen.((if small then small_list else list) (value_gen ~raise ~small type_value_r) >>= fun l ->
                   return (v_list l))
    | None -> raise.raise (Errors.generic_error type_expr.location "Expected list type")
  else if is_t_set type_expr then
    match get_t_set type_expr with
    | Some type_value_r ->
       QCheck.Gen.((if small then small_list else list) (value_gen ~raise ~small type_value_r) >>= fun l ->
                   return (v_set l))
    | None -> raise.raise (Errors.generic_error type_expr.location "Expected set type")
  else if is_t_sum type_expr then
    match get_t_sum_opt type_expr with
    | Some rows ->
       let l = LMap.to_kv_list rows.content in
       let gens = List.map ~f:(fun (Label label, row_el) ->
                      QCheck.Gen.(value_gen ~raise ~small row_el.associated_type >>= fun v ->
                                  return (v_ctor label v))) l in
       QCheck.Gen.oneof gens
    | None -> raise.raise (Errors.generic_error type_expr.location "Expected sum type")
  else if is_t_record type_expr then
    match get_t_record_opt type_expr with
    | Some rows ->
       let l = LMap.to_kv_list rows.content in
       let gens = List.map ~f:(fun (Label label, row_el) ->
                       (label, value_gen ~raise ~small row_el.associated_type)) l in
       let rec gen l : ((string * LT.value) list) QCheck.Gen.t = match l with
         | [] -> QCheck.Gen.(return [])
         | (label, expr) :: tl -> QCheck.Gen.(expr >>= fun row_el ->
                                              (gen tl >>= fun r ->
                                               return ((label, row_el) :: r))) in
       QCheck.Gen.(gen gens >>= fun l ->
                   return (v_record l))
    | None -> raise.raise (Errors.generic_error type_expr.location "Expected record type")
  else if is_t_map type_expr then
    match get_t_map type_expr with
    | Some (type_value_k, type_value_v) ->
       QCheck.Gen.((if small then small_list else list) (pair (value_gen ~raise ~small type_value_k) (value_gen ~raise ~small type_value_v)) >>= fun l ->
                   return (v_map l))
    | None -> raise.raise (Errors.generic_error type_expr.location "Expected map type")
  else
    raise.raise (Errors.generic_error type_expr.location @@ Format.asprintf "Test generator not implemented: %a" Ast_aggregated.PP.type_expression type_expr)
