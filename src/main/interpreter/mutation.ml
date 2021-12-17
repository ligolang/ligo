open Simple_utils.Trace
open Errors
module LT = Ligo_interpreter.Types

let mutate_some_value : raise:interpreter_error raise -> Location.t -> Z.t -> LT.value -> Ast_typed.type_expression -> (Ast_typed.expression * LT.mutation) option =
  fun ~raise loc z v v_type ->
    let n = Z.to_int z in
    let expr = Michelson_backend.val_to_ast ~raise ~loc v v_type in
    let module Fuzzer = Fuzz.Ast_typed.Mutator in
    Fuzzer.some_mutate_expression ~n expr

let mutate_all_value : raise:interpreter_error raise -> Location.t -> LT.value -> Ast_typed.type_expression -> (Ast_typed.expression * LT.mutation) list =
  fun ~raise loc v v_type ->
    let expr = Michelson_backend.val_to_ast ~raise ~loc v v_type in
    let module Fuzzer = Fuzz.Ast_typed.Mutator in
    Fuzzer.all_mutate_expression expr

let rec expr_gen : raise:interpreter_error raise -> Ast_typed.type_expression -> Ast_typed.expression QCheck.Gen.t =
  fun ~raise type_expr ->
  let open Ast_typed in
  if is_t_unit type_expr then
    QCheck.Gen.(unit >>= fun _ -> return e_a_unit)
  else if is_t_string type_expr then
    QCheck.Gen.(string >>= fun s -> return (e_a_string (Ligo_string.standard s)))
  else if is_t_address type_expr then
    QCheck.Gen.(int >>= fun i -> return (e_a_address (string_of_int i)))
  else if is_t_int type_expr then
    QCheck.Gen.(small_int >>= fun n ->
                return (e_a_int (Z.of_int n)))
  else if is_t_nat type_expr then
    QCheck.Gen.(small_nat >>= fun n ->
                return (e_a_nat (Z.of_int n)))
  else if is_t_list type_expr then
    match get_t_list type_expr with
    | Some type_expr_r ->
       let rec of_list l = match l with
         | [] -> e_a_nil type_expr_r
         | hd :: tl -> e_a_cons hd (of_list tl) in
       QCheck.Gen.(small_list (expr_gen ~raise type_expr_r) >>= fun l ->
                   return (of_list l))
    | None -> raise.raise (Errors.generic_error type_expr.location "Expected list type")
  else if is_t_set type_expr then
    match get_t_set type_expr with
    | Some type_expr_r ->
       let rec of_list l = match l with
         | [] -> e_a_set_empty type_expr_r
         | hd :: tl -> e_a_set_add hd (of_list tl) in
       QCheck.Gen.(small_list (expr_gen ~raise type_expr_r) >>= fun l ->
                   return (of_list l))
    | None -> raise.raise (Errors.generic_error type_expr.location "Expected set type")
  else if is_t_sum type_expr then
    match get_t_sum type_expr with
    | Some rows ->
       let l = LMap.to_kv_list rows.content in
       let gens = List.map ~f:(fun (label, row_el) ->
                      QCheck.Gen.(expr_gen ~raise row_el.associated_type >>= fun v ->
                                  return (e_a_constructor' ~layout:rows.layout rows.content label v))) l in
       QCheck.Gen.oneof gens
    | None -> raise.raise (Errors.generic_error type_expr.location "Expected sum type")
  else if is_t_record type_expr then
    match get_t_record type_expr with
    | Some rows ->
       let l = LMap.to_kv_list rows.content in
       let gens = List.map ~f:(fun (label, row_el) ->
                       (label, expr_gen ~raise row_el.associated_type)) l in
       let rec gen l : ((label * expression) list) QCheck.Gen.t = match l with
         | [] -> QCheck.Gen.(return [])
         | (label, expr) :: tl -> QCheck.Gen.(expr >>= fun row_el ->
                                              (gen tl >>= fun r ->
                                               return ((label, row_el) :: r))) in
       QCheck.Gen.(gen gens >>= fun l ->
                   return (e_a_record ~layout:rows.layout (LMap.of_list l)))
    | None -> raise.raise (Errors.generic_error type_expr.location "Expected record type")
  else if is_t_map type_expr then
    match get_t_map type_expr with
    | Some (type_expr_k, type_expr_v) ->
       let rec of_list l = match l with
         | [] -> e_a_map_empty type_expr_k type_expr_v
         | (k, v) :: tl -> e_a_map_add k v (of_list tl) in
       QCheck.Gen.(list (pair (expr_gen ~raise type_expr_k) (expr_gen ~raise type_expr_v)) >>= fun l ->
                   return (of_list l))
    | None -> raise.raise (Errors.generic_error type_expr.location "Expected map type")
  else
    raise.raise (Errors.generic_error type_expr.location "Test generator not implemented")
