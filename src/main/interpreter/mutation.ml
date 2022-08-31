open Simple_utils.Trace
open Errors
module LT = Ligo_interpreter.Types
module LC = Ligo_interpreter.Combinators
open Ligo_prim

let get_syntax ~raise syntax loc =
  match syntax with
  | Some syntax -> syntax
  | None -> match Location.get_file loc with
            | None -> raise.error (Errors.generic_error loc "Could not detect syntax")
            | Some r -> let file = r # file in
                        let syntax = Simple_utils.Trace.to_stdlib_result (Syntax.of_string_opt (Syntax_types.Syntax_name "auto") (Some file)) in
                        match syntax with
                        | Ok (r,_) -> r
                        | Error _ -> raise.error (Errors.generic_error loc "Could not detect syntax")


let mutate_some_contract : raise:(interpreter_error,_) raise -> ?syntax:_ -> Z.t -> Ast_aggregated.expression -> (Ast_aggregated.expression * LT.mutation) option =
  fun ~raise ?syntax z main ->
  let n = Z.to_int z in
  let module Fuzzer = Fuzz.Ast_aggregated.Mutator in
  let f (e, (l, m)) =
    let syntax = get_syntax ~raise syntax l in
    let s = Fuzz.Ast_aggregated.expression_to_string ~syntax m in
    (e, (l, m, s)) in
  Option.map ~f @@ Fuzzer.some_mutate_expression ~n main

let mutate_some_value : raise:(interpreter_error,_) raise -> ?syntax:_ -> Location.t -> Z.t -> LT.value -> Ast_aggregated.type_expression -> (Ast_aggregated.expression * LT.mutation) option =
  fun ~raise ?syntax loc z v v_type ->
    let n = Z.to_int z in
    let expr = Michelson_backend.val_to_ast ~raise ~loc v v_type in
    let module Fuzzer = Fuzz.Ast_aggregated.Mutator in
    let f (e, (loc, m)) =
      let syntax = get_syntax ~raise syntax loc in
      let s = Fuzz.Ast_aggregated.expression_to_string ~syntax m in
      (e, (loc, m, s)) in
    Option.map ~f @@ Fuzzer.some_mutate_expression ~n expr

let mutate_all_value : raise:(interpreter_error,_) raise -> ?syntax:_ -> Location.t -> LT.value -> Ast_aggregated.type_expression -> (Ast_aggregated.expression * LT.mutation) list =
  fun ~raise ?syntax loc v v_type ->
    let expr = Michelson_backend.val_to_ast ~raise ~loc v v_type in
    let module Fuzzer = Fuzz.Ast_aggregated.Mutator in
    let f (e, (loc, m)) =
      let syntax = get_syntax ~raise syntax loc in
      let s = Fuzz.Ast_aggregated.expression_to_string ~syntax m in
      (e, (loc, m, s)) in
    List.map ~f @@ Fuzzer.all_mutate_expression expr

let rec value_gen : raise:(interpreter_error, _) raise -> ?small:bool -> ?known_addresses:LT.mcontract list -> Ast_aggregated.type_expression -> LT.value QCheck.Gen.t =
  fun ~raise ?(small = true) ?known_addresses type_expr ->
  let open Ast_aggregated in
  let open LC in
  let addresses = [Michelson_to_value.contract_of_string ~raise "tz1fakefakefakefakefakefakefakcphLA5"] @ match known_addresses with
    | None -> [] | Some xs -> xs in
  if is_t_unit type_expr then
    QCheck.Gen.(unit >>= fun _ -> return (v_unit ()))
  else if is_t_string type_expr then
    QCheck.Gen.((if small then string_size ~gen:printable small_nat else string_printable) >>= fun s -> return (v_string s))
  else if is_t_bytes type_expr then
    QCheck.Gen.((if small then map Bytes.of_string small_string else map Bytes.of_string string) >>= fun s -> return (v_bytes s))
  else if is_t_address type_expr then
    QCheck.Gen.(oneofl addresses >>= fun addr -> return (v_address addr))
  else if is_t_int type_expr then
    QCheck.Gen.((if small then small_int else int) >>= fun n ->
                return (v_int (Z.of_int n)))
  else if is_t_nat type_expr then
    QCheck.Gen.((if small then small_nat else nat) >>= fun n ->
                return (v_nat (Z.of_int n)))
  else if is_t_tez type_expr then
    QCheck.Gen.((if small then small_nat else nat) >>= fun n ->
                return (v_mutez (Z.of_int n)))
  else if is_t_list type_expr then
    match get_t_list type_expr with
    | Some type_value_r ->
       QCheck.Gen.((if small then small_list else list) (value_gen ~raise ~small type_value_r) >>= fun l ->
                   return (v_list l))
    | None -> raise.error (Errors.generic_error type_expr.location "Expected list type")
  else if is_t_set type_expr then
    match get_t_set type_expr with
    | Some type_value_r ->
       QCheck.Gen.((if small then small_list else list) (value_gen ~raise ~small type_value_r) >>= fun l ->
                   return (v_set l))
    | None -> raise.error (Errors.generic_error type_expr.location "Expected set type")
  else if is_t_sum type_expr then
    match get_t_sum_opt type_expr with
    | Some rows ->
       let l = Record.LMap.to_kv_list rows.fields in
       let gens = List.map ~f:(fun (Label label, row_el) ->
                      QCheck.Gen.(value_gen ~raise ~small row_el.associated_type >>= fun v ->
                                  return (v_ctor label v))) l in
       QCheck.Gen.oneof gens
    | None -> raise.error (Errors.generic_error type_expr.location "Expected sum type")
  else if is_t_record type_expr then
    match get_t_record_opt type_expr with
    | Some rows ->
       let l = Record.LMap.to_kv_list rows.fields in
       let gens = List.map ~f:(fun (Label label, row_el) ->
                       (label, value_gen ~raise ~small row_el.associated_type)) l in
       let rec gen l : ((string * LT.value) list) QCheck.Gen.t = match l with
         | [] -> QCheck.Gen.(return [])
         | (label, expr) :: tl -> QCheck.Gen.(expr >>= fun row_el ->
                                              (gen tl >>= fun r ->
                                               return ((label, row_el) :: r))) in
       QCheck.Gen.(gen gens >>= fun l ->
                   return (v_record l))
    | None -> raise.error (Errors.generic_error type_expr.location "Expected record type")
  else if is_t_map type_expr then
    match get_t_map type_expr with
    | Some (type_value_k, type_value_v) ->
       QCheck.Gen.((if small then small_list else list) (pair (value_gen ~raise ~small type_value_k) (value_gen ~raise ~small type_value_v)) >>= fun l ->
                   return (v_map l))
    | None -> raise.error (Errors.generic_error type_expr.location "Expected map type")
  else if is_t_big_map type_expr then
    match get_t_big_map type_expr with
    | Some (type_value_k, type_value_v) ->
       QCheck.Gen.((if small then small_list else list) (pair (value_gen ~raise ~small type_value_k) (value_gen ~raise ~small type_value_v)) >>= fun l ->
                   return (v_map l))
    | None -> raise.error (Errors.generic_error type_expr.location "Expected big_map type")
  else
    raise.error (Errors.generic_error type_expr.location @@ Format.asprintf "Generator for type %a is not implemented. For now, only unit, string, bytes, address, int, nat, tez, records, sums, lists, sets, maps and big_maps can be generated." Ast_aggregated.PP.type_expression type_expr)
