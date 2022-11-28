open Ast_aggregated
open Ligo_prim.Constant
open Trace

let rec list_expression ~raise : expression -> expression list =
 fun expr ->
  let self = list_expression ~raise in
  match expr.expression_content with
  | E_constant { cons_name = C_CONS; arguments = [ hd; tl ] } -> hd :: self tl
  | E_constant { cons_name = C_LIST_EMPTY; arguments = [] } -> []
  | E_constant { cons_name = C_NIL; arguments = [] } -> []
  | _ ->
    raise.error
      (Errors.corner_case
      @@ Format.asprintf "Not a list expression: %a" PP.expression expr)


let pair_expression ~raise : expression -> expression * expression =
 fun expr ->
  match expr.expression_content with
  | E_constant { cons_name = C_PAIR; arguments = [ l; r ] } -> l, r
  | E_record p ->
    trace_option ~raise (Errors.corner_case "Not a pair?")
    @@ List.to_pair
    @@ Ligo_prim.Record.LMap.to_list p
  | _ ->
    raise.error
      (Errors.corner_case
      @@ Format.asprintf "Not a pair expression: %a" PP.expression expr)


let expression ~raise : expression -> expression =
 fun expr ->
  match expr.expression_content with
  | E_constant { cons_name = cst; arguments = lst } ->
    (match cst with
    | C_MAP_LITERAL ->
      let elt =
        trace_option ~raise (Errors.bad_single_arity cst expr) @@ List.to_singleton lst
      in
      let k_ty, v_ty =
        trace_option ~raise (Errors.bad_map_param_type cst expr)
        @@ get_t_map expr.type_expression
      in
      let lst = list_expression ~raise elt in
      let aux p = pair_expression ~raise p in
      let pairs = List.map ~f:aux lst in
      List.fold_right
        pairs
        ~f:(fun (k, v) m -> e_a_map_add k v m)
        ~init:(e_a_map_empty k_ty v_ty)
    | C_BIG_MAP_LITERAL ->
      let elt =
        trace_option ~raise (Errors.bad_single_arity cst expr) @@ List.to_singleton lst
      in
      let k_ty, v_ty =
        trace_option ~raise (Errors.bad_map_param_type cst expr)
        @@ get_t_big_map expr.type_expression
      in
      let lst = list_expression ~raise elt in
      let aux p = pair_expression ~raise p in
      let pairs = List.map ~f:aux lst in
      List.fold_right
        pairs
        ~f:(fun (k, v) m -> e_a_big_map_add k v m)
        ~init:(e_a_big_map_empty k_ty v_ty)
    | C_SET_LITERAL ->
      let elt =
        trace_option ~raise (Errors.bad_single_arity cst expr) @@ List.to_singleton lst
      in
      let v_ty =
        trace_option ~raise (Errors.bad_set_param_type cst expr)
        @@ get_t_set expr.type_expression
      in
      let lst = list_expression ~raise elt in
      List.fold_right lst ~f:(fun v s -> e_a_set_add v s) ~init:(e_a_set_empty v_ty)
    | _ -> expr)
  | _ -> expr
