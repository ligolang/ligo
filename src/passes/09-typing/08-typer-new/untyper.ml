open Trace
open Typer_common.Errors

module I = Ast_core
module O = Ast_typed
open O.Combinators

let unconvert_type_constant : O.type_constant -> I.type_constant = function
    | TC_unit -> TC_unit
    | TC_string -> TC_string
    | TC_bytes -> TC_bytes
    | TC_nat -> TC_nat
    | TC_int -> TC_int
    | TC_mutez -> TC_mutez
    | TC_operation -> TC_operation
    | TC_address -> TC_address
    | TC_key -> TC_key
    | TC_key_hash -> TC_key_hash
    | TC_chain_id -> TC_chain_id
    | TC_signature -> TC_signature
    | TC_timestamp -> TC_timestamp

let unconvert_constant' : O.constant' -> I.constant' = function
  | C_INT -> C_INT
  | C_UNIT -> C_UNIT
  | C_NIL -> C_NIL
  | C_NOW -> C_NOW
  | C_IS_NAT -> C_IS_NAT
  | C_SOME -> C_SOME
  | C_NONE -> C_NONE
  | C_ASSERTION -> C_ASSERTION
  | C_ASSERT_INFERRED -> C_ASSERT_INFERRED
  | C_FAILWITH -> C_FAILWITH
  | C_UPDATE -> C_UPDATE
  (* Loops *)
  | C_ITER -> C_ITER
  | C_FOLD_WHILE -> C_FOLD_WHILE
  | C_FOLD_CONTINUE -> C_FOLD_CONTINUE
  | C_FOLD_STOP -> C_FOLD_STOP
  | C_LOOP_LEFT -> C_LOOP_LEFT
  | C_LOOP_CONTINUE -> C_LOOP_CONTINUE
  | C_LOOP_STOP -> C_LOOP_STOP
  | C_FOLD -> C_FOLD
  (* MATH *)
  | C_NEG -> C_NEG
  | C_ABS -> C_ABS
  | C_ADD -> C_ADD
  | C_SUB -> C_SUB
  | C_MUL -> C_MUL
  | C_DIV -> C_DIV
  | C_EDIV -> C_EDIV
  | C_MOD -> C_MOD
  (* LOGIC *)
  | C_NOT -> C_NOT
  | C_AND -> C_AND
  | C_OR -> C_OR
  | C_XOR -> C_XOR
  | C_LSL -> C_LSL
  | C_LSR -> C_LSR
  (* COMPARATOR *)
  | C_EQ -> C_EQ
  | C_NEQ -> C_NEQ
  | C_LT -> C_LT
  | C_GT -> C_GT
  | C_LE -> C_LE
  | C_GE -> C_GE
  (* Bytes/ String *)
  | C_SIZE -> C_SIZE
  | C_CONCAT -> C_CONCAT
  | C_SLICE -> C_SLICE
  | C_BYTES_PACK -> C_BYTES_PACK
  | C_BYTES_UNPACK -> C_BYTES_UNPACK
  | C_CONS -> C_CONS
  (* Pair *)
  | C_PAIR -> C_PAIR
  | C_CAR -> C_CAR
  | C_CDR -> C_CDR
  | C_LEFT -> C_LEFT
  | C_RIGHT -> C_RIGHT
  (* Set *)
  | C_SET_EMPTY -> C_SET_EMPTY
  | C_SET_LITERAL -> C_SET_LITERAL
  | C_SET_ADD -> C_SET_ADD
  | C_SET_REMOVE -> C_SET_REMOVE
  | C_SET_ITER -> C_SET_ITER
  | C_SET_FOLD -> C_SET_FOLD
  | C_SET_MEM -> C_SET_MEM
  (* List *)
  | C_LIST_EMPTY -> C_LIST_EMPTY
  | C_LIST_LITERAL -> C_LIST_LITERAL
  | C_LIST_ITER -> C_LIST_ITER
  | C_LIST_MAP -> C_LIST_MAP
  | C_LIST_FOLD -> C_LIST_FOLD
  (* Maps *)
  | C_MAP -> C_MAP
  | C_MAP_EMPTY -> C_MAP_EMPTY
  | C_MAP_LITERAL -> C_MAP_LITERAL
  | C_MAP_GET -> C_MAP_GET
  | C_MAP_GET_FORCE -> C_MAP_GET_FORCE
  | C_MAP_ADD -> C_MAP_ADD
  | C_MAP_REMOVE -> C_MAP_REMOVE
  | C_MAP_UPDATE -> C_MAP_UPDATE
  | C_MAP_ITER -> C_MAP_ITER
  | C_MAP_MAP -> C_MAP_MAP
  | C_MAP_FOLD -> C_MAP_FOLD
  | C_MAP_MEM -> C_MAP_MEM
  | C_MAP_FIND -> C_MAP_FIND
  | C_MAP_FIND_OPT -> C_MAP_FIND_OPT
  (* Big Maps *)
  | C_BIG_MAP -> C_BIG_MAP
  | C_BIG_MAP_EMPTY -> C_BIG_MAP_EMPTY
  | C_BIG_MAP_LITERAL -> C_BIG_MAP_LITERAL
  (* Crypto *)
  | C_SHA256 -> C_SHA256
  | C_SHA512 -> C_SHA512
  | C_BLAKE2b -> C_BLAKE2b
  | C_HASH -> C_HASH
  | C_HASH_KEY -> C_HASH_KEY
  | C_CHECK_SIGNATURE -> C_CHECK_SIGNATURE
  | C_CHAIN_ID -> C_CHAIN_ID
  (* Blockchain *)
  | C_CALL -> C_CALL
  | C_CONTRACT -> C_CONTRACT
  | C_CONTRACT_OPT -> C_CONTRACT_OPT
  | C_CONTRACT_ENTRYPOINT -> C_CONTRACT_ENTRYPOINT
  | C_CONTRACT_ENTRYPOINT_OPT -> C_CONTRACT_ENTRYPOINT_OPT
  | C_AMOUNT -> C_AMOUNT
  | C_BALANCE -> C_BALANCE
  | C_SOURCE -> C_SOURCE
  | C_SENDER -> C_SENDER
  | C_ADDRESS -> C_ADDRESS
  | C_SELF -> C_SELF
  | C_SELF_ADDRESS -> C_SELF_ADDRESS
  | C_IMPLICIT_ACCOUNT -> C_IMPLICIT_ACCOUNT
  | C_SET_DELEGATE -> C_SET_DELEGATE
  | C_CREATE_CONTRACT -> C_CREATE_CONTRACT
  | C_CONVERT_TO_LEFT_COMB -> C_CONVERT_TO_LEFT_COMB
  | C_CONVERT_TO_RIGHT_COMB -> C_CONVERT_TO_RIGHT_COMB
  | C_CONVERT_FROM_LEFT_COMB -> C_CONVERT_FROM_LEFT_COMB
  | C_CONVERT_FROM_RIGHT_COMB -> C_CONVERT_FROM_RIGHT_COMB

let untype_type_value (t:O.type_expression) : (I.type_expression, typer_error) result =
  match t.type_meta with
  | Some s -> ok s
  | _ -> fail @@ corner_case "trying to untype generated type"

(*
  Tranform a Ast_typed type_expression into an ast_core type_expression
*)
let rec untype_type_expression (t:O.type_expression) : (I.type_expression, typer_error) result =
  (* TODO: or should we use t.core if present? *)
  let%bind t = match t.type_content with
  | O.T_sum x ->
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let%bind associated_type = untype_type_expression associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      ok @@ v' in
    let%bind x' = Stage_common.Helpers.bind_map_lmap aux x in
    ok @@ I.T_sum x'
  | O.T_record x ->
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let%bind associated_type = untype_type_expression associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      ok @@ v' in
    let%bind x' = Stage_common.Helpers.bind_map_lmap aux x in
    ok @@ I.T_record x'
  | O.T_constant (tag) ->
    ok @@ I.T_constant (unconvert_type_constant tag)
  | O.T_variable (name) -> ok @@ I.T_variable (Var.todo_cast name) (* TODO: is this the right conversion? *)
  | O.T_arrow {type1;type2} ->
    let%bind type1 = untype_type_expression type1 in
    let%bind type2 = untype_type_expression type2 in
    ok @@ I.T_arrow {type1;type2}
  | O.T_operator {operator;args} ->
    let%bind arguments = bind_map_list untype_type_expression args in
    ok @@ I.T_operator {type_operator=operator;arguments}
    in
  ok @@ I.make_t t


(* match t.core with *)
(* | Some s -> ok s *)
(* | _ -> fail @@ internal_assertion_failure "trying to untype generated type" *)

(*
  Tranform a Ast_typed literal into an ast_core literal
*)
(*
  Tranform a Ast_typed expression into an ast_core matching
*)
let rec untype_expression (e:O.expression) : (I.expression, typer_error) result =
  let open I in
  let return e = ok e in
  match e.expression_content with
  | E_literal l ->
    return (e_literal l)
  | E_constant {cons_name;arguments} ->
      let%bind lst' = bind_map_list untype_expression arguments in
      return (e_constant (unconvert_constant' cons_name) lst')
  | E_variable (n) ->
    return (e_variable ({n with wrap_content = Var.todo_cast n.wrap_content}))
  | E_application {lamb;args} ->
      let%bind f' = untype_expression lamb in
      let%bind arg' = untype_expression args in
      return (e_application f' arg')
  | E_lambda lambda ->
      let%bind lambda = untype_lambda e.type_expression lambda in
      let {binder;input_type;output_type;result} = lambda in
      return (e_lambda (binder) (input_type) (output_type) result)
  | E_constructor {constructor; element} ->
      let%bind p' = untype_expression element in
      let Label n = constructor in
      return (e_constructor n p')
  | E_record r ->
    let r = O.LMap.to_kv_list r in
    let%bind r' = bind_map_list (fun (Label k,e) -> let%bind e = untype_expression e in ok (I.Label k,e)) r in
    return (e_record @@ LMap.of_list r')
  | E_record_accessor {record; path} ->
    let%bind r' = untype_expression record in
    let Label path = path in
    return (e_record_accessor r' (Label path))
  | E_record_update {record; path; update} ->
    let%bind r' = untype_expression record in
    let%bind e = untype_expression update in 
    return (e_record_update r' path e)
  | E_matching {matchee;cases} ->
    let%bind ae' = untype_expression matchee in
    let%bind m' = untype_matching untype_expression cases in
    return (e_matching ae' m')
  (* | E_failwith ae ->
   *   let%bind ae' = untype_expression ae in
   *   return (e_failwith ae') *)
  | E_let_in {let_binder; rhs;let_result; inline} ->
    let%bind tv = untype_type_value rhs.type_expression in
    let%bind rhs = untype_expression rhs in
    let%bind result = untype_expression let_result in
    return (e_let_in ({ let_binder with wrap_content = Var.todo_cast let_binder.wrap_content} , (Some tv)) inline rhs result)
  | E_raw_code {language; code} ->
    let%bind code = untype_expression code in
    return @@ e_raw_code language code
  | E_recursive {fun_name; fun_type; lambda} ->
      let%bind lambda = untype_lambda fun_type lambda in
      let%bind fun_type = untype_type_expression fun_type in
      return @@ e_recursive { fun_name with wrap_content = Var.todo_cast fun_name.wrap_content } fun_type lambda

and untype_lambda ty {binder; result} : (I.lambda, typer_error) result =
      let%bind io = trace_option (corner_case "TODO") @@ get_t_function ty in
      let%bind (input_type , output_type) = bind_map_pair untype_type_value io in
      let%bind result = untype_expression result in
      ok ({binder={binder with wrap_content = Var.todo_cast binder.wrap_content};input_type = Some input_type; output_type = Some output_type; result}: I.lambda)

(*
  Tranform a Ast_typed matching into an ast_core matching
*)
and untype_matching : (O.expression -> (I.expression, typer_error) result) -> O.matching_expr -> (I.matching_expr, typer_error) result = fun f m ->
  let open I in
  match m with
  | Match_option {match_none ; match_some = {opt; body;tv=_}} ->
      let%bind match_none = f match_none in
      let%bind body = f body in
      let match_some = {opt= { opt with wrap_content = Var.todo_cast opt.wrap_content}; body} in
      ok @@ Match_option {match_none ; match_some}
  | Match_list {match_nil ; match_cons = {hd;tl;body;tv=_}} ->
      let%bind match_nil = f match_nil in
      let%bind body = f body in
      let hd = { hd with wrap_content = Var.todo_cast hd.wrap_content } in
      let tl = { tl with wrap_content = Var.todo_cast tl.wrap_content } in
      let match_cons = { hd ; tl ; body } in
      ok @@ Match_list {match_nil ; match_cons}
  | Match_variant { cases ; tv=_ } ->
      let aux ({constructor;pattern;body} : O.matching_content_case) =
        let%bind body = f body in
        ok @@ {constructor;proj=Location.map Var.todo_cast pattern;body} in
      let%bind lst' = bind_map_list aux cases in
      ok @@ Match_variant lst'
