
module Errors = Errors
module I = Ast_imperative
module O = Ast_sugar
open Trace

let rec decompile_type_expression : O.type_expression -> (I.type_expression, Errors.purification_error) result =
  fun te ->
  let return te = ok @@ I.make_t te in
  match te.type_content with
    | O.T_sum sum -> 
      (* This type sum could be a michelson_or as well, we could use is_michelson_or *)
      let%bind sum = 
        Stage_common.Helpers.bind_map_lmap (fun v ->
          let {associated_type;decl_pos} : O.row_element = v in
          let%bind v = decompile_type_expression associated_type in
          ok @@ ({associated_type=v;decl_pos}:I.row_element)
        ) sum
      in
      return @@ I.T_sum sum
    | O.T_record record -> 
      let%bind record = 
        Stage_common.Helpers.bind_map_lmap (fun v ->
          let {associated_type;decl_pos} : O.row_element = v in
          let%bind v = decompile_type_expression associated_type in
          ok @@ ({associated_type=v;decl_pos}:I.row_element)
        ) record
      in
      return @@ I.T_record record
    | O.T_tuple tuple ->
      let%bind tuple = bind_map_list decompile_type_expression tuple in
      return @@ I.T_tuple tuple
    | O.T_arrow {type1;type2} ->
      let%bind type1 = decompile_type_expression type1 in
      let%bind type2 = decompile_type_expression type2 in
      return @@ T_arrow {type1;type2}
    | O.T_variable type_variable -> return @@ T_variable type_variable 
    | O.T_wildcard               -> return @@ T_wildcard
    | O.T_constant (type_constant, lst) ->
      let%bind lst = bind_map_list decompile_type_expression lst in
      return @@ T_constant (type_constant, lst)

let rec decompile_expression : O.expression -> (I.expression, Errors.purification_error) result =
  fun e ->
  let return expr = ok @@ I.make_e ~loc:e.location expr in
  match e.expression_content with 
    O.E_literal lit -> return @@ I.E_literal lit
  | O.E_constant {cons_name;arguments} -> 
    let%bind arguments = bind_map_list decompile_expression arguments in
    return @@ I.E_constant {cons_name;arguments}
  | O.E_variable name     -> return @@ I.E_variable name
  | O.E_application {lamb; args} -> 
    let%bind lamb = decompile_expression lamb in
    let%bind args = decompile_expression args in
    return @@ I.E_application {lamb; args}
  | O.E_lambda lambda ->
    let%bind lambda = decompile_lambda lambda in
    return @@ I.E_lambda lambda
  | O.E_recursive {fun_name;fun_type;lambda} ->
    let%bind fun_type = decompile_type_expression fun_type in
    let%bind lambda = decompile_lambda lambda in
    return @@ I.E_recursive {fun_name;fun_type;lambda}
  | O.E_let_in {let_binder;inline;rhs;let_result} ->
    let%bind let_binder = decompile_binder let_binder in
    let%bind rhs = decompile_expression rhs in
    let%bind let_result = decompile_expression let_result in
    return @@ I.E_let_in {let_binder;inline;rhs;let_result}
  | O.E_raw_code {language;code} ->
    let%bind code  = decompile_expression code in
    return @@ I.E_raw_code {language;code} 
  | O.E_constructor {constructor;element} ->
    let%bind element = decompile_expression element in
    return @@ I.E_constructor {constructor;element}
  | O.E_matching {matchee; cases} ->
    let%bind matchee = decompile_expression matchee in
    let%bind cases   = decompile_matching cases in
    return @@ I.E_matching {matchee;cases}
  | O.E_record record ->
    let record = I.LMap.to_kv_list record in
    let%bind record = 
      bind_map_list (fun (k,v) ->
        let%bind v = decompile_expression v in
        ok @@ (k,v)
      ) record
    in
    return @@ I.E_record (O.LMap.of_list record)
  | O.E_accessor {record;path} ->
    let%bind record = decompile_expression record in
    let%bind path = decompile_path path in
    return @@ I.E_accessor {record;path}
  | O.E_update {record;path;update} ->
    let%bind record = decompile_expression record in
    let%bind path = decompile_path path in
    let%bind update = decompile_expression update in
    return @@ I.E_update {record;path;update}
  | O.E_tuple tuple ->
    let%bind tuple = bind_map_list decompile_expression tuple in
    return @@ I.E_tuple tuple
  | O.E_map map ->
    let%bind map = bind_map_list (
      bind_map_pair decompile_expression
    ) map
    in
    return @@ I.E_map map
  | O.E_big_map big_map ->
    let%bind big_map = bind_map_list (
      bind_map_pair decompile_expression
    ) big_map
    in
    return @@ I.E_big_map big_map
  | O.E_list lst ->
    let%bind lst = bind_map_list decompile_expression lst in
    return @@ I.E_list lst
  | O.E_set set ->
    let%bind set = bind_map_list decompile_expression set in
    return @@ I.E_set set 
  | O.E_ascription {anno_expr; type_annotation} ->
    let%bind anno_expr = decompile_expression anno_expr in
    let%bind type_annotation = decompile_type_expression type_annotation in
    return @@ I.E_ascription {anno_expr; type_annotation}
  | O.E_cond {condition;then_clause;else_clause} ->
    let%bind condition   = decompile_expression condition in
    let%bind then_clause = decompile_expression then_clause in
    let%bind else_clause = decompile_expression else_clause in
    return @@ I.E_cond {condition; then_clause; else_clause}
  | O.E_sequence {expr1; expr2} ->
    let%bind expr1 = decompile_expression expr1 in
    let%bind expr2 = decompile_expression expr2 in
    return @@ I.E_sequence {expr1; expr2}
  | O.E_skip -> return @@ I.E_skip

and decompile_binder : _ -> _  result = fun (var,ty) ->
    let%bind ty = decompile_type_expression ty in
    ok (var,ty)

and decompile_path : O.access list -> (I.access list, Errors.purification_error) result =
  fun path -> let aux a = match a with
    | O.Access_record s -> ok @@ I.Access_record s
    | O.Access_tuple  i -> ok @@ I.Access_tuple  i
    | O.Access_map e ->
      let%bind e = decompile_expression e in
      ok @@ I.Access_map e
  in
  bind_map_list aux path

and decompile_lambda : O.lambda -> (I.lambda, Errors.purification_error) result =
  fun {binder; result}->
    let%bind binder = decompile_binder binder in
    let%bind result = decompile_expression result in
    ok @@ I.{binder;result}
and decompile_matching : O.matching_expr -> (I.matching_expr, Errors.purification_error) result =
  fun m -> 
  match m with 
    | O.Match_list {match_nil;match_cons} ->
      let%bind match_nil = decompile_expression match_nil in
      let (hd,tl,expr) = match_cons in
      let%bind expr = decompile_expression expr in
      ok @@ I.Match_list {match_nil; match_cons=(hd,tl,expr)}
    | O.Match_option {match_none;match_some} ->
      let%bind match_none = decompile_expression match_none in
      let (n,expr) = match_some in
      let%bind expr = decompile_expression expr in
      ok @@ I.Match_option {match_none; match_some=(n,expr)}
    | O.Match_variant lst ->
      let%bind lst = bind_map_list (
        fun ((c,n),expr) ->
          let%bind expr = decompile_expression expr in
          ok @@ ((c,n),expr)
      ) lst 
      in
      ok @@ I.Match_variant lst
    | O.Match_record (lst,expr) ->
      let%bind expr = decompile_expression expr in
      let aux (a,b,c) = 
        let%bind (b,c) = decompile_binder (b,c) in
        ok @@ (a,b,c) in
      let%bind lst = bind_map_list aux lst in
      ok @@ I.Match_record (lst,expr)
    | O.Match_tuple (lst,expr) ->
      let%bind expr = decompile_expression expr in
      let%bind lst = bind_map_list decompile_binder lst in
      ok @@ I.Match_tuple (lst,expr)
    | O.Match_variable (binder,expr) ->
      let%bind expr = decompile_expression expr in
      let%bind binder = decompile_binder binder in
      ok @@ I.Match_variable (binder,expr)

let decompile_declaration : O.declaration Location.wrap -> _ result = fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with 
  | O.Declaration_constant (n, te, inline, expr) ->
    let%bind expr = decompile_expression expr in
    let%bind te   = decompile_type_expression te in
    return @@ I.Declaration_constant (n, te, inline, expr)
  | O.Declaration_type (n, te) ->
    let%bind te = decompile_type_expression te in
    return @@ I.Declaration_type (n,te)

let decompile_program : O.program -> (I.program, Errors.purification_error) result = fun prg ->
  bind_map_list decompile_declaration prg
