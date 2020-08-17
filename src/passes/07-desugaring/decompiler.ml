module I = Ast_sugar
module O = Ast_core

open Trace
open Errors

let cast_var = Location.map Var.todo_cast

let rec decompile_type_expression : O.type_expression -> (I.type_expression, desugaring_error) result =
  fun te ->
  let return te = ok @@ I.make_t te in
  match te.sugar with 
    Some te -> ok @@ te
  | None ->
    match te.content with
      | O.T_sum sum -> 
        let%bind sum = 
          Stage_common.Helpers.bind_map_lmap (fun v ->
            let {associated_type;michelson_annotation;decl_pos} : O.row_element = v in
            let%bind associated_type = decompile_type_expression associated_type in
            let v' : I.row_element = {associated_type;michelson_annotation;decl_pos} in
            ok @@ v'
          ) sum
        in
        return @@ I.T_sum sum
      | O.T_record record -> 
        let%bind record = 
          Stage_common.Helpers.bind_map_lmap (fun v ->
            let {associated_type;michelson_annotation;decl_pos} : O.row_element = v in
            let%bind associated_type = decompile_type_expression associated_type in
            let v' : I.row_element = {associated_type ; michelson_annotation=michelson_annotation ; decl_pos} in
            ok @@ v'
          ) record
        in
        return @@ I.T_record record
      | O.T_arrow {type1;type2} ->
        let%bind type1 = decompile_type_expression type1 in
        let%bind type2 = decompile_type_expression type2 in
        return @@ T_arrow {type1;type2}
      | O.T_variable type_variable -> return @@ T_variable (Var.todo_cast type_variable)
      | O.T_wildcard -> return @@ T_wildcard
      | O.T_constant { type_constant ; arguments } ->
        let%bind lst = bind_map_list decompile_type_expression arguments in
        return @@ T_constant (type_constant, lst)

let rec decompile_expression : O.expression -> (I.expression, desugaring_error) result =
  fun e ->
  let return expr = ok @@ I.make_e ~loc:e.location expr in
  match e.sugar with
    Some e -> ok @@ e
  | None -> 
    match e.content with 
      O.E_literal lit -> return @@ I.E_literal (lit)
    | O.E_constant {cons_name;arguments} -> 
      let%bind arguments = bind_map_list decompile_expression arguments in
      return @@ I.E_constant {cons_name = cons_name;arguments}
    | O.E_variable name -> return @@ I.E_variable (cast_var name)
    | O.E_application {lamb; args} -> 
      let%bind lamb = decompile_expression lamb in
      let%bind args = decompile_expression args in
      return @@ I.E_application {lamb; args}
    | O.E_lambda lambda ->
      let%bind lambda = decompile_lambda lambda in
      return @@ I.E_lambda lambda
    | O.E_recursive {fun_name;fun_type;lambda} ->
      let fun_name = cast_var fun_name in
      let%bind fun_type = decompile_type_expression fun_type in
      let%bind lambda = decompile_lambda lambda in
      return @@ I.E_recursive {fun_name;fun_type;lambda}
    | O.E_let_in {let_binder ;inline;rhs;let_result} ->
      let%bind let_binder = decompile_binder let_binder in
      let%bind rhs = decompile_expression rhs in
      let%bind let_result = decompile_expression let_result in
      return @@ I.E_let_in {let_binder;mut=false;inline;rhs;let_result}
    | O.E_raw_code {language;code} ->
      let%bind code = decompile_expression code in
      return @@ I.E_raw_code {language;code} 
    | O.E_constructor {constructor;element} ->
      let%bind element = decompile_expression element in
      return @@ I.E_constructor {constructor;element}
    | O.E_matching {matchee; cases} ->
      let%bind matchee = decompile_expression matchee in
      let%bind cases   = decompile_matching cases in
      return @@ I.E_matching {matchee;cases}
    | O.E_record record ->
      let record = O.LMap.to_kv_list record in
      let%bind record = 
        bind_map_list (fun (O.Label k,v) ->
          let%bind v = decompile_expression v in
          ok @@ (I.Label k,v)
        ) record
      in
      return @@ I.E_record (I.LMap.of_list record)
    | O.E_record_accessor {record;path} ->
      let%bind record = decompile_expression record in
      let Label path  = path in
      return @@ I.E_accessor {record;path=[I.Access_record path]}
    | O.E_record_update {record;path;update} ->
      let%bind record = decompile_expression record in
      let%bind update = decompile_expression update in
      let Label path  = path in
      return @@ I.E_update {record;path=[I.Access_record path];update}
    | O.E_ascription {anno_expr; type_annotation} ->
      let%bind anno_expr = decompile_expression anno_expr in
      let%bind type_annotation = decompile_type_expression type_annotation in
      return @@ I.E_ascription {anno_expr; type_annotation}

and decompile_binder : O.binder -> _ result = fun {var; ty} ->
  let var = cast_var var in
  let%bind ty = decompile_type_expression ty in
  ok (var,ty)

and decompile_lambda : O.lambda -> (I.lambda, desugaring_error) result =
  fun {binder;result}->
    let%bind binder = decompile_binder binder in
    let%bind result = decompile_expression result in
    ok @@ I.{binder;result}
and decompile_matching : O.matching_expr -> (I.matching_expr, desugaring_error) result =
  fun m -> 
  match m with 
    | O.Match_list {match_nil;match_cons = { hd ; tl ; body }} ->
      let hd = cast_var hd in
      let tl = cast_var tl in
      let%bind match_nil = decompile_expression match_nil in
      let%bind expr = decompile_expression body in
      ok @@ I.Match_list {match_nil; match_cons=(hd,tl,expr)}
    | O.Match_option {match_none; match_some = { opt ; body }} ->
      let opt = cast_var opt in
      let%bind match_none = decompile_expression match_none in
      let%bind expr = decompile_expression body in
      ok @@ I.Match_option {match_none; match_some=(opt,expr)}
    | O.Match_variant lst ->
      let%bind lst = bind_map_list (
        fun ({ constructor; proj ; body } : O.match_variant) ->
          let%bind expr = decompile_expression body in
          ok @@ ((constructor, cast_var proj),expr)
      ) lst 
      in
      ok @@ I.Match_variant lst

let decompile_declaration : O.declaration Location.wrap -> _ result = fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with 
  | O.Declaration_constant {binder ; attr={inline}; expr} ->
    let%bind (binder,ty) = decompile_binder binder in
    let%bind expr = decompile_expression expr in
    return @@ I.Declaration_constant (binder, ty, inline, expr)
  | O.Declaration_type {type_binder ; type_expr} ->
    let type_binder = Var.todo_cast type_binder in
    let%bind te = decompile_type_expression type_expr in
    return @@ I.Declaration_type (type_binder,te)

let decompile_program : O.program -> (I.program, desugaring_error) result = fun prg ->
  bind_map_list decompile_declaration prg
