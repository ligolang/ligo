module I = Ast_sugar
module O = Ast_core

open Trace
open Errors

let rec decompile_type_expression : O.type_expression -> (I.type_expression, desugaring_error) result =
  fun te ->
  let return te = ok @@ I.make_t te in
  match te.sugar with 
    Some te -> ok @@ te
  | None ->
    match te.content with
      | O.T_sum sum -> 
        let sum = I.CMap.to_kv_list sum in
        let%bind sum = 
          bind_map_list (fun (k,v) ->
            let {ctor_type;michelson_annotation;ctor_decl_pos} : O.ctor_content = v in
            let%bind ctor_type = decompile_type_expression ctor_type in
            let v' : I.ctor_content = {ctor_type;michelson_annotation;ctor_decl_pos} in
            ok @@ (k,v')
          ) sum
        in
        return @@ I.T_sum (O.CMap.of_list sum)
      | O.T_record record -> 
        let record = I.LMap.to_kv_list record in
        let%bind record = 
          bind_map_list (fun (k,v) ->
            let {field_type;field_annotation;field_decl_pos} : O.field_content = v in
            let%bind field_type = decompile_type_expression field_type in
            let v' : I.field_content = {field_type ; michelson_annotation=field_annotation ; field_decl_pos} in
            ok @@ (k,v')
          ) record
        in
        return @@ I.T_record (O.LMap.of_list record)
      | O.T_arrow {type1;type2} ->
        let%bind type1 = decompile_type_expression type1 in
        let%bind type2 = decompile_type_expression type2 in
        return @@ T_arrow {type1;type2}
      | O.T_variable type_variable -> return @@ T_variable type_variable 
      | O.T_constant type_constant -> return @@ T_constant type_constant
      | O.T_operator (type_operator, lst) ->
        let%bind lst = bind_map_list decompile_type_expression lst in
        return @@ T_operator (type_operator, lst)

let rec decompile_expression : O.expression -> (I.expression, desugaring_error) result =
  fun e ->
  let return expr = ok @@ I.make_e ~loc:e.location expr in
  match e.sugar with
    Some e -> ok @@ e
  | None -> 
    match e.content with 
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
    | O.E_let_in {let_binder = (var, ty);inline=false;rhs=expr1;let_result=expr2}
      when Var.equal var.wrap_content (Var.of_name "_")
           && Pervasives.(=) ty (Some (O.t_unit ())) ->
      let%bind expr1 = decompile_expression expr1 in
      let%bind expr2 = decompile_expression expr2 in
      return @@ I.E_sequence {expr1;expr2}
    | O.E_let_in {let_binder;inline;rhs;let_result} ->
      let (binder,ty_opt) = let_binder in
      let%bind ty_opt = bind_map_option decompile_type_expression ty_opt in
      let%bind rhs = decompile_expression rhs in
      let%bind let_result = decompile_expression let_result in
      return @@ I.E_let_in {let_binder=(binder,ty_opt);mut=false;inline;rhs;let_result}
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
      let record = I.LMap.to_kv_list record in
      let%bind record = 
        bind_map_list (fun (k,v) ->
          let%bind v = decompile_expression v in
          ok @@ (k,v)
        ) record
      in
      return @@ I.E_record (O.LMap.of_list record)
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

and decompile_lambda : O.lambda -> (I.lambda, desugaring_error) result =
  fun {binder;input_type;output_type;result}->
    let%bind input_type = bind_map_option decompile_type_expression input_type in
    let%bind output_type = bind_map_option decompile_type_expression output_type in
    let%bind result = decompile_expression result in
    ok @@ I.{binder;input_type;output_type;result}
and decompile_matching : O.matching_expr -> (I.matching_expr, desugaring_error) result =
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

let decompile_declaration : O.declaration Location.wrap -> _ result = fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with 
  | O.Declaration_constant (n, te_opt, {inline}, expr) ->
    let%bind expr = decompile_expression expr in
    let%bind te_opt = bind_map_option decompile_type_expression te_opt in
    return @@ I.Declaration_constant (n, te_opt, inline, expr)
  | O.Declaration_type (n, te) ->
    let%bind te = decompile_type_expression te in
    return @@ I.Declaration_type (n,te)

let decompile_program : O.program -> (I.program, desugaring_error) result = fun prg ->
  bind_map_list decompile_declaration prg
