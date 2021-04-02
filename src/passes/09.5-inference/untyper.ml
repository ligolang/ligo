open Trace
open Typer_common.Errors

module I = Ast_core
module O = Ast_core
open Stage_common.Maps

let untype_type_value (t:O.type_expression) : (I.type_expression, typer_error) result =
  ok @@ t

(*
  Tranform a Ast_inferred type_expression into an ast_core type_expression
*)
let rec untype_type_expression (t:O.type_expression) : (I.type_expression, typer_error) result =
  let self = untype_type_expression in
  let return t = ok @@ I.make_t t in
  match t.type_content with
  | O.T_sum {fields ; layout} ->
     let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
       let%bind associated_type = untype_type_expression associated_type in
       let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
       ok @@ v' in
     let%bind x' = Stage_common.Helpers.bind_map_lmap aux fields in
     return @@ I.T_sum { fields = x' ; layout }
  | O.T_record {fields;layout} -> (
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let%bind associated_type = untype_type_expression associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      ok @@ v' in
    let%bind x' = Stage_common.Helpers.bind_map_lmap aux fields in
    return @@ I.T_record {fields = x' ; layout}
  )
  | O.T_variable name -> return @@ I.T_variable (Var.todo_cast name)
  | O.T_arrow arr ->
    let%bind arr = arrow self arr in
    return @@ I.T_arrow arr
  | O.T_module_accessor ma ->
    let%bind ma = module_access self ma in
    return @@ I.T_module_accessor ma
  | O.T_singleton x ->
    return @@ I.T_singleton x
  | O.T_app {type_operator;arguments} ->
    let%bind arguments = bind_map_list self arguments in
    return @@ I.T_app {type_operator;arguments}

(*
  Transform a Ast_inferred expression into an ast_core expression
*)
let rec untype_expression (e:O.expression) : (I.expression, typer_error) result =
  let open I in
  let return e = ok @@ I.make_e e in
  match e.expression_content with
  | E_variable n -> return @@ E_variable (Location.map Var.todo_cast n)
  | E_literal l -> return @@ E_literal l
  | E_constant {cons_name;arguments} ->
    let%bind arguments = bind_map_list untype_expression arguments in
    return @@ E_constant {cons_name;arguments}
  | E_lambda lambda ->
    let%bind lambda = untype_lambda lambda in
    return @@ E_lambda lambda
  | E_application {lamb;args} ->
    let%bind lamb = untype_expression lamb in
    let%bind args = untype_expression args in
    return @@ E_application {lamb;args}
  | E_constructor {constructor; element} ->
    let%bind element = untype_expression element in
    return @@ E_constructor {constructor; element}
  | E_record r ->
    let%bind r' = Stage_common.Helpers.bind_map_lmap untype_expression r in
    return @@ E_record r'
  | E_record_accessor {record; path} ->
    let%bind record = untype_expression record in
    return @@ E_record_accessor {record; path}
  | E_record_update {record; path; update} ->
    let%bind record = untype_expression record in
    let%bind update = untype_expression update in
    return @@ E_record_update {record; path; update}
  | E_matching {matchee;cases} ->
    let%bind matchee = untype_expression matchee in
    let%bind cases   = untype_matching untype_expression cases in
    return @@ E_matching {matchee;cases}
  | E_let_in {let_binder; rhs;let_result; inline} ->
    let%bind rhs        = untype_expression rhs in
    let%bind let_result = untype_expression let_result in
    let%bind let_binder = Stage_common.Maps.binder untype_type_expression let_binder in
    return @@ E_let_in {let_binder; rhs; let_result; inline}
  | E_type_in ti ->
    let%bind ti = Stage_common.Maps.type_in untype_expression untype_type_expression ti in
    return @@ E_type_in ti
  | E_mod_in {module_binder; rhs;let_result} ->
    let%bind rhs        = untype_module_fully_inferred rhs in
    let%bind let_result = untype_expression let_result in
    return @@ E_mod_in {module_binder; rhs; let_result}
  | E_mod_alias ma ->
    let%bind ma = mod_alias untype_expression ma in
    return @@ E_mod_alias ma
  | E_raw_code {language; code} ->
    let%bind code = untype_expression code in
    return @@ E_raw_code {language; code}
  | E_recursive {fun_name; fun_type; lambda} ->
    let%bind lambda = untype_lambda lambda in
    let%bind fun_type = untype_type_expression fun_type in
    let fun_name = Location.map Var.todo_cast fun_name in
    return @@ E_recursive {fun_name; fun_type; lambda}
  | E_module_accessor ma ->
    let%bind ma = module_access untype_expression ma in
    return @@ E_module_accessor ma
  | E_ascription {anno_expr;type_annotation} ->
    let%bind anno_expr = untype_expression anno_expr in
    let%bind type_annotation = untype_type_expression type_annotation in
    return @@ E_ascription {anno_expr;type_annotation}

and untype_lambda {binder; output_type; result} : (_ O.lambda, typer_error) result =
    let%bind binder = Stage_common.Maps.binder untype_type_expression binder in
    let%bind output_type = bind_map_option untype_type_expression output_type in
    let%bind result = untype_expression result in
    ok ({binder; output_type; result}: _ I.lambda)

(*
  Transform a Ast_inferred matching into an ast_core matching
*)
and untype_matching : (O.expression -> (I.expression, typer_error) result) -> O.matching_expr -> (I.matching_expr, typer_error) result = fun f m ->
  let open I in
  match m with
  | Match_option {match_none ; match_some = {opt; body}} ->
    let%bind match_none = f match_none in
    let%bind body = f body in
    let opt = Location.map Var.todo_cast opt in
    let match_some = {opt; body} in
    ok @@ Match_option {match_none ; match_some}
  | Match_list {match_nil ; match_cons = {hd;tl;body}} ->
    let%bind match_nil = f match_nil in
    let hd = Location.map Var.todo_cast hd in
    let tl = Location.map Var.todo_cast tl in
    let%bind body = f body in
    let match_cons = {hd ; tl ; body} in
    ok @@ Match_list {match_nil ; match_cons}
  | Match_variant cases ->
    let aux ({constructor;proj;body} : O.match_variant) =
      let%bind body = f body in
      let proj = Location.map Var.todo_cast proj in
      ok ({constructor;proj;body} : I.match_variant) in
    let%bind lst' = bind_map_list aux cases in
    ok @@ Match_variant lst'
  | Match_record {fields; body} ->
    let%bind fields = bind_map_list
        (fun (label, binder) ->
          let%bind binder = Stage_common.Maps.binder untype_type_expression binder in
           ok (label, binder))
        (LMap.to_kv_list fields) in
    let fields = LMap.of_list fields in
    let%bind body = f body in
    ok @@ Match_record {fields; body}

and untype_declaration : O.declaration -> (I.declaration, typer_error) result =
let return (d: I.declaration) = ok @@ d in
function
  Declaration_type {type_binder; type_expr} ->
  let%bind type_expr = untype_type_expression type_expr in
  return @@ Declaration_type {type_binder; type_expr}
| Declaration_constant {name; binder;expr;attr={inline}} ->
  let%bind binder = Stage_common.Maps.binder untype_type_expression binder in
  let%bind expr = untype_expression expr in
  return @@ Declaration_constant {name; binder;expr;attr={inline}}
| Declaration_module {module_binder;module_} ->
  let%bind module_ = untype_module_fully_inferred module_ in
  return @@ Declaration_module {module_binder;module_}
| Module_alias ma ->
  return @@ Module_alias ma

and untype_module_fully_inferred : O.module_ -> (I.module_, typer_error) result = fun (m) ->
  bind_map_list (bind_map_location untype_declaration) m
