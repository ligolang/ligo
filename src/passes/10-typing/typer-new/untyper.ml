open Trace
open Typer_common.Errors

module I = Ast_core
module O = Ast_typed
open O.Combinators
open Stage_common.Maps

let untype_type_value (t:O.type_expression) : (I.type_expression, typer_error) result =
  match t.type_meta with
  | Some s -> ok s
  | _ -> fail @@ corner_case "trying to untype generated type"

(*
  Tranform a Ast_typed type_expression into an ast_core type_expression
*)
let rec untype_type_expression (t:O.type_expression) : (I.type_expression, typer_error) result =
  let self = untype_type_expression in
  let return t = ok @@ I.make_t t in
  match t.type_content with
  | O.T_sum {content ; layout} ->
     let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
       let%bind associated_type = untype_type_expression associated_type in
       let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
       ok @@ v' in
     let%bind x' = Stage_common.Helpers.bind_map_lmap aux content in
     return @@ I.T_sum { fields = x' ; layout = Some layout }
  | O.T_record {content;layout} -> (
    let aux ({associated_type ; michelson_annotation ; decl_pos} : O.row_element) =
      let%bind associated_type = untype_type_expression associated_type in
      let v' = ({associated_type ; michelson_annotation ; decl_pos} : I.row_element) in
      ok @@ v' in
    let%bind x' = Stage_common.Helpers.bind_map_lmap aux content in
    return @@ I.T_record {fields = x' ; layout = Some layout}
  )
  | O.T_variable name -> return @@ I.T_variable (Var.todo_cast name)
  | O.T_arrow arr ->
    let%bind arr = arrow self arr in
    return @@ I.T_arrow arr
  | O.T_constant {language=_;injection;parameters} ->
    let%bind arguments = bind_map_list self parameters in
    let type_operator = Var.of_name (Ligo_string.extract injection) in
    return @@ I.T_app {type_operator;arguments}
  | O.T_module_accessor ma ->
    let%bind ma = module_access self ma in
    return @@ I.T_module_accessor ma
  | O.T_singleton x ->
    return @@ I.T_singleton x

(*
  Transform a Ast_typed expression into an ast_core expression
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
    let%bind lambda = untype_lambda e.type_expression lambda in
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
    let%bind tv         = Typer_common.Untyper.untype_type_expression rhs.type_expression in
    let%bind rhs        = untype_expression rhs in
    let%bind let_result = untype_expression let_result in
    let var=Location.map Var.todo_cast let_binder in
    return @@ E_let_in {let_binder={var;ascr=Some tv}; rhs; let_result; inline}
  | E_type_in ti ->
    let%bind ti = Stage_common.Maps.type_in untype_expression untype_type_expression ti in
    return @@ E_type_in ti
  | E_mod_in {module_binder; rhs;let_result} ->
    let%bind rhs        = untype_module_fully_typed rhs in
    let%bind let_result = untype_expression let_result in
    return @@ E_mod_in {module_binder; rhs; let_result}
  | E_mod_alias ma ->
    let%bind ma = mod_alias untype_expression ma in
    return @@ E_mod_alias ma
  | E_raw_code {language; code} ->
    let%bind code = untype_expression code in
    return @@ E_raw_code {language; code}
  | E_recursive {fun_name; fun_type; lambda} ->
    let%bind lambda = untype_lambda fun_type lambda in
    let%bind fun_type = Typer_common.Untyper.untype_type_expression fun_type in
    let fun_name = Location.map Var.todo_cast fun_name in
    return @@ E_recursive {fun_name; fun_type; lambda}
  | E_module_accessor ma ->
    let%bind ma = module_access untype_expression ma in
    return @@ E_module_accessor ma

and untype_lambda ty {binder; result} : (_ I.lambda, typer_error) result =
    let%bind io = trace_option (corner_case "This has to be a lambda") @@ get_t_function ty in
    let%bind (input_type , output_type) = bind_map_pair Typer_common.Untyper.untype_type_expression io in
    let%bind result = untype_expression result in
    let binder : _ I.binder = {var=Location.map Var.todo_cast binder;ascr=Some input_type} in
    ok ({binder; output_type = Some output_type; result}: _ I.lambda)

(*
  Transform a Ast_typed matching into an ast_core matching
*)
and untype_matching : (O.expression -> (I.expression, typer_error) result) -> O.matching_expr -> (I.matching_expr, typer_error) result = fun f m ->
  let open I in
  match m with
  | Match_option {match_none ; match_some = {opt; body;tv=_}} ->
    let%bind match_none = f match_none in
    let%bind body = f body in
    let opt = Location.map Var.todo_cast opt in
    let match_some = {opt; body} in
    ok @@ Match_option {match_none ; match_some}
  | Match_list {match_nil ; match_cons = {hd;tl;body;tv=_}} ->
    let%bind match_nil = f match_nil in
    let hd = Location.map Var.todo_cast hd in
    let tl = Location.map Var.todo_cast tl in
    let%bind body = f body in
    let match_cons = {hd ; tl ; body} in
    ok @@ Match_list {match_nil ; match_cons}
  | Match_variant { cases ; tv=_ } ->
    let aux ({constructor;pattern;body} : O.matching_content_case) =
      let%bind body = f body in
      let proj = Location.map Var.todo_cast pattern in
      ok ({constructor;proj;body} : I.match_variant) in
    let%bind lst' = bind_map_list aux cases in
    ok @@ Match_variant lst'
  | Match_record {fields; body; record_type = _} ->
    let%bind fields = bind_map_list
        (fun (label, (var, ty)) ->
           let%bind ty = untype_type_expression ty in
           ok (label, {var; ascr = Some ty}))
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
| Declaration_constant {binder;expr;inline} ->
  let%bind ty = untype_type_expression expr.type_expression in
  let var = Location.map Var.todo_cast binder in
  let%bind expr = untype_expression expr in
  return @@ Declaration_constant {binder={var;ascr=Some ty};expr;attr={inline}}
| Declaration_module {module_binder;module_} ->
  let%bind module_ = untype_module_fully_typed module_ in
  return @@ Declaration_module {module_binder;module_}
| Module_alias ma ->
  return @@ Module_alias ma

and untype_module_fully_typed : O.module_fully_typed -> (I.module_, typer_error) result = fun (Module_Fully_Typed m) ->
  bind_map_list (bind_map_location untype_declaration) m
