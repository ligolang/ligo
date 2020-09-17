open Trace
open Typer_common.Errors

module I = Ast_core
module O = Ast_typed
open O.Combinators

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
    let var = Location.map Var.todo_cast let_binder in
    let%bind ty         = Typer_common.Untyper.untype_type_expression rhs.type_expression in
    let%bind rhs        = untype_expression rhs in
    let%bind let_result = untype_expression let_result in
    let let_binder = {var; ty} in
    return @@ E_let_in {let_binder; rhs; let_result; inline}
  | E_raw_code {language; code} ->
    let%bind code = untype_expression code in
    return @@ E_raw_code {language; code}
  | E_recursive {fun_name; fun_type; lambda} ->
    let%bind lambda = untype_lambda fun_type lambda in
    let%bind fun_type = Typer_common.Untyper.untype_type_expression fun_type in
    let fun_name = Location.map Var.todo_cast fun_name in
    return @@ E_recursive {fun_name; fun_type; lambda}


and untype_lambda ty {binder; result} : (I.lambda, typer_error) result =
    let%bind io = trace_option (corner_case "This has to be a lambda") @@ get_t_function ty in
    let%bind (ty , output_type) = bind_map_pair Typer_common.Untyper.untype_type_expression io in
    let var = Location.map Var.todo_cast binder in
    let%bind result = untype_expression result in
    let result = I.e_annotation result output_type in
    ok ({binder={var;ty}; result}: I.lambda)

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
