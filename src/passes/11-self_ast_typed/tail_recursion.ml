open Errors
open Ast_typed
open Trace

let var_equal = Location.equal_content ~equal:Var.equal

let rec check_recursive_call : expression_variable -> bool -> expression -> (unit, self_ast_typed_error) result = fun n final_path e ->
  match e.expression_content with
  | E_literal _   -> ok ()
  | E_constant c  ->
    let%bind _ = bind_map_list (check_recursive_call n false) c.arguments in
    ok ()
  | E_variable v  -> (
    let%bind _ = Assert.assert_true (recursive_call_is_only_allowed_as_the_last_operation n e.location)
      (final_path || not (var_equal n v)) in
    ok ()
    )
  | E_application {lamb;args} ->
    let%bind _ = check_recursive_call n final_path lamb in
    let%bind _ = check_recursive_call n false args in
    ok ()
  | E_lambda {result;_} ->
    let%bind _ = check_recursive_call n final_path result in
    ok ()
  | E_recursive { fun_name; lambda} ->
    let%bind _ = check_recursive_call fun_name true lambda.result in
    ok ()
  | E_let_in {rhs;let_result;_} ->
    let%bind _ = check_recursive_call n false rhs in
    let%bind _ = check_recursive_call n final_path let_result in
    ok ()
  | E_type_in {rhs=_;let_result;_} ->
    let%bind _ = check_recursive_call n final_path let_result in
    ok ()
  | E_raw_code _ ->
    ok ()
  | E_constructor {element;_} ->
    let%bind _ = check_recursive_call n false element in
    ok ()
  | E_matching {matchee;cases} ->
    let%bind _ = check_recursive_call n false matchee in
    let%bind _ = check_recursive_call_in_matching n final_path cases in
    ok ()
  | E_record elm ->
    let es = LMap.to_list elm in
    let%bind _ = bind_map_list (check_recursive_call n false) es in
    ok ()
  | E_record_accessor {record;_} ->
    let%bind _ = check_recursive_call n false record in
    ok ()
  | E_record_update {record;update;_} ->
    let%bind _ = check_recursive_call n false record in
    let%bind _ = check_recursive_call n false update in
    ok ()
  | E_module_accessor {element; _} ->
    let%bind _ = check_recursive_call n false element in
    ok ()

and check_recursive_call_in_matching = fun n final_path c ->
  match c with
  | Match_list {match_nil;match_cons={hd=_;tl=_;body;tv=_}} ->
    let%bind _ = check_recursive_call n final_path match_nil in
    let%bind _ = check_recursive_call n final_path body in
    ok ()
  | Match_option {match_none; match_some={opt=_;body;tv=_}} ->
    let%bind _ = check_recursive_call n final_path match_none in
    let%bind _ = check_recursive_call n final_path body in
    ok ()
  | Match_variant {cases;tv=_} ->
    let aux {constructor=_; pattern=_; body} =
      let%bind _ = check_recursive_call n final_path body in
      ok ()
    in
    let%bind _ = bind_map_list aux cases in
    ok ()


let peephole_expression : expression -> (expression, self_ast_typed_error) result = fun e ->
  let return expression_content = ok { e with expression_content } in
  match e.expression_content with
  | E_recursive {fun_name; lambda} as e-> (
    let%bind _ = check_recursive_call fun_name true lambda.result in
    return e
    )
  | e -> return e
