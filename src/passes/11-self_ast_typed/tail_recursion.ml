open Errors
open Ast_typed
open Trace

let var_equal = Location.equal_content ~equal:Var.equal

let rec check_recursive_call : expression_variable -> bool -> expression -> (unit, self_ast_typed_error) result = fun n final_path e ->
  match e.expression_content with
  | E_literal _   -> ok ()
  | E_constant c  ->
    let* _ = bind_map_list (check_recursive_call n false) c.arguments in
    ok ()
  | E_variable v  -> (
    let* _ = Assert.assert_true (recursive_call_is_only_allowed_as_the_last_operation n e.location)
      (final_path || not (var_equal n v)) in
    ok ()
    )
  | E_application {lamb;args} ->
    let* _ = check_recursive_call n final_path lamb in
    let* _ = check_recursive_call n false args in
    ok ()
  | E_lambda {result;_} ->
    let* _ = check_recursive_call n final_path result in
    ok ()
  | E_recursive { fun_name; lambda} ->
    let* _ = check_recursive_call fun_name true lambda.result in
    ok ()
  | E_let_in {rhs;let_result;_} ->
    let* _ = check_recursive_call n false rhs in
    let* _ = check_recursive_call n final_path let_result in
    ok ()
  | E_type_in {rhs=_;let_result;_} ->
    let* _ = check_recursive_call n final_path let_result in
    ok ()
  | E_mod_in {rhs=_;let_result;_} ->
    let* _ = check_recursive_call n final_path let_result in
    ok ()
  | E_mod_alias {alias=_;binders=_;result} ->
    let* _ = check_recursive_call n final_path result in
    ok ()
  | E_raw_code _ ->
    ok ()
  | E_constructor {element;_} ->
    let* _ = check_recursive_call n false element in
    ok ()
  | E_matching {matchee;cases} ->
    let* _ = check_recursive_call n false matchee in
    let* _ = check_recursive_call_in_matching n final_path cases in
    ok ()
  | E_record elm ->
    let es = LMap.to_list elm in
    let* _ = bind_map_list (check_recursive_call n false) es in
    ok ()
  | E_record_accessor {record;_} ->
    let* _ = check_recursive_call n false record in
    ok ()
  | E_record_update {record;update;_} ->
    let* _ = check_recursive_call n false record in
    let* _ = check_recursive_call n false update in
    ok ()
  | E_module_accessor {element; _} ->
    let* _ = check_recursive_call n false element in
    ok ()

and check_recursive_call_in_matching = fun n final_path c ->
  match c with
  | Match_variant {cases;tv=_} ->
    let aux {constructor=_; pattern=_; body} =
      let* _ = check_recursive_call n final_path body in
      ok ()
    in
    let* _ = bind_map_list aux cases in
    ok ()
  | Match_record {fields = _; body; tv = _} ->
    check_recursive_call n final_path body


let peephole_expression : expression -> (expression, self_ast_typed_error) result = fun e ->
  let return expression_content = ok { e with expression_content } in
  match e.expression_content with
  | E_recursive {fun_name; lambda} as e-> (
    let* _ = check_recursive_call fun_name true lambda.result in
    return e
    )
  | e -> return e
