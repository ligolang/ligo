module FV = Helpers.Free_variables

open Ligo_prim
open Ast_typed
open Errors
open Simple_utils.Trace

let var_equal = Value_var.equal

(* TODO: is this dead? *)
let rec check_recursive_call ~raise : Value_var.t -> bool -> expression -> unit = fun n final_path e ->
  match e.expression_content with
  | E_literal _   -> ()
  | E_constant c  ->
    List.iter ~f:(check_recursive_call ~raise n false) c.arguments
  | E_variable v  ->
    Assert.assert_true ~raise
      (recursive_call_is_only_allowed_as_the_last_operation n e.location)
      (final_path || not (var_equal n v))
  | E_application {lamb;args} ->
    check_recursive_call ~raise n final_path lamb;
    check_recursive_call ~raise n false args
  | E_lambda {result;_} ->
    check_recursive_call ~raise n final_path result
  | E_type_abstraction {result;_} ->
    check_recursive_call ~raise n final_path result
  | E_recursive { fun_name; fun_type=_; lambda} ->
    check_recursive_call ~raise fun_name true lambda.result
  | E_let_in {rhs;let_result;_} 
  ->
    check_recursive_call ~raise n false rhs;
    check_recursive_call ~raise n final_path let_result
  | E_mod_in {rhs=_;let_result;_} ->
    check_recursive_call ~raise n final_path let_result
  | E_raw_code _ -> ()
  | E_constructor {element;_} ->
    check_recursive_call ~raise n false element
  | E_matching {matchee;cases} ->
    check_recursive_call ~raise n false matchee;
    check_recursive_call_in_matching ~raise n final_path cases
  | E_record elm ->
    List.iter ~f:(check_recursive_call ~raise n false) @@ Record.LMap.to_list elm
  | E_accessor {struct_;_} ->
    check_recursive_call ~raise n false struct_
  | E_update {struct_;update;_} ->
    check_recursive_call ~raise n false struct_;
    check_recursive_call ~raise n false update
  | E_module_accessor _
  | E_type_inst _
  (* Wtf?? *)
  | E_assign _ | _ -> ()

and check_recursive_call_in_matching ~raise = fun n final_path ms ->
  let aux Match_expr.{pattern=_; body} =
    check_recursive_call ~raise n final_path body
  in
  List.iter ~f:aux ms

let check_rec_binder_shadowed ~fun_name ~(lambda : _ Lambda.t) =
  let _, fv, _ = FV.expression lambda.result in
  let is_binder_shadowed_in_body
    = not @@ List.mem fv fun_name ~equal:var_equal in
  var_equal fun_name (Param.get_var lambda.binder) ||
  is_binder_shadowed_in_body

let check_tail_expression ~raise : expression -> expression = fun e ->
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_recursive {fun_name; fun_type=_; lambda} as e-> (
    let is_shadowed = check_rec_binder_shadowed ~fun_name ~lambda in
    let () =
      if is_shadowed
      then () (* No recursion, so no need to check if tail call*)
      else check_recursive_call ~raise fun_name true lambda.result in
    return e
    )
  | e -> return e

let show_unused_rec_warning ~raise ~warn_unused_rec fun_name =
  if warn_unused_rec then
    raise.warning
      (`Self_ast_typed_warning_unused_rec
        (Value_var.get_location fun_name, Format.asprintf "%a" Value_var.pp fun_name))
  else ()

let remove_rec_expression ~raise ~warn_unused_rec : expression -> expression
  = fun e ->
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_recursive {fun_name; fun_type=_; lambda} as e-> (
    let is_shadowed = check_rec_binder_shadowed ~fun_name ~lambda in
    if is_shadowed then
      let () = show_unused_rec_warning ~raise ~warn_unused_rec fun_name in
      return (E_lambda lambda)
    else
      return e
    )
  | e -> return e
