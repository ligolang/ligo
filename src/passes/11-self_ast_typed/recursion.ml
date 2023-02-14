module FV = Helpers.Free_variables
open Ligo_prim
open Ast_typed
open Simple_utils.Trace

let var_equal = Value_var.equal

let check_rec_binder_shadowed ~fun_name ~(lambda : _ Lambda.t) =
  let _, fv, _ = FV.expression lambda.result in
  let is_binder_shadowed_in_body = not @@ List.mem fv fun_name ~equal:var_equal in
  var_equal fun_name (Param.get_var lambda.binder) || is_binder_shadowed_in_body


let show_unused_rec_warning ~raise ~warn_unused_rec fun_name =
  if warn_unused_rec
  then
    raise.warning
      (`Self_ast_typed_warning_unused_rec
        (Value_var.get_location fun_name, Format.asprintf "%a" Value_var.pp fun_name))
  else ()


let remove_rec_expression ~raise ~warn_unused_rec : expression -> expression =
 fun e ->
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_recursive { fun_name; fun_type = _; lambda; force_lambdarec = _ } as e ->
    let is_shadowed = check_rec_binder_shadowed ~fun_name ~lambda in
    if is_shadowed
    then (
      let () = show_unused_rec_warning ~raise ~warn_unused_rec fun_name in
      return (E_lambda lambda))
    else return e
  | e -> return e
