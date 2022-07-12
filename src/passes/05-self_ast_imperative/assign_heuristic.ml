open Ast_imperative

let peephole_expression : expression -> expression = fun e ->
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_let_in {let_binder;rhs={expression_content=E_update {record={expression_content=E_variable v;_};path=_;update};_};let_result;attributes=_}
      when ValueVar.equal let_binder.var v ->
    let assign = return @@ E_assign {
      binder = let_binder;
      expression  = update;
    } in
    return @@ E_sequence {
      expr1 = assign;
      expr2 = let_result;
    }
  | e -> return e
