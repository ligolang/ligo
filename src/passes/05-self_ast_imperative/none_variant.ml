open Errors
open Ast_imperative
open Trace

let peephole_expression : expression -> (expression , self_ast_imperative_error) result = fun e ->
  let return expression_content = ok { e with expression_content } in
  match e.expression_content with
  | E_constructor {constructor=Label "Some";element=e} ->
     return @@ E_constant {cons_name=(Const C_SOME);arguments=[ e ]}
  | E_constructor {constructor=Label "None";element=_} ->
     return @@ E_constant {cons_name=(Const C_NONE) ; arguments=[]}
  | e -> return e
