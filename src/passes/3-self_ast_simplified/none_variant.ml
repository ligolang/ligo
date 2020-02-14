open Ast_simplified
open Trace

let peephole_expression : expression -> expression result = fun e ->
  let return expression_content = ok { e with expression_content } in
  match e.expression_content with
  | E_constructor {constructor=Constructor "Some";element=e} -> return @@ E_constant {cons_name=C_SOME;arguments=[ e ]}
  | E_constructor {constructor=Constructor "None"; _} -> return @@ E_constant {cons_name=C_NONE ; arguments=[]}
  | e -> return e
