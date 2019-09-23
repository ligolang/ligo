open Ast_simplified
open Trace

let peephole_expression : expression -> expression result = fun e ->
  let return expression = ok { e with expression } in
  match e.expression with
  | E_constructor ("Some" , e) -> return @@ E_constant ("SOME" , [ e ])
  | E_constructor ("None" , _) -> return @@ E_constant ("NONE" , [ ])
  | e -> return e
