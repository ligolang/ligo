open Ast_simplified
open Trace

let peephole_expression : expression -> expression result = fun e ->
  let return expression = ok { e with expression } in
  match e.expression with
  | E_constructor (Constructor "Some" , e) -> return @@ E_constant (C_SOME , [ e ])
  | E_constructor (Constructor "None" , _) -> return @@ E_constant (C_NONE , [ ])
  | e -> return e
