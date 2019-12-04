open Ast_simplified
open Trace

let peephole_expression : expression -> expression result = fun e ->
  let return expression = ok { e with expression } in
  match e.expression with
  | E_ascription (e' , t) as e -> (
      match (e'.expression , t.type_expression') with
      | (E_literal (Literal_string str) , T_constant (TC_address)) -> return @@ E_literal (Literal_address str)
      | (E_literal (Literal_string str) , T_constant (TC_bytes)) -> (
          let%bind e' = e'_bytes str in
          return e'
        )
      | _ -> return e
    )
  | e -> return e
