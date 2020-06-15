open Errors
open Ast_imperative
open Trace
open Stage_common.Helpers

let peephole_type_expression : type_expression -> (type_expression , self_ast_imperative_error) result = fun e ->
  let return type_content = ok {type_content; location=e.location } in
  match e.type_content with
  | T_sum cmap ->
    let%bind _uu = bind_map_cmapi
      (fun k _ ->
        let (Constructor name) = k in
        if (String.length name  >= 32) then fail @@ too_long_constructor name e
        else ok ()
      )
      cmap in
    ok e
  | e -> return e
