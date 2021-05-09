open Errors
open Ast_imperative
open Trace
open Stage_common.Helpers

let peephole_type_expression : type_expression -> (type_expression , self_ast_imperative_error) result = fun e ->
  let return type_content = ok {type_content; location=e.location } in
  match e.type_content with
  | T_sum cmap ->
    let* _ = bind_map_lmapi
      (fun k _ ->
        let (Label name) = k in
        if (String.length name >= 32) then fail @@ too_long_constructor name e
        (*RL TODO: move this to some passes after typer*)
        else ok ()
      )
      cmap.fields in
    ok e
  | e -> return e
