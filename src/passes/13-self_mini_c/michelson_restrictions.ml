open Errors
open Mini_c
open Trace

let self_in_lambdas : expression -> (expression,_) result = 
  fun e ->
    match e.content with
    | E_closure {binder=_ ; body} ->
      let* _self_in_lambdas = Helpers.map_expression
        (fun e -> match e.content with
        | E_constant {cons_name=C_SELF; _} -> fail (bad_self_address C_SELF)
        | _ -> ok e)
        body in
      ok e
    | _ -> ok e
