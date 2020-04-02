open Mini_c
open Trace

module Errors = struct

  let bad_self_address cst () =
    let title = thunk @@
      Format.asprintf "Wrong %a location" Stage_common.PP.constant cst in
    let message = thunk @@
      Format.asprintf "%a is only allowed at top-level" Stage_common.PP.constant cst in
    error title message ()
  
end
open Errors

let self_in_lambdas : expression -> expression result = 
  fun e ->
    match e.content with
    | E_closure {binder=_ ; body} ->
      let%bind _self_in_lambdas = Helpers.map_expression
        (fun e -> match e.content with
        | E_constant {cons_name=C_SELF_ADDRESS; _} -> fail (bad_self_address C_SELF_ADDRESS)
        | _ -> ok e)
        body in
      ok e
    | _ -> ok e
