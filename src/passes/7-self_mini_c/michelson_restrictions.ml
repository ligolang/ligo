open Mini_c
open Trace

module Errors = struct

  let bad_self_address cst () =
    let title = thunk @@
      Format.asprintf "Wrong %alocation" Mini_c.PP.expression' cst in
    let message = thunk @@
      Format.asprintf "%ais only allowed at top-level" Mini_c.PP.expression' cst in
    error title message ()
  
end
open Errors

let self_in_lambdas : expression -> expression result = 
  fun e ->
    match e.content with
    | E_closure {binder=_ ; body} ->
      let%bind _self_in_lambdas = Helpers.map_expression
        (fun e -> match e.content with
        | E_constant {cons_name=C_SELF_ADDRESS; _} as c -> fail (bad_self_address c)
        | _ -> ok e)
        body in
      ok e
    | _ -> ok e
