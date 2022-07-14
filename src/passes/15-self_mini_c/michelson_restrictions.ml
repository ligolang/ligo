open Errors
open Mini_c
open Simple_utils.Trace

let self_in_lambdas ~raise : expression -> expression =
  fun e ->
    match e.content with
    | E_closure {binder=_ ; body} ->
      let f = fun ~raise e -> match e.content with
				| E_constant {cons_name=C_SELF; _} -> raise.error (bad_self_address C_SELF)
				| _ -> e
			in
      let _self_in_lambdas : expression = Helpers.map_expression
        (f ~raise)
        body in
      e
    | _ -> e
