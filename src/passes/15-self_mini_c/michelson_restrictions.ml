open Errors
open Mini_c
open Simple_utils.Trace

let self_in_lambdas ~raise : expression -> expression =
  fun e ->
    match e.content with
    | E_closure {binder=_ ; body} ->
      let f = fun ~raise e -> match e.content with
        | E_raw_michelson (code, _) ->
          let code = Tezos_utils.Michelson.lseq Location.generated code in
          let code = Tezos_micheline.Micheline.(map_node (fun _ -> ()) (fun x -> x) code) in
          if Tezos_utils.Michelson.has_prim "SELF" code then
            raise.error bad_self_address
          else
            e
	| _ -> e
			in
      let _self_in_lambdas : expression = Helpers.map_expression
        (f ~raise)
        body in
      e
    | _ -> e
