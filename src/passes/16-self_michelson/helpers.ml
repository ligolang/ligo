open Trace
open Tezos_utils
open Michelson
open Tezos_micheline.Micheline

type ('l, 'error) mapper = 'l michelson -> ('l michelson, 'error) result

let rec map_expression : ('l, 'error) mapper -> 'l michelson -> ('l michelson, 'error) result = fun f e ->
  let self = map_expression f in
  let* e' = f e in
  match e' with
  | Prim (l , p , lst , a) -> (
      let* lst' = bind_map_list self lst in
      ok @@ Prim (l , p , lst' , a)
    )
  | Seq (l , lst) -> (
      let* lst' = bind_map_list self lst in
      ok @@ Seq (l , lst')
    )
  | x -> ok x

let fetch_contract_inputs : _ michelson -> (_ michelson * _ michelson) option =
  function
  | Prim (_, "lambda", [Prim (_, "pair", [param_ty; storage_ty], _); _], _) ->
    Some (param_ty, storage_ty)
  | _ -> None
