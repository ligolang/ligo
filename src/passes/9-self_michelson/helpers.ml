open Trace
open Tezos_utils
open Michelson
open Tezos_micheline.Micheline

type mapper = michelson -> michelson result
let rec map_expression : mapper -> michelson -> michelson result = fun f e ->
  let self = map_expression f in
  let%bind e' = f e in
  match e' with
  | Prim (l , p , lst , a) -> (
      let%bind lst' = bind_map_list self lst in
      ok @@ Prim (l , p , lst' , a)
    )
  | Seq (l , lst) -> (
      let%bind lst' = bind_map_list self lst in
      ok @@ Seq (l , lst')
    )
  | x -> ok x
