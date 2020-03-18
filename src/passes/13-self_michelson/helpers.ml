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

open Memory_proto_alpha.Protocol.Script_ir_translator
let fetch_contract_inputs : ex_ty -> (ex_ty * ex_ty) result =
  let error ()  = simple_fail "Invalid contract: Failed to fetch parameter and storage" in
  function
  | Ex_ty (Lambda_t (in_ty, _, _)) -> (
    match in_ty with
    | Pair_t ((param_ty,_,_),(storage_ty,_,_),_,_) ->
      ok (Ex_ty param_ty, Ex_ty storage_ty)
    |_ -> error () )
  | _ -> error ()
