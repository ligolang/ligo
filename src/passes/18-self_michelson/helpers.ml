open Tezos_utils
open Michelson
open Tezos_micheline.Micheline

let fetch_contract_ty_inputs : _ michelson -> (_ michelson * _ michelson) option
  = function
  | Prim (_, "lambda", [ Prim (_, "pair", [ param_ty; storage_ty ], _); _ ], _) ->
    Some (param_ty, storage_ty)
  | Prim (_, "lambda", [ Prim (l, "pair", param_ty :: storage_tys, _); _ ], _) ->
    Some (param_ty, Prim (l, "pair", storage_tys, []))
  | _ -> None


let fetch_views_ty : _ michelson -> (_ michelson * _ michelson) option = function
  | Prim (_, "lambda", [ Prim (_, "pair", [ param_ty; _storage_ty ], _); ret_ty ], _) ->
    Some (param_ty, ret_ty)
  | _ -> None
