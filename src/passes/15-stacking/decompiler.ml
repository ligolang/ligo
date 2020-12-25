open Errors
open Mini_c.Types
open Tezos_micheline.Micheline
open Trace

let rec decompile_value :
  ('l, string) node -> ('l, string) node -> (value , stacking_error) result =
  fun ty value ->
  match (ty, value) with
  | Prim (_, "pair", [a_ty; b_ty], _), Prim (_, "Pair", [a; b], _) -> (
      let%bind a = decompile_value a_ty a in
      let%bind b = decompile_value b_ty b in
      ok @@ D_pair(a, b)
    )
  | Prim (_, "or", [a_ty; _], _), Prim (_, "Left", [a], _) -> (
      let%bind a = decompile_value a_ty a in
      ok @@ D_left a
    )
  | Prim (_, "or", [_; b_ty], _), Prim (_, "Right", [b], _) -> (
      let%bind b = decompile_value b_ty b in
      ok @@ D_right b
    )
  | Prim (_, "int", [], _), Int (_, n) ->
      ok @@ D_int n
  | Prim (_, "nat", [], _), Int (_, n) ->
      ok @@ D_nat n
  | Prim (_, "chain_id", [], _), Bytes (_, id) ->
    let id = Tezos_base.TzPervasives.Chain_id.of_bytes_exn id in
    let str = Tezos_crypto.Base58.simple_encode
      (Tezos_base__TzPervasives.Chain_id.b58check_encoding)
      id in
    ok @@ D_string str
  | Prim (_, "key_hash", [], _), String (_, n) ->
    ok @@ D_string n
  | Prim (_, "key", [], _), String (_, n) ->
    ok @@ D_string n
  | Prim (_, "signature", [], _), String (_, n) ->
    ok @@ D_string n
  | Prim (_, "timestamp", [], _), Int (_, n) ->
      ok @@ D_timestamp n
  | Prim (_, "timestamp", [], _), String (_, n) ->
      let open Tezos_base.TzPervasives.Time.Protocol in
      let n = Z.of_int64 (to_seconds (of_notation_exn n)) in
      ok @@ D_timestamp n
  | Prim (_, "mutez", [], _), Int (_, n) ->
      ok @@ D_mutez n
  | Prim (_, "bool", [], _), Prim (_, "True", [], _) ->
      ok @@ D_bool true
  | Prim (_, "bool", [], _), Prim (_, "False", [], _) ->
      ok @@ D_bool false
  | Prim (_, "string", [], _), String (_, s) ->
      ok @@ D_string s
  | Prim (_, "bytes", [], _), Bytes (_, b) ->
      ok @@ D_bytes b
  | Prim (_, "address", [], _), String (_, s) ->
      ok @@ D_string s
  | Prim (_, "unit", [], _), Prim (_, "Unit", [], _) ->
      ok @@ D_unit
  | Prim (_, "option", [_], _), Prim (_, "None", [], _) ->
      ok @@ D_none
  | Prim (_, "option", [o_ty], _), Prim (_, "Some", [s], _) ->
      let%bind s' = decompile_value o_ty s in
      ok @@ D_some s'
  | Prim (_, "map", [k_ty; v_ty], _), Seq (_, lst) ->
      let%bind lst' =
        let aux elt =
          match elt with
          | Prim (_, "Elt", [k; v], _) ->
            let%bind k' = decompile_value k_ty k in
            let%bind v' = decompile_value v_ty v in
            ok (k', v')
          | _ ->
            let ty = root (strip_locations ty) in
            let value = root (strip_locations value) in
            fail (untranspilable ty value)
        in
        bind_map_list aux lst
      in
      ok @@ D_map lst'
  | Prim (_, "big_map", [k_ty; v_ty], _), Seq (_, lst) ->
      let%bind lst' =
        let aux elt =
          match elt with
          | Prim (_, "Elt", [k; v], _) ->
            let%bind k' = decompile_value k_ty k in
            let%bind v' = decompile_value v_ty v in
            ok (k', v')
          | _ ->
            let ty = root (strip_locations ty) in
            let value = root (strip_locations value) in
            fail (untranspilable ty value)
        in
        bind_map_list aux lst
      in
      ok @@ D_big_map lst'
  | Prim (_, "list", [ty], _), Seq (_, lst) ->
      let%bind lst' =
        bind_map_list (decompile_value ty) lst
      in
      ok @@ D_list lst'
  | Prim (_, "set", [ty], _), Seq (_, lst) -> (
      let lst' =
        let aux acc cur = cur :: acc in
        let lst = List.fold_left aux lst [] in
        List.rev lst in
      let%bind lst'' =
        let aux = fun t -> decompile_value ty t in
        bind_map_list aux lst'
      in
      ok @@ D_set lst''
    )
  | Prim (_, "operation", [], _), Bytes (_, op) -> (
      ok @@ D_operation op
    )
  | Prim (_, "lambda", [_; _], _) as ty, Seq (_, _) ->
      let pp_lambda =
        Format.asprintf "[lambda of type: %a ]" Michelson.pp ty in
        ok @@ D_string pp_lambda
  | ty, v ->
      fail (untranspilable ty v)
