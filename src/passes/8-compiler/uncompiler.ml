open Mini_c.Types
open Proto_alpha_utils.Memory_proto_alpha
open X
open Proto_alpha_utils.Trace
open Protocol
open Script_typed_ir
open Script_ir_translator

let rec translate_value ?bm_opt (Ex_typed_value (ty, value)) : value result =
  match (ty, value) with
  | Pair_t ((a_ty, _, _), (b_ty, _, _), _ , _), (a, b) -> (
      let%bind a = translate_value ?bm_opt @@ Ex_typed_value(a_ty, a) in
      let%bind b = translate_value ?bm_opt @@ Ex_typed_value(b_ty, b) in
      ok @@ D_pair(a, b)
    )
  | Union_t ((a_ty, _), _, _ , _), L a -> (
      let%bind a = translate_value ?bm_opt @@ Ex_typed_value(a_ty, a) in
      ok @@ D_left a
    )
  | Union_t (_, (b_ty, _), _ , _), R b -> (
      let%bind b = translate_value ?bm_opt @@ Ex_typed_value(b_ty, b) in
      ok @@ D_right b
    )
  | (Int_t _), n ->
      let%bind n =
        trace_option (simple_error "too big to fit an int") @@
        Alpha_context.Script_int.to_int n in
      ok @@ D_int n
  | (Nat_t _), n ->
      let%bind n =
        trace_option (simple_error "too big to fit an int") @@
        Alpha_context.Script_int.to_int n in
      ok @@ D_nat n
  | (Timestamp_t _), n ->
      let n =
        Z.to_int @@
        Alpha_context.Script_timestamp.to_zint n in
      ok @@ D_timestamp n
  | (Mutez_t _), n ->
      let%bind n =
        generic_try (simple_error "too big to fit an int") @@
        (fun () -> Int64.to_int @@ Alpha_context.Tez.to_mutez n) in
      ok @@ D_mutez n
  | (Bool_t _), b ->
      ok @@ D_bool b
  | (String_t _), s ->
      ok @@ D_string s
  | (Bytes_t _), b ->
      ok @@ D_bytes (Tezos_stdlib.MBytes.to_bytes b)
  | (Address_t _), (s , _) ->
      ok @@ D_string (Alpha_context.Contract.to_b58check s)
  | (Unit_t _), () ->
      ok @@ D_unit
  | (Option_t _), None ->
      ok @@ D_none
  | (Option_t (o_ty, _, _)), Some s ->
      let%bind s' = translate_value @@ Ex_typed_value (o_ty, s) in
      ok @@ D_some s'
  | (Map_t (k_cty, v_ty, _ , _)), m ->
      let k_ty = Script_ir_translator.ty_of_comparable_ty k_cty in
      let lst =
        let aux k v acc = (k, v) :: acc in
        let lst = Script_ir_translator.map_fold aux m [] in
        List.rev lst in
      let%bind lst' =
        let aux (k, v) =
          let%bind k' = translate_value (Ex_typed_value (k_ty, k)) in
          let%bind v' = translate_value (Ex_typed_value (v_ty, v)) in
          ok (k', v')
        in
        bind_map_list aux lst
      in
      ok @@ D_map lst'
  | (Big_map_t (k_cty, v_ty, _)), m ->
      let k_ty = Script_ir_translator.ty_of_comparable_ty k_cty in
      let lst =
        let aux k v acc = (k, v) :: acc in
        let lst = Script_ir_translator.map_fold aux m.diff [] in
        List.rev lst in
      let%bind original_big_map =
        match bm_opt with
        | Some (D_big_map l) -> ok @@ l
        | _ -> ok []
        (* | _ -> fail @@ simple_error "Do not have access to the original big_map" . When does this matter? *)
      in
      let%bind lst' =
        let aux orig (k, v) =
          let%bind k' = translate_value (Ex_typed_value (k_ty, k)) in
          let orig_rem = List.remove_assoc k' orig in
          match v with
          | Some vadd ->
            let%bind v' = translate_value (Ex_typed_value (v_ty, vadd)) in
            if (List.mem_assoc k' orig) then ok @@ (k', v')::orig_rem
            else ok @@ (k', v')::orig
          | None -> ok orig_rem in
        bind_fold_list aux original_big_map lst in
      ok @@ D_big_map lst'
  | (List_t (ty, _ , _)), lst ->
      let%bind lst' =
        let aux = fun t -> translate_value (Ex_typed_value (ty, t)) in
        bind_map_list aux lst
      in
      ok @@ D_list lst'
  | (Set_t (ty, _)), (module S) -> (
      let lst = S.OPS.elements S.boxed in
      let lst' =
        let aux acc cur = cur :: acc in
        let lst = List.fold_left aux lst [] in
        List.rev lst in
      let%bind lst'' =
        let aux = fun t -> translate_value (Ex_typed_value (ty_of_comparable_ty ty, t)) in
        bind_map_list aux lst'
      in
      ok @@ D_set lst''
    )
  | (Operation_t _) , (op , _) ->
      ok @@ D_operation op
  | ty, v ->
      let%bind error =
        let%bind m_data =
          trace_tzresult_lwt (simple_error "unparsing unrecognized data") @@
          Proto_alpha_utils.Memory_proto_alpha.unparse_michelson_data ty v in
        let%bind m_ty =
          trace_tzresult_lwt (simple_error "unparsing unrecognized data") @@
          Proto_alpha_utils.Memory_proto_alpha.unparse_michelson_ty ty in
        let error_content () =
          Format.asprintf "%a : %a"
            Michelson.pp m_data
            Michelson.pp m_ty in
        ok @@ (fun () -> error (thunk "this value can't be transpiled back yet") error_content ())
      in
      fail error
