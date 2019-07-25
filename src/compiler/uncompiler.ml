open Mini_c.Types
open Memory_proto_alpha
open Proto_alpha_utils.Trace
open Script_typed_ir
open Script_ir_translator

let rec translate_value (Ex_typed_value (ty, value)) : value result =
  match (ty, value) with
  | Pair_t ((a_ty, _, _), (b_ty, _, _), _), (a, b) -> (
      let%bind a = translate_value @@ Ex_typed_value(a_ty, a) in
      let%bind b = translate_value @@ Ex_typed_value(b_ty, b) in
      ok @@ D_pair(a, b)
    )
  | Union_t ((a_ty, _), _, _), L a -> (
      let%bind a = translate_value @@ Ex_typed_value(a_ty, a) in
      ok @@ D_left a
    )
  | Union_t (_, (b_ty, _), _), R b -> (
      let%bind b = translate_value @@ Ex_typed_value(b_ty, b) in
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
      ok @@ D_nat n
  | (Bool_t _), b ->
      ok @@ D_bool b
  | (String_t _), s ->
      ok @@ D_string s
  | (Address_t _), s ->
      ok @@ D_string (Alpha_context.Contract.to_b58check s)
  | (Unit_t _), () ->
      ok @@ D_unit
  | (Option_t _), None ->
      ok @@ D_none
  | (Option_t ((o_ty, _), _, _)), Some s ->
      let%bind s' = translate_value @@ Ex_typed_value (o_ty, s) in
      ok @@ D_some s'
  | (Map_t (k_cty, v_ty, _)), m ->
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
  | (List_t (ty, _)), lst ->
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
  | (Operation_t _) , op ->
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
