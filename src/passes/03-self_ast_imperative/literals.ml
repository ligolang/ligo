open Errors
open Ast_imperative
open Trace
open Proto_alpha_utils

let peephole_expression : expression -> (expression , self_ast_imperative_error) result = fun e ->
  let return expression_content = ok { e with expression_content } in
  match e.expression_content with
  | E_literal (Literal_key_hash s) as l -> (
    let open Tezos_crypto in
    let%bind (_pkh:Crypto.Signature.public_key_hash) =
      Trace.trace_tzresult (bad_format e) @@
      Signature.Public_key_hash.of_b58check s in
    return l
    )
  | E_literal (Literal_address s) as l -> (
    let open Memory_proto_alpha in
    let%bind (_contract:Protocol.Alpha_context.Contract.t) = 
      Trace.trace_alpha_tzresult (bad_format e) @@
      Protocol.Alpha_context.Contract.of_b58check s in
    return l
    )
  | E_literal (Literal_signature s) as l -> (
    let open Tezos_crypto in
    let%bind (_sig:Crypto.Signature.t) = 
      Trace.trace_tzresult (bad_format e) @@
      Signature.of_b58check s in
    return l
    )
  | E_literal (Literal_key s) as l -> (
    let open Tezos_crypto in
    let%bind (_k:Crypto.Signature.public_key) = 
      Trace.trace_tzresult (bad_format e) @@
      Signature.Public_key.of_b58check s in
    return l
    )
  | E_constant {cons_name=C_BIG_MAP_LITERAL as cst; arguments=lst} -> (
      let%bind elt =
        trace_option (bad_single_arity cst e) @@
        List.to_singleton lst
      in
      let%bind lst =
        trace_option (bad_map_param_type cst e) @@
        get_e_list elt.expression_content
      in
      let aux = fun (e : expression) ->
        trace_option (bad_map_param_type cst e) @@
          Option.(get_e_tuple e.expression_content >>= fun t ->
            List.to_pair t)
      in
      let%bind pairs = bind_map_list aux lst in
      return @@ E_big_map pairs
    )
  | E_constant {cons_name=C_MAP_LITERAL as cst; arguments=lst} -> (
      let%bind elt =
        trace_option (bad_single_arity cst e) @@
        List.to_singleton lst
      in
      let%bind lst =
        trace_option (bad_map_param_type cst e) @@
        get_e_list elt.expression_content
      in
      let aux = fun (e : expression) ->
        trace_option (bad_map_param_type cst e) @@
          Option.(get_e_tuple e.expression_content >>= fun t ->
            List.to_pair t)
      in
      let%bind pairs = bind_map_list aux lst in
      return @@ E_map pairs
    )
  | E_constant {cons_name=C_BIG_MAP_EMPTY as cst; arguments=lst} -> (
      let%bind () =
        Assert.assert_list_empty (bad_empty_arity cst e) lst
      in
      return @@ E_big_map []
    )
  | E_constant {cons_name=C_MAP_EMPTY as cst; arguments=lst} -> (
      let%bind () =
        Assert.assert_list_empty (bad_empty_arity cst e) lst
      in
      return @@ E_map []
    )

  | E_constant {cons_name=C_SET_LITERAL as cst; arguments=lst} -> (
      let%bind elt =
        trace_option (bad_single_arity cst e) @@
        List.to_singleton lst
      in
      let%bind lst =
        trace_option (bad_set_param_type cst e) @@
        get_e_list elt.expression_content
      in
      return @@ E_set lst
    )
  | E_constant {cons_name=C_SET_EMPTY as cst; arguments=lst} -> (
      let%bind () =
        Assert.assert_list_empty (bad_empty_arity cst e) lst
      in
      return @@ E_set []
    )
  | e -> return e
