open Ast_simplified
open Trace
open Proto_alpha_utils

module Errors = struct

  let bad_format e () =
    let title = (thunk ("Badly formatted literal")) in
    let message () = Format.asprintf "%a" Ast_simplified.PP.expression e in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp e.location)
    ] in
    error ~data title message ()

  let bad_empty_arity cst loc () =
    let cst_name = thunk @@ Format.asprintf "%a" Stage_common.PP.constant cst in
    let title = thunk @@ "Wrong "^(cst_name ())^" literal arity" in
    let message = thunk @@ (cst_name ())^" literal expects no parameter" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc) ;
    ] in
    error ~data title message ()

  let bad_single_arity cst loc () =
    let cst_name = thunk @@ Format.asprintf "%a" Stage_common.PP.constant cst in
    let title = thunk @@ "Wrong "^(cst_name ())^" literal arity" in
    let message = thunk @@ (cst_name ())^" literal expects a single parameter" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc) ;
    ] in
    error ~data title message ()

  let bad_map_param_type cst loc () =
    let cst_name = thunk @@ Format.asprintf "%a" Stage_common.PP.constant cst in
    let title = thunk @@ "Wrong "^(cst_name ())^" literal parameter type" in
    let message = thunk @@ (cst_name ())^" literal expects a list of pairs as parameter" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc) ;
    ] in
    error ~data title message ()

  let bad_set_param_type cst loc () =
    let cst_name = thunk @@ Format.asprintf "%a" Stage_common.PP.constant cst in
    let title = thunk @@ "Wrong "^(cst_name ())^" literal parameter type" in
    let message = thunk @@ (cst_name ())^" literal expects a list as parameter" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp loc) ;
    ] in
    error ~data title message ()
  
end
open Errors

let peephole_expression : expression -> expression result = fun e ->
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
        trace_option (bad_single_arity cst e.location) @@
        List.to_singleton lst
      in
      let%bind lst =
        trace_strong (bad_map_param_type cst e.location) @@
        get_e_list elt.expression_content
      in
      let aux = fun (e : expression) ->
        trace_strong (bad_map_param_type cst e.location) @@
        let%bind tpl = get_e_tuple e.expression_content in
        let%bind (a , b) =
          trace_option (simple_error "of pairs") @@
          List.to_pair tpl
        in
        ok (a , b)
      in
      let%bind pairs = bind_map_list aux lst in
      return @@ E_big_map pairs
    )
  | E_constant {cons_name=C_MAP_LITERAL as cst; arguments=lst} -> (
      let%bind elt =
        trace_option (bad_single_arity cst e.location) @@
        List.to_singleton lst
      in
      let%bind lst =
        trace_strong (bad_map_param_type cst e.location) @@
        get_e_list elt.expression_content
      in
      let aux = fun (e : expression) ->
        trace_strong (bad_map_param_type cst e.location) @@
        let%bind tpl = get_e_tuple e.expression_content in
        let%bind (a , b) =
          trace_option (simple_error "of pairs") @@
          List.to_pair tpl
        in
        ok (a , b)
      in
      let%bind pairs = bind_map_list aux lst in
      return @@ E_map pairs
    )
  | E_constant {cons_name=C_BIG_MAP_EMPTY as cst; arguments=lst} -> (
      let%bind () =
        trace_strong (bad_empty_arity cst e.location) @@
        Assert.assert_list_empty lst
      in
      return @@ E_big_map []
    )
  | E_constant {cons_name=C_MAP_EMPTY as cst; arguments=lst} -> (
      let%bind () =
        trace_strong (bad_empty_arity cst e.location) @@
        Assert.assert_list_empty lst
      in
      return @@ E_map []
    )

  | E_constant {cons_name=C_SET_LITERAL as cst; arguments=lst} -> (
      let%bind elt =
        trace_option (bad_single_arity cst e.location) @@
        List.to_singleton lst
      in
      let%bind lst =
        trace_strong (bad_set_param_type cst e.location) @@
        get_e_list elt.expression_content
      in
      return @@ E_set lst
    )
  | E_constant {cons_name=C_SET_EMPTY as cst; arguments=lst} -> (
      let%bind () =
        trace_strong (bad_empty_arity cst e.location) @@
        Assert.assert_list_empty lst
      in
      return @@ E_set []
    )
  | e -> return e
