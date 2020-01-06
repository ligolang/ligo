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

end
open Errors

let peephole_expression : expression -> expression result = fun e ->
  let return expression = ok { e with expression } in
  match e.expression with
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
  | E_constant (C_BIG_MAP_LITERAL , lst) -> (
      let%bind elt =
        trace_option (simple_error "big_map literal expects a single parameter") @@
        List.to_singleton lst
      in
      let%bind lst =
        trace (simple_error "big_map literal expects a list as parameter") @@
        get_e_list elt.expression
      in
      let aux = fun (e : expression) ->
        trace (simple_error "big_map literal expects a list of pairs as parameter") @@
        let%bind tpl = get_e_tuple e.expression in
        let%bind (a , b) =
          trace_option (simple_error "of pairs") @@
          List.to_pair tpl
        in
        ok (a , b)
      in
      let%bind pairs = bind_map_list aux lst in
      return @@ E_big_map pairs
    )
  | E_constant (C_MAP_LITERAL, lst) -> (
      let%bind elt =
        trace_option (simple_error "map literal expects a single parameter") @@
        List.to_singleton lst
      in
      let%bind lst =
        trace (simple_error "map literal expects a list as parameter") @@
        get_e_list elt.expression
      in
      let aux = fun (e : expression) ->
        trace (simple_error "map literal expects a list of pairs as parameter") @@
        let%bind tpl = get_e_tuple e.expression in
        let%bind (a , b) =
          trace_option (simple_error "of pairs") @@
          List.to_pair tpl
        in
        ok (a , b)
      in
      let%bind pairs = bind_map_list aux lst in
      return @@ E_map pairs
    )
  | E_constant (C_BIG_MAP_EMPTY, lst) -> (
      let%bind () =
        trace_strong (simple_error "BIG_MAP_EMPTY expects no parameter") @@
        Assert.assert_list_empty lst
      in
      return @@ E_big_map []
    )
  | E_constant (C_MAP_EMPTY, lst) -> (
      let%bind () =
        trace_strong (simple_error "MAP_EMPTY expects no parameter") @@
        Assert.assert_list_empty lst
      in
      return @@ E_map []
    )
  | E_constant (C_SET_LITERAL, lst) -> (
      let%bind elt =
        trace_option (simple_error "map literal expects a single parameter") @@
        List.to_singleton lst
      in
      let%bind lst =
        trace (simple_error "map literal expects a list as parameter") @@
        get_e_list elt.expression
      in
      return @@ E_set lst
    )
  | E_constant (C_SET_EMPTY, lst) -> (
      let%bind () =
        trace_strong (simple_error "SET_EMPTY expects no parameter") @@
        Assert.assert_list_empty lst
      in
      return @@ E_set []
    )
  | e -> return e
