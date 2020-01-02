open Ast_simplified
open Trace
open Proto_alpha_utils

let peephole_expression : expression -> expression result = fun e ->
  let return expression = ok { e with expression } in
  match e.expression with
  | E_literal (Literal_address s) as e -> (
    let open Memory_proto_alpha in
    let%bind (_contract:Protocol.Alpha_context.Contract.t) = 
      Trace.trace_alpha_tzresult (simple_error ("address \""^s^"\" is not a valid address")) @@
      Protocol.Alpha_context.Contract.of_b58check s in
    return e
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
