open Ast_typed.Types
open Core
open Ast_typed.Misc
open Ast_typed.Reasons

let tc description type_vars allowed_list : type_constraint = {
    c = C_typeclass {tc_args = type_vars ; original_id = None ; typeclass = allowed_list} ;
    reason = "typeclass for operator: " ^ description
  }

let forall binder f =
  let () = ignore binder in
  let freshvar = fresh_type_variable () in
  let var = wrap Forall @@ P_variable freshvar in
  let body = f var in
  wrap Forall @@ P_forall { binder = freshvar ; constraints = [] ; body }

let forall_tc binder f =
  let () = ignore binder in
  let freshvar = fresh_type_variable () in
  let var = wrap Forall_TC @@ P_variable freshvar in
  let (tc, ty) = f var in
  wrap Forall_TC @@ P_forall { binder = freshvar ; constraints = tc ; body = ty }

(* chained forall *)
let forall2 a b f =
  forall a @@ fun a' ->
  forall b @@ fun b' ->
  f a' b'

let forall3 a b c f =
  forall a @@ fun a' ->
  forall b @@ fun b' ->
  forall c @@ fun c' ->
  f a' b' c'

let forall4 a b c d f =
  forall a @@ fun a' ->
  forall b @@ fun b' ->
  forall c @@ fun c' ->
  forall d @@ fun d' ->
  f a' b' c' d'

let forall4_tc a b c d f =
  forall    a @@ fun a' ->
  forall    b @@ fun b' ->
  forall    c @@ fun c' ->
  forall_tc d @@ fun d' ->
  f a' b' c' d'

let forall3_tc a b c f =
  forall    a @@ fun a' ->
  forall    b @@ fun b' ->
  forall_tc c @@ fun c' ->
  f a' b' c'

let forall2_tc a b f =
  forall    a @@ fun a' ->
  forall_tc b @@ fun b' ->
  f a' b'

let (=>) tc ty = (tc , ty)
let (-->) arg ret = p_constant C_arrow     [arg; ret]
let option t      = p_constant C_option    [t]
let pair a b      = p_row_ez   C_record    [("0",a);("1",b)]
let sum  a b      = p_row_ez   C_variant   [("0",a);("1",b)]
let map  k v      = p_constant C_map       [k; v]
let big_map k v = p_constant C_big_map [k; v]
let unit          = p_constant C_unit      []
let list   t      = p_constant C_list      [t]
let set    t      = p_constant C_set       [t]
let bool          = p_row_ez C_variant    [("true",unit);("false",unit)]
let string        = p_constant C_string    []
let nat           = p_constant C_nat       []
let mutez         = p_constant C_mutez     []
let timestamp     = p_constant C_timestamp []
let int           = p_constant C_int       []
let address       = p_constant C_address   []
let chain_id      = p_constant C_chain_id  []
let bytes         = p_constant C_bytes     []
let key           = p_constant C_key       []
let key_hash      = p_constant C_key_hash  []
let signature     = p_constant C_signature []
let operation     = p_constant C_operation []
let contract t    = p_constant C_contract  [t]
let ( * ) a b = pair a b

(* These are used temporarily to de-curry functions that correspond to Michelson operators *)
let tuple0        = p_row_ez   C_record    []
let tuple1 a      = p_row_ez   C_record    [("0",a)]
let tuple2 a b    = p_row_ez   C_record    [("0",a);("1",b)]
let tuple3 a b c  = p_row_ez   C_record    [("0",a);("1",b);("2",c)]
