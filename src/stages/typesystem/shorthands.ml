open Ast_typed.Types
open Core
open Ast_typed.Misc

let tc description type_vars allowed_list : type_constraint = {
    c = C_typeclass {tc_args = type_vars ;typeclass = allowed_list} ;
    reason = "typeclass for operator: " ^ description
  }

let forall binder f =
  let () = ignore binder in
  let freshvar = fresh_type_variable () in
  let body = f { tsrc = "shorthands.ml/forall" ; t = P_variable freshvar } in
  { tsrc = "shorthands.ml/forall" ;
    t = P_forall { binder = freshvar ; constraints = [] ; body } }

let forall_tc binder f =
  let () = ignore binder in
  let freshvar = fresh_type_variable () in
  let (tc, ty) = f { tsrc = "shorthands.ml/forall_tc" ; t = P_variable freshvar } in
  { tsrc = "shorthands.ml/forall_tc" ;
    t = P_forall { binder = freshvar ; constraints = tc ; body = ty } }

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
let pair a b      = p_constant C_record    [a; b]
let sum  a b      = p_constant C_variant   [a; b]
let map  k v      = p_constant C_map       [k; v]
let unit          = p_constant C_unit      []
let list   t      = p_constant C_list      [t]
let set    t      = p_constant C_set       [t]
let bool          = { tsrc = "built-in type" ; t = P_variable Stage_common.Constant.t_bool }
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
let tuple0        = p_constant C_record    []
let tuple1 a      = p_constant C_record    [a]
let tuple2 a b    = p_constant C_record    [a; b]
let tuple3 a b c  = p_constant C_record    [a; b; c]
