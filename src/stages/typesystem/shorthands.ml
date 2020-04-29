open Core

let tc type_vars allowed_list =
  Core.C_typeclass (type_vars , allowed_list)

let forall binder f =
  let () = ignore binder in
  let freshvar = fresh_type_variable () in
  P_forall { binder = freshvar ; constraints = [] ; body = f (P_variable freshvar) }

let forall_tc binder f =
  let () = ignore binder in
  let freshvar = fresh_type_variable () in
  let (tc, ty) = f (P_variable freshvar) in
  P_forall { binder = freshvar ; constraints = tc ; body = ty }

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
let (-->) arg ret = P_constant (C_arrow  , [arg; ret])
let option t  = P_constant (C_option  , [t])
let pair a b  = P_constant (C_record , [a; b])
let sum  a b  = P_constant (C_variant, [a; b])
let map  k v  = P_constant (C_map   , [k; v])
let unit      = P_constant (C_unit    , [])
let list   t  = P_constant (C_list    , [t])
let set    t  = P_constant (C_set     , [t])
let bool      = P_variable (Stage_common.Constant.t_bool)
let string    = P_constant (C_string  , [])
let nat       = P_constant (C_nat     , [])
let mutez     = P_constant (C_mutez  , [])
let timestamp = P_constant (C_timestamp , [])
let int       = P_constant (C_int     , [])
let address   = P_constant (C_address , [])
let chain_id  = P_constant (C_chain_id , [])
let bytes     = P_constant (C_bytes   , [])
let key       = P_constant (C_key     , [])
let key_hash  = P_constant (C_key_hash  , [])
let signature = P_constant (C_signature , [])
let operation = P_constant (C_operation , [])
let contract t = P_constant (C_contract , [t])
let ( * ) a b = pair a b

(* These are used temporarily to de-curry functions that correspond to Michelson operators *)
let tuple0       = P_constant (C_record , [])
let tuple1 a     = P_constant (C_record , [a])
let tuple2 a b   = P_constant (C_record , [a; b])
let tuple3 a b c = P_constant (C_record , [a; b; c])
