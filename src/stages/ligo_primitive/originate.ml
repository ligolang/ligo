open Var

type 'expr t =
  { contract : Contract_var.t
  ; storage : 'expr
  ; key_hash : 'expr
  ; tez : 'expr
  }
[@@deriving equal, compare, yojson, hash, fold, map]

let fold_map f acc { contract; storage; key_hash; tez } =
  let acc, storage = f acc storage in
  let acc, key_hash = f acc key_hash in
  let acc, tez = f acc tez in
  acc, { contract; storage; key_hash; tez }


let pp pp_expr ppf { contract; storage; key_hash; tez } =
  Format.fprintf
    ppf
    "@[originate %a ~storage:%a ~key_hash:%a ~tez:%a@]"
    Contract_var.pp
    contract
    pp_expr
    storage
    pp_expr
    key_hash
    pp_expr
    tez
