type ('e, 't) t =
  { type_binder : Var.Type_var.t
  ; rhs : 't
  ; let_result : 'e
  }
[@@deriving eq, compare, yojson, hash, fold, iter, map]

let pp f g ppf { type_binder; rhs; let_result } =
  Format.fprintf
    ppf
    "@[let %a =@;<1 2>%a in@ %a@]"
    Var.Type_var.pp
    type_binder
    g
    rhs
    f
    let_result
