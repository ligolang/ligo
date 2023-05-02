type ('e, 'dcl) t =
  { module_binder : Var.Module_var.t
  ; rhs : 'dcl
  ; let_result : 'e
  }
[@@deriving eq, compare, yojson, hash, fold, map]

let pp f h ppf { module_binder; rhs; let_result } =
  Format.fprintf
    ppf
    "@[module %a =@;<1 2>%a in@ %a@]"
    Var.Module_var.pp
    module_binder
    h
    rhs
    f
    let_result
