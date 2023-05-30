type 'sig_expr t =
  { name : Ligo_prim.Module_var.t
  ; sig_expr : 'sig_expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
