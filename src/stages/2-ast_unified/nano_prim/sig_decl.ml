type 'sig_expr t =
  { name : Ligo_prim.Module_var.t
  ; extends : 'sig_expr list
  ; sig_expr : 'sig_expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
