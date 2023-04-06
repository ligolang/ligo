type 'ty_expr t =
  { name : Ligo_prim.Type_var.t
  ; type_expr : 'ty_expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
