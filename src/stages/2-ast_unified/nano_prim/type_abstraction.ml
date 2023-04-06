type 'expr t =
  { type_binder : Ligo_prim.Type_var.t
  ; result : 'expr
  }
[@@deriving eq, compare, yojson, sexp, fold, iter, map, compare, hash]
