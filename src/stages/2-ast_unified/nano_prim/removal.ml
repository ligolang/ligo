type 'expr t =
  { item_expr : 'expr
  ; remove_kind : [ `Set | `Map ]
  ; collection : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
