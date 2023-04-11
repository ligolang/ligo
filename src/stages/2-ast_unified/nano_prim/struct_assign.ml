type 'expr t =
  { lhs_expr : 'expr
  ; rhs_expr : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
