type ('expr, 'pattern) t =
  { binder : 'pattern
  ; rhs : 'expr
  ; let_result : 'expr
  }
[@@deriving eq, compare, yojson, iter, fold, map, sexp, compare, hash]
