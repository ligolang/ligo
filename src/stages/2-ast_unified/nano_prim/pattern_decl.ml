type ('expr, 'pattern) t =
  { pattern : 'pattern
  ; expr : 'expr
  }
[@@deriving eq, compare, yojson, iter, fold, map, sexp, compare, hash]
