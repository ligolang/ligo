type ('expr, 'block) t =
  { block : 'block
  ; expr : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
