type 'expr t =
  { x : 'expr
  ; f : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
