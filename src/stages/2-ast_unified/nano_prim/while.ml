type ('expr, 'block) t =
  { cond : 'expr
  ; block : 'block
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

let _ = of_yojson
