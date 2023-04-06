type 'expr t =
  { struct_ : 'expr
  ; label : Ligo_prim.Label.t
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
