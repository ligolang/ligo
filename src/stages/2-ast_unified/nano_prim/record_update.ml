type 'e t =
  { struct_ : 'e
  ; label : Ligo_prim.Label.t
  ; update : 'e
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
