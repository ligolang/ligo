type 'expr t =
  { map : 'expr
  ; keys : 'expr Simple_utils.List.Ne.t
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
