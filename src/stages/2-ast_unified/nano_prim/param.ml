type 'pattern t =
  { param_kind : [ `Var | `Const ]
  ; pattern : 'pattern
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
