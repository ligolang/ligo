type ('lhs, 'ty) t =
  { constr : 'lhs
  ; type_args : 'ty Simple_utils.List.Ne.t
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
