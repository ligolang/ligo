type ('lhs, 'ty) t =
  { module_path : 'lhs
  ; field : 'ty
  ; field_as_open : bool
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
