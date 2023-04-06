type ('expr, 'ty_expr) t =
  { type_decl : 'ty_expr Type_abstraction_decl.t
  ; body : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
