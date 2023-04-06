type ('expr, 'mod_expr) t =
  { module_name : Ligo_prim.Module_var.t
  ; rhs : 'mod_expr
  ; body : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
