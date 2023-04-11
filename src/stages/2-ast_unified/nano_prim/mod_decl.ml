type 'mod_expr t =
  { name : Ligo_prim.Module_var.t
  ; mod_expr : 'mod_expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
