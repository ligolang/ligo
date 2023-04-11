type 'ty_expr t =
  { name : Ligo_prim.Type_var.t
  ; params : Ligo_prim.Type_var.t Simple_utils.List.Ne.t option
  ; type_expr : 'ty_expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
