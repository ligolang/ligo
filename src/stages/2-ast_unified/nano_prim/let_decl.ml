type ('expr, 'lhs, 'ty_expr) t =
  { is_rec : bool [@sexp.bool]
  ; type_params : Ligo_prim.Type_var.t Simple_utils.List.Ne.t option [@sexp.option]
  ; pattern : 'lhs
  ; rhs_type : 'ty_expr option [@sexp.option]
  ; let_rhs : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
