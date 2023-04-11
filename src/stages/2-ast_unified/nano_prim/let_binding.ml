type ('pattern, 'expr, 'ty_expr) t =
  { is_rec : bool [@default false] [@sexp_drop_default.equal]
  ; type_params : Ligo_prim.Type_var.t Simple_utils.List.Ne.t option [@sexp.option]
  ; lhs : 'pattern Simple_utils.List.Ne.t
  ; rhs_type : 'ty_expr option [@sexp.option]
  ; rhs : 'expr
  ; body : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
