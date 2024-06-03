module Ne_list = Simple_utils.Ne_list
module Type_var = Ligo_prim.Type_var

type ('expr, 'lhs, 'ty_expr) t =
  { is_rec : bool [@sexp.bool]
  ; type_params : Type_var.t Ne_list.t option [@sexp.option]
  ; pattern : 'lhs
  ; rhs_type : 'ty_expr option [@sexp.option]
  ; let_rhs : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
