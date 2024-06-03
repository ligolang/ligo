module Ne_list = Simple_utils.Ne_list
module Type_var = Ligo_prim.Type_var

type ('pattern, 'expr, 'ty_expr) t =
  { is_rec : bool [@default false] [@sexp_drop_default.equal]
  ; type_params : Type_var.t Ne_list.t option [@sexp.option]
  ; lhs : 'pattern Ne_list.t
  ; rhs_type : 'ty_expr option [@sexp.option]
  ; rhs : 'expr
  ; body : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
