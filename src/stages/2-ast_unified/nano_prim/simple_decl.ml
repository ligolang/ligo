module Ne_list = Simple_utils.Ne_list
module Type_var = Ligo_prim.Type_var

type ('binder, 'expr, 'ty_expr) t =
  { type_params : Type_var.t Ne_list.t option [@sexp.option]
  ; pattern : 'binder
  ; rhs_type : 'ty_expr option [@sexp.option]
  ; let_rhs : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
