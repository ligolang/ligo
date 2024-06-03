module Ne_list = Simple_utils.Ne_list
module Type_var = Ligo_prim.Type_var

type 'ty_expr t =
  { name : Type_var.t
  ; params : Type_var.t Ne_list.t option
  ; type_expr : 'ty_expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
