module Ne_list = Simple_utils.Ne_list
module Type_var = Ligo_prim.Type_var

type ('expr, 'ty_expr, 'pattern) t =
  { type_params : Type_var.t Ne_list.t option [@sexp.option]
  ; parameters : 'pattern Param.t list
  ; ret_type : 'ty_expr option [@sexp.option]
  ; body : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
