type ('expr, 'ty_expr, 'pattern) t =
  { type_params : Ligo_prim.Type_var.t Simple_utils.List.Ne.t option [@sexp.option]
  ; parameters : 'pattern Param.t list
  ; ret_type : 'ty_expr option [@sexp.option]
  ; body : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
