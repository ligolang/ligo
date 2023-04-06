type ('ty_expr, 'body, 'param) t =
  { is_rec : bool
  ; fun_name : Ligo_prim.Value_var.t
  ; type_params : Ligo_prim.Type_var.t Simple_utils.List.Ne.t option
  ; parameters : 'param list
  ; ret_type : 'ty_expr option
  ; return : 'body
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
