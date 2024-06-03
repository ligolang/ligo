module Ne_list = Simple_utils.Ne_list
module Value_var = Ligo_prim.Value_var
module Type_var = Ligo_prim.Type_var

type ('ty_expr, 'body, 'param) t =
  { is_rec : bool
  ; fun_name : Value_var.t
  ; type_params : Type_var.t Ne_list.t option
  ; parameters : 'param list
  ; ret_type : 'ty_expr option
  ; return : 'body
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
