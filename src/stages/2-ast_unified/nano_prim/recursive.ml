module Ne_list = Simple_utils.Ne_list
module Value_var = Ligo_prim.Value_var

type ('ty_expr, 'lambda) specialized =
  { fun_name : Value_var.t
  ; fun_type : 'ty_expr
  ; lambda : 'lambda
  }
[@@deriving eq, compare, yojson, sexp, fold, iter, map, hash]

(* in this type, fun_type is either a user provided type, or a list extracted from parameters *)
type ('ty_expr, 'lambda) general =
  { fun_name : Value_var.t
  ; fun_type : 'ty_expr general_fun_type
  ; lambda : 'lambda
  }

and 'ty_expr general_fun_type =
  | Extracted of 'ty_expr Ne_list.t
  | User of 'ty_expr
[@@deriving eq, compare, yojson, sexp, fold, iter, map, hash]
