[@@@warning "-30"]

type incr_decr =
  | Increment
  | Decrement

and op = incr_decr Simple_utils.Location.wrap

and prefix =
  { pre_op : op
  ; variable : Ligo_prim.Value_var.t
  }

and postfix =
  { post_op : op
  ; variable : Ligo_prim.Value_var.t
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
