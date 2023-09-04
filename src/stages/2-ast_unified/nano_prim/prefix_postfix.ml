[@@@warning "-30"]

type incr_decr =
  | Increment
  | Decrement

and op = incr_decr Simple_utils.Location.wrap

and 'e prefix =
  { pre_op : op
  ; expr : 'e
  }

and 'e postfix =
  { post_op : op
  ; expr : 'e
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
