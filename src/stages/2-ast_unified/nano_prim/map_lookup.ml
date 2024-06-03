module Ne_list = Simple_utils.Ne_list

type 'expr t =
  { map : 'expr
  ; keys : 'expr Ne_list.t
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
