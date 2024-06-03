module Ne_list = Simple_utils.Ne_list

type ('lhs, 'ty) t =
  { constr : 'lhs
  ; type_args : 'ty Ne_list.t
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
