module Ne_list = Simple_utils.Ne_list

type ('expr, 'statement) t =
  { initialiser : 'statement option
  ; condition : 'expr option
  ; afterthought : 'expr Ne_list.t option
  ; statement : 'statement option
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
