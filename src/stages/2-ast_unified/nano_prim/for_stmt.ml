type ('expr, 'statement) t =
  { initialiser : 'statement option
  ; condition : 'expr option
  ; afterthought : 'expr Simple_utils.List.Ne.t option
  ; statement : 'statement option
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
