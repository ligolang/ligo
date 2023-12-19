type 'ty t = string list * 'ty * 'ty
[@@deriving yojson, map, fold, iter, sexp, eq, compare, hash]
